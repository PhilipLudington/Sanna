//! Verification Engine
//!
//! This module orchestrates the verification process, coordinating between:
//! - Obligation generation from specifications
//! - SMT encoding and solver invocation
//! - Result caching and aggregation
//!
//! ## Usage
//!
//! ```zig
//! var engine = VerificationEngine.init(allocator, .{});
//! defer engine.deinit();
//!
//! const report = try engine.verifyBinding(spec_binding);
//! if (report.allPassed()) {
//!     // All specifications verified!
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtExpr = SmtTypes.SmtExpr;
const SmtCommand = SmtTypes.SmtCommand;
const SmtScript = SmtTypes.SmtScript;
const SmtLibWriter = @import("SmtLibWriter.zig").SmtLibWriter;
const Z3Solver = @import("Z3Solver.zig").Z3Solver;
const SatStatus = @import("Z3Solver.zig").SatStatus;
const ProofObligation = @import("ProofObligation.zig");
const Obligation = ProofObligation.ProofObligation;
const ObligationSet = ProofObligation.ObligationSet;
const VerificationStatus = ProofObligation.VerificationStatus;
const ObligationGenerator = @import("ObligationGenerator.zig").ObligationGenerator;
const VerificationCache = @import("VerificationCache.zig").VerificationCache;
const CacheEntry = @import("VerificationCache.zig").CacheEntry;
const VerificationResult = @import("VerificationResult.zig");
const VerificationReport = VerificationResult.VerificationReport;
const ObligationResult = VerificationResult.ObligationResult;
const Diagnostic = VerificationResult.Diagnostic;
const SpecBinding = @import("../specs/root.zig").SpecBinding;

// ============================================================================
// Verification Engine
// ============================================================================

/// Main verification engine orchestrator
pub const VerificationEngine = struct {
    allocator: Allocator,
    /// Configuration
    config: Config,
    /// Z3 solver instance
    solver: Z3Solver,
    /// Verification cache
    cache: VerificationCache,
    /// SMT-LIB writer for generating solver input
    writer: SmtLibWriter,
    /// Engine statistics
    stats: EngineStats,

    /// Engine configuration
    pub const Config = struct {
        /// Timeout per obligation in milliseconds
        timeout_ms: u32 = 30000,
        /// Whether to use caching
        use_cache: bool = true,
        /// Cache directory (null for memory-only)
        cache_dir: ?[]const u8 = null,
        /// SMT logic to use
        logic: []const u8 = "ALL",
        /// Whether to produce models on failure
        produce_models: bool = true,
        /// Whether to skip @trusted specifications
        respect_trusted: bool = true,
        /// Whether to handle @admitted as success
        respect_admitted: bool = true,
        /// Maximum parallel verifications (0 = sequential)
        max_parallel: u32 = 0,
        /// Verbosity level (0 = quiet, 1 = normal, 2 = verbose)
        verbosity: u32 = 1,
    };

    pub fn init(allocator: Allocator, config: Config) VerificationEngine {
        var engine = VerificationEngine{
            .allocator = allocator,
            .config = config,
            .solver = Z3Solver.init(allocator, .{
                .timeout_ms = config.timeout_ms,
                .produce_models = config.produce_models,
            }),
            .cache = VerificationCache.init(allocator),
            .writer = SmtLibWriter.init(allocator),
            .stats = .{},
        };

        // Configure cache
        if (config.cache_dir) |dir| {
            engine.cache.setCacheDir(dir) catch {};
        }

        return engine;
    }

    pub fn deinit(self: *VerificationEngine) void {
        self.solver.deinit();
        self.cache.deinit();
        self.writer.deinit();
    }

    // ========================================================================
    // Main Verification API
    // ========================================================================

    /// Verify all specifications in a SpecBinding
    pub fn verifyBinding(self: *VerificationEngine, binding: *const SpecBinding) !VerificationReport {
        var report = VerificationReport.init(self.allocator, "verification");

        // Generate obligations
        var generator = ObligationGenerator.init(self.allocator);
        defer generator.deinit();

        try generator.generateFromBinding(binding);

        // Check for generation errors
        if (generator.hasErrors()) {
            for (generator.getErrors()) |err| {
                const diag = Diagnostic.err(err.message, err.span);
                try report.results.append(self.allocator, blk: {
                    var result = VerificationResult.VerificationResult.init("generation_error");
                    result.status = .failed;
                    try result.diagnostics.append(self.allocator, diag);
                    break :blk result;
                });
            }
            report.complete();
            return report;
        }

        // Verify each obligation
        const obligations = generator.getObligations();
        for (obligations.ordered.items) |ob| {
            const ob_result = try self.verifyObligation(ob);
            try self.addObligationToReport(&report, ob, &ob_result);
        }

        report.complete();
        self.stats.total_verifications += 1;
        return report;
    }

    /// Verify a single obligation
    pub fn verifyObligation(self: *VerificationEngine, obligation: *Obligation) !ObligationResult {
        const start_time = std.time.milliTimestamp();

        // Check cache first
        if (self.config.use_cache) {
            if (self.cache.lookup(obligation.name)) |entry| {
                if (entry.isValid(24 * 60 * 60 * 1000)) { // 24 hours
                    self.stats.cache_hits += 1;
                    return ObligationResult{
                        .obligation_id = obligation.id,
                        .name = obligation.name,
                        .kind = obligation.kind,
                        .status = entry.status,
                        .span = obligation.span,
                        .solver_status = null,
                        .model = null,
                        .time_ms = entry.verification_time_ms,
                        .error_message = null,
                    };
                }
            }
        }

        // Generate SMT-LIB script
        const script = try self.generateSmtScript(obligation);
        defer self.allocator.free(script);

        // Run solver
        const solver_result = try self.solver.checkSatText(script);

        const end_time = std.time.milliTimestamp();
        const duration_ms: u64 = @intCast(end_time - start_time);

        // Convert solver result to obligation result
        var ob_result = self.convertSolverResult(obligation, &solver_result, duration_ms);

        // Update obligation status
        obligation.status = ob_result.status;

        // Cache the result
        if (self.config.use_cache and ob_result.status.isTerminal()) {
            const cache_entry = CacheEntry.init(ob_result.status, duration_ms);
            try self.cache.store(obligation.name, cache_entry);
        }

        self.stats.obligations_verified += 1;
        self.stats.total_time_ms += duration_ms;

        return ob_result;
    }

    /// Generate SMT-LIB script for an obligation
    fn generateSmtScript(self: *VerificationEngine, obligation: *const Obligation) ![]u8 {
        self.writer.reset();

        // Set logic
        const set_logic = SmtCommand.setLogic(self.config.logic);
        try self.writer.writeCommand(&set_logic);
        try self.writer.buffer.append(self.writer.allocator, '\n');

        // Declare free variables
        for (obligation.free_vars) |fv| {
            const decl = SmtTypes.SmtDecl.constDecl(fv.name, fv.sort);
            try self.writer.writeDeclaration(&decl);
            try self.writer.buffer.append(self.writer.allocator, '\n');
        }

        // Add declarations
        for (obligation.declarations) |decl| {
            try self.writer.writeDeclaration(&decl);
            try self.writer.buffer.append(self.writer.allocator, '\n');
        }

        // Assert assumptions
        for (obligation.assumptions) |assumption| {
            const assert_cmd = SmtCommand.assert(assumption);
            try self.writer.writeCommand(&assert_cmd);
            try self.writer.buffer.append(self.writer.allocator, '\n');
        }

        // Assert negation of goal (we check for unsat)
        // If unsat, the goal is valid (no counterexample exists)
        const neg_goal = try self.negateGoal(obligation.goal);
        const assert_neg = SmtCommand.assert(neg_goal);
        try self.writer.writeCommand(&assert_neg);
        try self.writer.buffer.append(self.writer.allocator, '\n');

        // Check sat
        const check_sat = SmtCommand.checkSat();
        try self.writer.writeCommand(&check_sat);
        try self.writer.buffer.append(self.writer.allocator, '\n');

        // Get model on sat (for counterexample)
        if (self.config.produce_models) {
            const get_model = SmtCommand.getModel();
            try self.writer.writeCommand(&get_model);
            try self.writer.buffer.append(self.writer.allocator, '\n');
        }

        return self.allocator.dupe(u8, self.writer.getOutput());
    }

    /// Negate a goal expression
    fn negateGoal(self: *VerificationEngine, goal: *const SmtExpr) !*const SmtExpr {
        _ = self;
        // For now, return the goal as-is since we're checking satisfiability
        // of the negation in the SMT script generation
        const arena = std.heap.page_allocator;
        const neg = try arena.create(SmtExpr);
        neg.* = SmtExpr.notExpr(goal);
        return neg;
    }

    /// Convert solver result to obligation result
    fn convertSolverResult(
        self: *VerificationEngine,
        obligation: *const Obligation,
        solver_result: *const @import("Z3Solver.zig").SolverResult,
        duration_ms: u64,
    ) ObligationResult {
        _ = self;

        return switch (solver_result.status) {
            // unsat means the negation of our goal is unsatisfiable
            // therefore our goal is valid (verified!)
            .unsat => ObligationResult.verified(
                obligation.id,
                obligation.name,
                obligation.kind,
                duration_ms,
            ),
            // sat means we found a counterexample
            .sat => ObligationResult.failed(
                obligation.id,
                obligation.name,
                obligation.kind,
                solver_result.model,
                duration_ms,
            ),
            .timeout => ObligationResult.timeout(
                obligation.id,
                obligation.name,
                obligation.kind,
                duration_ms,
            ),
            .unknown => ObligationResult.unknown(
                obligation.id,
                obligation.name,
                obligation.kind,
                duration_ms,
            ),
            .@"error" => ObligationResult.withError(
                obligation.id,
                obligation.name,
                obligation.kind,
                solver_result.error_message orelse "Unknown solver error",
            ),
        };
    }

    /// Add an obligation result to the report
    fn addObligationToReport(
        self: *VerificationEngine,
        report: *VerificationReport,
        obligation: *const Obligation,
        ob_result: *const ObligationResult,
    ) !void {
        _ = self;

        // Find or create result for this spec
        const spec_name = obligation.source.name;

        // Check if we already have a result for this spec
        for (report.results.items) |*existing| {
            if (std.mem.eql(u8, existing.spec_name, spec_name)) {
                try existing.addObligation(report.allocator, ob_result.*);

                // Update overall status (worst case wins)
                if (ob_result.status == .failed) {
                    existing.status = .failed;
                } else if (ob_result.status == .timeout and existing.status != .failed) {
                    existing.status = .timeout;
                }

                existing.time_ms += ob_result.time_ms;
                return;
            }
        }

        // Create new result
        var result = VerificationResult.VerificationResult.init(spec_name);
        result.status = ob_result.status;
        result.span = obligation.span;
        result.time_ms = ob_result.time_ms;
        try result.addObligation(report.allocator, ob_result.*);
        try report.addResult(result);
    }

    // ========================================================================
    // Utility Methods
    // ========================================================================

    /// Check if Z3 solver is available
    pub fn isSolverAvailable(self: *VerificationEngine) bool {
        return self.solver.isAvailable();
    }

    /// Get solver version
    pub fn getSolverVersion(self: *VerificationEngine) !?[]u8 {
        return self.solver.getVersion();
    }

    /// Get engine statistics
    pub fn getStats(self: *const VerificationEngine) EngineStats {
        return self.stats;
    }

    /// Reset statistics
    pub fn resetStats(self: *VerificationEngine) void {
        self.stats = .{};
    }

    /// Get cache statistics
    pub fn getCacheStats(self: *const VerificationEngine) @import("VerificationCache.zig").CacheStats {
        return self.cache.getStats();
    }

    /// Clear the verification cache
    pub fn clearCache(self: *VerificationEngine) void {
        self.cache.clear();
    }
};

/// Engine statistics
pub const EngineStats = struct {
    /// Total verification runs
    total_verifications: u64 = 0,
    /// Total obligations verified
    obligations_verified: u64 = 0,
    /// Cache hits
    cache_hits: u64 = 0,
    /// Total verification time in ms
    total_time_ms: u64 = 0,
    /// Number of verified obligations
    verified_count: u64 = 0,
    /// Number of failed obligations
    failed_count: u64 = 0,
    /// Number of timeout obligations
    timeout_count: u64 = 0,

    pub fn averageTimeMs(self: EngineStats) f64 {
        if (self.obligations_verified == 0) return 0.0;
        const total: f64 = @floatFromInt(self.total_time_ms);
        const count: f64 = @floatFromInt(self.obligations_verified);
        return total / count;
    }

    pub fn cacheHitRate(self: EngineStats) f64 {
        if (self.obligations_verified == 0) return 0.0;
        const hits: f64 = @floatFromInt(self.cache_hits);
        const total: f64 = @floatFromInt(self.obligations_verified + self.cache_hits);
        return hits / total;
    }
};

// ============================================================================
// Convenience Functions
// ============================================================================

/// Quick verification check - returns true if all specs pass
pub fn quickVerify(allocator: Allocator, binding: *const SpecBinding) !bool {
    var engine = VerificationEngine.init(allocator, .{
        .timeout_ms = 10000,
        .use_cache = false,
        .verbosity = 0,
    });
    defer engine.deinit();

    const report = try engine.verifyBinding(binding);
    return report.allPassed();
}

// ============================================================================
// Tests
// ============================================================================

test "VerificationEngine initialization" {
    const testing = std.testing;

    var engine = VerificationEngine.init(testing.allocator, .{});
    defer engine.deinit();

    try testing.expectEqual(@as(u32, 30000), engine.config.timeout_ms);
    try testing.expect(engine.config.use_cache);
}

test "VerificationEngine config" {
    const testing = std.testing;

    var engine = VerificationEngine.init(testing.allocator, .{
        .timeout_ms = 5000,
        .use_cache = false,
        .logic = "QF_LIA",
    });
    defer engine.deinit();

    try testing.expectEqual(@as(u32, 5000), engine.config.timeout_ms);
    try testing.expect(!engine.config.use_cache);
    try testing.expectEqualStrings("QF_LIA", engine.config.logic);
}

test "EngineStats calculations" {
    const testing = std.testing;

    var stats = EngineStats{};

    // No verifications = 0 average
    try testing.expectEqual(@as(f64, 0.0), stats.averageTimeMs());

    // With data
    stats.obligations_verified = 10;
    stats.total_time_ms = 1000;
    try testing.expectEqual(@as(f64, 100.0), stats.averageTimeMs());

    // Cache hit rate
    stats.cache_hits = 5;
    // total = 10 verified + 5 cache hits = 15
    // rate = 5/15 = 0.333...
    try testing.expect(stats.cacheHitRate() > 0.33);
    try testing.expect(stats.cacheHitRate() < 0.34);
}

test "VerificationEngine stats" {
    const testing = std.testing;

    var engine = VerificationEngine.init(testing.allocator, .{});
    defer engine.deinit();

    const stats = engine.getStats();
    try testing.expectEqual(@as(u64, 0), stats.total_verifications);
    try testing.expectEqual(@as(u64, 0), stats.obligations_verified);

    engine.resetStats();
    try testing.expectEqual(@as(u64, 0), engine.stats.total_verifications);
}
