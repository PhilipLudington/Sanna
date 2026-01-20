//! Verification Result Types
//!
//! This module defines the data structures for representing verification results,
//! including outcomes, counterexamples, and diagnostic information.
//!
//! ## Result Hierarchy
//!
//! - VerificationReport: Complete report for a module/project
//!   - VerificationResult: Result for a single specification
//!     - ObligationResult: Result for a single proof obligation

const std = @import("std");
const Allocator = std.mem.Allocator;
const Span = @import("../lexer/root.zig").Span;
const ProofObligation = @import("ProofObligation.zig");
const VerificationStatus = ProofObligation.VerificationStatus;
const ObligationKind = ProofObligation.ObligationKind;
const Z3Solver = @import("Z3Solver.zig");
const Model = Z3Solver.Model;

// ============================================================================
// Verification Report
// ============================================================================

/// A complete verification report for a module or project
pub const VerificationReport = struct {
    allocator: Allocator,
    /// Module/project name
    name: []const u8,
    /// Individual specification results
    results: std.ArrayListUnmanaged(VerificationResult),
    /// Overall summary
    summary: Summary,
    /// Verification start time
    start_time: i64,
    /// Verification end time
    end_time: i64,
    /// Solver configuration used
    solver_config: ?SolverConfig,

    pub fn init(allocator: Allocator, name: []const u8) VerificationReport {
        return .{
            .allocator = allocator,
            .name = name,
            .results = .{},
            .summary = .{},
            .start_time = std.time.milliTimestamp(),
            .end_time = 0,
            .solver_config = null,
        };
    }

    pub fn deinit(self: *VerificationReport) void {
        for (self.results.items) |*result| {
            result.deinit(self.allocator);
        }
        self.results.deinit(self.allocator);
    }

    /// Add a verification result
    pub fn addResult(self: *VerificationReport, result: VerificationResult) !void {
        try self.results.append(self.allocator, result);
        self.updateSummary(&result);
    }

    /// Mark the report as complete
    pub fn complete(self: *VerificationReport) void {
        self.end_time = std.time.milliTimestamp();
    }

    /// Get duration in milliseconds
    pub fn getDurationMs(self: *const VerificationReport) i64 {
        const end = if (self.end_time > 0) self.end_time else std.time.milliTimestamp();
        return end - self.start_time;
    }

    /// Check if all verifications passed
    pub fn allPassed(self: *const VerificationReport) bool {
        return self.summary.failed == 0 and self.summary.timeout == 0;
    }

    /// Get overall status
    pub fn getOverallStatus(self: *const VerificationReport) OverallStatus {
        if (self.summary.failed > 0) return .failed;
        if (self.summary.timeout > 0) return .timeout;
        if (self.summary.unknown > 0) return .partial;
        if (self.summary.verified + self.summary.trusted + self.summary.admitted == self.summary.total) return .verified;
        return .partial;
    }

    fn updateSummary(self: *VerificationReport, result: *const VerificationResult) void {
        self.summary.total += 1;
        switch (result.status) {
            .verified => self.summary.verified += 1,
            .failed => self.summary.failed += 1,
            .timeout => self.summary.timeout += 1,
            .unknown => self.summary.unknown += 1,
            .trusted => self.summary.trusted += 1,
            .admitted => self.summary.admitted += 1,
            .skipped => self.summary.skipped += 1,
            else => {},
        }
    }

    /// Summary statistics
    pub const Summary = struct {
        total: usize = 0,
        verified: usize = 0,
        failed: usize = 0,
        timeout: usize = 0,
        unknown: usize = 0,
        trusted: usize = 0,
        admitted: usize = 0,
        skipped: usize = 0,

        pub fn successRate(self: Summary) f64 {
            if (self.total == 0) return 1.0;
            const successful: f64 = @floatFromInt(self.verified + self.trusted + self.admitted);
            const total: f64 = @floatFromInt(self.total);
            return successful / total;
        }
    };

    /// Overall verification status
    pub const OverallStatus = enum {
        /// All specifications verified
        verified,
        /// Some specifications failed
        failed,
        /// Some specifications timed out
        timeout,
        /// Some specifications could not be verified
        partial,
    };
};

/// Solver configuration used for verification
pub const SolverConfig = struct {
    solver_name: []const u8,
    solver_version: []const u8,
    timeout_ms: u32,
    logic: ?[]const u8,
};

// ============================================================================
// Verification Result
// ============================================================================

/// Result of verifying a single specification
pub const VerificationResult = struct {
    /// Specification name (fully qualified)
    spec_name: []const u8,
    /// Source location
    span: ?Span,
    /// Overall verification status
    status: VerificationStatus,
    /// Individual obligation results
    obligations: std.ArrayListUnmanaged(ObligationResult),
    /// Counterexample (if failed)
    counterexample: ?Counterexample,
    /// Diagnostic messages
    diagnostics: std.ArrayListUnmanaged(Diagnostic),
    /// Verification time in milliseconds
    time_ms: u64,
    /// Whether this result was cached
    from_cache: bool,

    pub fn init(spec_name: []const u8) VerificationResult {
        return .{
            .spec_name = spec_name,
            .span = null,
            .status = .pending,
            .obligations = .{},
            .counterexample = null,
            .diagnostics = .{},
            .time_ms = 0,
            .from_cache = false,
        };
    }

    pub fn deinit(self: *VerificationResult, allocator: Allocator) void {
        self.obligations.deinit(allocator);
        if (self.counterexample) |*ce| {
            ce.deinit(allocator);
        }
        for (self.diagnostics.items) |*diag| {
            allocator.free(diag.message);
        }
        self.diagnostics.deinit(allocator);
    }

    /// Add an obligation result
    pub fn addObligation(self: *VerificationResult, allocator: Allocator, result: ObligationResult) !void {
        try self.obligations.append(allocator, result);
    }

    /// Add a diagnostic message
    pub fn addDiagnostic(self: *VerificationResult, allocator: Allocator, diag: Diagnostic) !void {
        try self.diagnostics.append(allocator, diag);
    }

    /// Check if verified successfully
    pub fn isVerified(self: *const VerificationResult) bool {
        return self.status.isSuccess();
    }

    /// Check if failed
    pub fn isFailed(self: *const VerificationResult) bool {
        return self.status.isFailure();
    }

    /// Get number of failed obligations
    pub fn getFailedCount(self: *const VerificationResult) usize {
        var count: usize = 0;
        for (self.obligations.items) |ob| {
            if (ob.status == .failed) count += 1;
        }
        return count;
    }
};

// ============================================================================
// Obligation Result
// ============================================================================

/// Result of verifying a single proof obligation
pub const ObligationResult = struct {
    /// Obligation ID
    obligation_id: u64,
    /// Obligation name
    name: []const u8,
    /// Obligation kind
    kind: ObligationKind,
    /// Verification status
    status: VerificationStatus,
    /// Source location
    span: ?Span,
    /// Solver result (sat/unsat/unknown)
    solver_status: ?Z3Solver.SatStatus,
    /// Counterexample model (if sat)
    model: ?Model,
    /// Verification time in milliseconds
    time_ms: u64,
    /// Error message (if any)
    error_message: ?[]const u8,

    pub fn verified(id: u64, name: []const u8, kind: ObligationKind, time_ms: u64) ObligationResult {
        return .{
            .obligation_id = id,
            .name = name,
            .kind = kind,
            .status = .verified,
            .span = null,
            .solver_status = .unsat,
            .model = null,
            .time_ms = time_ms,
            .error_message = null,
        };
    }

    pub fn failed(id: u64, name: []const u8, kind: ObligationKind, model: ?Model, time_ms: u64) ObligationResult {
        return .{
            .obligation_id = id,
            .name = name,
            .kind = kind,
            .status = .failed,
            .span = null,
            .solver_status = .sat,
            .model = model,
            .time_ms = time_ms,
            .error_message = null,
        };
    }

    pub fn timeout(id: u64, name: []const u8, kind: ObligationKind, time_ms: u64) ObligationResult {
        return .{
            .obligation_id = id,
            .name = name,
            .kind = kind,
            .status = .timeout,
            .span = null,
            .solver_status = .timeout,
            .model = null,
            .time_ms = time_ms,
            .error_message = null,
        };
    }

    pub fn unknown(id: u64, name: []const u8, kind: ObligationKind, time_ms: u64) ObligationResult {
        return .{
            .obligation_id = id,
            .name = name,
            .kind = kind,
            .status = .unknown,
            .span = null,
            .solver_status = .unknown,
            .model = null,
            .time_ms = time_ms,
            .error_message = null,
        };
    }

    pub fn withError(id: u64, name: []const u8, kind: ObligationKind, err: []const u8) ObligationResult {
        return .{
            .obligation_id = id,
            .name = name,
            .kind = kind,
            .status = .failed,
            .span = null,
            .solver_status = .@"error",
            .model = null,
            .time_ms = 0,
            .error_message = err,
        };
    }
};

// ============================================================================
// Counterexample
// ============================================================================

/// A counterexample that shows why a specification failed
pub const Counterexample = struct {
    /// Variable assignments
    assignments: std.StringHashMapUnmanaged(Value),
    /// Pre-state values (for old() expressions)
    pre_state: std.StringHashMapUnmanaged(Value),
    /// Post-state values
    post_state: std.StringHashMapUnmanaged(Value),
    /// Result value (if applicable)
    result_value: ?Value,
    /// Raw model from solver
    raw_model: ?[]const u8,

    pub fn init() Counterexample {
        return .{
            .assignments = .{},
            .pre_state = .{},
            .post_state = .{},
            .result_value = null,
            .raw_model = null,
        };
    }

    pub fn deinit(self: *Counterexample, allocator: Allocator) void {
        var iter = self.assignments.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        self.assignments.deinit(allocator);

        iter = self.pre_state.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        self.pre_state.deinit(allocator);

        iter = self.post_state.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        self.post_state.deinit(allocator);

        if (self.result_value) |*rv| {
            rv.deinit(allocator);
        }

        if (self.raw_model) |rm| {
            allocator.free(rm);
        }
    }

    /// Add an assignment
    pub fn addAssignment(self: *Counterexample, allocator: Allocator, name: []const u8, value: Value) !void {
        try self.assignments.put(allocator, name, value);
    }

    /// Get a human-readable description
    pub fn describe(self: *const Counterexample, allocator: Allocator) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(allocator);

        try buf.appendSlice(allocator, "Counterexample:\n");

        if (self.pre_state.count() > 0) {
            try buf.appendSlice(allocator, "  Pre-state:\n");
            var iter = self.pre_state.iterator();
            while (iter.next()) |entry| {
                const line = try std.fmt.allocPrint(allocator, "    {s} = {s}\n", .{ entry.key_ptr.*, entry.value_ptr.toString() });
                defer allocator.free(line);
                try buf.appendSlice(allocator, line);
            }
        }

        if (self.assignments.count() > 0) {
            try buf.appendSlice(allocator, "  Assignments:\n");
            var iter = self.assignments.iterator();
            while (iter.next()) |entry| {
                const line = try std.fmt.allocPrint(allocator, "    {s} = {s}\n", .{ entry.key_ptr.*, entry.value_ptr.toString() });
                defer allocator.free(line);
                try buf.appendSlice(allocator, line);
            }
        }

        if (self.result_value) |rv| {
            const line = try std.fmt.allocPrint(allocator, "  Result: {s}\n", .{rv.toString()});
            defer allocator.free(line);
            try buf.appendSlice(allocator, line);
        }

        return buf.toOwnedSlice(allocator);
    }
};

/// A value in a counterexample
pub const Value = union(enum) {
    bool_val: bool,
    int_val: i64,
    real_val: f64,
    string_val: []const u8,
    unknown: []const u8,

    pub fn deinit(self: *Value, allocator: Allocator) void {
        switch (self.*) {
            .string_val => |s| allocator.free(s),
            .unknown => |s| allocator.free(s),
            else => {},
        }
    }

    pub fn toString(self: *const Value) []const u8 {
        return switch (self.*) {
            .bool_val => |v| if (v) "true" else "false",
            .int_val => "int", // Would need formatting
            .real_val => "real", // Would need formatting
            .string_val => |s| s,
            .unknown => |s| s,
        };
    }
};

// ============================================================================
// Diagnostic
// ============================================================================

/// A diagnostic message from verification
pub const Diagnostic = struct {
    /// Severity level
    severity: Severity,
    /// Message
    message: []const u8,
    /// Source location
    span: ?Span,
    /// Category
    category: Category,

    pub const Severity = enum {
        @"error",
        warning,
        info,
        hint,
    };

    pub const Category = enum {
        /// Verification failure
        verification,
        /// Encoding issue
        encoding,
        /// Solver issue
        solver,
        /// Performance warning
        performance,
        /// General
        general,
    };

    pub fn err(message: []const u8, span: ?Span) Diagnostic {
        return .{
            .severity = .@"error",
            .message = message,
            .span = span,
            .category = .verification,
        };
    }

    pub fn warning(message: []const u8, span: ?Span) Diagnostic {
        return .{
            .severity = .warning,
            .message = message,
            .span = span,
            .category = .general,
        };
    }

    pub fn info(message: []const u8) Diagnostic {
        return .{
            .severity = .info,
            .message = message,
            .span = null,
            .category = .general,
        };
    }
};

// ============================================================================
// Result Formatting
// ============================================================================

/// Format a verification report for display
pub fn formatReport(allocator: Allocator, report: *const VerificationReport) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    // Helper to append formatted strings
    const appendFmt = struct {
        fn call(b: *std.ArrayListUnmanaged(u8), alloc: Allocator, comptime fmt: []const u8, args: anytype) !void {
            const s = try std.fmt.allocPrint(alloc, fmt, args);
            defer alloc.free(s);
            try b.appendSlice(alloc, s);
        }
    }.call;

    try appendFmt(&buf, allocator, "Verification Report: {s}\n", .{report.name});
    try buf.appendSlice(allocator, "=" ** 60 ++ "\n");

    // Summary
    const summary = report.summary;
    try appendFmt(&buf, allocator, "\nSummary: {d}/{d} verified", .{ summary.verified, summary.total });
    if (summary.failed > 0) {
        try appendFmt(&buf, allocator, ", {d} failed", .{summary.failed});
    }
    if (summary.timeout > 0) {
        try appendFmt(&buf, allocator, ", {d} timeout", .{summary.timeout});
    }
    if (summary.trusted > 0) {
        try appendFmt(&buf, allocator, ", {d} trusted", .{summary.trusted});
    }
    if (summary.admitted > 0) {
        try appendFmt(&buf, allocator, ", {d} admitted", .{summary.admitted});
    }
    try buf.appendSlice(allocator, "\n");

    // Duration
    try appendFmt(&buf, allocator, "Duration: {d}ms\n", .{report.getDurationMs()});

    // Individual results
    if (report.results.items.len > 0) {
        try buf.appendSlice(allocator, "\nResults:\n");
        for (report.results.items) |result| {
            const status_str = result.status.description();
            try appendFmt(&buf, allocator, "  [{s}] {s} ({d}ms)\n", .{
                status_str,
                result.spec_name,
                result.time_ms,
            });

            // Show failed obligations
            for (result.obligations.items) |ob| {
                if (ob.status == .failed) {
                    try appendFmt(&buf, allocator, "    - {s}: {s}\n", .{ ob.name, ob.kind.description() });
                }
            }
        }
    }

    return buf.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "VerificationReport initialization" {
    const testing = std.testing;

    var report = VerificationReport.init(testing.allocator, "test_module");
    defer report.deinit();

    try testing.expectEqualStrings("test_module", report.name);
    try testing.expectEqual(@as(usize, 0), report.summary.total);
    try testing.expect(report.allPassed());
}

test "VerificationResult status checks" {
    var result = VerificationResult.init("test.spec");

    result.status = .verified;
    try std.testing.expect(result.isVerified());
    try std.testing.expect(!result.isFailed());

    result.status = .failed;
    try std.testing.expect(!result.isVerified());
    try std.testing.expect(result.isFailed());
}

test "ObligationResult constructors" {
    const testing = std.testing;

    const verified = ObligationResult.verified(1, "test", .postcondition_verification, 100);
    try testing.expectEqual(VerificationStatus.verified, verified.status);
    try testing.expectEqual(@as(u64, 100), verified.time_ms);

    const failed = ObligationResult.failed(2, "test", .postcondition_verification, null, 200);
    try testing.expectEqual(VerificationStatus.failed, failed.status);

    const timeout_result = ObligationResult.timeout(3, "test", .postcondition_verification, 30000);
    try testing.expectEqual(VerificationStatus.timeout, timeout_result.status);
}

test "Diagnostic construction" {
    const diag = Diagnostic.err("Test error", null);
    try std.testing.expectEqual(Diagnostic.Severity.@"error", diag.severity);
    try std.testing.expectEqualStrings("Test error", diag.message);

    const warn = Diagnostic.warning("Test warning", null);
    try std.testing.expectEqual(Diagnostic.Severity.warning, warn.severity);
}

test "VerificationReport summary" {
    const testing = std.testing;

    var report = VerificationReport.init(testing.allocator, "test");
    defer report.deinit();

    var result1 = VerificationResult.init("spec1");
    result1.status = .verified;
    try report.addResult(result1);

    var result2 = VerificationResult.init("spec2");
    result2.status = .verified;
    try report.addResult(result2);

    var result3 = VerificationResult.init("spec3");
    result3.status = .failed;
    try report.addResult(result3);

    try testing.expectEqual(@as(usize, 3), report.summary.total);
    try testing.expectEqual(@as(usize, 2), report.summary.verified);
    try testing.expectEqual(@as(usize, 1), report.summary.failed);
    try testing.expect(!report.allPassed());
    try testing.expectEqual(VerificationReport.OverallStatus.failed, report.getOverallStatus());
}
