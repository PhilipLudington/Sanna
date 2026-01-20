//! Z3 Solver Integration
//!
//! This module provides integration with the Z3 SMT solver via subprocess.
//! It handles spawning Z3, sending SMT-LIB input, and parsing the output.
//!
//! ## Usage
//!
//! ```zig
//! var solver = Z3Solver.init(allocator, .{});
//! defer solver.deinit();
//!
//! const result = try solver.checkSat(smt_script);
//! switch (result.status) {
//!     .sat => // Satisfiable, counterexample available
//!     .unsat => // Unsatisfiable (proof found!)
//!     .unknown => // Solver gave up
//!     .timeout => // Hit time limit
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtScript = SmtTypes.SmtScript;
const SmtLibWriter = @import("SmtLibWriter.zig").SmtLibWriter;

// ============================================================================
// Z3 Solver
// ============================================================================

/// Z3 solver integration via subprocess
pub const Z3Solver = struct {
    allocator: Allocator,
    /// Configuration options
    config: Config,
    /// Path to Z3 executable
    z3_path: []const u8,
    /// Last solver output (for debugging)
    last_output: ?[]u8,
    /// Last solver error output
    last_error: ?[]u8,

    /// Solver configuration
    pub const Config = struct {
        /// Timeout in milliseconds (0 = no timeout)
        timeout_ms: u32 = 30000,
        /// Memory limit in MB (0 = no limit)
        memory_limit_mb: u32 = 0,
        /// Path to Z3 executable (null = find in PATH)
        z3_path: ?[]const u8 = null,
        /// Whether to produce models on sat
        produce_models: bool = true,
        /// Whether to produce unsat cores on unsat
        produce_unsat_cores: bool = false,
        /// Logic to use (null = let Z3 decide)
        logic: ?[]const u8 = null,
        /// Additional Z3 options
        extra_options: []const [2][]const u8 = &.{},
    };

    pub fn init(allocator: Allocator, config: Config) Z3Solver {
        return .{
            .allocator = allocator,
            .config = config,
            .z3_path = config.z3_path orelse "z3",
            .last_output = null,
            .last_error = null,
        };
    }

    pub fn deinit(self: *Z3Solver) void {
        if (self.last_output) |output| {
            self.allocator.free(output);
        }
        if (self.last_error) |err| {
            self.allocator.free(err);
        }
    }

    /// Check if Z3 is available
    pub fn isAvailable(self: *Z3Solver) bool {
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &.{ self.z3_path, "-version" },
        }) catch return false;

        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);

        return result.term.Exited == 0;
    }

    /// Get Z3 version string
    pub fn getVersion(self: *Z3Solver) !?[]u8 {
        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &.{ self.z3_path, "-version" },
        }) catch return null;

        defer self.allocator.free(result.stderr);

        if (result.term.Exited != 0) {
            self.allocator.free(result.stdout);
            return null;
        }

        // Parse version from output like "Z3 version 4.8.17 - 64 bit"
        return result.stdout;
    }

    /// Check satisfiability of an SMT script
    pub fn checkSat(self: *Z3Solver, script: *const SmtScript) !SolverResult {
        // Generate SMT-LIB text
        var writer = SmtLibWriter.init(self.allocator);
        defer writer.deinit();

        try writer.writeScript(script);
        const smt_text = writer.getOutput();

        return self.checkSatText(smt_text);
    }

    /// Check satisfiability from raw SMT-LIB text
    pub fn checkSatText(self: *Z3Solver, smt_text: []const u8) !SolverResult {
        // Clear previous output
        if (self.last_output) |output| {
            self.allocator.free(output);
            self.last_output = null;
        }
        if (self.last_error) |err| {
            self.allocator.free(err);
            self.last_error = null;
        }

        // Build Z3 arguments
        var args = std.ArrayListUnmanaged([]const u8){};
        defer args.deinit(self.allocator);

        try args.append(self.allocator, self.z3_path);
        try args.append(self.allocator, "-in");
        try args.append(self.allocator, "-smt2");

        // Timeout
        if (self.config.timeout_ms > 0) {
            const timeout_str = try std.fmt.allocPrint(self.allocator, "-t:{d}", .{self.config.timeout_ms});
            defer self.allocator.free(timeout_str);
            try args.append(self.allocator, timeout_str);
        }

        // Memory limit
        if (self.config.memory_limit_mb > 0) {
            const memory_str = try std.fmt.allocPrint(self.allocator, "-memory:{d}", .{self.config.memory_limit_mb});
            defer self.allocator.free(memory_str);
            try args.append(self.allocator, memory_str);
        }

        // Run Z3 process
        var child = std.process.Child.init(args.items, self.allocator);
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();

        // Write input to Z3
        if (child.stdin) |stdin| {
            _ = stdin.write(smt_text) catch {};
            stdin.close();
            child.stdin = null;
        }

        // Read output
        const stdout = child.stdout orelse return error.NoStdout;
        const stderr = child.stderr orelse return error.NoStderr;

        const output = try stdout.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        const error_output = try stderr.readToEndAlloc(self.allocator, 1024 * 1024);

        self.last_output = output;
        self.last_error = error_output;

        const term = try child.wait();

        // Parse result
        return self.parseResult(output, error_output, term);
    }

    /// Parse solver output into a result
    fn parseResult(self: *Z3Solver, output: []const u8, error_output: []const u8, term: std.process.Child.Term) !SolverResult {
        _ = error_output;

        var result = SolverResult{
            .status = .unknown,
            .model = null,
            .unsat_core = null,
            .statistics = null,
            .error_message = null,
        };

        // Check for timeout or error
        if (term.Exited != 0) {
            // Check if timeout
            if (std.mem.indexOf(u8, output, "timeout") != null) {
                result.status = .timeout;
                return result;
            }

            // Other error
            result.status = .@"error";
            result.error_message = try self.allocator.dupe(u8, output);
            return result;
        }

        // Parse output line by line
        var lines = std.mem.splitScalar(u8, output, '\n');

        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");

            if (std.mem.eql(u8, trimmed, "sat")) {
                result.status = .sat;
            } else if (std.mem.eql(u8, trimmed, "unsat")) {
                result.status = .unsat;
            } else if (std.mem.eql(u8, trimmed, "unknown")) {
                result.status = .unknown;
            } else if (std.mem.eql(u8, trimmed, "timeout")) {
                result.status = .timeout;
            } else if (std.mem.startsWith(u8, trimmed, "(model")) {
                // Model starts - collect until closing paren
                result.model = try self.parseModel(output);
            } else if (std.mem.startsWith(u8, trimmed, "(error")) {
                result.status = .@"error";
                result.error_message = try self.extractError(trimmed);
            }
        }

        return result;
    }

    /// Parse a model from Z3 output
    fn parseModel(self: *Z3Solver, output: []const u8) !?Model {
        // Find model section
        const model_start = std.mem.indexOf(u8, output, "(model") orelse return null;

        // Find matching closing paren
        var depth: u32 = 0;
        var model_end: usize = model_start;

        for (output[model_start..], model_start..) |c, i| {
            if (c == '(') depth += 1;
            if (c == ')') {
                depth -= 1;
                if (depth == 0) {
                    model_end = i + 1;
                    break;
                }
            }
        }

        const model_text = output[model_start..model_end];

        var model = Model{
            .raw_text = try self.allocator.dupe(u8, model_text),
            .definitions = std.StringHashMapUnmanaged([]const u8){},
        };

        // Parse definitions (simplified - just extract function definitions)
        var lines = std.mem.splitScalar(u8, model_text, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (std.mem.startsWith(u8, trimmed, "(define-fun")) {
                // Extract name and value
                const name_start = std.mem.indexOf(u8, trimmed, " ") orelse continue;
                const name_end = std.mem.indexOfPos(u8, trimmed, name_start + 1, " ") orelse continue;
                const name = trimmed[name_start + 1 .. name_end];

                try model.definitions.put(self.allocator, name, try self.allocator.dupe(u8, trimmed));
            }
        }

        return model;
    }

    /// Extract error message from Z3 output
    fn extractError(self: *Z3Solver, line: []const u8) ![]const u8 {
        // (error "message")
        if (std.mem.indexOf(u8, line, "\"")) |start| {
            if (std.mem.lastIndexOf(u8, line, "\"")) |end| {
                if (end > start) {
                    return self.allocator.dupe(u8, line[start + 1 .. end]);
                }
            }
        }
        return self.allocator.dupe(u8, line);
    }

    /// Get the last solver output (for debugging)
    pub fn getLastOutput(self: *const Z3Solver) ?[]const u8 {
        return self.last_output;
    }

    /// Get the last solver error output
    pub fn getLastError(self: *const Z3Solver) ?[]const u8 {
        return self.last_error;
    }
};

// ============================================================================
// Solver Result
// ============================================================================

/// Result from SMT solver
pub const SolverResult = struct {
    /// Satisfiability status
    status: SatStatus,
    /// Model (if sat and model production enabled)
    model: ?Model,
    /// Unsat core (if unsat and core production enabled)
    unsat_core: ?[]const []const u8,
    /// Solver statistics
    statistics: ?SolverStats,
    /// Error message (if error)
    error_message: ?[]const u8,

    pub fn isSat(self: *const SolverResult) bool {
        return self.status == .sat;
    }

    pub fn isUnsat(self: *const SolverResult) bool {
        return self.status == .unsat;
    }

    pub fn isUnknown(self: *const SolverResult) bool {
        return self.status == .unknown;
    }

    pub fn isTimeout(self: *const SolverResult) bool {
        return self.status == .timeout;
    }

    pub fn isError(self: *const SolverResult) bool {
        return self.status == .@"error";
    }
};

/// Satisfiability status
pub const SatStatus = enum {
    /// Formula is satisfiable (counterexample exists)
    sat,
    /// Formula is unsatisfiable (proof found)
    unsat,
    /// Solver could not determine
    unknown,
    /// Solver timed out
    timeout,
    /// Solver encountered an error
    @"error",

    pub fn description(self: SatStatus) []const u8 {
        return switch (self) {
            .sat => "satisfiable",
            .unsat => "unsatisfiable",
            .unknown => "unknown",
            .timeout => "timeout",
            .@"error" => "error",
        };
    }
};

/// A model (counterexample) from the solver
pub const Model = struct {
    /// Raw model text
    raw_text: []const u8,
    /// Parsed definitions (name -> definition text)
    definitions: std.StringHashMapUnmanaged([]const u8),

    pub fn deinit(self: *Model, allocator: Allocator) void {
        allocator.free(self.raw_text);
        var iter = self.definitions.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.value_ptr.*);
        }
        self.definitions.deinit(allocator);
    }

    /// Get the value of a variable in the model
    pub fn getValue(self: *const Model, name: []const u8) ?[]const u8 {
        return self.definitions.get(name);
    }
};

/// Solver statistics
pub const SolverStats = struct {
    /// Total time in milliseconds
    time_ms: u64,
    /// Memory used in bytes
    memory_bytes: u64,
    /// Number of conflicts
    conflicts: u64,
    /// Number of decisions
    decisions: u64,
    /// Number of propagations
    propagations: u64,
};

// ============================================================================
// Convenience Functions
// ============================================================================

/// Check if Z3 is installed and available
pub fn isZ3Available(allocator: Allocator) bool {
    var solver = Z3Solver.init(allocator, .{});
    defer solver.deinit();
    return solver.isAvailable();
}

/// Quick check: is a formula satisfiable?
pub fn quickCheck(allocator: Allocator, smt_text: []const u8) !SatStatus {
    var solver = Z3Solver.init(allocator, .{ .timeout_ms = 5000, .produce_models = false });
    defer solver.deinit();

    const result = try solver.checkSatText(smt_text);
    return result.status;
}

// ============================================================================
// Tests
// ============================================================================

test "Z3Solver initialization" {
    const testing = std.testing;

    var solver = Z3Solver.init(testing.allocator, .{});
    defer solver.deinit();

    try testing.expectEqual(@as(u32, 30000), solver.config.timeout_ms);
    try testing.expectEqualStrings("z3", solver.z3_path);
}

test "SatStatus description" {
    const testing = std.testing;

    try testing.expectEqualStrings("satisfiable", SatStatus.sat.description());
    try testing.expectEqualStrings("unsatisfiable", SatStatus.unsat.description());
    try testing.expectEqualStrings("unknown", SatStatus.unknown.description());
    try testing.expectEqualStrings("timeout", SatStatus.timeout.description());
}

test "SolverResult status checks" {
    const testing = std.testing;

    const sat_result = SolverResult{
        .status = .sat,
        .model = null,
        .unsat_core = null,
        .statistics = null,
        .error_message = null,
    };
    try testing.expect(sat_result.isSat());
    try testing.expect(!sat_result.isUnsat());

    const unsat_result = SolverResult{
        .status = .unsat,
        .model = null,
        .unsat_core = null,
        .statistics = null,
        .error_message = null,
    };
    try testing.expect(unsat_result.isUnsat());
    try testing.expect(!unsat_result.isSat());
}

test "Z3Solver availability check" {
    // This test checks if Z3 is available on the system
    // It's a "soft" test - it passes even if Z3 is not installed
    const testing = std.testing;

    var solver = Z3Solver.init(testing.allocator, .{});
    defer solver.deinit();

    const available = solver.isAvailable();
    // Just verify the check doesn't crash
    _ = available;
}
