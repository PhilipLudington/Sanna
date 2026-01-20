//! Command-line argument parser
//!
//! Provides a simple interface for parsing command-line arguments
//! with support for flags, options, and positional arguments.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Args = @This();

/// Raw argument iterator
args: []const [:0]const u8,
/// Current position in args
index: usize = 0,
/// Whether we own the args (need to free)
owned: bool = false,

/// Initialize from process arguments
pub fn initFromProcess(allocator: Allocator) !Args {
    var args_iter = std.process.args();
    var list = std.ArrayListUnmanaged([:0]const u8){};
    errdefer {
        for (list.items) |arg| {
            allocator.free(arg);
        }
        list.deinit(allocator);
    }

    while (args_iter.next()) |arg| {
        const duped = try allocator.dupeZ(u8, arg);
        try list.append(allocator, duped);
    }

    return .{
        .args = try list.toOwnedSlice(allocator),
        .index = 1, // Skip program name
        .owned = true,
    };
}

/// Initialize from slice (for testing)
pub fn initFromSlice(args: []const [:0]const u8) Args {
    return .{
        .args = args,
        .index = 0,
        .owned = false,
    };
}

/// Free process arguments
pub fn deinit(self: *Args, allocator: Allocator) void {
    if (self.owned) {
        for (self.args) |arg| {
            allocator.free(arg);
        }
        allocator.free(self.args);
    }
}

/// Get the next positional argument (not starting with -)
pub fn nextPositional(self: *Args) ?[]const u8 {
    while (self.index < self.args.len) {
        const arg = self.args[self.index];
        if (arg.len > 0 and arg[0] == '-') {
            // Skip flags during positional iteration
            self.index += 1;
            // If it's a flag with value, skip the value too
            if (self.index < self.args.len and arg.len > 1 and arg[1] != '-') {
                if (self.index < self.args.len) {
                    const next = self.args[self.index];
                    if (next.len == 0 or next[0] != '-') {
                        self.index += 1;
                    }
                }
            }
        } else {
            self.index += 1;
            return arg;
        }
    }
    return null;
}

/// Get the next flag (--flag or -f), returns flag name without dashes
pub fn nextFlag(self: *Args) ?[]const u8 {
    while (self.index < self.args.len) {
        const arg = self.args[self.index];
        if (arg.len > 1 and arg[0] == '-') {
            self.index += 1;
            if (arg[1] == '-') {
                // Long flag: --flag
                const flag = arg[2..];
                // Check for --flag=value format
                if (std.mem.indexOfScalar(u8, flag, '=')) |eq_idx| {
                    return flag[0..eq_idx];
                }
                return flag;
            } else {
                // Short flag: -f
                return arg[1..2];
            }
        } else {
            self.index += 1;
        }
    }
    return null;
}

/// Peek at the next flag without consuming it
pub fn peekFlag(self: *const Args) ?[]const u8 {
    var idx = self.index;
    while (idx < self.args.len) {
        const arg = self.args[idx];
        if (arg.len > 1 and arg[0] == '-') {
            if (arg[1] == '-') {
                // Long flag: --flag
                const flag = arg[2..];
                // Check for --flag=value format
                if (std.mem.indexOfScalar(u8, flag, '=')) |eq_idx| {
                    return flag[0..eq_idx];
                }
                return flag;
            } else {
                // Short flag: -f
                return arg[1..2];
            }
        } else {
            idx += 1;
        }
    }
    return null;
}

/// Check if a specific flag is present
pub fn hasFlag(self: *const Args, short: ?u8, long: ?[]const u8) bool {
    for (self.args) |arg| {
        if (arg.len > 1 and arg[0] == '-') {
            if (arg[1] == '-') {
                // Long flag
                if (long) |l| {
                    const flag = arg[2..];
                    // Handle --flag=value
                    const eq_idx = std.mem.indexOfScalar(u8, flag, '=') orelse flag.len;
                    if (std.mem.eql(u8, flag[0..eq_idx], l)) {
                        return true;
                    }
                }
            } else {
                // Short flag
                if (short) |s| {
                    if (arg[1] == s) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

/// Get the value for a flag (--flag=value or --flag value)
pub fn getFlagValue(self: *const Args, short: ?u8, long: ?[]const u8) ?[]const u8 {
    var i: usize = 0;
    while (i < self.args.len) : (i += 1) {
        const arg = self.args[i];
        if (arg.len > 1 and arg[0] == '-') {
            if (arg[1] == '-') {
                // Long flag
                if (long) |l| {
                    const flag = arg[2..];
                    // Check --flag=value format
                    if (std.mem.indexOfScalar(u8, flag, '=')) |eq_idx| {
                        if (std.mem.eql(u8, flag[0..eq_idx], l)) {
                            return flag[eq_idx + 1 ..];
                        }
                    } else if (std.mem.eql(u8, flag, l)) {
                        // Check next arg for value
                        if (i + 1 < self.args.len) {
                            const next = self.args[i + 1];
                            if (next.len == 0 or next[0] != '-') {
                                return next;
                            }
                        }
                    }
                }
            } else {
                // Short flag
                if (short) |s| {
                    if (arg[1] == s) {
                        // Check for -fvalue format
                        if (arg.len > 2) {
                            return arg[2..];
                        }
                        // Check next arg for value
                        if (i + 1 < self.args.len) {
                            const next = self.args[i + 1];
                            if (next.len == 0 or next[0] != '-') {
                                return next;
                            }
                        }
                    }
                }
            }
        }
    }
    return null;
}

/// Get all remaining positional arguments
pub fn remaining(self: *Args, allocator: Allocator) ![]const []const u8 {
    var list = std.ArrayListUnmanaged([]const u8){};
    errdefer list.deinit(allocator);

    while (self.nextPositional()) |pos| {
        try list.append(allocator, pos);
    }

    return list.toOwnedSlice(allocator);
}

/// Reset the parser to the beginning
pub fn reset(self: *Args) void {
    self.index = if (self.args.len > 0 and std.mem.len(self.args[0]) > 0) 1 else 0;
}

test "Args.hasFlag" {
    const args = [_][:0]const u8{ "sanna", "--verbose", "-t", "klar", "--output=foo.json" };
    const parser = Args.initFromSlice(&args);

    try std.testing.expect(parser.hasFlag('v', "verbose"));
    try std.testing.expect(parser.hasFlag('t', "target"));
    try std.testing.expect(parser.hasFlag(null, "output"));
    try std.testing.expect(!parser.hasFlag('x', "unknown"));
}

test "Args.getFlagValue" {
    const args = [_][:0]const u8{ "sanna", "--target", "klar", "--output=foo.json", "-n", "42" };
    const parser = Args.initFromSlice(&args);

    try std.testing.expectEqualStrings("klar", parser.getFlagValue('t', "target").?);
    try std.testing.expectEqualStrings("foo.json", parser.getFlagValue('o', "output").?);
    try std.testing.expectEqualStrings("42", parser.getFlagValue('n', "num").?);
    try std.testing.expect(parser.getFlagValue('x', "unknown") == null);
}

test "Args.nextPositional" {
    const args = [_][:0]const u8{ "check", "file1.sanna", "--verbose", "file2.sanna" };
    var parser = Args.initFromSlice(&args);

    try std.testing.expectEqualStrings("check", parser.nextPositional().?);
    try std.testing.expectEqualStrings("file1.sanna", parser.nextPositional().?);
    try std.testing.expectEqualStrings("file2.sanna", parser.nextPositional().?);
    try std.testing.expect(parser.nextPositional() == null);
}
