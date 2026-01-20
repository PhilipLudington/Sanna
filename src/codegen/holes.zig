//! Typed Hole Generation
//!
//! Generates typed holes (???) for partial implementations where the AI
//! cannot confidently generate code or where human review is required.

const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("types.zig");

const TypedHole = types.TypedHole;
const TargetLanguage = types.TargetLanguage;

/// Generator for typed holes in partial implementations.
pub const HoleGenerator = struct {
    allocator: Allocator,
    target: TargetLanguage,
    hole_counter: u32,
    holes: std.ArrayListUnmanaged(TypedHole),

    pub fn init(allocator: Allocator, target: TargetLanguage) HoleGenerator {
        return .{
            .allocator = allocator,
            .target = target,
            .hole_counter = 0,
            .holes = .{},
        };
    }

    pub fn deinit(self: *HoleGenerator) void {
        self.holes.deinit(self.allocator);
    }

    pub fn reset(self: *HoleGenerator) void {
        self.hole_counter = 0;
        self.holes.clearRetainingCapacity();
    }

    pub fn generateHole(
        self: *HoleGenerator,
        description: []const u8,
        expected_type: []const u8,
        location: TypedHole.Location,
        reason: TypedHole.Reason,
    ) !TypedHole {
        self.hole_counter += 1;

        const hole = TypedHole{
            .id = self.hole_counter,
            .description = description,
            .expected_type = expected_type,
            .location = location,
            .reason = reason,
            .suggestions = &.{},
        };

        try self.holes.append(self.allocator, hole);
        return hole;
    }

    pub fn generateFunctionHole(
        self: *HoleGenerator,
        name: []const u8,
        return_type: []const u8,
        context: ?[]const u8,
        line: u32,
        column: u32,
    ) !TypedHole {
        const reason = self.inferReason(name, context);

        return self.generateHole(
            name,
            return_type,
            .{ .line = line, .column = column, .length = 3 },
            reason,
        );
    }

    pub fn generateHoleCode(self: *HoleGenerator, hole: TypedHole) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        switch (self.target) {
            .zig => {
                try buf.appendSlice(self.allocator, "@compileError(\"HOLE: ");
                try buf.appendSlice(self.allocator, hole.description);
                try buf.appendSlice(self.allocator, "\")");
            },
            .rust => {
                try buf.appendSlice(self.allocator, "todo!(\"");
                try buf.appendSlice(self.allocator, hole.description);
                try buf.appendSlice(self.allocator, "\")");
            },
            .typescript => {
                try buf.appendSlice(self.allocator, "undefined /* TODO: ");
                try buf.appendSlice(self.allocator, hole.description);
                try buf.appendSlice(self.allocator, " */");
            },
            .python => {
                try buf.appendSlice(self.allocator, "raise NotImplementedError(\"");
                try buf.appendSlice(self.allocator, hole.description);
                try buf.appendSlice(self.allocator, "\")");
            },
            .klar, .kira => {
                try buf.appendSlice(self.allocator, "??? /* ");
                try buf.appendSlice(self.allocator, hole.description);
                try buf.appendSlice(self.allocator, " */");
            },
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn inferReason(_: *HoleGenerator, name: []const u8, context: ?[]const u8) TypedHole.Reason {
        const security_kw = [_][]const u8{ "password", "secret", "key", "token", "auth", "encrypt", "crypto" };
        const perf_kw = [_][]const u8{ "critical", "hot", "optimize", "performance", "fast", "cache" };

        for (security_kw) |kw| {
            if (std.mem.indexOf(u8, name, kw) != null) return .security_sensitive;
        }

        for (perf_kw) |kw| {
            if (std.mem.indexOf(u8, name, kw) != null) return .performance_critical;
        }

        if (context) |ctx| {
            for (security_kw) |kw| {
                if (std.mem.indexOf(u8, ctx, kw) != null) return .security_sensitive;
            }
        }

        return .explicit_hole;
    }

    pub fn getHoles(self: *HoleGenerator) []const TypedHole {
        return self.holes.items;
    }
};

/// Scans source code for explicit hole markers.
pub const HoleScanner = struct {
    allocator: Allocator,
    target: TargetLanguage,

    pub fn init(allocator: Allocator, target: TargetLanguage) HoleScanner {
        return .{
            .allocator = allocator,
            .target = target,
        };
    }

    pub fn scan(self: *HoleScanner, source: []const u8) ![]TypedHole {
        var holes = std.ArrayListUnmanaged(TypedHole){};
        errdefer holes.deinit(self.allocator);

        const markers = self.getMarkers();

        var pos: usize = 0;
        var line: u32 = 1;
        var col: u32 = 1;
        var hole_id: u32 = 0;

        while (pos < source.len) {
            var found = false;

            for (markers) |marker| {
                if (pos + marker.len <= source.len and
                    std.mem.eql(u8, source[pos .. pos + marker.len], marker))
                {
                    hole_id += 1;
                    try holes.append(self.allocator, .{
                        .id = hole_id,
                        .description = "implementation needed",
                        .expected_type = "unknown",
                        .location = .{
                            .line = line,
                            .column = col,
                            .length = @intCast(marker.len),
                        },
                        .reason = .explicit_hole,
                    });
                    found = true;
                    pos += marker.len;
                    col += @intCast(marker.len);
                    break;
                }
            }

            if (!found) {
                if (source[pos] == '\n') {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                pos += 1;
            }
        }

        return holes.toOwnedSlice(self.allocator);
    }

    fn getMarkers(self: *HoleScanner) []const []const u8 {
        return switch (self.target) {
            .zig => &.{ "@compileError(\"HOLE", "TODO:" },
            .rust => &.{ "todo!()", "unimplemented!()", "TODO:" },
            .typescript => &.{ "// TODO:", "throw new Error(\"TODO" },
            .python => &.{ "raise NotImplementedError", "# TODO:" },
            .klar, .kira => &.{ "???", "// TODO:" },
        };
    }
};

/// Represents a partial implementation with holes.
pub const PartialImplementation = struct {
    source: []const u8,
    holes: []const TypedHole,
    completion: f32,
    is_compilable: bool,

    pub fn isComplete(self: PartialImplementation) bool {
        return self.holes.len == 0;
    }

    pub fn remainingHoles(self: PartialImplementation) usize {
        return self.holes.len;
    }
};

pub fn createPartialImplementation(
    source: []const u8,
    holes: []const TypedHole,
) PartialImplementation {
    const completion: f32 = if (holes.len == 0)
        1.0
    else
        @max(0.0, 1.0 - @as(f32, @floatFromInt(holes.len)) * 0.1);

    var is_compilable = true;
    for (holes) |hole| {
        if (hole.reason == .security_sensitive or hole.reason == .external_dependency) {
            is_compilable = false;
            break;
        }
    }

    return .{
        .source = source,
        .holes = holes,
        .completion = completion,
        .is_compilable = is_compilable,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "HoleGenerator basic" {
    const testing = std.testing;
    var gen = HoleGenerator.init(testing.allocator, .zig);
    defer gen.deinit();

    const hole = try gen.generateHole(
        "implement sorting",
        "[]i32",
        .{ .line = 10, .column = 5, .length = 3 },
        .complex_algorithm,
    );

    try testing.expectEqual(@as(u32, 1), hole.id);
    try testing.expectEqualStrings("implement sorting", hole.description);
}

test "HoleGenerator function hole" {
    const testing = std.testing;
    var gen = HoleGenerator.init(testing.allocator, .rust);
    defer gen.deinit();

    const hole = try gen.generateFunctionHole(
        "encrypt_password",
        "Result<Vec<u8>, Error>",
        "security module",
        15,
        4,
    );

    try testing.expectEqual(TypedHole.Reason.security_sensitive, hole.reason);
}

test "HoleGenerator generates code" {
    const testing = std.testing;
    var gen = HoleGenerator.init(testing.allocator, .zig);
    defer gen.deinit();

    const hole = TypedHole{
        .id = 1,
        .description = "implement algorithm",
        .expected_type = "u32",
        .location = .{ .line = 1, .column = 1, .length = 3 },
        .reason = .complex_algorithm,
    };

    const code = try gen.generateHoleCode(hole);
    defer testing.allocator.free(code);

    try testing.expect(std.mem.indexOf(u8, code, "@compileError") != null);
}

test "HoleScanner finds holes" {
    const testing = std.testing;
    var scanner = HoleScanner.init(testing.allocator, .klar);

    const source =
        \\fn compute(x: i32) -> i32 {
        \\    let result = ???;
        \\    return result;
        \\}
    ;

    const holes = try scanner.scan(source);
    defer testing.allocator.free(holes);

    try testing.expectEqual(@as(usize, 1), holes.len);
}

test "PartialImplementation completion" {
    const holes = [_]TypedHole{
        .{
            .id = 1,
            .description = "hole 1",
            .expected_type = "i32",
            .location = .{ .line = 1, .column = 1, .length = 3 },
            .reason = .explicit_hole,
        },
    };

    const partial = createPartialImplementation("source", &holes);

    const testing = std.testing;
    try testing.expect(!partial.isComplete());
    try testing.expectEqual(@as(usize, 1), partial.remainingHoles());
    try testing.expect(partial.is_compilable);
}
