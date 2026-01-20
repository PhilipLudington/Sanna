//! Generated Code Parser
//!
//! Parses AI-generated code and integrates it with the Sanna AST.
//! Handles validation, error recovery, and typed hole detection.

const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("types.zig");

const GeneratedCode = types.GeneratedCode;
const TypedHole = types.TypedHole;
const TargetLanguage = types.TargetLanguage;
const CodeComponent = types.GeneratedCode.CodeComponent;

/// Result of parsing generated code.
pub const ParseResult = struct {
    success: bool,
    components: []const CodeComponent,
    holes: []const TypedHole,
    errors: []const ParseError,
    warnings: []const Warning,
    annotated_source: ?[]const u8,

    pub const ParseError = struct {
        message: []const u8,
        location: Location,
        severity: Severity,
        suggestion: ?[]const u8 = null,

        pub const Location = struct {
            line: u32,
            column: u32,
            length: u32 = 0,
        };

        pub const Severity = enum {
            error_val,
            warning,
            hint,
        };
    };

    pub const Warning = struct {
        message: []const u8,
        location: ParseError.Location,
        code: WarningCode,

        pub const WarningCode = enum {
            missing_type_annotation,
            implicit_return,
            unused_parameter,
            complex_expression,
            potential_overflow,
            missing_error_handling,
        };
    };
};

/// Parser for AI-generated code.
pub const CodeParser = struct {
    allocator: Allocator,
    target: TargetLanguage,
    components: std.ArrayListUnmanaged(CodeComponent),
    holes: std.ArrayListUnmanaged(TypedHole),
    errors: std.ArrayListUnmanaged(ParseResult.ParseError),
    warnings: std.ArrayListUnmanaged(ParseResult.Warning),
    source: []const u8,
    current_line: u32,
    current_column: u32,
    hole_counter: u32,

    pub fn init(allocator: Allocator, target: TargetLanguage) CodeParser {
        return .{
            .allocator = allocator,
            .target = target,
            .components = .{},
            .holes = .{},
            .errors = .{},
            .warnings = .{},
            .source = "",
            .current_line = 1,
            .current_column = 1,
            .hole_counter = 0,
        };
    }

    pub fn deinit(self: *CodeParser) void {
        self.components.deinit(self.allocator);
        self.holes.deinit(self.allocator);
        self.errors.deinit(self.allocator);
        self.warnings.deinit(self.allocator);
    }

    pub fn parse(self: *CodeParser, source: []const u8) !ParseResult {
        self.source = source;
        self.current_line = 1;
        self.current_column = 1;
        self.hole_counter = 0;

        self.components.clearRetainingCapacity();
        self.holes.clearRetainingCapacity();
        self.errors.clearRetainingCapacity();
        self.warnings.clearRetainingCapacity();

        switch (self.target) {
            .klar, .kira, .zig => try self.parseZigLike(source),
            .rust => try self.parseRust(source),
            .typescript => try self.parseTypeScript(source),
            .python => try self.parsePython(source),
        }

        return ParseResult{
            .success = self.errors.items.len == 0,
            .components = self.components.items,
            .holes = self.holes.items,
            .errors = self.errors.items,
            .warnings = self.warnings.items,
            .annotated_source = null,
        };
    }

    fn parseZigLike(self: *CodeParser, source: []const u8) !void {
        var pos: usize = 0;

        while (pos < source.len) {
            while (pos < source.len and isWhitespace(source[pos])) {
                pos += 1;
            }
            if (pos >= source.len) break;

            if (std.mem.startsWith(u8, source[pos..], "pub fn ") or
                std.mem.startsWith(u8, source[pos..], "fn "))
            {
                const start = pos;
                const end = try self.findBlockEnd(source, pos);
                const name = extractName(source[start..end], "fn ");

                try self.components.append(self.allocator, .{
                    .kind = .function,
                    .name = name,
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.8,
                });

                try self.scanForHoles(source[start..end]);
                pos = end;
            } else if (std.mem.startsWith(u8, source[pos..], "pub const ") or
                std.mem.startsWith(u8, source[pos..], "const "))
            {
                const start = pos;
                const end = try self.findStatementOrBlockEnd(source, pos);
                const name = extractName(source[start..end], "const ");

                try self.components.append(self.allocator, .{
                    .kind = if (std.mem.indexOf(u8, source[start..end], "struct {") != null)
                        .type_impl
                    else
                        .constant,
                    .name = name,
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.85,
                });

                pos = end;
            } else if (std.mem.startsWith(u8, source[pos..], "test \"")) {
                const start = pos;
                const end = try self.findBlockEnd(source, pos);
                const name = extractTestName(source[start..end]);

                try self.components.append(self.allocator, .{
                    .kind = .test_case,
                    .name = name,
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.75,
                });

                pos = end;
            } else if (std.mem.startsWith(u8, source[pos..], "//")) {
                while (pos < source.len and source[pos] != '\n') pos += 1;
                if (pos < source.len) pos += 1;
            } else {
                while (pos < source.len and source[pos] != '\n') pos += 1;
                if (pos < source.len) pos += 1;
            }
        }
    }

    fn parseRust(self: *CodeParser, source: []const u8) !void {
        try self.parseZigLike(source);
    }

    fn parseTypeScript(self: *CodeParser, source: []const u8) !void {
        var pos: usize = 0;

        while (pos < source.len) {
            while (pos < source.len and isWhitespace(source[pos])) pos += 1;
            if (pos >= source.len) break;

            if (std.mem.startsWith(u8, source[pos..], "export function ") or
                std.mem.startsWith(u8, source[pos..], "function "))
            {
                const start = pos;
                const end = try self.findBlockEnd(source, pos);
                const name = extractName(source[start..end], "function ");

                try self.components.append(self.allocator, .{
                    .kind = .function,
                    .name = name,
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.75,
                });

                pos = end;
            } else if (std.mem.startsWith(u8, source[pos..], "interface ") or
                std.mem.startsWith(u8, source[pos..], "class "))
            {
                const start = pos;
                const end = try self.findBlockEnd(source, pos);

                try self.components.append(self.allocator, .{
                    .kind = .type_impl,
                    .name = "TypeDef",
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.8,
                });

                pos = end;
            } else {
                while (pos < source.len and source[pos] != '\n') pos += 1;
                if (pos < source.len) pos += 1;
            }
        }
    }

    fn parsePython(self: *CodeParser, source: []const u8) !void {
        var pos: usize = 0;

        while (pos < source.len) {
            while (pos < source.len and isWhitespace(source[pos])) pos += 1;
            if (pos >= source.len) break;

            if (std.mem.startsWith(u8, source[pos..], "def ") or
                std.mem.startsWith(u8, source[pos..], "async def "))
            {
                const start = pos;
                const end = try self.findPythonBlockEnd(source, pos);
                const name = extractName(source[start..end], "def ");

                try self.components.append(self.allocator, .{
                    .kind = .function,
                    .name = name,
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.7,
                });

                pos = end;
            } else if (std.mem.startsWith(u8, source[pos..], "class ")) {
                const start = pos;
                const end = try self.findPythonBlockEnd(source, pos);

                try self.components.append(self.allocator, .{
                    .kind = .type_impl,
                    .name = "Class",
                    .source = source[start..end],
                    .start_offset = start,
                    .end_offset = end,
                    .confidence = 0.75,
                });

                pos = end;
            } else {
                while (pos < source.len and source[pos] != '\n') pos += 1;
                if (pos < source.len) pos += 1;
            }
        }
    }

    fn findBlockEnd(_: *CodeParser, source: []const u8, start: usize) !usize {
        var pos = start;
        var brace_depth: i32 = 0;
        var in_body = false;

        while (pos < source.len) {
            if (source[pos] == '{') {
                brace_depth += 1;
                in_body = true;
            } else if (source[pos] == '}') {
                brace_depth -= 1;
                if (in_body and brace_depth == 0) {
                    return pos + 1;
                }
            }
            pos += 1;
        }
        return source.len;
    }

    fn findStatementOrBlockEnd(self: *CodeParser, source: []const u8, start: usize) !usize {
        var pos = start;
        while (pos < source.len) {
            if (source[pos] == '{') {
                return self.findBlockEnd(source, start);
            } else if (source[pos] == ';') {
                return pos + 1;
            } else if (source[pos] == '\n') {
                return pos + 1;
            }
            pos += 1;
        }
        return source.len;
    }

    fn findPythonBlockEnd(_: *CodeParser, source: []const u8, start: usize) !usize {
        var pos = start;
        while (pos < source.len and source[pos] != '\n') pos += 1;
        if (pos < source.len) pos += 1;

        var body_indent: usize = 0;
        while (pos + body_indent < source.len and source[pos + body_indent] == ' ') {
            body_indent += 1;
        }
        if (body_indent == 0) return pos;

        while (pos < source.len) {
            var line_indent: usize = 0;
            while (pos + line_indent < source.len and source[pos + line_indent] == ' ') {
                line_indent += 1;
            }

            if (pos + line_indent < source.len and source[pos + line_indent] == '\n') {
                pos += line_indent + 1;
                continue;
            }

            if (line_indent < body_indent and pos + line_indent < source.len and source[pos + line_indent] != '\n') {
                return pos;
            }

            while (pos < source.len and source[pos] != '\n') pos += 1;
            if (pos < source.len) pos += 1;
        }
        return source.len;
    }

    fn scanForHoles(self: *CodeParser, source: []const u8) !void {
        const markers = [_][]const u8{ "???", "TODO:", "@compileError(\"HOLE", "todo!()" };

        var pos: usize = 0;
        var line: u32 = 1;
        var col: u32 = 1;

        while (pos < source.len) {
            for (markers) |marker| {
                if (pos + marker.len <= source.len and
                    std.mem.eql(u8, source[pos .. pos + marker.len], marker))
                {
                    self.hole_counter += 1;
                    try self.holes.append(self.allocator, .{
                        .id = self.hole_counter,
                        .description = "implementation needed",
                        .expected_type = "unknown",
                        .location = .{ .line = line, .column = col, .length = @intCast(marker.len) },
                        .reason = .explicit_hole,
                    });
                    pos += marker.len;
                    col += @intCast(marker.len);
                    break;
                }
            } else {
                if (source[pos] == '\n') {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                pos += 1;
            }
        }
    }
};

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn extractName(source: []const u8, keyword: []const u8) []const u8 {
    if (std.mem.indexOf(u8, source, keyword)) |idx| {
        const name_start = idx + keyword.len;
        var name_end = name_start;
        while (name_end < source.len and isIdentChar(source[name_end])) {
            name_end += 1;
        }
        if (name_end > name_start) {
            return source[name_start..name_end];
        }
    }
    return "unknown";
}

fn extractTestName(source: []const u8) []const u8 {
    if (std.mem.indexOf(u8, source, "test \"")) |idx| {
        const name_start = idx + 6;
        if (std.mem.indexOfScalar(u8, source[name_start..], '"')) |end| {
            return source[name_start .. name_start + end];
        }
    }
    return "unknown";
}

// ============================================================================
// Tests
// ============================================================================

test "CodeParser parses Zig function" {
    const testing = std.testing;
    var parser = CodeParser.init(testing.allocator, .zig);
    defer parser.deinit();

    const source =
        \\pub fn add(a: i32, b: i32) i32 {
        \\    return a + b;
        \\}
    ;

    const result = try parser.parse(source);

    try testing.expect(result.success);
    try testing.expectEqual(@as(usize, 1), result.components.len);
    try testing.expectEqualStrings("add", result.components[0].name);
}

test "CodeParser detects typed holes" {
    const testing = std.testing;
    var parser = CodeParser.init(testing.allocator, .zig);
    defer parser.deinit();

    const source =
        \\pub fn complex_algorithm(data: []const u8) !Result {
        \\    const result = ???;
        \\    return result;
        \\}
    ;

    const result = try parser.parse(source);
    try testing.expectEqual(@as(usize, 1), result.holes.len);
}

test "CodeParser parses TypeScript" {
    const testing = std.testing;
    var parser = CodeParser.init(testing.allocator, .typescript);
    defer parser.deinit();

    const source =
        \\export function createUser(name: string): User {
        \\    return { id: 1, name };
        \\}
    ;

    const result = try parser.parse(source);
    try testing.expect(result.success);
    try testing.expect(result.components.len >= 1);
}
