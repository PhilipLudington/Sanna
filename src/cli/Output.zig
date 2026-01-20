//! Output formatting with colors and JSON mode
//!
//! Provides consistent output formatting across all CLI commands,
//! with support for colored terminal output and structured JSON output.

const std = @import("std");
const Allocator = std.mem.Allocator;
const fs = std.fs;

const Output = @This();

/// ANSI color codes
pub const Color = enum(u8) {
    reset = 0,
    bold = 1,
    dim = 2,
    red = 31,
    green = 32,
    yellow = 33,
    blue = 34,
    magenta = 35,
    cyan = 36,
    white = 37,

    pub fn code(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .bold => "\x1b[1m",
            .dim => "\x1b[2m",
            .red => "\x1b[31m",
            .green => "\x1b[32m",
            .yellow => "\x1b[33m",
            .blue => "\x1b[34m",
            .magenta => "\x1b[35m",
            .cyan => "\x1b[36m",
            .white => "\x1b[37m",
        };
    }
};

/// Whether to output JSON
json_mode: bool = false,
/// Whether colors are enabled
color_enabled: bool = true,
/// Collected JSON objects for batch output
json_buffer: std.ArrayListUnmanaged(u8) = .{},

pub fn init(json_mode: bool) Output {
    // Check if stdout is a TTY for color support
    const stdout_file = fs.File{ .handle = std.posix.STDOUT_FILENO };
    const color_enabled = stdout_file.isTty();

    return .{
        .json_mode = json_mode,
        .color_enabled = color_enabled,
    };
}

pub fn deinit(self: *Output, allocator: Allocator) void {
    self.json_buffer.deinit(allocator);
}

fn writeStdout(data: []const u8) void {
    const stdout_file = fs.File{ .handle = std.posix.STDOUT_FILENO };
    stdout_file.writeAll(data) catch {};
}

fn writeStderr(data: []const u8) void {
    const stderr_file = fs.File{ .handle = std.posix.STDERR_FILENO };
    stderr_file.writeAll(data) catch {};
}

/// Print text to stdout
pub fn print(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    _ = self;
    var buf: [4096]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStdout(formatted);
}

/// Print with color
pub fn printColored(self: *const Output, color: Color, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;

    if (self.color_enabled) {
        writeStdout(color.code());
    }
    var buf: [4096]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStdout(formatted);
    if (self.color_enabled) {
        writeStdout(Color.reset.code());
    }
}

/// Print a success message (green)
pub fn success(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.printColored(.green, "✓ ", .{});
    self.print(fmt ++ "\n", args);
}

/// Print a warning message (yellow)
pub fn warn(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.printColored(.yellow, "⚠ ", .{});
    self.print(fmt ++ "\n", args);
}

/// Print an error message (red)
pub fn err(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    if (self.color_enabled) {
        writeStderr(Color.red.code());
    }
    writeStderr("✗ ");
    if (self.color_enabled) {
        writeStderr(Color.reset.code());
    }
    var buf: [4096]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, fmt ++ "\n", args) catch return;
    writeStderr(formatted);
}

/// Print an info message (cyan)
pub fn info(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.printColored(.cyan, "ℹ ", .{});
    self.print(fmt ++ "\n", args);
}

/// Print a section header (bold)
pub fn header(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.print("\n", .{});
    self.printColored(.bold, fmt, args);
    self.print("\n", .{});
}

/// Print a progress indicator
pub fn progress(self: *const Output, current: usize, total: usize, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.printColored(.dim, "[{d}/{d}] ", .{ current, total });
    self.print(fmt ++ "\n", args);
}

/// Print a spinner-style progress (for long operations)
pub fn spinner(self: *const Output, comptime fmt: []const u8, args: anytype) void {
    if (self.json_mode) return;
    self.printColored(.cyan, "◐ ", .{});
    self.print(fmt ++ "\r", args);
}

/// Clear the current line (for progress updates)
pub fn clearLine(self: *const Output) void {
    if (self.json_mode) return;
    self.print("\x1b[2K\r", .{});
}

/// Print help message
pub fn printHelp(self: *const Output) void {
    _ = self;
    const help =
        \\Sanna - AI-assisted software development with formal specifications
        \\
        \\USAGE:
        \\    sanna <COMMAND> [OPTIONS]
        \\
        \\COMMANDS:
        \\    init        Create a new Sanna project
        \\    check       Check specification syntax and types
        \\    generate    Generate code from specifications
        \\    verify      Verify implementations against specifications
        \\    trust       Generate and query trust reports
        \\    review      List items needing review
        \\    approve     Approve code for deployment
        \\    repl        Interactive mode for specifications
        \\    help        Print help information
        \\
        \\OPTIONS:
        \\    -h, --help       Print help information
        \\    -V, --version    Print version information
        \\    --json           Output in JSON format
        \\    --no-color       Disable colored output
        \\
        \\EXAMPLES:
        \\    sanna init myproject
        \\    sanna check specs/*.sanna
        \\    sanna generate --target klar specs/auth.sanna
        \\    sanna verify --timeout 60
        \\    sanna trust --below 0.6
        \\    sanna review --unproven
        \\    sanna approve auth.sanna::authenticate --note "Reviewed by Alice"
        \\    sanna repl
        \\
        \\For more information, visit: https://sanna-lang.org
        \\
    ;
    writeStdout(help);
}

/// Print version information
pub fn printVersion(self: *const Output) void {
    _ = self;
    const version =
        \\Sanna 0.1.0
        \\Copyright (c) 2024 Sanna Authors
        \\License: MIT
        \\
    ;
    writeStdout(version);
}

/// Write a JSON object to stdout (manual serialization for simple structs)
pub fn writeJson(self: *const Output, val: anytype) void {
    if (!self.json_mode) return;
    self.writeJsonValue(val);
    writeStdout("\n");
}

/// Write a JSON object with pretty printing
pub fn writeJsonPretty(self: *const Output, val: anytype) void {
    if (!self.json_mode) return;
    self.writeJsonValue(val);
    writeStdout("\n");
}

fn writeJsonValue(self: *const Output, val: anytype) void {
    _ = self;
    const T = @TypeOf(val);
    const type_info = @typeInfo(T);

    switch (type_info) {
        .@"struct" => |s| {
            writeStdout("{");
            inline for (s.fields, 0..) |field, i| {
                if (i > 0) writeStdout(",");
                writeStdout("\"");
                writeStdout(field.name);
                writeStdout("\":");
                writeJsonFieldValue(@field(val, field.name));
            }
            writeStdout("}");
        },
        else => {
            writeStdout("null");
        },
    }
}

fn writeJsonFieldValue(val: anytype) void {
    const T = @TypeOf(val);
    const type_info = @typeInfo(T);

    switch (type_info) {
        .int, .comptime_int => {
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
            writeStdout(formatted);
        },
        .float, .comptime_float => {
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d:.4}", .{val}) catch "0";
            writeStdout(formatted);
        },
        .bool => {
            writeStdout(if (val) "true" else "false");
        },
        .pointer => |ptr| {
            if (ptr.size == .slice and ptr.child == u8) {
                writeStdout("\"");
                writeStdout(val);
                writeStdout("\"");
            } else {
                writeStdout("null");
            }
        },
        .optional => {
            if (val) |v| {
                writeJsonFieldValue(v);
            } else {
                writeStdout("null");
            }
        },
        .@"enum" => {
            writeStdout("\"");
            writeStdout(@tagName(val));
            writeStdout("\"");
        },
        else => {
            writeStdout("null");
        },
    }
}

/// Format a trust score with appropriate color
pub fn formatTrustScore(self: *const Output, score: f32) void {
    const color: Color = if (score >= 0.8) .green else if (score >= 0.6) .yellow else .red;

    var buf: [8]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, "{d:.2}", .{score}) catch "?";
    self.printColored(color, "{s}", .{formatted});
}

/// Format a verification status with appropriate symbol and color
pub fn formatVerificationStatus(self: *const Output, status: VerificationStatus) void {
    switch (status) {
        .proven => self.printColored(.green, "✓ proven", .{}),
        .unproven => self.printColored(.yellow, "? unproven", .{}),
        .failed => self.printColored(.red, "✗ failed", .{}),
        .timeout => self.printColored(.yellow, "◐ timeout", .{}),
        .skipped => self.printColored(.dim, "- skipped", .{}),
    }
}

pub const VerificationStatus = enum {
    proven,
    unproven,
    failed,
    timeout,
    skipped,
};

/// Print a table row
pub fn tableRow(self: *const Output, columns: []const TableCell) void {
    if (self.json_mode) return;
    for (columns, 0..) |cell, i| {
        if (i > 0) self.print("  ", .{});
        self.printColored(cell.color, "{s: <[1]}", .{ cell.text, cell.width });
    }
    self.print("\n", .{});
}

pub const TableCell = struct {
    text: []const u8,
    width: usize = 20,
    color: Color = .reset,
};

test "Output.init" {
    const output = Output.init(false);
    try std.testing.expect(!output.json_mode);
}
