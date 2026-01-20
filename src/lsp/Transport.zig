//! LSP Transport Layer
//!
//! Handles reading and writing JSON-RPC messages over stdin/stdout
//! according to the Language Server Protocol specification.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Protocol = @import("Protocol.zig");

const Transport = @This();

allocator: Allocator,
stdin: std.fs.File,
stdout: std.fs.File,
read_buffer: std.ArrayListUnmanaged(u8),
line_buffer: std.ArrayListUnmanaged(u8),
stdin_buf: [8192]u8,

pub fn init(allocator: Allocator) Transport {
    return .{
        .allocator = allocator,
        .stdin = std.fs.File.stdin(),
        .stdout = std.fs.File.stdout(),
        .read_buffer = .{},
        .line_buffer = .{},
        .stdin_buf = undefined,
    };
}

pub fn deinit(self: *Transport) void {
    self.read_buffer.deinit(self.allocator);
    self.line_buffer.deinit(self.allocator);
}

/// Read a complete LSP message from stdin
/// Returns the JSON content or null on EOF
pub fn readMessage(self: *Transport) !?[]const u8 {
    // Read headers
    var content_length: ?usize = null;

    while (true) {
        const line = self.readLine() catch |err| {
            if (err == error.EndOfStream or err == error.ReadFailed) return null;
            return err;
        };

        if (line.len == 0) {
            // Empty line marks end of headers
            break;
        }

        // Parse Content-Length header
        if (std.mem.startsWith(u8, line, "Content-Length: ")) {
            const value_str = line["Content-Length: ".len..];
            content_length = std.fmt.parseInt(usize, value_str, 10) catch {
                return error.InvalidContentLength;
            };
        }
        // Ignore other headers (Content-Type, etc.)
    }

    const length = content_length orelse return error.MissingContentLength;

    // Read JSON content
    self.read_buffer.clearRetainingCapacity();
    try self.read_buffer.ensureTotalCapacity(self.allocator, length);
    self.read_buffer.items.len = length;

    var reader = self.stdin.reader(&self.stdin_buf);

    // Read exactly 'length' bytes using interface.readSliceAll
    reader.interface.readSliceAll(self.read_buffer.items) catch |err| {
        return err;
    };

    return self.read_buffer.items;
}

/// Read a line ending with \r\n or \n
fn readLine(self: *Transport) ![]const u8 {
    var reader = self.stdin.reader(&self.stdin_buf);

    // Use interface.takeDelimiterExclusive to read until newline
    const line = reader.interface.takeDelimiterExclusive('\n') catch |err| {
        return err;
    };

    // Trim trailing \r if present (for \r\n line endings)
    if (line.len > 0 and line[line.len - 1] == '\r') {
        return line[0 .. line.len - 1];
    }

    return line;
}

/// Write an LSP message to stdout
pub fn writeMessage(self: *Transport, content: []const u8) !void {
    // Write headers
    var header_buf: [64]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{content.len}) catch unreachable;
    try self.stdout.writeAll(header);

    // Write content
    try self.stdout.writeAll(content);
}

/// Send a JSON-RPC response
pub fn sendResponse(self: *Transport, id: Protocol.MessageId, result: anytype) !void {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(self.allocator);

    try buffer.appendSlice(self.allocator, "{\"jsonrpc\":\"2.0\",\"id\":");
    try appendMessageId(&buffer, self.allocator, id);
    try buffer.appendSlice(self.allocator, ",\"result\":");
    try appendJsonValue(&buffer, self.allocator, result);
    try buffer.append(self.allocator, '}');

    try self.writeMessage(buffer.items);
}

/// Send a JSON-RPC error response
pub fn sendError(self: *Transport, id: Protocol.MessageId, code: i32, message: []const u8) !void {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(self.allocator);

    try buffer.appendSlice(self.allocator, "{\"jsonrpc\":\"2.0\",\"id\":");
    try appendMessageId(&buffer, self.allocator, id);
    try buffer.appendSlice(self.allocator, ",\"error\":{\"code\":");
    var code_buf: [16]u8 = undefined;
    const code_str = std.fmt.bufPrint(&code_buf, "{d}", .{code}) catch "0";
    try buffer.appendSlice(self.allocator, code_str);
    try buffer.appendSlice(self.allocator, ",\"message\":");
    try appendJsonString(&buffer, self.allocator, message);
    try buffer.appendSlice(self.allocator, "}}");

    try self.writeMessage(buffer.items);
}

/// Send a JSON-RPC notification (no response expected)
pub fn sendNotification(self: *Transport, method: []const u8, params: anytype) !void {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(self.allocator);

    try buffer.appendSlice(self.allocator, "{\"jsonrpc\":\"2.0\",\"method\":");
    try appendJsonString(&buffer, self.allocator, method);
    try buffer.appendSlice(self.allocator, ",\"params\":");
    try appendJsonValue(&buffer, self.allocator, params);
    try buffer.append(self.allocator, '}');

    try self.writeMessage(buffer.items);
}

/// Send diagnostics notification
pub fn publishDiagnostics(self: *Transport, uri: []const u8, diagnostics: []const Protocol.Diagnostic) !void {
    try self.sendNotification("textDocument/publishDiagnostics", Protocol.PublishDiagnosticsParams{
        .uri = uri,
        .diagnostics = diagnostics,
    });
}

/// Send a log message to the client
pub fn logMessage(self: *Transport, msg_type: Protocol.MessageType, message: []const u8) !void {
    try self.sendNotification("window/logMessage", Protocol.LogMessageParams{
        .type = msg_type,
        .message = message,
    });
}

/// Send a show message notification to the client
pub fn showMessage(self: *Transport, msg_type: Protocol.MessageType, message: []const u8) !void {
    try self.sendNotification("window/showMessage", Protocol.ShowMessageParams{
        .type = msg_type,
        .message = message,
    });
}

// ============================================================================
// JSON Serialization Helpers
// ============================================================================

fn appendMessageId(buffer: *std.ArrayListUnmanaged(u8), alloc: Allocator, id: Protocol.MessageId) !void {
    switch (id) {
        .string => |s| try appendJsonString(buffer, alloc, s),
        .number => |n| {
            var buf: [24]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{n}) catch "0";
            try buffer.appendSlice(alloc, str);
        },
        .@"null" => try buffer.appendSlice(alloc, "null"),
    }
}

fn appendJsonString(buffer: *std.ArrayListUnmanaged(u8), alloc: Allocator, s: []const u8) !void {
    try buffer.append(alloc, '"');
    for (s) |c| {
        switch (c) {
            '"' => try buffer.appendSlice(alloc, "\\\""),
            '\\' => try buffer.appendSlice(alloc, "\\\\"),
            '\n' => try buffer.appendSlice(alloc, "\\n"),
            '\r' => try buffer.appendSlice(alloc, "\\r"),
            '\t' => try buffer.appendSlice(alloc, "\\t"),
            else => {
                if (c < 0x20) {
                    var hex_buf: [8]u8 = undefined;
                    const hex = std.fmt.bufPrint(&hex_buf, "\\u{x:0>4}", .{c}) catch continue;
                    try buffer.appendSlice(alloc, hex);
                } else {
                    try buffer.append(alloc, c);
                }
            },
        }
    }
    try buffer.append(alloc, '"');
}

fn appendJsonValue(buffer: *std.ArrayListUnmanaged(u8), alloc: Allocator, val: anytype) !void {
    const T = @TypeOf(val);
    const type_info = @typeInfo(T);

    switch (type_info) {
        .@"struct" => |s| {
            try buffer.append(alloc, '{');
            inline for (s.fields, 0..) |field, idx| {
                const field_val = @field(val, field.name);
                if (idx > 0) try buffer.append(alloc, ',');
                try appendJsonString(buffer, alloc, field.name);
                try buffer.append(alloc, ':');
                try appendJsonValue(buffer, alloc, field_val);
            }
            try buffer.append(alloc, '}');
        },
        .pointer => |ptr| {
            if (ptr.size == .slice) {
                if (ptr.child == u8) {
                    // String
                    try appendJsonString(buffer, alloc, val);
                } else {
                    // Array
                    try buffer.append(alloc, '[');
                    for (val, 0..) |item, i| {
                        if (i > 0) try buffer.append(alloc, ',');
                        try appendJsonValue(buffer, alloc, item);
                    }
                    try buffer.append(alloc, ']');
                }
            } else {
                try buffer.appendSlice(alloc, "null");
            }
        },
        .optional => {
            if (val) |v| {
                try appendJsonValue(buffer, alloc, v);
            } else {
                try buffer.appendSlice(alloc, "null");
            }
        },
        .int, .comptime_int => {
            var buf: [24]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
            try buffer.appendSlice(alloc, str);
        },
        .float, .comptime_float => {
            var buf: [32]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
            try buffer.appendSlice(alloc, str);
        },
        .bool => {
            try buffer.appendSlice(alloc, if (val) "true" else "false");
        },
        .@"enum" => {
            // Check if enum has jsonStringify method
            if (@hasDecl(T, "jsonStringify")) {
                // Custom serialization - output raw value
                var tmp_buf: [64]u8 = undefined;
                var fbs = std.io.fixedBufferStream(&tmp_buf);
                val.jsonStringify(.{}, fbs.writer()) catch {};
                try buffer.appendSlice(alloc, fbs.getWritten());
            } else {
                try appendJsonString(buffer, alloc, @tagName(val));
            }
        },
        .@"null" => {
            try buffer.appendSlice(alloc, "null");
        },
        else => {
            try buffer.appendSlice(alloc, "null");
        },
    }
}

pub const ReadError = error{
    InvalidContentLength,
    MissingContentLength,
    UnexpectedEof,
    EndOfStream,
    ReadFailed,
    StreamTooLong,
    OutOfMemory,
    InputOutput,
    BrokenPipe,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    NotOpenForReading,
    SystemResources,
    Unexpected,
    WouldBlock,
    OperationAborted,
    IsDir,
    AccessDenied,
};

test "appendJsonString escapes correctly" {
    const testing = std.testing;
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(testing.allocator);

    try appendJsonString(&buffer, testing.allocator, "hello\nworld");
    try testing.expectEqualStrings("\"hello\\nworld\"", buffer.items);
}

test "appendJsonValue serializes struct" {
    const testing = std.testing;
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(testing.allocator);

    const TestStruct = struct {
        name: []const u8,
        value: i32,
    };

    try appendJsonValue(&buffer, testing.allocator, TestStruct{ .name = "test", .value = 42 });
    try testing.expectEqualStrings("{\"name\":\"test\",\"value\":42}", buffer.items);
}
