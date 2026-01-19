const std = @import("std");
const Sanna = @import("Sanna");

pub fn main() !void {
    // Get stdout with a buffer for writing
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Demo: tokenize a simple spec
    const source =
        \\spec fn divide(a: i32, b: i32) -> i32
        \\  requires:
        \\    b != 0
        \\  ensures:
        \\    result * b + (a % b) == a
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var lexer = Sanna.Lexer.init(allocator, source);
    defer lexer.deinit();

    var tokens = try lexer.tokenize();
    defer tokens.deinit(allocator);

    try stdout.print("Sanna Lexer Demo\n", .{});
    try stdout.print("================\n\n", .{});
    try stdout.print("Source:\n{s}\n\n", .{source});
    try stdout.print("Tokens ({d}):\n", .{tokens.items.len});

    for (tokens.items) |token| {
        try stdout.print("  {s: <20} '{s}'\n", .{ @tagName(token.type), token.lexeme });
    }

    if (lexer.hasErrors()) {
        try stdout.print("\nErrors:\n", .{});
        for (lexer.errors.items) |err| {
            try stdout.print("  Line {d}, Col {d}: {s}\n", .{
                err.location.line,
                err.location.column,
                err.message,
            });
        }
    } else {
        try stdout.print("\nNo lexer errors.\n", .{});
    }

    try stdout.flush();
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(gpa);
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
