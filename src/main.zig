const std = @import("std");
const Sanna = @import("Sanna");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize CLI
    var cli = try Sanna.cli.Cli.init(allocator);
    defer cli.deinit();

    // Parse command-line arguments
    var args = try Sanna.cli.Args.initFromProcess(allocator);
    defer args.deinit(allocator);

    // Run the CLI
    return cli.run(&args);
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(gpa);
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
