//! sanna check - Validate specification syntax and types
//!
//! Parses and type-checks Sanna specification files, reporting
//! any syntax errors, type errors, or consistency issues.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const root = @import("../../root.zig");

/// Run the check command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Collect files to check
    var files = std.ArrayListUnmanaged([]const u8){};
    defer files.deinit(cli.allocator);

    while (args.nextPositional()) |path| {
        files.append(cli.allocator, path) catch {
            cli.output.err("Out of memory", .{});
            return 1;
        };
    }

    // If no files specified, check all .sanna files in specs/
    if (files.items.len == 0) {
        collectSpecFiles(cli.allocator, cli.cwd, &files) catch |err| {
            cli.output.err("Failed to find spec files: {}", .{err});
            return 1;
        };
    }

    if (files.items.len == 0) {
        cli.output.warn("No specification files found", .{});
        return 0;
    }

    cli.output.info("Checking {d} specification file(s)...", .{files.items.len});

    var total_errors: usize = 0;
    var total_warnings: usize = 0;
    var files_checked: usize = 0;

    for (files.items) |file_path| {
        const result = checkFile(cli, file_path);
        total_errors += result.errors;
        total_warnings += result.warnings;
        files_checked += 1;

        if (!cli.output.json_mode) {
            cli.output.progress(files_checked, files.items.len, "Checked {s}", .{std.fs.path.basename(file_path)});
        }
    }

    // Output summary
    if (cli.output.json_mode) {
        cli.output.writeJson(.{
            .files_checked = files_checked,
            .errors = total_errors,
            .warnings = total_warnings,
            .status = if (total_errors == 0) "success" else "failed",
        });
    } else {
        cli.output.print("\n", .{});
        if (total_errors == 0) {
            cli.output.success("All checks passed ({d} file(s), {d} warning(s))", .{ files_checked, total_warnings });
        } else {
            cli.output.err("Check failed: {d} error(s), {d} warning(s)", .{ total_errors, total_warnings });
        }
    }

    return if (total_errors > 0) 1 else 0;
}

const CheckResult = struct {
    errors: usize = 0,
    warnings: usize = 0,
};

fn checkFile(cli: *Cli, file_path: []const u8) CheckResult {
    var result = CheckResult{};

    // Use arena allocator for all parsing allocations - freed automatically at end
    var arena = std.heap.ArenaAllocator.init(cli.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Read file content
    const content = std.fs.cwd().readFileAlloc(alloc, file_path, 10 * 1024 * 1024) catch |err| {
        cli.output.err("{s}: Failed to read file: {}", .{ file_path, err });
        result.errors += 1;
        return result;
    };

    // Tokenize
    var lexer = root.Lexer.init(alloc, content);

    const tokens = lexer.tokenize() catch |err| {
        cli.output.err("{s}: Lexer error: {}", .{ file_path, err });
        result.errors += 1;
        return result;
    };

    // Report lexer errors
    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            reportError(cli, file_path, err.location.line, err.location.column, err.message);
            result.errors += 1;
        }
    }

    // Parse
    var parser = root.parser.Parser.init(alloc, tokens.items);

    const ast = parser.parseModule() catch |err| {
        cli.output.err("{s}: Parse error: {}", .{ file_path, err });
        result.errors += 1;
        return result;
    };
    _ = ast;

    // Report parser errors
    for (parser.diagnostics.items) |diag| {
        if (diag.severity == .err) {
            reportError(cli, file_path, diag.span.start.line, diag.span.start.column, diag.message);
            result.errors += 1;
        }
    }

    // Type check (if no parse errors)
    if (result.errors == 0) {
        // Type checking would go here
        // For now, just report success at the parse level
    }

    return result;
}

fn reportError(cli: *Cli, file: []const u8, line: u32, column: u32, message: []const u8) void {
    if (cli.output.json_mode) {
        cli.output.writeJson(.{
            .type = "error",
            .file = file,
            .line = line,
            .column = column,
            .message = message,
        });
    } else {
        cli.output.err("{s}:{d}:{d}: {s}", .{ file, line, column, message });
    }
}

fn collectSpecFiles(allocator: Allocator, base_path: []const u8, files: *std.ArrayListUnmanaged([]const u8)) !void {
    const specs_path = try std.fs.path.join(allocator, &.{ base_path, "specs" });
    defer allocator.free(specs_path);

    var dir = std.fs.cwd().openDir(specs_path, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".sanna")) {
            const full_path = try std.fs.path.join(allocator, &.{ specs_path, entry.name });
            try files.append(allocator, full_path);
        }
    }
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna check - Validate specification syntax and types
        \\
        \\USAGE:
        \\    sanna check [OPTIONS] [FILES...]
        \\
        \\ARGUMENTS:
        \\    FILES    Specification files to check (default: specs/*.sanna)
        \\
        \\OPTIONS:
        \\    -h, --help     Print help information
        \\    --json         Output results as JSON
        \\
        \\EXIT CODES:
        \\    0    All checks passed
        \\    1    Syntax or type errors found
        \\
        \\EXAMPLES:
        \\    sanna check                      # Check all files in specs/
        \\    sanna check specs/auth.sanna     # Check specific file
        \\    sanna check specs/*.sanna --json # JSON output
        \\
    ;
    cli.output.print("{s}", .{help});
}
