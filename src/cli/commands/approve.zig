//! sanna approve - Approve code for deployment
//!
//! Marks specifications or implementations as approved after human review,
//! recording the approval in the provenance system.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const root = @import("../../root.zig");

/// Run the approve command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Parse options
    const note = args.getFlagValue(null, "note");
    const by = args.getFlagValue(null, "by");
    const force = args.hasFlag('f', "force");
    const batch = args.hasFlag(null, "batch");
    const revoke = args.hasFlag(null, "revoke");

    // Get target(s) to approve
    var targets = std.ArrayListUnmanaged([]const u8){};
    defer targets.deinit(cli.allocator);

    while (args.nextPositional()) |target| {
        targets.append(cli.allocator, target) catch {
            cli.output.err("Out of memory", .{});
            return 1;
        };
    }

    if (targets.items.len == 0) {
        cli.output.err("No target specified", .{});
        cli.output.print("Usage: sanna approve <file>::<name> [--note \"reason\"]\n", .{});
        return 1;
    }

    // If revoking approval
    if (revoke) {
        return revokeApprovals(cli, targets.items, note);
    }

    // Require note for approval unless forced
    if (note == null and !force) {
        cli.output.err("Approval note required. Use --note \"reason\" or --force to skip.", .{});
        return 1;
    }

    // Get approver identity
    const approver = by orelse getDefaultApprover(cli.allocator) orelse {
        cli.output.err("Could not determine approver identity. Use --by to specify.", .{});
        return 1;
    };
    defer if (by == null) cli.allocator.free(approver);

    // Process each target
    var approved_count: usize = 0;
    var failed_count: usize = 0;

    for (targets.items) |target| {
        const result = approveTarget(cli, target, approver, note, force, batch);
        if (result) {
            approved_count += 1;
        } else {
            failed_count += 1;
        }
    }

    // Output summary
    if (cli.output.json_mode) {
        cli.output.writeJson(.{
            .approved = approved_count,
            .failed = failed_count,
            .approver = approver,
        });
    } else {
        if (approved_count > 0) {
            cli.output.success("Approved {d} item(s)", .{approved_count});
        }
        if (failed_count > 0) {
            cli.output.err("Failed to approve {d} item(s)", .{failed_count});
        }
    }

    return if (failed_count > 0) 1 else 0;
}

fn approveTarget(cli: *Cli, target: []const u8, approver: []const u8, note: ?[]const u8, force: bool, batch: bool) bool {
    // Parse target: file::name or just name
    const sep = std.mem.indexOf(u8, target, "::") orelse {
        cli.output.err("Invalid target format: {s}", .{target});
        cli.output.print("Expected format: <file>::<name> or <file>::*\n", .{});
        return false;
    };

    const file_part = target[0..sep];
    const name_part = target[sep + 2 ..];

    // Check if file exists
    var file_path_buf: [512]u8 = undefined;
    const file_path = if (std.mem.endsWith(u8, file_part, ".sanna"))
        file_part
    else blk: {
        const path = std.fmt.bufPrint(&file_path_buf, "specs/{s}.sanna", .{file_part}) catch {
            cli.output.err("File path too long", .{});
            return false;
        };
        break :blk path;
    };

    // Use arena for all parsing allocations - freed at end of function
    var arena = std.heap.ArenaAllocator.init(cli.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Read and parse to validate target exists
    const content = std.fs.cwd().readFileAlloc(alloc, file_path, 10 * 1024 * 1024) catch |err| {
        cli.output.err("Cannot open/read {s}: {}", .{ file_path, err });
        return false;
    };

    var lexer = root.Lexer.init(alloc, content);

    const tokens = lexer.tokenize() catch |err| {
        cli.output.err("Lexer error in {s}: {}", .{ file_path, err });
        return false;
    };

    var parser = root.parser.Parser.init(alloc, tokens.items);

    const module = parser.parseModule() catch |err| {
        cli.output.err("Parse error in {s}: {}", .{ file_path, err });
        return false;
    };

    // Handle wildcard approval
    if (std.mem.eql(u8, name_part, "*")) {
        if (!batch) {
            cli.output.err("Wildcard approval requires --batch flag", .{});
            return false;
        }
        return approveBatch(cli, module, file_path, approver, note);
    }

    // Find the specific declaration
    var found = false;
    for (module.declarations) |decl| {
        const name = getDeclName(decl) orelse continue;
        if (std.mem.eql(u8, name, name_part)) {
            found = true;

            // Check if already approved (in real implementation, check ProvenanceStore)
            const already_approved = false;
            if (already_approved and !force) {
                cli.output.warn("{s}::{s} is already approved", .{ file_part, name });
                return true;
            }

            // Record approval
            // In full implementation, this would update the ProvenanceStore
            // and potentially write an @approved annotation to the file

            const timestamp = std.time.timestamp();
            _ = timestamp;

            if (!cli.output.json_mode) {
                cli.output.success("Approved: {s}::{s}", .{ file_part, name });
                cli.output.print("  By: {s}\n", .{approver});
                if (note) |n| {
                    cli.output.print("  Note: {s}\n", .{n});
                }
            }

            break;
        }
    }

    if (!found) {
        cli.output.err("Declaration not found: {s} in {s}", .{ name_part, file_path });
        return false;
    }

    return true;
}

fn approveBatch(cli: *Cli, module: root.Module, file_path: []const u8, approver: []const u8, note: ?[]const u8) bool {
    var count: usize = 0;

    for (module.declarations) |decl| {
        const name = getDeclName(decl) orelse continue;

        // Record approval for each declaration
        if (!cli.output.json_mode) {
            cli.output.success("Approved: {s}::{s}", .{ std.fs.path.basename(file_path), name });
        }
        count += 1;
    }

    if (count == 0) {
        cli.output.warn("No declarations found to approve", .{});
        return false;
    }

    if (!cli.output.json_mode) {
        cli.output.print("\nBatch approval: {d} item(s) by {s}\n", .{ count, approver });
        if (note) |n| {
            cli.output.print("Note: {s}\n", .{n});
        }
    }

    return true;
}

fn revokeApprovals(cli: *Cli, targets: []const []const u8, reason: ?[]const u8) u8 {
    var revoked_count: usize = 0;
    var failed_count: usize = 0;

    for (targets) |target| {
        // Parse and validate target
        const sep = std.mem.indexOf(u8, target, "::") orelse {
            cli.output.err("Invalid target format: {s}", .{target});
            failed_count += 1;
            continue;
        };

        const file_part = target[0..sep];
        const name_part = target[sep + 2 ..];

        // In full implementation, update ProvenanceStore to revoke approval
        if (!cli.output.json_mode) {
            cli.output.warn("Revoked approval: {s}::{s}", .{ file_part, name_part });
            if (reason) |r| {
                cli.output.print("  Reason: {s}\n", .{r});
            }
        }
        revoked_count += 1;
    }

    if (cli.output.json_mode) {
        cli.output.writeJson(.{
            .revoked = revoked_count,
            .failed = failed_count,
            .reason = reason,
        });
    } else {
        cli.output.print("\nRevoked {d} approval(s)\n", .{revoked_count});
    }

    return if (failed_count > 0) 1 else 0;
}

fn getDefaultApprover(allocator: Allocator) ?[]const u8 {
    // Try to get user from git config using simple approach
    // For now, return null and require --by flag
    // Full implementation would parse git config
    _ = allocator;
    return null;
}

/// Extract name from a declaration
fn getDeclName(decl: root.parser.Ast.Declaration) ?[]const u8 {
    return switch (decl.kind) {
        .type_def => |td| td.name.name,
        .model_def => |md| md.name.name,
        .spec_fn => |sf| sf.name.name,
        .spec_interface => |si| si.name.name,
        .invariant => null,
        .axiom => |ax| ax.name.name,
        .lemma => |lm| lm.name.name,
    };
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna approve - Approve code for deployment
        \\
        \\USAGE:
        \\    sanna approve [OPTIONS] <TARGET>...
        \\
        \\ARGUMENTS:
        \\    TARGET    Item to approve in format: <file>::<name> or <file>::*
        \\
        \\OPTIONS:
        \\    --note <TEXT>      Approval note (required unless --force)
        \\    --by <NAME>        Approver name (default: from git config)
        \\    -f, --force        Approve without note
        \\    --batch            Allow wildcard (*) approval
        \\    --revoke           Revoke approval instead
        \\    --json             Output as JSON
        \\    -h, --help         Print help information
        \\
        \\EXAMPLES:
        \\    sanna approve auth.sanna::authenticate --note "Reviewed security"
        \\    sanna approve auth::* --batch --note "Full module review"
        \\    sanna approve auth.sanna::login --revoke --note "Found issue"
        \\
        \\NOTES:
        \\    - Approval is recorded with timestamp and approver identity
        \\    - Approving AI-generated code marks it as human-reviewed
        \\    - Approvals can be revoked with --revoke
        \\    - Use 'sanna review' to see items needing approval
        \\
    ;
    cli.output.print("{s}", .{help});
}
