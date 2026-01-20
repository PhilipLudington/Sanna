//! sanna init - Create a new Sanna project
//!
//! Creates a new Sanna project with the standard directory structure
//! and a default sanna.toml configuration file.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const Config = @import("../Config.zig");

/// Run the init command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Get project name from args or use current directory name
    const project_name = args.getFlagValue('n', "name") orelse
        args.nextPositional() orelse
        std.fs.path.basename(cli.cwd);

    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Determine project directory
    const project_dir = args.nextPositional() orelse cli.cwd;

    cli.output.info("Creating new Sanna project: {s}", .{project_name});

    // Create directory structure
    createProjectStructure(cli.allocator, project_dir) catch |err| {
        cli.output.err("Failed to create project structure: {}", .{err});
        return 1;
    };

    // Create sanna.toml
    createConfigFile(cli.allocator, project_dir, project_name) catch |err| {
        cli.output.err("Failed to create sanna.toml: {}", .{err});
        return 1;
    };

    // Create example spec file
    createExampleSpec(cli.allocator, project_dir) catch |err| {
        cli.output.err("Failed to create example spec: {}", .{err});
        return 1;
    };

    // Create .gitignore
    createGitignore(cli.allocator, project_dir) catch |err| {
        cli.output.err("Failed to create .gitignore: {}", .{err});
        return 1;
    };

    cli.output.success("Project created successfully!", .{});
    cli.output.print("\nNext steps:\n", .{});
    cli.output.print("  cd {s}\n", .{project_dir});
    cli.output.print("  sanna check specs/\n", .{});
    cli.output.print("  sanna generate --target klar specs/example.sanna\n", .{});

    return 0;
}

fn createProjectStructure(allocator: Allocator, base_path: []const u8) !void {
    const dirs = [_][]const u8{
        "specs",
        "src",
        "tests",
        ".sanna-cache",
    };

    for (dirs) |dir| {
        const full_path = try std.fs.path.join(allocator, &.{ base_path, dir });
        defer allocator.free(full_path);

        std.fs.cwd().makePath(full_path) catch |err| switch (err) {
            error.PathAlreadyExists => {}, // OK if exists
            else => return err,
        };
    }
}

fn createConfigFile(allocator: Allocator, base_path: []const u8, project_name: []const u8) !void {
    const config_path = try std.fs.path.join(allocator, &.{ base_path, "sanna.toml" });
    defer allocator.free(config_path);

    // Check if file already exists
    if (std.fs.cwd().access(config_path, .{})) {
        return; // Don't overwrite existing config
    } else |_| {}

    const file = try std.fs.cwd().createFile(config_path, .{});
    defer file.close();

    // Generate config with project name
    var content_buf: [2048]u8 = undefined;
    const content = std.fmt.bufPrint(&content_buf,
        \\# Sanna Project Configuration
        \\
        \\[project]
        \\name = "{s}"
        \\version = "0.1.0"
        \\
        \\[generation]
        \\target = "klar"              # Target language: klar, kira, zig, rust, typescript, python
        \\default_model = "anthropic/claude-3-opus"
        \\confidence_threshold = 0.7   # Warn below this confidence
        \\emit_contracts = true        # Generate runtime contract checks
        \\emit_docs = true             # Generate documentation comments
        \\
        \\[verification]
        \\timeout = 30                 # Seconds per proof obligation
        \\solver = "z3"                # SMT solver: z3 or cvc5
        \\parallel = true              # Verify obligations in parallel
        \\cache_enabled = true         # Cache verification results
        \\cache_dir = ".sanna-cache"   # Cache directory
        \\
        \\[trust]
        \\require_review_below = 0.6   # Require human review below this score
        \\auto_approve_above = 0.95    # Auto-approve above this (proven + high confidence)
        \\block_deployment_below = 0.3 # Block deployment below this
        \\age_decay_enabled = true     # Enable trust decay over time
        \\age_decay_half_life_days = 90
        \\
        \\[provenance]
        \\require_identity = true      # All code must have an author
        \\require_approval_for_ai = true # AI-generated code needs approval
        \\
    , .{project_name}) catch return error.BufferOverflow;

    try file.writeAll(content);
}

fn createExampleSpec(allocator: Allocator, base_path: []const u8) !void {
    const spec_path = try std.fs.path.join(allocator, &.{ base_path, "specs", "example.sanna" });
    defer allocator.free(spec_path);

    // Check if file already exists
    if (std.fs.cwd().access(spec_path, .{})) {
        return;
    } else |_| {}

    const file = try std.fs.cwd().createFile(spec_path, .{});
    defer file.close();

    const content =
        \\// Example Sanna specification
        \\module example
        \\
        \\// A simple type with an invariant
        \\type PositiveInt = i32
        \\  invariant:
        \\    self > 0
        \\
        \\// A function specification with pre/post conditions
        \\spec fn divide(a: i32, b: PositiveInt) -> i32
        \\  requires:
        \\    b != 0
        \\  ensures:
        \\    result * b + (a % b) == a
        \\
        \\// A specification with old() for referencing pre-state
        \\spec fn increment(counter: *i32) -> void
        \\  modifies:
        \\    counter
        \\  ensures:
        \\    *counter == old(*counter) + 1
        \\
        \\// A specification with quantifiers
        \\spec fn find_max(items: []i32) -> i32
        \\  requires:
        \\    items.len > 0
        \\  ensures:
        \\    forall i in 0..items.len: result >= items[i]
        \\    exists j in 0..items.len: result == items[j]
        \\
    ;

    try file.writeAll(content);
}

fn createGitignore(allocator: Allocator, base_path: []const u8) !void {
    const gitignore_path = try std.fs.path.join(allocator, &.{ base_path, ".gitignore" });
    defer allocator.free(gitignore_path);

    // Check if file already exists
    if (std.fs.cwd().access(gitignore_path, .{})) {
        return;
    } else |_| {}

    const file = try std.fs.cwd().createFile(gitignore_path, .{});
    defer file.close();

    const content =
        \\# Sanna cache
        \\.sanna-cache/
        \\
        \\# Generated code
        \\*.generated.*
        \\
        \\# Trust reports
        \\trust-report.json
        \\
        \\# Editor files
        \\.vscode/
        \\.idea/
        \\*.swp
        \\*~
        \\
    ;

    try file.writeAll(content);
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna init - Create a new Sanna project
        \\
        \\USAGE:
        \\    sanna init [OPTIONS] [DIRECTORY]
        \\
        \\ARGUMENTS:
        \\    DIRECTORY    Directory to create project in (default: current directory)
        \\
        \\OPTIONS:
        \\    -n, --name <NAME>    Project name (default: directory name)
        \\    -h, --help           Print help information
        \\
        \\EXAMPLES:
        \\    sanna init                    # Initialize in current directory
        \\    sanna init myproject          # Create myproject directory
        \\    sanna init -n myapp ./app     # Create in ./app with name "myapp"
        \\
    ;
    cli.output.print("{s}", .{help});
}

test "createProjectStructure" {
    // Test would need a temp directory
}
