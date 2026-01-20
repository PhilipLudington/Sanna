//! Sanna CLI - Command-line interface for Sanna specifications
//!
//! Provides commands for working with Sanna specification files:
//! - init: Create new Sanna project
//! - check: Validate specification syntax and types
//! - generate: Generate code from specifications
//! - verify: Run verification against implementations
//! - trust: Generate and query trust reports
//! - review: List items needing review
//! - approve: Approve code for deployment
//! - repl: Interactive mode for specifications
//! - lsp: Start the Language Server Protocol server

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Config = @import("Config.zig");
pub const Args = @import("Args.zig");
pub const Output = @import("Output.zig");

pub const commands = struct {
    pub const init = @import("commands/init.zig");
    pub const check = @import("commands/check.zig");
    pub const generate = @import("commands/generate.zig");
    pub const verify = @import("commands/verify.zig");
    pub const trust = @import("commands/trust.zig");
    pub const review = @import("commands/review.zig");
    pub const approve = @import("commands/approve.zig");
    pub const repl = @import("commands/repl.zig");
    pub const lsp = @import("commands/lsp.zig");
};

/// CLI application state
pub const Cli = struct {
    allocator: Allocator,
    config: ?Config,
    output: Output,
    cwd: []const u8,

    pub fn init(allocator: Allocator) !Cli {
        const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
        errdefer allocator.free(cwd);

        return .{
            .allocator = allocator,
            .config = null,
            .output = Output.init(false),
            .cwd = cwd,
        };
    }

    pub fn deinit(self: *Cli) void {
        if (self.config) |*cfg| {
            cfg.deinit();
        }
        self.allocator.free(self.cwd);
    }

    /// Load project configuration from sanna.toml
    pub fn loadConfig(self: *Cli) !void {
        self.config = Config.load(self.allocator, self.cwd) catch |err| switch (err) {
            error.FileNotFound => null,
            else => return err,
        };
    }

    /// Run the CLI with given arguments
    pub fn run(self: *Cli, args: *Args) !u8 {
        // Check for global flags before command (--json, --no-color)
        // Note: --help and --version are checked per-command to allow command-specific help
        while (args.peekFlag()) |flag| {
            if (std.mem.eql(u8, flag, "json")) {
                _ = args.nextFlag();
                self.output.json_mode = true;
            } else if (std.mem.eql(u8, flag, "no-color")) {
                _ = args.nextFlag();
                self.output.color_enabled = false;
            } else {
                break; // Stop at unrecognized flags
            }
        }

        const command = args.nextPositional() orelse {
            // No command - check for --help or --version
            if (args.hasFlag('h', "help")) {
                self.output.printHelp();
                return 0;
            }
            if (args.hasFlag('V', "version")) {
                self.output.printVersion();
                return 0;
            }
            self.output.printHelp();
            return 0;
        };

        // Dispatch to command handlers (commands handle their own --help)
        if (std.mem.eql(u8, command, "init")) {
            return commands.init.run(self, args);
        } else if (std.mem.eql(u8, command, "check")) {
            try self.loadConfig();
            return commands.check.run(self, args);
        } else if (std.mem.eql(u8, command, "generate")) {
            try self.loadConfig();
            return commands.generate.run(self, args);
        } else if (std.mem.eql(u8, command, "verify")) {
            try self.loadConfig();
            return commands.verify.run(self, args);
        } else if (std.mem.eql(u8, command, "trust")) {
            try self.loadConfig();
            return commands.trust.run(self, args);
        } else if (std.mem.eql(u8, command, "review")) {
            try self.loadConfig();
            return commands.review.run(self, args);
        } else if (std.mem.eql(u8, command, "approve")) {
            try self.loadConfig();
            return commands.approve.run(self, args);
        } else if (std.mem.eql(u8, command, "repl")) {
            return commands.repl.run(self, args);
        } else if (std.mem.eql(u8, command, "lsp")) {
            return commands.lsp.run(self, args);
        } else if (std.mem.eql(u8, command, "help")) {
            self.output.printHelp();
            return 0;
        } else {
            self.output.err("Unknown command: {s}", .{command});
            self.output.printHelp();
            return 1;
        }
    }
};

test {
    _ = Config;
    _ = Args;
    _ = Output;
    _ = commands.init;
    _ = commands.check;
    _ = commands.generate;
    _ = commands.verify;
    _ = commands.trust;
    _ = commands.review;
    _ = commands.approve;
    _ = commands.repl;
    _ = commands.lsp;
}
