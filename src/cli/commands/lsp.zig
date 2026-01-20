//! sanna lsp - Start the Language Server Protocol server
//!
//! Starts the Sanna LSP server for IDE integration.
//! The server communicates via stdin/stdout using the JSON-RPC protocol.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const lsp = @import("../../lsp/root.zig");

/// Run the lsp command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Start the LSP server
    lsp.run(cli.allocator) catch |err| {
        // Log errors to stderr since stdout is used for LSP
        const stderr = std.fs.File.stderr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "LSP server error: {}\n", .{err}) catch "LSP server error\n";
        stderr.writeAll(msg) catch {};
        return 1;
    };

    return 0;
}

fn printHelp(cli: *Cli) void {
    cli.output.print(
        \\sanna lsp - Start the Language Server Protocol server
        \\
        \\USAGE:
        \\    sanna lsp [OPTIONS]
        \\
        \\DESCRIPTION:
        \\    Starts the Sanna LSP server for IDE integration. The server
        \\    communicates over stdin/stdout using JSON-RPC 2.0.
        \\
        \\    The LSP server provides:
        \\      - Syntax error highlighting
        \\      - Type error diagnostics
        \\      - Code completion for keywords and types
        \\      - Hover information with trust scores
        \\      - Go-to-definition for identifiers
        \\      - Custom commands for approve/review workflows
        \\
        \\OPTIONS:
        \\    -h, --help    Show this help message
        \\
        \\IDE CONFIGURATION:
        \\
        \\    VS Code:
        \\        Install the Sanna extension from the marketplace, or
        \\        configure a custom language server pointing to this command.
        \\
        \\    Neovim (with nvim-lspconfig):
        \\        require('lspconfig.configs').sanna = {{
        \\          default_config = {{
        \\            cmd = {{ 'sanna', 'lsp' }},
        \\            filetypes = {{ 'sanna' }},
        \\            root_dir = function(fname)
        \\              return require('lspconfig.util').find_git_ancestor(fname)
        \\            end,
        \\          }},
        \\        }}
        \\        require('lspconfig').sanna.setup{{}}
        \\
        \\    Other editors:
        \\        Configure the language server with:
        \\          Command: sanna lsp
        \\          File types: *.sanna
        \\
    , .{});
}
