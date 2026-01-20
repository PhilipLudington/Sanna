//! Sanna Language Server Protocol Implementation
//!
//! This module provides LSP support for the Sanna specification language,
//! enabling IDE features like:
//!
//! - Syntax error highlighting
//! - Type error diagnostics
//! - Code completion
//! - Hover information with trust scores
//! - Go-to-definition
//! - Custom commands for approval/review workflows
//!
//! ## Usage
//!
//! The LSP server can be started via the CLI:
//! ```
//! sanna lsp
//! ```
//!
//! Or directly:
//! ```
//! sanna-lsp
//! ```

const std = @import("std");

pub const Server = @import("Server.zig");
pub const Protocol = @import("Protocol.zig");
pub const Transport = @import("Transport.zig");
pub const DocumentStore = @import("DocumentStore.zig");
pub const Analyzer = @import("Analyzer.zig");

/// Start the LSP server
pub fn run(allocator: std.mem.Allocator) !void {
    var server = Server.init(allocator);
    defer server.deinit();
    try server.run();
}

test {
    _ = Server;
    _ = Protocol;
    _ = Transport;
    _ = DocumentStore;
    _ = Analyzer;
}
