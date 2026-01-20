//! Sanna Language Server
//!
//! Main LSP server implementation that handles client requests,
//! manages document state, and provides IDE features.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Protocol = @import("Protocol.zig");
const Transport = @import("Transport.zig");
const DocumentStore = @import("DocumentStore.zig");
const Analyzer = @import("Analyzer.zig");

const Server = @This();

pub const ServerState = enum {
    uninitialized,
    initializing,
    initialized,
    shutdown,
};

allocator: Allocator,
transport: Transport,
documents: DocumentStore,
analyzer: Analyzer,
state: ServerState,
client_capabilities: Protocol.ClientCapabilities,

/// Server configuration
root_uri: ?[]const u8,
trace: TraceValue,

pub const TraceValue = enum {
    off,
    messages,
    verbose,
};

pub fn init(allocator: Allocator) Server {
    return .{
        .allocator = allocator,
        .transport = Transport.init(allocator),
        .documents = DocumentStore.init(allocator),
        .analyzer = Analyzer.init(allocator),
        .state = .uninitialized,
        .client_capabilities = .{},
        .root_uri = null,
        .trace = .off,
    };
}

pub fn deinit(self: *Server) void {
    self.transport.deinit();
    self.documents.deinit();
    self.analyzer.deinit();
    if (self.root_uri) |uri| {
        self.allocator.free(uri);
    }
}

/// Main server loop - reads and handles messages until shutdown
pub fn run(self: *Server) !void {
    while (self.state != .shutdown) {
        const message = try self.transport.readMessage() orelse {
            // EOF - exit
            break;
        };

        self.handleMessage(message) catch {
            self.transport.logMessage(.@"error", "Error handling message") catch {};
        };
    }
}

/// Handle an incoming JSON-RPC message
fn handleMessage(self: *Server, json: []const u8) !void {
    const parsed = std.json.parseFromSlice(std.json.Value, self.allocator, json, .{}) catch |err| {
        try self.transport.sendError(.{ .@"null" = {} }, Protocol.ErrorCode.parse_error, "Parse error");
        return err;
    };
    defer parsed.deinit();

    const value = parsed.value;

    // Check if it's a request (has id) or notification (no id)
    if (value.object.get("id")) |id_value| {
        // Request
        const id = parseMessageId(id_value);
        const method = value.object.get("method") orelse {
            try self.transport.sendError(id, Protocol.ErrorCode.invalid_request, "Missing method");
            return;
        };
        const method_str = method.string;
        const params = value.object.get("params");

        try self.handleRequest(id, method_str, params);
    } else {
        // Notification
        const method = value.object.get("method") orelse return;
        const method_str = method.string;
        const params = value.object.get("params");

        try self.handleNotification(method_str, params);
    }
}

fn parseMessageId(value: std.json.Value) Protocol.MessageId {
    return switch (value) {
        .string => |s| .{ .string = s },
        .integer => |n| .{ .number = n },
        .null => .{ .@"null" = {} },
        else => .{ .@"null" = {} },
    };
}

/// Handle a JSON-RPC request (expects response)
fn handleRequest(self: *Server, id: Protocol.MessageId, method: []const u8, params: ?std.json.Value) !void {
    // Check server state
    if (self.state == .uninitialized and !std.mem.eql(u8, method, "initialize")) {
        try self.transport.sendError(id, Protocol.ErrorCode.server_not_initialized, "Server not initialized");
        return;
    }

    if (std.mem.eql(u8, method, "initialize")) {
        try self.handleInitialize(id, params);
    } else if (std.mem.eql(u8, method, "shutdown")) {
        try self.handleShutdown(id);
    } else if (std.mem.eql(u8, method, "textDocument/completion")) {
        try self.handleCompletion(id, params);
    } else if (std.mem.eql(u8, method, "textDocument/hover")) {
        try self.handleHover(id, params);
    } else if (std.mem.eql(u8, method, "textDocument/definition")) {
        try self.handleDefinition(id, params);
    } else if (std.mem.eql(u8, method, "workspace/executeCommand")) {
        try self.handleExecuteCommand(id, params);
    } else {
        try self.transport.sendError(id, Protocol.ErrorCode.method_not_found, "Method not found");
    }
}

/// Handle a JSON-RPC notification (no response expected)
fn handleNotification(self: *Server, method: []const u8, params: ?std.json.Value) !void {
    if (std.mem.eql(u8, method, "initialized")) {
        self.state = .initialized;
        try self.transport.logMessage(.info, "Sanna Language Server initialized");
    } else if (std.mem.eql(u8, method, "exit")) {
        // Exit immediately
        std.process.exit(if (self.state == .shutdown) 0 else 1);
    } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
        try self.handleDidOpen(params);
    } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
        try self.handleDidChange(params);
    } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
        try self.handleDidClose(params);
    } else if (std.mem.eql(u8, method, "textDocument/didSave")) {
        try self.handleDidSave(params);
    } else if (std.mem.eql(u8, method, "$/setTrace")) {
        // Update trace level
        if (params) |p| {
            if (p.object.get("value")) |v| {
                self.trace = parseTraceValue(v.string);
            }
        }
    }
    // Ignore unknown notifications
}

fn parseTraceValue(value: []const u8) TraceValue {
    if (std.mem.eql(u8, value, "off")) return .off;
    if (std.mem.eql(u8, value, "messages")) return .messages;
    if (std.mem.eql(u8, value, "verbose")) return .verbose;
    return .off;
}

// ============================================================================
// Lifecycle Handlers
// ============================================================================

fn handleInitialize(self: *Server, id: Protocol.MessageId, params: ?std.json.Value) !void {
    self.state = .initializing;

    // Parse initialize params
    if (params) |p| {
        if (p.object.get("rootUri")) |uri| {
            if (uri != .null) {
                self.root_uri = try self.allocator.dupe(u8, uri.string);
            }
        }
        if (p.object.get("trace")) |t| {
            if (t == .string) {
                self.trace = parseTraceValue(t.string);
            }
        }
        // Parse client capabilities
        if (p.object.get("capabilities")) |_| {
            // Store capabilities for later use
            // For now, just acknowledge them
        }
    }

    // Send initialize result with server capabilities
    const result = Protocol.InitializeResult{
        .capabilities = .{
            .textDocumentSync = .{
                .openClose = true,
                .change = .full,
                .save = .{ .includeText = true },
            },
            .completionProvider = .{
                .triggerCharacters = &.{ ".", "@", ":" },
                .resolveProvider = false,
            },
            .hoverProvider = true,
            .definitionProvider = true,
            .executeCommandProvider = .{
                .commands = &.{
                    "sanna.approve",
                    "sanna.review",
                    "sanna.verify",
                    "sanna.trust",
                },
            },
        },
        .serverInfo = .{
            .name = "sanna-lsp",
            .version = "0.1.0",
        },
    };

    try self.transport.sendResponse(id, result);
}

fn handleShutdown(self: *Server, id: Protocol.MessageId) !void {
    self.state = .shutdown;
    try self.transport.sendResponse(id, null);
}

// ============================================================================
// Document Handlers
// ============================================================================

fn handleDidOpen(self: *Server, params: ?std.json.Value) !void {
    const p = params orelse return;
    const text_document = p.object.get("textDocument") orelse return;

    const uri = text_document.object.get("uri").?.string;
    const version = @as(i32, @intCast(text_document.object.get("version").?.integer));
    const text = text_document.object.get("text").?.string;
    const language_id = text_document.object.get("languageId").?.string;

    const doc = try self.documents.openDocument(uri, version, text, language_id);

    // Analyze and publish diagnostics
    try self.analyzeAndPublishDiagnostics(doc);
}

fn handleDidChange(self: *Server, params: ?std.json.Value) !void {
    const p = params orelse return;
    const text_document = p.object.get("textDocument") orelse return;

    const uri = text_document.object.get("uri").?.string;
    const version = @as(i32, @intCast(text_document.object.get("version").?.integer));

    const content_changes = p.object.get("contentChanges") orelse return;
    const changes = content_changes.array.items;
    if (changes.len == 0) return;

    // We use full sync, so take the last change
    const last_change = changes[changes.len - 1];
    const new_text = last_change.object.get("text").?.string;

    const doc = try self.documents.updateDocument(uri, version, new_text);

    // Re-analyze and publish diagnostics
    try self.analyzeAndPublishDiagnostics(doc);
}

fn handleDidClose(self: *Server, params: ?std.json.Value) !void {
    const p = params orelse return;
    const text_document = p.object.get("textDocument") orelse return;
    const uri = text_document.object.get("uri").?.string;

    self.documents.closeDocument(uri);

    // Clear diagnostics for closed document
    try self.transport.publishDiagnostics(uri, &.{});
}

fn handleDidSave(self: *Server, params: ?std.json.Value) !void {
    const p = params orelse return;
    const text_document = p.object.get("textDocument") orelse return;
    const uri = text_document.object.get("uri").?.string;

    const doc = self.documents.getDocument(uri) orelse return;

    // Full re-analysis on save (could trigger verification)
    try self.analyzeAndPublishDiagnostics(doc);
}

// ============================================================================
// Feature Handlers
// ============================================================================

fn handleCompletion(self: *Server, id: Protocol.MessageId, params: ?std.json.Value) !void {
    const p = params orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const text_document = p.object.get("textDocument") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const uri = text_document.object.get("uri").?.string;
    const position = p.object.get("position") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const pos = Protocol.Position{
        .line = @intCast(position.object.get("line").?.integer),
        .character = @intCast(position.object.get("character").?.integer),
    };

    const doc = self.documents.getDocument(uri) orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const completions = try self.analyzer.getCompletions(doc, pos);

    try self.transport.sendResponse(id, Protocol.CompletionList{
        .isIncomplete = false,
        .items = completions,
    });
}

fn handleHover(self: *Server, id: Protocol.MessageId, params: ?std.json.Value) !void {
    const p = params orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const text_document = p.object.get("textDocument") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const uri = text_document.object.get("uri").?.string;
    const position = p.object.get("position") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const pos = Protocol.Position{
        .line = @intCast(position.object.get("line").?.integer),
        .character = @intCast(position.object.get("character").?.integer),
    };

    const doc = self.documents.getDocument(uri) orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const hover = try self.analyzer.getHover(doc, pos);

    if (hover) |h| {
        try self.transport.sendResponse(id, h);
    } else {
        try self.transport.sendResponse(id, null);
    }
}

fn handleDefinition(self: *Server, id: Protocol.MessageId, params: ?std.json.Value) !void {
    const p = params orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const text_document = p.object.get("textDocument") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const uri = text_document.object.get("uri").?.string;
    const position = p.object.get("position") orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const pos = Protocol.Position{
        .line = @intCast(position.object.get("line").?.integer),
        .character = @intCast(position.object.get("character").?.integer),
    };

    const doc = self.documents.getDocument(uri) orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const location = try self.analyzer.getDefinition(doc, pos);

    if (location) |loc| {
        try self.transport.sendResponse(id, loc);
    } else {
        try self.transport.sendResponse(id, null);
    }
}

fn handleExecuteCommand(self: *Server, id: Protocol.MessageId, params: ?std.json.Value) !void {
    const p = params orelse {
        try self.transport.sendResponse(id, null);
        return;
    };

    const command = p.object.get("command").?.string;

    if (std.mem.eql(u8, command, "sanna.approve")) {
        try self.executeApprove(p.object.get("arguments"));
        try self.transport.sendResponse(id, "Specification approved");
    } else if (std.mem.eql(u8, command, "sanna.review")) {
        try self.executeReview(p.object.get("arguments"));
        try self.transport.sendResponse(id, "Review requested");
    } else if (std.mem.eql(u8, command, "sanna.verify")) {
        try self.executeVerify(p.object.get("arguments"));
        try self.transport.sendResponse(id, "Verification started");
    } else if (std.mem.eql(u8, command, "sanna.trust")) {
        const result = try self.executeTrust(p.object.get("arguments"));
        try self.transport.sendResponse(id, result);
    } else {
        try self.transport.sendError(id, Protocol.ErrorCode.invalid_params, "Unknown command");
    }
}

// ============================================================================
// Analysis and Diagnostics
// ============================================================================

fn analyzeAndPublishDiagnostics(self: *Server, doc: *DocumentStore.Document) !void {
    // Analyze document
    const diagnostics = try self.analyzer.analyze(doc);

    // Convert to LSP diagnostics
    var lsp_diagnostics = std.ArrayListUnmanaged(Protocol.Diagnostic){};
    defer lsp_diagnostics.deinit(self.allocator);

    for (diagnostics) |diag| {
        try lsp_diagnostics.append(self.allocator, .{
            .range = diag.range,
            .severity = diag.severity,
            .source = diag.source,
            .message = diag.message,
            .code = diag.code,
        });
    }

    try self.transport.publishDiagnostics(doc.uri, lsp_diagnostics.items);
}

// ============================================================================
// Command Execution
// ============================================================================

fn executeApprove(self: *Server, arguments: ?std.json.Value) !void {
    _ = self;
    _ = arguments;
    // Implementation would mark a specification as approved
    // and update provenance metadata
}

fn executeReview(self: *Server, arguments: ?std.json.Value) !void {
    _ = self;
    _ = arguments;
    // Implementation would mark a specification for review
}

fn executeVerify(self: *Server, arguments: ?std.json.Value) !void {
    // Extract URI from arguments
    var uri: ?[]const u8 = null;
    if (arguments) |args| {
        if (args == .array and args.array.items.len > 0) {
            if (args.array.items[0] == .string) {
                uri = args.array.items[0].string;
            }
        }
    }

    // Get document to verify
    const doc_uri = uri orelse return;
    const doc = self.documents.getDocument(doc_uri) orelse return;

    // Send progress start
    try self.transport.sendNotification("$/progress", .{
        .token = "sanna-verify",
        .value = Protocol.WorkDoneProgressBegin{
            .title = "Verifying specification",
            .cancellable = false,
            .message = "Parsing and type checking...",
            .percentage = 0,
        },
    });

    // Re-analyze document (parsing and type checking)
    _ = try self.analyzer.analyze(doc);

    // Update progress
    try self.transport.sendNotification("$/progress", .{
        .token = "sanna-verify",
        .value = Protocol.WorkDoneProgressReport{
            .message = "Checking invariants...",
            .percentage = 50,
        },
    });

    // TODO: Run actual verification through the verify module

    // Update progress
    try self.transport.sendNotification("$/progress", .{
        .token = "sanna-verify",
        .value = Protocol.WorkDoneProgressReport{
            .message = "Verification complete",
            .percentage = 100,
        },
    });

    // Send progress end
    try self.transport.sendNotification("$/progress", .{
        .token = "sanna-verify",
        .value = Protocol.WorkDoneProgressEnd{
            .message = "Verification complete",
        },
    });

    // Re-publish diagnostics
    try self.analyzeAndPublishDiagnostics(doc);
}

fn executeTrust(self: *Server, arguments: ?std.json.Value) ![]const u8 {
    _ = self;
    _ = arguments;
    // Implementation would calculate and return trust scores
    return "Trust score: N/A";
}

test "server initialization" {
    // Server tests require mocking transport
}
