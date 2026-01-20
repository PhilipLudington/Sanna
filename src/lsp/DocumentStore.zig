//! Document Store
//!
//! Manages open documents and their analysis results.
//! Provides caching of parse trees, type check results, and diagnostics.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Protocol = @import("Protocol.zig");

const sanna = @import("../root.zig");
const Lexer = sanna.Lexer;
const Ast = sanna.Ast;
const Token = sanna.Token.Token;

const DocumentStore = @This();

/// Represents an open document with its analysis state
pub const Document = struct {
    uri: []const u8,
    version: i32,
    content: []const u8,
    language_id: []const u8,

    /// Cached analysis results
    tokens: ?[]const Token = null,
    ast: ?Ast.Module = null,
    parse_diagnostics: std.ArrayListUnmanaged(Diagnostic) = .{},
    type_diagnostics: std.ArrayListUnmanaged(Diagnostic) = .{},
    semantic_diagnostics: std.ArrayListUnmanaged(Diagnostic) = .{},

    /// Arena for all document allocations
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator, uri: []const u8, version: i32, content: []const u8, language_id: []const u8) !Document {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        const alloc = arena.allocator();

        return .{
            .uri = try alloc.dupe(u8, uri),
            .version = version,
            .content = try alloc.dupe(u8, content),
            .language_id = try alloc.dupe(u8, language_id),
            .arena = arena,
        };
    }

    pub fn deinit(self: *Document) void {
        self.arena.deinit();
    }

    /// Update document content
    pub fn update(self: *Document, allocator: Allocator, version: i32, content: []const u8) !void {
        // Create new arena and copy content
        var new_arena = std.heap.ArenaAllocator.init(allocator);
        errdefer new_arena.deinit();

        const alloc = new_arena.allocator();

        const new_uri = try alloc.dupe(u8, self.uri);
        const new_language_id = try alloc.dupe(u8, self.language_id);
        const new_content = try alloc.dupe(u8, content);

        // Free old arena
        self.arena.deinit();

        // Update to new values
        self.arena = new_arena;
        self.uri = new_uri;
        self.language_id = new_language_id;
        self.version = version;
        self.content = new_content;

        // Clear cached analysis results
        self.tokens = null;
        self.ast = null;
        self.parse_diagnostics = .{};
        self.type_diagnostics = .{};
        self.semantic_diagnostics = .{};
    }

    /// Get all diagnostics combined
    pub fn getAllDiagnostics(self: *const Document) []const Diagnostic {
        const total = self.parse_diagnostics.items.len +
            self.type_diagnostics.items.len +
            self.semantic_diagnostics.items.len;

        if (total == 0) return &.{};

        // Return parse diagnostics first (most important)
        // In a real implementation, would combine all into a single array
        return self.parse_diagnostics.items;
    }
};

/// Diagnostic from analysis
pub const Diagnostic = struct {
    range: Protocol.Range,
    severity: Protocol.DiagnosticSeverity,
    message: []const u8,
    source: []const u8,
    code: ?[]const u8 = null,
};

allocator: Allocator,
documents: std.StringHashMapUnmanaged(*Document),

pub fn init(allocator: Allocator) DocumentStore {
    return .{
        .allocator = allocator,
        .documents = .{},
    };
}

pub fn deinit(self: *DocumentStore) void {
    var iter = self.documents.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.*.deinit();
        self.allocator.destroy(entry.value_ptr.*);
    }
    self.documents.deinit(self.allocator);
}

/// Open a new document
pub fn openDocument(self: *DocumentStore, uri: []const u8, version: i32, content: []const u8, language_id: []const u8) !*Document {
    // Check if already open
    if (self.documents.get(uri)) |existing| {
        try existing.update(self.allocator, version, content);
        return existing;
    }

    // Create new document
    const doc = try self.allocator.create(Document);
    errdefer self.allocator.destroy(doc);

    doc.* = try Document.init(self.allocator, uri, version, content, language_id);
    errdefer doc.deinit();

    // Store using the document's owned URI copy
    try self.documents.put(self.allocator, doc.uri, doc);

    return doc;
}

/// Update an existing document
pub fn updateDocument(self: *DocumentStore, uri: []const u8, version: i32, content: []const u8) !*Document {
    const doc = self.documents.get(uri) orelse return error.DocumentNotFound;
    try doc.update(self.allocator, version, content);
    return doc;
}

/// Close a document
pub fn closeDocument(self: *DocumentStore, uri: []const u8) void {
    if (self.documents.fetchRemove(uri)) |kv| {
        kv.value.deinit();
        self.allocator.destroy(kv.value);
    }
}

/// Get a document by URI
pub fn getDocument(self: *DocumentStore, uri: []const u8) ?*Document {
    return self.documents.get(uri);
}

/// Check if a document is open
pub fn isOpen(self: *DocumentStore, uri: []const u8) bool {
    return self.documents.contains(uri);
}

/// Get list of all open document URIs
pub fn getOpenUris(self: *DocumentStore) []const []const u8 {
    var uris = std.ArrayListUnmanaged([]const u8){};
    var iter = self.documents.keyIterator();
    while (iter.next()) |key| {
        uris.append(self.allocator, key.*) catch continue;
    }
    return uris.items;
}

/// Convert a file:// URI to a local path
pub fn uriToPath(uri: []const u8) ?[]const u8 {
    if (std.mem.startsWith(u8, uri, "file://")) {
        return uri["file://".len..];
    }
    return null;
}

/// Convert a local path to a file:// URI
pub fn pathToUri(allocator: Allocator, path: []const u8) ![]const u8 {
    return try std.fmt.allocPrint(allocator, "file://{s}", .{path});
}

test "document lifecycle" {
    const testing = std.testing;
    var store = DocumentStore.init(testing.allocator);
    defer store.deinit();

    // Open document
    const doc = try store.openDocument("file:///test.sanna", 1, "spec Test {}", "sanna");
    try testing.expectEqualStrings("file:///test.sanna", doc.uri);
    try testing.expectEqual(@as(i32, 1), doc.version);

    // Update document
    _ = try store.updateDocument("file:///test.sanna", 2, "spec Test2 {}");
    try testing.expectEqual(@as(i32, 2), doc.version);

    // Close document
    store.closeDocument("file:///test.sanna");
    try testing.expect(!store.isOpen("file:///test.sanna"));
}
