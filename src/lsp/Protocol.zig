//! LSP Protocol Types
//!
//! JSON-RPC and Language Server Protocol type definitions for communication
//! between the Sanna language server and IDE clients.

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// JSON-RPC Base Types
// ============================================================================

/// JSON-RPC message ID - can be string or integer
pub const MessageId = union(enum) {
    string: []const u8,
    number: i64,
    @"null",
};

/// JSON-RPC Request message
pub const Request = struct {
    jsonrpc: []const u8 = "2.0",
    id: MessageId,
    method: []const u8,
};

/// JSON-RPC Response message
pub const Response = struct {
    jsonrpc: []const u8 = "2.0",
    id: MessageId,
};

/// JSON-RPC Notification (request without id)
pub const Notification = struct {
    jsonrpc: []const u8 = "2.0",
    method: []const u8,
};

/// JSON-RPC Error
pub const ResponseError = struct {
    code: i32,
    message: []const u8,
};

/// Standard JSON-RPC error codes
pub const ErrorCode = struct {
    pub const parse_error: i32 = -32700;
    pub const invalid_request: i32 = -32600;
    pub const method_not_found: i32 = -32601;
    pub const invalid_params: i32 = -32602;
    pub const internal_error: i32 = -32603;

    // LSP-specific error codes
    pub const server_not_initialized: i32 = -32002;
    pub const unknown_error: i32 = -32001;
    pub const request_cancelled: i32 = -32800;
    pub const content_modified: i32 = -32801;
};

// ============================================================================
// LSP Lifecycle Types
// ============================================================================

/// Initialize request parameters
pub const InitializeParams = struct {
    processId: ?i32 = null,
    rootUri: ?[]const u8 = null,
    rootPath: ?[]const u8 = null,
    capabilities: ClientCapabilities = .{},
    trace: ?[]const u8 = null,
    workspaceFolders: ?[]const WorkspaceFolder = null,
};

pub const ClientCapabilities = struct {
    textDocument: ?TextDocumentClientCapabilities = null,
    workspace: ?WorkspaceClientCapabilities = null,
};

pub const TextDocumentClientCapabilities = struct {
    synchronization: ?TextDocumentSyncClientCapabilities = null,
    completion: ?CompletionClientCapabilities = null,
    hover: ?HoverClientCapabilities = null,
    publishDiagnostics: ?PublishDiagnosticsClientCapabilities = null,
};

pub const TextDocumentSyncClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    willSave: ?bool = null,
    willSaveWaitUntil: ?bool = null,
    didSave: ?bool = null,
};

pub const CompletionClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    completionItem: ?CompletionItemCapabilities = null,
};

pub const CompletionItemCapabilities = struct {
    snippetSupport: ?bool = null,
    commitCharactersSupport: ?bool = null,
    documentationFormat: ?[]const []const u8 = null,
};

pub const HoverClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    contentFormat: ?[]const []const u8 = null,
};

pub const PublishDiagnosticsClientCapabilities = struct {
    relatedInformation: ?bool = null,
    tagSupport: ?DiagnosticTagSupport = null,
};

pub const DiagnosticTagSupport = struct {
    valueSet: ?[]const DiagnosticTag = null,
};

pub const WorkspaceClientCapabilities = struct {
    applyEdit: ?bool = null,
    workspaceEdit: ?WorkspaceEditClientCapabilities = null,
    didChangeConfiguration: ?DidChangeConfigurationClientCapabilities = null,
    executeCommand: ?ExecuteCommandClientCapabilities = null,
};

pub const WorkspaceEditClientCapabilities = struct {
    documentChanges: ?bool = null,
};

pub const DidChangeConfigurationClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const ExecuteCommandClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const WorkspaceFolder = struct {
    uri: []const u8,
    name: []const u8,
};

/// Initialize response result
pub const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?ServerInfo = null,
};

pub const ServerInfo = struct {
    name: []const u8,
    version: ?[]const u8 = null,
};

pub const ServerCapabilities = struct {
    textDocumentSync: ?TextDocumentSyncOptions = null,
    completionProvider: ?CompletionOptions = null,
    hoverProvider: ?bool = null,
    definitionProvider: ?bool = null,
    referencesProvider: ?bool = null,
    documentHighlightProvider: ?bool = null,
    documentSymbolProvider: ?bool = null,
    workspaceSymbolProvider: ?bool = null,
    codeActionProvider: ?bool = null,
    executeCommandProvider: ?ExecuteCommandOptions = null,
};

pub const TextDocumentSyncOptions = struct {
    openClose: ?bool = null,
    change: ?TextDocumentSyncKind = null,
    save: ?SaveOptions = null,
};

/// TextDocumentSyncKind - serializes as integer
pub const TextDocumentSyncKind = enum(u8) {
    none = 0,
    full = 1,
    incremental = 2,

    pub const serialize_as_int = true;
};

pub const SaveOptions = struct {
    includeText: ?bool = null,
};

pub const CompletionOptions = struct {
    triggerCharacters: ?[]const []const u8 = null,
    resolveProvider: ?bool = null,
};

pub const ExecuteCommandOptions = struct {
    commands: []const []const u8,
};

// ============================================================================
// Document Types
// ============================================================================

pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const VersionedTextDocumentIdentifier = struct {
    uri: []const u8,
    version: i32,
};

pub const TextDocumentItem = struct {
    uri: []const u8,
    languageId: []const u8,
    version: i32,
    text: []const u8,
};

pub const DidOpenTextDocumentParams = struct {
    textDocument: TextDocumentItem,
};

pub const DidCloseTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
};

pub const DidChangeTextDocumentParams = struct {
    textDocument: VersionedTextDocumentIdentifier,
    contentChanges: []const TextDocumentContentChangeEvent,
};

pub const TextDocumentContentChangeEvent = struct {
    range: ?Range = null,
    rangeLength: ?u32 = null,
    text: []const u8,
};

pub const DidSaveTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
    text: ?[]const u8 = null,
};

// ============================================================================
// Position and Range Types
// ============================================================================

pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Location = struct {
    uri: []const u8,
    range: Range,
};

pub const LocationLink = struct {
    originSelectionRange: ?Range = null,
    targetUri: []const u8,
    targetRange: Range,
    targetSelectionRange: Range,
};

// ============================================================================
// Diagnostic Types
// ============================================================================

/// DiagnosticSeverity - serializes as integer
pub const DiagnosticSeverity = enum(u8) {
    @"error" = 1,
    warning = 2,
    information = 3,
    hint = 4,

    pub const serialize_as_int = true;
};

/// DiagnosticTag - serializes as integer
pub const DiagnosticTag = enum(u8) {
    unnecessary = 1,
    deprecated = 2,

    pub const serialize_as_int = true;
};

pub const Diagnostic = struct {
    range: Range,
    severity: ?DiagnosticSeverity = null,
    code: ?[]const u8 = null,
    source: ?[]const u8 = null,
    message: []const u8,
    tags: ?[]const DiagnosticTag = null,
    relatedInformation: ?[]const DiagnosticRelatedInformation = null,
};

pub const DiagnosticRelatedInformation = struct {
    location: Location,
    message: []const u8,
};

pub const PublishDiagnosticsParams = struct {
    uri: []const u8,
    version: ?i32 = null,
    diagnostics: []const Diagnostic,
};

// ============================================================================
// Completion Types
// ============================================================================

pub const CompletionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
    context: ?CompletionContext = null,
};

pub const CompletionContext = struct {
    triggerKind: CompletionTriggerKind,
    triggerCharacter: ?[]const u8 = null,
};

pub const CompletionTriggerKind = enum(u8) {
    invoked = 1,
    trigger_character = 2,
    trigger_for_incomplete_completions = 3,

    pub const serialize_as_int = true;
};

pub const CompletionList = struct {
    isIncomplete: bool,
    items: []const CompletionItem,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: ?CompletionItemKind = null,
    detail: ?[]const u8 = null,
    documentation: ?MarkupContent = null,
    deprecated: ?bool = null,
    preselect: ?bool = null,
    sortText: ?[]const u8 = null,
    filterText: ?[]const u8 = null,
    insertText: ?[]const u8 = null,
    insertTextFormat: ?InsertTextFormat = null,
};

/// CompletionItemKind - serializes as integer
pub const CompletionItemKind = enum(u8) {
    text = 1,
    method = 2,
    function = 3,
    constructor = 4,
    field = 5,
    variable = 6,
    class = 7,
    interface = 8,
    module = 9,
    property = 10,
    unit = 11,
    value = 12,
    @"enum" = 13,
    keyword = 14,
    snippet = 15,
    color = 16,
    file = 17,
    reference = 18,
    folder = 19,
    enum_member = 20,
    constant = 21,
    @"struct" = 22,
    event = 23,
    operator = 24,
    type_parameter = 25,

    pub const serialize_as_int = true;
};

/// InsertTextFormat - serializes as integer
pub const InsertTextFormat = enum(u8) {
    plain_text = 1,
    snippet = 2,

    pub const serialize_as_int = true;
};

// ============================================================================
// Hover Types
// ============================================================================

pub const HoverParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

pub const Hover = struct {
    contents: MarkupContent,
    range: ?Range = null,
};

pub const MarkupContent = struct {
    kind: MarkupKind,
    value: []const u8,
};

/// MarkupKind - serializes as string
pub const MarkupKind = enum {
    plaintext,
    markdown,

    // Serializes as string "plaintext" or "markdown"
};

// ============================================================================
// Definition Types
// ============================================================================

pub const DefinitionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

// ============================================================================
// Progress Types
// ============================================================================

pub const ProgressToken = union(enum) {
    string: []const u8,
    number: i32,
};

pub const WorkDoneProgressBegin = struct {
    kind: []const u8 = "begin",
    title: []const u8,
    cancellable: ?bool = null,
    message: ?[]const u8 = null,
    percentage: ?u32 = null,
};

pub const WorkDoneProgressReport = struct {
    kind: []const u8 = "report",
    cancellable: ?bool = null,
    message: ?[]const u8 = null,
    percentage: ?u32 = null,
};

pub const WorkDoneProgressEnd = struct {
    kind: []const u8 = "end",
    message: ?[]const u8 = null,
};

// ============================================================================
// Command Types
// ============================================================================

pub const ExecuteCommandParams = struct {
    command: []const u8,
};

// ============================================================================
// Window Types
// ============================================================================

/// MessageType - serializes as integer
pub const MessageType = enum(u8) {
    @"error" = 1,
    warning = 2,
    info = 3,
    log = 4,

    pub const serialize_as_int = true;
};

pub const ShowMessageParams = struct {
    type: MessageType,
    message: []const u8,
};

pub const LogMessageParams = struct {
    type: MessageType,
    message: []const u8,
};

// ============================================================================
// Utility Functions
// ============================================================================

/// Convert a Sanna span to an LSP range
/// Note: LSP uses 0-based lines and columns, Sanna uses 1-based
pub fn spanToRange(span: anytype) Range {
    return .{
        .start = .{
            .line = if (span.start.line > 0) span.start.line - 1 else 0,
            .character = if (span.start.column > 0) span.start.column - 1 else 0,
        },
        .end = .{
            .line = if (span.end.line > 0) span.end.line - 1 else 0,
            .character = if (span.end.column > 0) span.end.column - 1 else 0,
        },
    };
}

/// Convert an LSP position to a Sanna location (1-based)
pub fn positionToLocation(pos: Position) struct { line: u32, column: u32 } {
    return .{
        .line = pos.line + 1,
        .column = pos.character + 1,
    };
}

test "span to range conversion" {
    const testing = std.testing;

    const MockSpan = struct {
        start: struct { line: u32, column: u32 },
        end: struct { line: u32, column: u32 },
    };

    const span = MockSpan{
        .start = .{ .line = 1, .column = 1 },
        .end = .{ .line = 1, .column = 10 },
    };

    const range = spanToRange(span);
    try testing.expectEqual(@as(u32, 0), range.start.line);
    try testing.expectEqual(@as(u32, 0), range.start.character);
    try testing.expectEqual(@as(u32, 0), range.end.line);
    try testing.expectEqual(@as(u32, 9), range.end.character);
}
