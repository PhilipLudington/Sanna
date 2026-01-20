//! LSP Analyzer
//!
//! Performs language analysis for LSP features including:
//! - Parsing and diagnostics
//! - Type checking
//! - Semantic analysis
//! - Completion suggestions
//! - Hover information
//! - Go-to-definition

const std = @import("std");
const Allocator = std.mem.Allocator;
const Protocol = @import("Protocol.zig");
const DocumentStore = @import("DocumentStore.zig");

const sanna = @import("../root.zig");
const Lexer = sanna.Lexer;
const Token = sanna.Token.Token;
const TokenType = sanna.TokenType;
const Span = sanna.Span;
const Ast = sanna.Ast;

const Analyzer = @This();

allocator: Allocator,

/// Keyword completions
const keyword_completions = [_]Protocol.CompletionItem{
    .{ .label = "spec", .kind = .keyword, .detail = "Function specification", .insertText = "spec ${1:name}(${2:params}) -> ${3:ReturnType} {\n    requires ${4:precondition}\n    ensures ${5:postcondition}\n}" },
    .{ .label = "type", .kind = .keyword, .detail = "Type definition", .insertText = "type ${1:Name} = {\n    ${2:fields}\n}" },
    .{ .label = "model", .kind = .keyword, .detail = "Mathematical model type", .insertText = "model ${1:Name} = ${2:definition}" },
    .{ .label = "invariant", .kind = .keyword, .detail = "Type invariant", .insertText = "invariant ${1:condition}" },
    .{ .label = "axiom", .kind = .keyword, .detail = "Axiom declaration", .insertText = "axiom ${1:name}: ${2:statement}" },
    .{ .label = "requires", .kind = .keyword, .detail = "Precondition", .insertText = "requires ${1:condition}" },
    .{ .label = "ensures", .kind = .keyword, .detail = "Postcondition", .insertText = "ensures ${1:condition}" },
    .{ .label = "modifies", .kind = .keyword, .detail = "Frame condition", .insertText = "modifies ${1:variable}" },
    .{ .label = "pure", .kind = .keyword, .detail = "Pure function marker", .insertText = "pure" },
    .{ .label = "forall", .kind = .keyword, .detail = "Universal quantifier", .insertText = "forall ${1:x}: ${2:Type} :: ${3:condition}" },
    .{ .label = "exists", .kind = .keyword, .detail = "Existential quantifier", .insertText = "exists ${1:x}: ${2:Type} :: ${3:condition}" },
    .{ .label = "old", .kind = .function, .detail = "Previous value in postcondition", .insertText = "old(${1:expr})" },
    .{ .label = "result", .kind = .keyword, .detail = "Return value in postcondition" },
    .{ .label = "import", .kind = .keyword, .detail = "Import declaration", .insertText = "import ${1:module}" },
    .{ .label = "module", .kind = .keyword, .detail = "Module declaration", .insertText = "module ${1:name}" },
    .{ .label = "interface", .kind = .keyword, .detail = "Interface specification", .insertText = "interface ${1:Name} {\n    ${2:specs}\n}" },
    .{ .label = "lemma", .kind = .keyword, .detail = "Lemma for verification", .insertText = "lemma ${1:name}: ${2:statement}" },
    .{ .label = "trusted", .kind = .keyword, .detail = "Trusted block marker", .insertText = "@trusted(reason = \"${1:reason}\")" },
    .{ .label = "true", .kind = .keyword, .detail = "Boolean true" },
    .{ .label = "false", .kind = .keyword, .detail = "Boolean false" },
    .{ .label = "if", .kind = .keyword, .detail = "Conditional expression", .insertText = "if ${1:condition} then ${2:true_expr} else ${3:false_expr}" },
    .{ .label = "match", .kind = .keyword, .detail = "Pattern matching", .insertText = "match ${1:expr} {\n    ${2:pattern} => ${3:result}\n}" },
    .{ .label = "let", .kind = .keyword, .detail = "Let binding", .insertText = "let ${1:name} = ${2:value} in ${3:expr}" },
};

/// Attribute completions (after @)
const attribute_completions = [_]Protocol.CompletionItem{
    .{ .label = "author", .kind = .property, .detail = "Author attribution", .insertText = "author(\"${1:name}\")" },
    .{ .label = "confidence", .kind = .property, .detail = "AI confidence level", .insertText = "confidence(${1:0.0})" },
    .{ .label = "needs_review", .kind = .property, .detail = "Mark for review", .insertText = "needs_review(reason = \"${1:reason}\")" },
    .{ .label = "approved", .kind = .property, .detail = "Approval marker", .insertText = "approved(by = \"${1:approver}\", at = \"${2:date}\")" },
    .{ .label = "trusted", .kind = .property, .detail = "Trusted block", .insertText = "trusted(reason = \"${1:reason}\")" },
    .{ .label = "deprecated", .kind = .property, .detail = "Deprecation marker", .insertText = "deprecated(\"${1:message}\")" },
    .{ .label = "hint", .kind = .property, .detail = "Verification hint", .insertText = "hint(\"${1:hint}\")" },
};

/// Primitive type completions
const type_completions = [_]Protocol.CompletionItem{
    .{ .label = "Int", .kind = .type_parameter, .detail = "Integer type" },
    .{ .label = "Nat", .kind = .type_parameter, .detail = "Natural number (non-negative integer)" },
    .{ .label = "Bool", .kind = .type_parameter, .detail = "Boolean type" },
    .{ .label = "String", .kind = .type_parameter, .detail = "String type" },
    .{ .label = "Float", .kind = .type_parameter, .detail = "Floating-point type" },
    .{ .label = "Unit", .kind = .type_parameter, .detail = "Unit type (void)" },
    .{ .label = "List", .kind = .type_parameter, .detail = "List collection", .insertText = "List<${1:T}>" },
    .{ .label = "Set", .kind = .type_parameter, .detail = "Set collection", .insertText = "Set<${1:T}>" },
    .{ .label = "Map", .kind = .type_parameter, .detail = "Map collection", .insertText = "Map<${1:K}, ${2:V}>" },
    .{ .label = "Option", .kind = .type_parameter, .detail = "Optional value", .insertText = "Option<${1:T}>" },
    .{ .label = "Result", .kind = .type_parameter, .detail = "Result type", .insertText = "Result<${1:T}, ${2:E}>" },
    .{ .label = "Money", .kind = .type_parameter, .detail = "Monetary value" },
    .{ .label = "DateTime", .kind = .type_parameter, .detail = "Date and time" },
    .{ .label = "Duration", .kind = .type_parameter, .detail = "Time duration" },
};

pub fn init(allocator: Allocator) Analyzer {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *Analyzer) void {
    _ = self;
}

/// Analyze a document and return diagnostics
pub fn analyze(self: *Analyzer, doc: *DocumentStore.Document) ![]const DocumentStore.Diagnostic {
    _ = self;
    const alloc = doc.arena.allocator();

    // Clear previous diagnostics
    doc.parse_diagnostics = .{};
    doc.type_diagnostics = .{};
    doc.semantic_diagnostics = .{};

    // Tokenize
    var lexer = Lexer.init(alloc, doc.content);
    const tokens_result = lexer.tokenize() catch |err| {
        try doc.parse_diagnostics.append(alloc, .{
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 0 },
            },
            .severity = .@"error",
            .message = try std.fmt.allocPrint(alloc, "Lexer error: {}", .{err}),
            .source = "sanna",
        });
        return doc.parse_diagnostics.items;
    };

    // Report lexer errors
    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            try doc.parse_diagnostics.append(alloc, .{
                .range = .{
                    .start = .{
                        .line = if (err.location.line > 0) err.location.line - 1 else 0,
                        .character = if (err.location.column > 0) err.location.column - 1 else 0,
                    },
                    .end = .{
                        .line = if (err.location.line > 0) err.location.line - 1 else 0,
                        .character = if (err.location.column > 0) err.location.column else 0,
                    },
                },
                .severity = .@"error",
                .message = try alloc.dupe(u8, err.message),
                .source = "sanna-lexer",
            });
        }
    }

    // Store tokens
    doc.tokens = tokens_result.items;

    // Parse
    var parser = sanna.parser.Parser.init(alloc, tokens_result.items);
    const ast_result = parser.parseModule() catch |err| {
        try doc.parse_diagnostics.append(alloc, .{
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 0 },
            },
            .severity = .@"error",
            .message = try std.fmt.allocPrint(alloc, "Parse error: {}", .{err}),
            .source = "sanna",
        });
        return doc.parse_diagnostics.items;
    };

    // Report parser diagnostics
    for (parser.diagnostics.items) |diag| {
        const severity: Protocol.DiagnosticSeverity = switch (diag.severity) {
            .err => .@"error",
            .warning => .warning,
            .hint => .hint,
        };

        try doc.parse_diagnostics.append(alloc, .{
            .range = Protocol.spanToRange(diag.span),
            .severity = severity,
            .message = try alloc.dupe(u8, diag.message),
            .source = "sanna-parser",
        });
    }

    // Store AST
    doc.ast = ast_result;

    // Type checking would be done here if parse succeeded without errors
    if (doc.parse_diagnostics.items.len == 0) {
        // TODO: Run type checker and collect type errors
        // TODO: Run semantic analyzer and collect semantic errors
    }

    // Return all diagnostics
    return doc.parse_diagnostics.items;
}

/// Get completion items at a position
pub fn getCompletions(self: *Analyzer, doc: *const DocumentStore.Document, position: Protocol.Position) ![]const Protocol.CompletionItem {
    _ = self;

    // Get context at position
    const context = getContextAtPosition(doc, position);

    return switch (context) {
        .after_at => &attribute_completions,
        .type_position => &type_completions,
        .top_level, .in_spec, .unknown => &keyword_completions,
    };
}

const CompletionContext = enum {
    after_at, // After @ for attributes
    type_position, // In a type annotation
    top_level, // At module top level
    in_spec, // Inside a spec body
    unknown,
};

fn getContextAtPosition(doc: *const DocumentStore.Document, position: Protocol.Position) CompletionContext {
    // Simple heuristic: look at character before cursor
    const line = position.line;
    const char = position.character;

    // Find line start in content
    var line_start: usize = 0;
    var current_line: u32 = 0;
    for (doc.content, 0..) |c, i| {
        if (current_line == line) {
            line_start = i;
            break;
        }
        if (c == '\n') {
            current_line += 1;
        }
    }

    // Get characters before cursor on this line
    const line_end = line_start + char;
    if (line_end > 0 and line_end <= doc.content.len) {
        const before_cursor = doc.content[line_start..line_end];

        // Check for @ at end
        if (before_cursor.len > 0 and before_cursor[before_cursor.len - 1] == '@') {
            return .after_at;
        }

        // Check for : before cursor (type position)
        if (std.mem.lastIndexOf(u8, before_cursor, ":")) |_| {
            // Very simplistic check
            return .type_position;
        }
    }

    return .unknown;
}

/// Get hover information at a position
pub fn getHover(self: *Analyzer, doc: *DocumentStore.Document, position: Protocol.Position) !?Protocol.Hover {
    _ = self;

    // Find token at position
    const token = findTokenAtPosition(doc, position) orelse return null;

    // Build hover content based on token type
    var content = std.ArrayListUnmanaged(u8){};
    const arena_alloc = doc.arena.allocator();
    const writer = content.writer(arena_alloc);

    try writer.writeAll("```sanna\n");

    switch (token.type) {
        .kw_spec => {
            try writer.writeAll("spec\n```\n\n");
            try writer.writeAll("**Function Specification**\n\n");
            try writer.writeAll("Declares a specification for a function with preconditions (`requires`) and postconditions (`ensures`).");
        },
        .kw_type => {
            try writer.writeAll("type\n```\n\n");
            try writer.writeAll("**Type Definition**\n\n");
            try writer.writeAll("Defines a new type with fields and invariants.");
        },
        .kw_requires => {
            try writer.writeAll("requires <condition>\n```\n\n");
            try writer.writeAll("**Precondition**\n\n");
            try writer.writeAll("Specifies a condition that must hold when a function is called.");
        },
        .kw_ensures => {
            try writer.writeAll("ensures <condition>\n```\n\n");
            try writer.writeAll("**Postcondition**\n\n");
            try writer.writeAll("Specifies a condition that must hold when a function returns. Use `old(x)` to refer to pre-call values and `result` for the return value.");
        },
        .kw_invariant => {
            try writer.writeAll("invariant <condition>\n```\n\n");
            try writer.writeAll("**Type Invariant**\n\n");
            try writer.writeAll("Specifies a condition that must always hold for values of this type.");
        },
        .kw_forall => {
            try writer.writeAll("forall x: T :: P(x)\n```\n\n");
            try writer.writeAll("**Universal Quantifier**\n\n");
            try writer.writeAll("For all values `x` of type `T`, the property `P(x)` holds.");
        },
        .kw_exists => {
            try writer.writeAll("exists x: T :: P(x)\n```\n\n");
            try writer.writeAll("**Existential Quantifier**\n\n");
            try writer.writeAll("There exists a value `x` of type `T` such that `P(x)` holds.");
        },
        .kw_old => {
            try writer.writeAll("old(expr)\n```\n\n");
            try writer.writeAll("**Pre-state Expression**\n\n");
            try writer.writeAll("In a postcondition, refers to the value of `expr` before the function was called.");
        },
        .kw_result => {
            try writer.writeAll("result\n```\n\n");
            try writer.writeAll("**Return Value**\n\n");
            try writer.writeAll("In a postcondition, refers to the value returned by the function.");
        },
        .kw_modifies => {
            try writer.writeAll("modifies <variables>\n```\n\n");
            try writer.writeAll("**Frame Condition**\n\n");
            try writer.writeAll("Specifies which variables may be modified by the function. Unlisted variables must remain unchanged.");
        },
        .kw_pure => {
            try writer.writeAll("pure\n```\n\n");
            try writer.writeAll("**Pure Function**\n\n");
            try writer.writeAll("Marks a function as having no side effects. Pure functions can only call other pure functions.");
        },
        .kw_trusted => {
            try writer.writeAll("@trusted(reason = \"...\")\n```\n\n");
            try writer.writeAll("**Trusted Block**\n\n");
            try writer.writeAll("Marks code as trusted without verification. Requires a reason for documentation.");
        },
        .identifier => {
            try writer.print("{s}\n```\n\n", .{token.lexeme});
            try writer.writeAll("Identifier");
            // TODO: Look up in symbol table for type info
        },
        else => {
            try writer.print("{s}\n```", .{token.lexeme});
        },
    }

    return .{
        .contents = .{
            .kind = .markdown,
            .value = content.items,
        },
        .range = Protocol.spanToRange(token.span),
    };
}

fn findTokenAtPosition(doc: *const DocumentStore.Document, position: Protocol.Position) ?Token {
    const tokens = doc.tokens orelse return null;

    // Convert LSP position (0-based) to Sanna position (1-based)
    const target_line = position.line + 1;
    const target_col = position.character + 1;

    for (tokens) |token| {
        // Check if position is within token span
        if (token.span.start.line <= target_line and token.span.end.line >= target_line) {
            if (token.span.start.line == target_line and token.span.start.column > target_col) {
                continue;
            }
            if (token.span.end.line == target_line and token.span.end.column < target_col) {
                continue;
            }
            return token;
        }
    }

    return null;
}

/// Get definition location for symbol at position
pub fn getDefinition(self: *Analyzer, doc: *const DocumentStore.Document, position: Protocol.Position) !?Protocol.Location {
    _ = self;

    // Find token at position
    const token = findTokenAtPosition(doc, position) orelse return null;

    // Only handle identifiers
    if (token.type != .identifier) return null;

    // Find definition in AST
    const ast = doc.ast orelse return null;

    // Search declarations for matching name
    for (ast.declarations) |decl| {
        const decl_name = getDeclarationName(decl) orelse continue;
        if (std.mem.eql(u8, decl_name, token.lexeme)) {
            const span = getDeclarationSpan(decl) orelse continue;
            return .{
                .uri = doc.uri,
                .range = Protocol.spanToRange(span),
            };
        }
    }

    return null;
}

fn getDeclarationName(decl: Ast.Declaration) ?[]const u8 {
    return switch (decl.kind) {
        .type_def => |td| td.name.name,
        .model_def => |md| md.name.name,
        .spec_fn => |fs| fs.name.name,
        .spec_interface => |is| is.name.name,
        .invariant => |inv| if (inv.name) |n| n.name else null,
        .axiom => |ax| ax.name.name,
        .lemma => |lem| lem.name.name,
    };
}

fn getDeclarationSpan(decl: Ast.Declaration) ?Span {
    return switch (decl.kind) {
        .type_def => |td| td.name.span,
        .model_def => |md| md.name.span,
        .spec_fn => |fs| fs.name.span,
        .spec_interface => |is| is.name.span,
        .invariant => |inv| if (inv.name) |n| n.span else null,
        .axiom => |ax| ax.name.span,
        .lemma => |lem| lem.name.span,
    };
}

test "analyzer completions" {
    const testing = std.testing;
    var analyzer = Analyzer.init(testing.allocator);
    defer analyzer.deinit();

    // Test keyword completions
    const completions = keyword_completions;
    try testing.expect(completions.len > 0);

    var found_spec = false;
    for (completions) |c| {
        if (std.mem.eql(u8, c.label, "spec")) {
            found_spec = true;
            break;
        }
    }
    try testing.expect(found_spec);
}
