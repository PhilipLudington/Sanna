const std = @import("std");
const Allocator = std.mem.Allocator;

const lexer = @import("../lexer/root.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token.Token;
const TokenType = lexer.TokenType;
const Span = lexer.Span;
const Location = lexer.Location;

const Ast = @import("Ast.zig");
const Module = Ast.Module;
const Import = Ast.Import;
const ImportItem = Ast.ImportItem;
const Declaration = Ast.Declaration;
const DeclarationKind = Ast.DeclarationKind;
const Identifier = Ast.Identifier;
const QualifiedName = Ast.QualifiedName;
const TypeParameter = Ast.TypeParameter;
const Attribute = Ast.Attribute;
const AttributeArg = Ast.AttributeArg;
const AttributeValue = Ast.AttributeValue;
const Visibility = Ast.Visibility;
const TypeDefinition = Ast.TypeDefinition;
const TypeBody = Ast.TypeBody;
const TypeExpr = Ast.TypeExpr;
const TypeExprKind = Ast.TypeExprKind;
const GenericType = Ast.GenericType;
const Field = Ast.Field;
const Variant = Ast.Variant;
const ModelDefinition = Ast.ModelDefinition;
const FunctionSpec = Ast.FunctionSpec;
const InterfaceSpec = Ast.InterfaceSpec;
const InvariantDecl = Ast.InvariantDecl;
const AxiomDecl = Ast.AxiomDecl;
const LemmaDecl = Ast.LemmaDecl;
const Parameter = Ast.Parameter;
const Expression = Ast.Expression;
const ExpressionKind = Ast.ExpressionKind;

const Parser = @This();

// ============================================================================
// Error Types
// ============================================================================

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEof,
    InvalidSyntax,
    OutOfMemory,
};

pub const Diagnostic = struct {
    message: []const u8,
    span: Span,
    severity: Severity,
    context: ?[]const u8,

    pub const Severity = enum {
        err,
        warning,
        hint,
    };

    pub fn format(self: Diagnostic) FormattedDiagnostic {
        return .{ .diagnostic = self };
    }
};

/// Helper for formatted diagnostic output
pub const FormattedDiagnostic = struct {
    diagnostic: Diagnostic,

    pub fn print(self: FormattedDiagnostic, writer: anytype) !void {
        const d = self.diagnostic;
        const severity_str = switch (d.severity) {
            .err => "error",
            .warning => "warning",
            .hint => "hint",
        };
        try writer.print("{d}:{d}: {s}: {s}", .{
            d.span.start.line,
            d.span.start.column,
            severity_str,
            d.message,
        });
        if (d.context) |ctx| {
            try writer.print(" (while parsing {s})", .{ctx});
        }
        try writer.writeByte('\n');
    }
};

/// Context for tracking what we're currently parsing (for better error messages)
pub const ParseContext = enum {
    module,
    import_decl,
    declaration,
    type_definition,
    type_body,
    function_spec,
    interface_spec,
    invariant,
    axiom,
    lemma,
    expression,
    attribute,
    type_parameters,
    parameters,

    pub fn name(self: ParseContext) []const u8 {
        return switch (self) {
            .module => "module",
            .import_decl => "import declaration",
            .declaration => "declaration",
            .type_definition => "type definition",
            .type_body => "type body",
            .function_spec => "function specification",
            .interface_spec => "interface specification",
            .invariant => "invariant",
            .axiom => "axiom",
            .lemma => "lemma",
            .expression => "expression",
            .attribute => "attribute",
            .type_parameters => "type parameters",
            .parameters => "parameters",
        };
    }
};

// ============================================================================
// Parser State
// ============================================================================

allocator: Allocator,
tokens: []const Token,
current: usize,
diagnostics: std.ArrayListUnmanaged(Diagnostic),
context_stack: std.ArrayListUnmanaged(ParseContext),
panic_mode: bool,

// ============================================================================
// Initialization
// ============================================================================

pub fn init(allocator: Allocator, tokens: []const Token) Parser {
    return .{
        .allocator = allocator,
        .tokens = tokens,
        .current = 0,
        .diagnostics = .{},
        .context_stack = .{},
        .panic_mode = false,
    };
}

pub fn deinit(self: *Parser) void {
    self.diagnostics.deinit(self.allocator);
    self.context_stack.deinit(self.allocator);
}

// ============================================================================
// Token Navigation
// ============================================================================

fn peek(self: *const Parser) Token {
    if (self.current >= self.tokens.len) {
        const loc = Location.init(0, 0, 0);
        return Token.init(.eof, "", Span.init(loc, loc));
    }
    return self.tokens[self.current];
}

fn peekAhead(self: *const Parser, offset: usize) Token {
    const idx = self.current + offset;
    if (idx >= self.tokens.len) {
        const loc = Location.init(0, 0, 0);
        return Token.init(.eof, "", Span.init(loc, loc));
    }
    return self.tokens[idx];
}

fn advance(self: *Parser) Token {
    const token = self.peek();
    if (self.current < self.tokens.len) {
        self.current += 1;
    }
    return token;
}

fn isAtEnd(self: *const Parser) bool {
    return self.peek().type == .eof;
}

fn check(self: *const Parser, token_type: TokenType) bool {
    return self.peek().type == token_type;
}

fn match(self: *Parser, token_type: TokenType) bool {
    if (self.check(token_type)) {
        _ = self.advance();
        return true;
    }
    return false;
}

fn expect(self: *Parser, token_type: TokenType, message: []const u8) ParseError!Token {
    if (self.check(token_type)) {
        return self.advance();
    }
    try self.addDiagnostic(message, self.peek().span, .err);
    return ParseError.UnexpectedToken;
}

fn previous(self: *const Parser) Token {
    if (self.current == 0) {
        return self.peek();
    }
    return self.tokens[self.current - 1];
}

// ============================================================================
// Context Management
// ============================================================================

fn pushContext(self: *Parser, ctx: ParseContext) !void {
    try self.context_stack.append(self.allocator, ctx);
}

fn popContext(self: *Parser) void {
    if (self.context_stack.items.len > 0) {
        _ = self.context_stack.pop();
    }
}

fn currentContext(self: *const Parser) ?ParseContext {
    if (self.context_stack.items.len == 0) return null;
    return self.context_stack.items[self.context_stack.items.len - 1];
}

fn currentContextName(self: *const Parser) ?[]const u8 {
    if (self.currentContext()) |ctx| {
        return ctx.name();
    }
    return null;
}

// ============================================================================
// Diagnostics
// ============================================================================

fn addDiagnostic(self: *Parser, message: []const u8, span: Span, severity: Diagnostic.Severity) !void {
    // In panic mode, suppress cascading errors
    if (self.panic_mode and severity == .err) return;

    try self.diagnostics.append(self.allocator, .{
        .message = message,
        .span = span,
        .severity = severity,
        .context = self.currentContextName(),
    });

    // Enter panic mode on first error
    if (severity == .err) {
        self.panic_mode = true;
    }
}

pub fn hasErrors(self: *const Parser) bool {
    for (self.diagnostics.items) |d| {
        if (d.severity == .err) return true;
    }
    return false;
}

pub fn errorCount(self: *const Parser) usize {
    var count: usize = 0;
    for (self.diagnostics.items) |d| {
        if (d.severity == .err) count += 1;
    }
    return count;
}

// ============================================================================
// Error Recovery
// ============================================================================

/// Token types that can start a new top-level declaration
fn isDeclarationStart(token_type: TokenType) bool {
    return switch (token_type) {
        .kw_type, .kw_spec, .kw_model, .kw_interface, .kw_invariant, .kw_axiom, .kw_lemma, .kw_pub, .op_at, .doc_comment => true,
        else => false,
    };
}

/// Synchronize parser state after an error - skip to next declaration boundary
fn synchronize(self: *Parser) void {
    self.panic_mode = false;

    while (!self.isAtEnd()) {
        // If we just passed a declaration-ending point, we're synchronized
        // (Sanna doesn't use semicolons, so we rely on keyword detection)

        // If current token starts a new declaration, we're synchronized
        if (isDeclarationStart(self.peek().type)) {
            return;
        }

        // Skip past import keyword for import recovery
        if (self.peek().type == .kw_import) {
            return;
        }

        _ = self.advance();
    }
}

/// Synchronize to closing brace, useful for recovering from type body errors
fn synchronizeToBrace(self: *Parser) void {
    self.panic_mode = false;
    var brace_depth: i32 = 1;

    while (!self.isAtEnd()) {
        switch (self.peek().type) {
            .lbrace => brace_depth += 1,
            .rbrace => {
                brace_depth -= 1;
                if (brace_depth == 0) {
                    _ = self.advance(); // consume the closing brace
                    return;
                }
            },
            else => {},
        }
        _ = self.advance();
    }
}

/// Synchronize to closing bracket, useful for recovering from type parameter errors
fn synchronizeToBracket(self: *Parser) void {
    self.panic_mode = false;
    var bracket_depth: i32 = 1;

    while (!self.isAtEnd()) {
        switch (self.peek().type) {
            .lbracket => bracket_depth += 1,
            .rbracket => {
                bracket_depth -= 1;
                if (bracket_depth == 0) {
                    _ = self.advance(); // consume the closing bracket
                    return;
                }
            },
            else => {},
        }
        _ = self.advance();
    }
}

/// Synchronize to closing parenthesis
fn synchronizeToParen(self: *Parser) void {
    self.panic_mode = false;
    var paren_depth: i32 = 1;

    while (!self.isAtEnd()) {
        switch (self.peek().type) {
            .lparen => paren_depth += 1,
            .rparen => {
                paren_depth -= 1;
                if (paren_depth == 0) {
                    _ = self.advance(); // consume the closing paren
                    return;
                }
            },
            else => {},
        }
        _ = self.advance();
    }
}

// ============================================================================
// Module Parsing
// ============================================================================

/// Parse a complete module with error recovery
pub fn parseModule(self: *Parser) ParseError!Module {
    try self.pushContext(.module);
    defer self.popContext();

    const start_loc = self.peek().span.start;

    // Parse optional module declaration
    var module_name: ?QualifiedName = null;
    if (self.check(.kw_module)) {
        module_name = self.parseModuleDecl() catch |err| blk: {
            if (err == error.OutOfMemory) return err;
            self.synchronize();
            break :blk null;
        };
    }

    // Parse imports with error recovery
    var imports = std.ArrayListUnmanaged(Import){};
    errdefer imports.deinit(self.allocator);

    while (self.check(.kw_import)) {
        if (self.parseImport()) |import_decl| {
            try imports.append(self.allocator, import_decl);
        } else |err| {
            if (err == error.OutOfMemory) return err;
            // Skip to next import or declaration
            self.synchronize();
        }
    }

    // Parse declarations with error recovery
    var declarations = std.ArrayListUnmanaged(Declaration){};
    errdefer declarations.deinit(self.allocator);

    while (!self.isAtEnd()) {
        if (self.parseDeclaration()) |decl| {
            try declarations.append(self.allocator, decl);
        } else |err| {
            if (err == error.OutOfMemory) return err;
            // Skip to next declaration
            self.synchronize();
            // If we're still stuck on non-declaration token, skip it
            if (!self.isAtEnd() and !isDeclarationStart(self.peek().type)) {
                _ = self.advance();
            }
        }
    }

    const end_loc = if (self.current > 0) self.previous().span.end else start_loc;

    return Module.init(
        module_name,
        try imports.toOwnedSlice(self.allocator),
        try declarations.toOwnedSlice(self.allocator),
        Span.init(start_loc, end_loc),
    );
}

/// Parse module declaration: `module qualified.name`
fn parseModuleDecl(self: *Parser) ParseError!QualifiedName {
    _ = try self.expect(.kw_module, "Expected 'module'");
    return self.parseQualifiedName();
}

// ============================================================================
// Import Parsing
// ============================================================================

/// Parse an import declaration
/// import path.to.module
/// import path.to.module as alias
/// import path.to.module { Item1, Item2 as Alias2 }
fn parseImport(self: *Parser) ParseError!Import {
    try self.pushContext(.import_decl);
    defer self.popContext();

    const start = self.peek().span.start;
    _ = try self.expect(.kw_import, "Expected 'import'");

    const path = try self.parseQualifiedName();

    // Check for import list { ... }
    var items = std.ArrayListUnmanaged(ImportItem){};
    errdefer items.deinit(self.allocator);

    var alias: ?Identifier = null;

    if (self.match(.lbrace)) {
        // Parse import items
        if (!self.check(.rbrace)) {
            const first_item = try self.parseImportItem();
            try items.append(self.allocator, first_item);

            while (self.match(.comma)) {
                if (self.check(.rbrace)) break;
                const item = try self.parseImportItem();
                try items.append(self.allocator, item);
            }
        }
        _ = try self.expect(.rbrace, "Expected '}'");
    } else if (self.check(.identifier) and std.mem.eql(u8, self.peek().lexeme, "as")) {
        // import path as alias
        _ = self.advance(); // consume 'as'
        alias = try self.parseIdentifier();
    }

    const end = self.previous().span.end;

    return Import.init(
        path,
        try items.toOwnedSlice(self.allocator),
        alias,
        Span.init(start, end),
    );
}

/// Parse a single import item: `Name` or `Name as Alias`
fn parseImportItem(self: *Parser) ParseError!ImportItem {
    const start = self.peek().span.start;
    const name = try self.parseIdentifier();

    var alias: ?Identifier = null;
    if (self.check(.identifier) and std.mem.eql(u8, self.peek().lexeme, "as")) {
        _ = self.advance();
        alias = try self.parseIdentifier();
    }

    const end = self.previous().span.end;
    return ImportItem.init(name, alias, Span.init(start, end));
}

// ============================================================================
// Declaration Parsing
// ============================================================================

/// Parse a top-level declaration
fn parseDeclaration(self: *Parser) ParseError!Declaration {
    try self.pushContext(.declaration);
    defer self.popContext();

    const start = self.peek().span.start;

    // Parse doc comment if present
    var doc_comment: ?[]const u8 = null;
    if (self.check(.doc_comment)) {
        doc_comment = self.advance().lexeme;
    }

    // Parse attributes
    var attributes = std.ArrayListUnmanaged(Attribute){};
    errdefer attributes.deinit(self.allocator);

    while (self.check(.op_at)) {
        const attr = try self.parseAttribute();
        try attributes.append(self.allocator, attr);
    }

    // Parse visibility
    var visibility: Visibility = .private;
    if (self.match(.kw_pub)) {
        visibility = .public;
    }

    // Parse the declaration kind
    const kind = try self.parseDeclarationKind();
    const end = self.previous().span.end;

    return Declaration.init(
        kind,
        visibility,
        try attributes.toOwnedSlice(self.allocator),
        doc_comment,
        Span.init(start, end),
    );
}

/// Parse the kind of declaration (type, spec, model, etc.)
fn parseDeclarationKind(self: *Parser) ParseError!DeclarationKind {
    const token = self.peek();

    return switch (token.type) {
        .kw_type => .{ .type_def = try self.parseTypeDefinition() },
        .kw_model => .{ .model_def = try self.parseModelDefinition() },
        .kw_spec => .{ .spec_fn = try self.parseFunctionSpec() },
        .kw_interface => .{ .spec_interface = try self.parseInterfaceSpec() },
        .kw_invariant => .{ .invariant = try self.parseInvariantDecl() },
        .kw_axiom => .{ .axiom = try self.parseAxiomDecl() },
        .kw_lemma => .{ .lemma = try self.parseLemmaDecl() },
        else => {
            try self.addDiagnostic("Expected declaration (type, spec, model, interface, invariant, axiom, or lemma)", token.span, .err);
            return ParseError.UnexpectedToken;
        },
    };
}

// ============================================================================
// Attribute Parsing
// ============================================================================

/// Parse an attribute: @name or @name(args)
fn parseAttribute(self: *Parser) ParseError!Attribute {
    try self.pushContext(.attribute);
    defer self.popContext();

    const start = self.peek().span.start;
    _ = try self.expect(.op_at, "Expected '@'");

    // Attribute names can be keywords like author, confidence, etc.
    const name = try self.parseIdentifierOrKeyword();

    var args = std.ArrayListUnmanaged(AttributeArg){};
    errdefer args.deinit(self.allocator);

    // Parse optional arguments
    if (self.match(.lparen)) {
        if (!self.check(.rparen)) {
            const first_arg = try self.parseAttributeArg();
            try args.append(self.allocator, first_arg);

            while (self.match(.comma)) {
                if (self.check(.rparen)) break;
                const arg = try self.parseAttributeArg();
                try args.append(self.allocator, arg);
            }
        }
        _ = try self.expect(.rparen, "Expected ')'");
    }

    const end = self.previous().span.end;
    return Attribute.init(name, try args.toOwnedSlice(self.allocator), Span.init(start, end));
}

/// Parse an attribute argument: value or name: value
fn parseAttributeArg(self: *Parser) ParseError!AttributeArg {
    const start = self.peek().span.start;

    // Check for named argument
    var arg_name: ?Identifier = null;
    if (self.check(.identifier) and self.peekAhead(1).type == .colon) {
        arg_name = try self.parseIdentifier();
        _ = self.advance(); // consume ':'
    }

    const value = try self.parseAttributeValue();
    const end = self.previous().span.end;

    return .{
        .name = arg_name,
        .value = value,
        .span = Span.init(start, end),
    };
}

/// Parse an attribute value
fn parseAttributeValue(self: *Parser) ParseError!AttributeValue {
    const token = self.peek();

    switch (token.type) {
        .string_literal => {
            _ = self.advance();
            // Strip quotes from the lexeme
            const content = if (token.lexeme.len >= 2)
                token.lexeme[1 .. token.lexeme.len - 1]
            else
                token.lexeme;
            return .{ .string = content };
        },
        .int_literal => {
            _ = self.advance();
            const value = std.fmt.parseInt(i64, token.lexeme, 10) catch 0;
            return .{ .int = value };
        },
        .float_literal => {
            _ = self.advance();
            const value = std.fmt.parseFloat(f64, token.lexeme) catch 0.0;
            return .{ .float = value };
        },
        .kw_true => {
            _ = self.advance();
            return .{ .bool = true };
        },
        .kw_false => {
            _ = self.advance();
            return .{ .bool = false };
        },
        .identifier => {
            const ident = try self.parseIdentifier();
            return .{ .identifier = ident };
        },
        else => {
            try self.addDiagnostic("Expected attribute value", token.span, .err);
            return ParseError.UnexpectedToken;
        },
    }
}

// ============================================================================
// Type Definition Parsing
// ============================================================================

/// Parse a type definition
fn parseTypeDefinition(self: *Parser) ParseError!TypeDefinition {
    try self.pushContext(.type_definition);
    defer self.popContext();

    const start = self.peek().span.start;
    _ = try self.expect(.kw_type, "Expected 'type'");

    const name = try self.parseIdentifier();

    // Parse optional type parameters
    const type_params = try self.parseTypeParameters();

    // Parse type body
    _ = try self.expect(.op_assign, "Expected '=' after type name");
    const body = try self.parseTypeBody();

    // Parse optional invariants
    var invariants = std.ArrayListUnmanaged(Expression){};
    errdefer invariants.deinit(self.allocator);

    while (self.check(.kw_invariant)) {
        _ = self.advance();
        if (self.match(.colon)) {
            // invariant: expr
        }
        const inv_expr = try self.parseExpression();
        try invariants.append(self.allocator, inv_expr);
    }

    const end = self.previous().span.end;

    return TypeDefinition.init(
        name,
        type_params,
        body,
        try invariants.toOwnedSlice(self.allocator),
        Span.init(start, end),
    );
}

/// Parse optional type parameters: [T, U: Ord]
fn parseTypeParameters(self: *Parser) ParseError![]const TypeParameter {
    if (!self.match(.lbracket)) {
        return &.{};
    }

    try self.pushContext(.type_parameters);
    defer self.popContext();

    var params = std.ArrayListUnmanaged(TypeParameter){};
    errdefer params.deinit(self.allocator);

    if (!self.check(.rbracket)) {
        const first = try self.parseTypeParameter();
        try params.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rbracket)) break;
            const param = try self.parseTypeParameter();
            try params.append(self.allocator, param);
        }
    }

    _ = try self.expect(.rbracket, "Expected ']'");
    return params.toOwnedSlice(self.allocator);
}

/// Parse a single type parameter: T or T: Bound1 + Bound2
fn parseTypeParameter(self: *Parser) ParseError!TypeParameter {
    const start = self.peek().span.start;
    const name = try self.parseIdentifier();

    var bounds = std.ArrayListUnmanaged(QualifiedName){};
    errdefer bounds.deinit(self.allocator);

    if (self.match(.colon)) {
        const first_bound = try self.parseQualifiedName();
        try bounds.append(self.allocator, first_bound);

        while (self.match(.op_plus)) {
            const bound = try self.parseQualifiedName();
            try bounds.append(self.allocator, bound);
        }
    }

    const end = self.previous().span.end;
    return TypeParameter.init(name, try bounds.toOwnedSlice(self.allocator), Span.init(start, end));
}

/// Parse the body of a type definition
fn parseTypeBody(self: *Parser) ParseError!TypeBody {
    // Check for sum type (starts with |)
    if (self.check(.op_pipe)) {
        return .{ .sum = try self.parseSumType() };
    }

    // Check for product type (starts with {)
    if (self.check(.lbrace)) {
        return .{ .product = try self.parseProductType() };
    }

    // Otherwise it's a type alias
    return .{ .alias = try self.parseTypeExpr() };
}

/// Parse a product type: { field1: Type1, field2: Type2 }
fn parseProductType(self: *Parser) ParseError![]const Field {
    _ = try self.expect(.lbrace, "Expected '{'");

    var fields = std.ArrayListUnmanaged(Field){};
    errdefer fields.deinit(self.allocator);

    if (!self.check(.rbrace)) {
        const first = try self.parseField();
        try fields.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rbrace)) break;
            const field = try self.parseField();
            try fields.append(self.allocator, field);
        }
    }

    _ = try self.expect(.rbrace, "Expected '}'");
    return fields.toOwnedSlice(self.allocator);
}

/// Parse a single field: name: Type or name: Type = default
fn parseField(self: *Parser) ParseError!Field {
    const start = self.peek().span.start;
    const name = try self.parseIdentifier();

    _ = try self.expect(.colon, "Expected ':'");
    const type_expr = try self.parseTypeExpr();

    var default_value: ?Expression = null;
    if (self.match(.op_assign)) {
        default_value = try self.parseExpression();
    }

    const end = self.previous().span.end;
    return Field.init(name, type_expr, default_value, Span.init(start, end));
}

/// Parse a sum type: | Variant1 | Variant2 { field: Type }
fn parseSumType(self: *Parser) ParseError![]const Variant {
    var variants = std.ArrayListUnmanaged(Variant){};
    errdefer variants.deinit(self.allocator);

    while (self.match(.op_pipe)) {
        const variant = try self.parseVariant();
        try variants.append(self.allocator, variant);
    }

    return variants.toOwnedSlice(self.allocator);
}

/// Parse a single variant: Name or Name { fields }
fn parseVariant(self: *Parser) ParseError!Variant {
    const start = self.peek().span.start;
    const name = try self.parseIdentifier();

    var fields: []const Field = &.{};
    if (self.check(.lbrace)) {
        fields = try self.parseProductType();
    }

    const end = self.previous().span.end;
    return Variant.init(name, fields, Span.init(start, end));
}

// ============================================================================
// Model Definition Parsing
// ============================================================================

/// Parse a model definition (placeholder - will be expanded)
fn parseModelDefinition(self: *Parser) ParseError!ModelDefinition {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_model, "Expected 'model'");

    const name = try self.parseIdentifier();
    const type_params = try self.parseTypeParameters();

    _ = try self.expect(.lbrace, "Expected '{'");
    // TODO: Parse model members
    _ = try self.expect(.rbrace, "Expected '}'");

    const end = self.previous().span.end;
    return ModelDefinition.init(name, type_params, &.{}, Span.init(start, end));
}

// ============================================================================
// Function Specification Parsing
// ============================================================================

/// Parse a function specification (placeholder - will be expanded)
fn parseFunctionSpec(self: *Parser) ParseError!FunctionSpec {
    try self.pushContext(.function_spec);
    defer self.popContext();

    const start = self.peek().span.start;
    _ = try self.expect(.kw_spec, "Expected 'spec'");

    // Optional 'pure' modifier
    var is_pure = false;
    if (self.match(.kw_pure)) {
        is_pure = true;
    }

    _ = try self.expect(.kw_fn, "Expected 'fn'");

    const name = try self.parseIdentifier();
    const type_params = try self.parseTypeParameters();

    // Parse parameters
    _ = try self.expect(.lparen, "Expected '('");
    const params = try self.parseParameters();
    _ = try self.expect(.rparen, "Expected ')'");

    // Parse return type
    _ = try self.expect(.op_arrow, "Expected '->'");
    const return_type = try self.parseTypeExpr();

    // Parse requires/ensures/modifies clauses
    var requires = std.ArrayListUnmanaged(Expression){};
    errdefer requires.deinit(self.allocator);

    var ensures = std.ArrayListUnmanaged(Expression){};
    errdefer ensures.deinit(self.allocator);

    var modifies = std.ArrayListUnmanaged(Expression){};
    errdefer modifies.deinit(self.allocator);

    var decreases: ?Expression = null;

    while (true) {
        if (self.match(.kw_requires)) {
            _ = self.match(.colon);
            const expr = try self.parseExpression();
            try requires.append(self.allocator, expr);
        } else if (self.match(.kw_ensures)) {
            _ = self.match(.colon);
            const expr = try self.parseExpression();
            try ensures.append(self.allocator, expr);
        } else if (self.match(.kw_modifies)) {
            _ = self.match(.colon);
            const expr = try self.parseExpression();
            try modifies.append(self.allocator, expr);
        } else if (self.match(.kw_decreases)) {
            _ = self.match(.colon);
            decreases = try self.parseExpression();
        } else {
            break;
        }
    }

    const end = self.previous().span.end;

    var spec = FunctionSpec.init(name, type_params, params, return_type, Span.init(start, end));
    spec.requires = try requires.toOwnedSlice(self.allocator);
    spec.ensures = try ensures.toOwnedSlice(self.allocator);
    spec.modifies = try modifies.toOwnedSlice(self.allocator);
    spec.decreases = decreases;
    spec.is_pure = is_pure;

    return spec;
}

/// Parse function parameters
fn parseParameters(self: *Parser) ParseError![]const Parameter {
    try self.pushContext(.parameters);
    defer self.popContext();

    var params = std.ArrayListUnmanaged(Parameter){};
    errdefer params.deinit(self.allocator);

    if (!self.check(.rparen)) {
        const first = try self.parseParameter();
        try params.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rparen)) break;
            const param = try self.parseParameter();
            try params.append(self.allocator, param);
        }
    }

    return params.toOwnedSlice(self.allocator);
}

/// Parse a single parameter: name: Type
fn parseParameter(self: *Parser) ParseError!Parameter {
    const start = self.peek().span.start;

    // Check for 'self' parameter
    if (self.check(.kw_self)) {
        _ = self.advance();
        const end = self.previous().span.end;
        const span = Span.init(start, end);

        // self might have a type annotation
        var type_expr = TypeExpr.init(.{ .self_type = {} }, span);
        if (self.match(.colon)) {
            type_expr = try self.parseTypeExpr();
        }

        return Parameter.selfParam(type_expr, span);
    }

    const name = try self.parseIdentifier();
    _ = try self.expect(.colon, "Expected ':'");
    const type_expr = try self.parseTypeExpr();

    const end = self.previous().span.end;
    return Parameter.init(name, type_expr, Span.init(start, end));
}

// ============================================================================
// Interface Specification Parsing
// ============================================================================

/// Parse an interface specification (placeholder - will be expanded)
fn parseInterfaceSpec(self: *Parser) ParseError!InterfaceSpec {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_interface, "Expected 'interface'");

    const name = try self.parseIdentifier();
    const type_params = try self.parseTypeParameters();

    // Parse optional super traits
    var super_traits = std.ArrayListUnmanaged(QualifiedName){};
    errdefer super_traits.deinit(self.allocator);

    if (self.match(.colon)) {
        const first = try self.parseQualifiedName();
        try super_traits.append(self.allocator, first);

        while (self.match(.op_plus)) {
            const trait = try self.parseQualifiedName();
            try super_traits.append(self.allocator, trait);
        }
    }

    _ = try self.expect(.lbrace, "Expected '{'");
    // TODO: Parse interface members
    _ = try self.expect(.rbrace, "Expected '}'");

    const end = self.previous().span.end;
    return InterfaceSpec.init(
        name,
        type_params,
        try super_traits.toOwnedSlice(self.allocator),
        &.{},
        &.{},
        Span.init(start, end),
    );
}

// ============================================================================
// Invariant, Axiom, Lemma Parsing
// ============================================================================

/// Parse an invariant declaration
fn parseInvariantDecl(self: *Parser) ParseError!InvariantDecl {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_invariant, "Expected 'invariant'");

    var name: ?Identifier = null;
    if (self.check(.identifier)) {
        name = try self.parseIdentifier();
    }

    _ = self.match(.colon);
    const condition = try self.parseExpression();

    const end = self.previous().span.end;
    return InvariantDecl.init(name, condition, Span.init(start, end));
}

/// Parse an axiom declaration
fn parseAxiomDecl(self: *Parser) ParseError!AxiomDecl {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_axiom, "Expected 'axiom'");

    const name = try self.parseIdentifier();
    _ = self.match(.colon);
    const condition = try self.parseExpression();

    const end = self.previous().span.end;
    return AxiomDecl.init(name, condition, Span.init(start, end));
}

/// Parse a lemma declaration (placeholder - will be expanded)
fn parseLemmaDecl(self: *Parser) ParseError!LemmaDecl {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_lemma, "Expected 'lemma'");

    const name = try self.parseIdentifier();
    const type_params = try self.parseTypeParameters();

    // Parse optional parameters
    var params: []const Parameter = &.{};
    if (self.match(.lparen)) {
        params = try self.parseParameters();
        _ = try self.expect(.rparen, "Expected ')'");
    }

    _ = self.match(.colon);
    const condition = try self.parseExpression();

    const end = self.previous().span.end;
    return LemmaDecl.init(name, type_params, params, condition, Span.init(start, end));
}

// ============================================================================
// Type Expression Parsing
// ============================================================================

/// Parse a type expression
fn parseTypeExpr(self: *Parser) ParseError!TypeExpr {
    const start = self.peek().span.start;

    // Check for optional type prefix (?)
    if (self.match(.op_question)) {
        const inner = try self.parseTypeExpr();
        const inner_ptr = try self.allocator.create(TypeExpr);
        inner_ptr.* = inner;
        const end = self.previous().span.end;
        return TypeExpr.init(.{ .optional = inner_ptr }, Span.init(start, end));
    }

    // Check for hole type (???)
    if (self.check(.hole)) {
        _ = self.advance();
        const end = self.previous().span.end;
        return TypeExpr.init(.{ .hole = {} }, Span.init(start, end));
    }

    // Check for self type
    if (self.match(.kw_self)) {
        const end = self.previous().span.end;
        return TypeExpr.init(.{ .self_type = {} }, Span.init(start, end));
    }

    // Check for function type: fn(params) -> return
    if (self.match(.kw_fn)) {
        return self.parseFunctionType(start);
    }

    // Check for tuple type: (T1, T2)
    if (self.check(.lparen)) {
        return self.parseTupleType(start);
    }

    // Named or generic type
    const name = try self.parseQualifiedName();

    // Check for type arguments [T, U]
    if (self.check(.lbracket)) {
        return self.parseGenericType(name, start);
    }

    const end = self.previous().span.end;
    return TypeExpr.init(.{ .named = name }, Span.init(start, end));
}

/// Parse a function type: fn(T1, T2) -> R
fn parseFunctionType(self: *Parser, start: Location) ParseError!TypeExpr {
    _ = try self.expect(.lparen, "Expected '('");

    var params = std.ArrayListUnmanaged(TypeExpr){};
    errdefer params.deinit(self.allocator);

    if (!self.check(.rparen)) {
        const first = try self.parseTypeExpr();
        try params.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rparen)) break;
            const param = try self.parseTypeExpr();
            try params.append(self.allocator, param);
        }
    }

    _ = try self.expect(.rparen, "Expected ')'");
    _ = try self.expect(.op_arrow, "Expected '->'");

    const return_type = try self.parseTypeExpr();
    const return_ptr = try self.allocator.create(TypeExpr);
    return_ptr.* = return_type;

    const end = self.previous().span.end;
    return TypeExpr.init(.{
        .function = .{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_ptr,
        },
    }, Span.init(start, end));
}

/// Parse a tuple type: (T1, T2, T3)
fn parseTupleType(self: *Parser, start: Location) ParseError!TypeExpr {
    _ = try self.expect(.lparen, "Expected '('");

    var types = std.ArrayListUnmanaged(TypeExpr){};
    errdefer types.deinit(self.allocator);

    if (!self.check(.rparen)) {
        const first = try self.parseTypeExpr();
        try types.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rparen)) break;
            const t = try self.parseTypeExpr();
            try types.append(self.allocator, t);
        }
    }

    _ = try self.expect(.rparen, "Expected ')'");

    const end = self.previous().span.end;

    // Single element tuple is just the type itself
    if (types.items.len == 1) {
        const single = types.items[0];
        types.deinit(self.allocator);
        return single;
    }

    return TypeExpr.init(.{ .tuple = try types.toOwnedSlice(self.allocator) }, Span.init(start, end));
}

/// Parse a generic type: Name[T, U]
fn parseGenericType(self: *Parser, base: QualifiedName, start: Location) ParseError!TypeExpr {
    _ = try self.expect(.lbracket, "Expected '['");

    var args = std.ArrayListUnmanaged(TypeExpr){};
    errdefer args.deinit(self.allocator);

    if (!self.check(.rbracket)) {
        const first = try self.parseTypeExpr();
        try args.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rbracket)) break;
            const arg = try self.parseTypeExpr();
            try args.append(self.allocator, arg);
        }
    }

    _ = try self.expect(.rbracket, "Expected ']'");

    const end = self.previous().span.end;
    return TypeExpr.init(.{
        .generic = GenericType.init(base, try args.toOwnedSlice(self.allocator)),
    }, Span.init(start, end));
}

// ============================================================================
// Expression Parsing (Basic - will be expanded)
// ============================================================================

/// Parse an expression (simplified for initial implementation)
fn parseExpression(self: *Parser) ParseError!Expression {
    try self.pushContext(.expression);
    defer self.popContext();

    return self.parseLogicalOr();
}

fn parseLogicalOr(self: *Parser) ParseError!Expression {
    var left = try self.parseLogicalAnd();

    while (self.check(.kw_or)) {
        _ = self.advance();
        const right = try self.parseLogicalAnd();

        const left_ptr = try self.allocator.create(Expression);
        left_ptr.* = left;
        const right_ptr = try self.allocator.create(Expression);
        right_ptr.* = right;

        const start = left.span.start;
        const end = right.span.end;

        left = Expression.init(.{
            .or_expr = .{ .left = left_ptr, .right = right_ptr },
        }, Span.init(start, end));
    }

    return left;
}

fn parseLogicalAnd(self: *Parser) ParseError!Expression {
    var left = try self.parseImplication();

    while (self.check(.kw_and)) {
        _ = self.advance();
        const right = try self.parseImplication();

        const left_ptr = try self.allocator.create(Expression);
        left_ptr.* = left;
        const right_ptr = try self.allocator.create(Expression);
        right_ptr.* = right;

        const start = left.span.start;
        const end = right.span.end;

        left = Expression.init(.{
            .and_expr = .{ .left = left_ptr, .right = right_ptr },
        }, Span.init(start, end));
    }

    return left;
}

fn parseImplication(self: *Parser) ParseError!Expression {
    var left = try self.parseComparison();

    while (true) {
        if (self.match(.op_implies)) {
            const right = try self.parseComparison();

            const left_ptr = try self.allocator.create(Expression);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(Expression);
            right_ptr.* = right;

            const start = left.span.start;
            const end = right.span.end;

            left = Expression.init(.{
                .implies = .{ .antecedent = left_ptr, .consequent = right_ptr },
            }, Span.init(start, end));
        } else if (self.match(.op_iff)) {
            const right = try self.parseComparison();

            const left_ptr = try self.allocator.create(Expression);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(Expression);
            right_ptr.* = right;

            const start = left.span.start;
            const end = right.span.end;

            left = Expression.init(.{
                .iff = .{ .left = left_ptr, .right = right_ptr },
            }, Span.init(start, end));
        } else {
            break;
        }
    }

    return left;
}

fn parseComparison(self: *Parser) ParseError!Expression {
    var left = try self.parseAdditive();

    while (true) {
        const op: ?Ast.BinaryOp = switch (self.peek().type) {
            .op_eq => .eq,
            .op_ne => .ne,
            .op_lt => .lt,
            .op_le => .le,
            .op_gt => .gt,
            .op_ge => .ge,
            .kw_in => .in_op,
            .kw_subset => .subset,
            else => null,
        };

        if (op) |binary_op| {
            _ = self.advance();
            const right = try self.parseAdditive();

            const left_ptr = try self.allocator.create(Expression);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(Expression);
            right_ptr.* = right;

            const start = left.span.start;
            const end = right.span.end;

            left = Expression.init(.{
                .binary = Ast.BinaryExpr.init(binary_op, left_ptr, right_ptr),
            }, Span.init(start, end));
        } else {
            break;
        }
    }

    return left;
}

fn parseAdditive(self: *Parser) ParseError!Expression {
    var left = try self.parseMultiplicative();

    while (true) {
        const op: ?Ast.BinaryOp = switch (self.peek().type) {
            .op_plus => .add,
            .op_minus => .sub,
            .kw_union => .union_op,
            else => null,
        };

        if (op) |binary_op| {
            _ = self.advance();
            const right = try self.parseMultiplicative();

            const left_ptr = try self.allocator.create(Expression);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(Expression);
            right_ptr.* = right;

            const start = left.span.start;
            const end = right.span.end;

            left = Expression.init(.{
                .binary = Ast.BinaryExpr.init(binary_op, left_ptr, right_ptr),
            }, Span.init(start, end));
        } else {
            break;
        }
    }

    return left;
}

fn parseMultiplicative(self: *Parser) ParseError!Expression {
    var left = try self.parseUnary();

    while (true) {
        const op: ?Ast.BinaryOp = switch (self.peek().type) {
            .op_star => .mul,
            .op_slash => .div,
            .op_percent => .mod,
            .kw_intersect => .intersect,
            else => null,
        };

        if (op) |binary_op| {
            _ = self.advance();
            const right = try self.parseUnary();

            const left_ptr = try self.allocator.create(Expression);
            left_ptr.* = left;
            const right_ptr = try self.allocator.create(Expression);
            right_ptr.* = right;

            const start = left.span.start;
            const end = right.span.end;

            left = Expression.init(.{
                .binary = Ast.BinaryExpr.init(binary_op, left_ptr, right_ptr),
            }, Span.init(start, end));
        } else {
            break;
        }
    }

    return left;
}

fn parseUnary(self: *Parser) ParseError!Expression {
    const start = self.peek().span.start;

    if (self.match(.kw_not)) {
        const operand = try self.parseUnary();
        const operand_ptr = try self.allocator.create(Expression);
        operand_ptr.* = operand;
        const end = self.previous().span.end;
        return Expression.init(.{ .not_expr = .{ .operand = operand_ptr } }, Span.init(start, end));
    }

    if (self.match(.op_minus)) {
        const operand = try self.parseUnary();
        const operand_ptr = try self.allocator.create(Expression);
        operand_ptr.* = operand;
        const end = self.previous().span.end;
        return Expression.init(.{
            .unary = Ast.UnaryExpr.init(.neg, operand_ptr),
        }, Span.init(start, end));
    }

    return self.parsePostfix();
}

fn parsePostfix(self: *Parser) ParseError!Expression {
    var expr = try self.parsePrimary();

    while (true) {
        if (self.match(.dot)) {
            const field_name = try self.parseIdentifier();

            // Check for method call
            if (self.check(.lparen)) {
                _ = self.advance();
                var args = std.ArrayListUnmanaged(Expression){};
                errdefer args.deinit(self.allocator);

                if (!self.check(.rparen)) {
                    const first = try self.parseExpression();
                    try args.append(self.allocator, first);

                    while (self.match(.comma)) {
                        if (self.check(.rparen)) break;
                        const arg = try self.parseExpression();
                        try args.append(self.allocator, arg);
                    }
                }
                _ = try self.expect(.rparen, "Expected ')'");

                const expr_ptr = try self.allocator.create(Expression);
                expr_ptr.* = expr;
                const end = self.previous().span.end;

                expr = Expression.init(.{
                    .method_call = .{
                        .object = expr_ptr,
                        .method = field_name,
                        .args = try args.toOwnedSlice(self.allocator),
                    },
                }, Span.init(expr.span.start, end));
            } else {
                const expr_ptr = try self.allocator.create(Expression);
                expr_ptr.* = expr;
                const end = self.previous().span.end;

                expr = Expression.init(.{
                    .field_access = .{
                        .object = expr_ptr,
                        .field = field_name,
                    },
                }, Span.init(expr.span.start, end));
            }
        } else if (self.match(.lbracket)) {
            const index = try self.parseExpression();
            _ = try self.expect(.rbracket, "Expected ']'");

            const expr_ptr = try self.allocator.create(Expression);
            expr_ptr.* = expr;
            const index_ptr = try self.allocator.create(Expression);
            index_ptr.* = index;
            const end = self.previous().span.end;

            expr = Expression.init(.{
                .index_access = .{
                    .object = expr_ptr,
                    .index = index_ptr,
                },
            }, Span.init(expr.span.start, end));
        } else if (self.match(.lparen)) {
            // Function call
            var args = std.ArrayListUnmanaged(Expression){};
            errdefer args.deinit(self.allocator);

            if (!self.check(.rparen)) {
                const first = try self.parseExpression();
                try args.append(self.allocator, first);

                while (self.match(.comma)) {
                    if (self.check(.rparen)) break;
                    const arg = try self.parseExpression();
                    try args.append(self.allocator, arg);
                }
            }
            _ = try self.expect(.rparen, "Expected ')'");

            const expr_ptr = try self.allocator.create(Expression);
            expr_ptr.* = expr;
            const end = self.previous().span.end;

            expr = Expression.init(.{
                .call = .{
                    .callee = expr_ptr,
                    .type_args = &.{},
                    .args = try args.toOwnedSlice(self.allocator),
                },
            }, Span.init(expr.span.start, end));
        } else {
            break;
        }
    }

    return expr;
}

fn parsePrimary(self: *Parser) ParseError!Expression {
    const token = self.peek();
    const start = token.span.start;

    switch (token.type) {
        .int_literal => {
            _ = self.advance();
            // Handle underscore separators in number
            var clean_lexeme: []const u8 = token.lexeme;
            var has_underscore = false;
            for (token.lexeme) |c| {
                if (c == '_') {
                    has_underscore = true;
                    break;
                }
            }
            if (has_underscore) {
                var buf = std.ArrayListUnmanaged(u8){};
                for (token.lexeme) |c| {
                    if (c != '_') {
                        buf.append(self.allocator, c) catch {};
                    }
                }
                clean_lexeme = buf.items;
            }
            const value = std.fmt.parseInt(i64, clean_lexeme, 10) catch 0;
            const end = self.previous().span.end;
            return Expression.init(.{ .int_literal = value }, Span.init(start, end));
        },

        .float_literal => {
            _ = self.advance();
            const value = std.fmt.parseFloat(f64, token.lexeme) catch 0.0;
            const end = self.previous().span.end;
            return Expression.init(.{ .float_literal = value }, Span.init(start, end));
        },

        .string_literal => {
            _ = self.advance();
            const content = if (token.lexeme.len >= 2)
                token.lexeme[1 .. token.lexeme.len - 1]
            else
                token.lexeme;
            const end = self.previous().span.end;
            return Expression.init(.{ .string_literal = content }, Span.init(start, end));
        },

        .kw_true => {
            _ = self.advance();
            const end = self.previous().span.end;
            return Expression.init(.{ .bool_literal = true }, Span.init(start, end));
        },

        .kw_false => {
            _ = self.advance();
            const end = self.previous().span.end;
            return Expression.init(.{ .bool_literal = false }, Span.init(start, end));
        },

        .kw_result => {
            _ = self.advance();
            const end = self.previous().span.end;
            return Expression.init(.{ .result = {} }, Span.init(start, end));
        },

        .kw_self => {
            _ = self.advance();
            const end = self.previous().span.end;
            return Expression.init(.{ .self_expr = {} }, Span.init(start, end));
        },

        .hole => {
            _ = self.advance();
            const end = self.previous().span.end;
            return Expression.init(.{ .hole = {} }, Span.init(start, end));
        },

        .kw_old => {
            _ = self.advance();
            _ = try self.expect(.lparen, "Expected '(' after 'old'");
            const inner = try self.parseExpression();
            _ = try self.expect(.rparen, "Expected ')'");
            const inner_ptr = try self.allocator.create(Expression);
            inner_ptr.* = inner;
            const end = self.previous().span.end;
            return Expression.init(.{ .old = inner_ptr }, Span.init(start, end));
        },

        .kw_forall => {
            return self.parseQuantifier(.forall);
        },

        .kw_exists => {
            return self.parseQuantifier(.exists);
        },

        .kw_if => {
            return self.parseIfExpr();
        },

        .kw_let => {
            return self.parseLetExpr();
        },

        .lparen => {
            _ = self.advance();
            const expr = try self.parseExpression();
            _ = try self.expect(.rparen, "Expected ')'");
            return expr;
        },

        .lbracket => {
            return self.parseSequenceLiteral();
        },

        .lbrace => {
            return self.parseSetOrMapLiteral();
        },

        .identifier => {
            const ident = try self.parseIdentifier();
            const end = self.previous().span.end;

            // Check if it's a qualified name (a.b.c)
            if (self.check(.dot) and self.peekAhead(1).type == .identifier) {
                var parts = std.ArrayListUnmanaged(Identifier){};
                errdefer parts.deinit(self.allocator);
                try parts.append(self.allocator, ident);

                while (self.check(.dot) and self.peekAhead(1).type == .identifier) {
                    _ = self.advance(); // consume '.'
                    const part = try self.parseIdentifier();
                    try parts.append(self.allocator, part);
                }

                const qname_end = self.previous().span.end;
                return Expression.init(.{
                    .qualified = QualifiedName.init(
                        try parts.toOwnedSlice(self.allocator),
                        Span.init(start, qname_end),
                    ),
                }, Span.init(start, qname_end));
            }

            return Expression.init(.{ .identifier = ident }, Span.init(start, end));
        },

        else => {
            try self.addDiagnostic("Expected expression", token.span, .err);
            return ParseError.UnexpectedToken;
        },
    }
}

fn parseQuantifier(self: *Parser, kind: enum { forall, exists }) ParseError!Expression {
    const start = self.peek().span.start;
    _ = self.advance(); // consume forall/exists

    // Parse quantifier variables
    var variables = std.ArrayListUnmanaged(Ast.QuantifierVar){};
    errdefer variables.deinit(self.allocator);

    const first_var = try self.parseQuantifierVar();
    try variables.append(self.allocator, first_var);

    while (self.match(.comma)) {
        const qvar = try self.parseQuantifierVar();
        try variables.append(self.allocator, qvar);
    }

    // Parse optional such_that condition
    var condition: ?*const Expression = null;
    if (self.match(.kw_such_that) or self.match(.op_pipe)) {
        const cond = try self.parseExpression();
        const cond_ptr = try self.allocator.create(Expression);
        cond_ptr.* = cond;
        condition = cond_ptr;
    }

    // Parse body after colon
    _ = try self.expect(.colon, "Expected ':' before quantifier body");
    const body = try self.parseExpression();
    const body_ptr = try self.allocator.create(Expression);
    body_ptr.* = body;

    const end = self.previous().span.end;

    const qexpr = Ast.QuantifierExpr{
        .variables = try variables.toOwnedSlice(self.allocator),
        .condition = condition,
        .body = body_ptr,
    };

    return Expression.init(
        if (kind == .forall) .{ .forall = qexpr } else .{ .exists = qexpr },
        Span.init(start, end),
    );
}

fn parseQuantifierVar(self: *Parser) ParseError!Ast.QuantifierVar {
    const start = self.peek().span.start;
    const name = try self.parseIdentifier();

    // Parse domain: either type (: T) or collection (in coll)
    var domain: Ast.QuantifierDomain = undefined;

    if (self.match(.colon)) {
        const type_expr = try self.parseTypeExpr();
        domain = .{ .type_domain = type_expr };
    } else if (self.match(.kw_in)) {
        const coll = try self.parseExpression();
        const coll_ptr = try self.allocator.create(Expression);
        coll_ptr.* = coll;
        domain = .{ .collection = coll_ptr };
    } else {
        try self.addDiagnostic("Expected ':' or 'in' in quantifier variable", self.peek().span, .err);
        return ParseError.UnexpectedToken;
    }

    const end = self.previous().span.end;
    return .{
        .name = name,
        .domain = domain,
        .span = Span.init(start, end),
    };
}

fn parseIfExpr(self: *Parser) ParseError!Expression {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_if, "Expected 'if'");

    const condition = try self.parseExpression();
    _ = try self.expect(.kw_then, "Expected 'then'");
    const then_branch = try self.parseExpression();

    var else_branch: ?*const Expression = null;
    if (self.match(.kw_else)) {
        const else_expr = try self.parseExpression();
        const else_ptr = try self.allocator.create(Expression);
        else_ptr.* = else_expr;
        else_branch = else_ptr;
    }

    const cond_ptr = try self.allocator.create(Expression);
    cond_ptr.* = condition;
    const then_ptr = try self.allocator.create(Expression);
    then_ptr.* = then_branch;

    const end = self.previous().span.end;
    return Expression.init(.{
        .if_expr = .{
            .condition = cond_ptr,
            .then_branch = then_ptr,
            .else_branch = else_branch,
        },
    }, Span.init(start, end));
}

fn parseLetExpr(self: *Parser) ParseError!Expression {
    const start = self.peek().span.start;
    _ = try self.expect(.kw_let, "Expected 'let'");

    const name = try self.parseIdentifier();

    // Optional type annotation
    var type_annotation: ?TypeExpr = null;
    if (self.match(.colon)) {
        type_annotation = try self.parseTypeExpr();
    }

    _ = try self.expect(.op_assign, "Expected '='");
    // Parse value but stop at 'in' keyword (don't consume it as a comparison)
    const value = try self.parseAdditive();
    _ = try self.expect(.kw_in, "Expected 'in'");
    const body = try self.parseExpression();

    const value_ptr = try self.allocator.create(Expression);
    value_ptr.* = value;
    const body_ptr = try self.allocator.create(Expression);
    body_ptr.* = body;

    const end = self.previous().span.end;
    return Expression.init(.{
        .let_expr = .{
            .name = name,
            .type_annotation = type_annotation,
            .value = value_ptr,
            .body = body_ptr,
        },
    }, Span.init(start, end));
}

fn parseSequenceLiteral(self: *Parser) ParseError!Expression {
    const start = self.peek().span.start;
    _ = try self.expect(.lbracket, "Expected '['");

    var elements = std.ArrayListUnmanaged(Expression){};
    errdefer elements.deinit(self.allocator);

    if (!self.check(.rbracket)) {
        const first = try self.parseExpression();
        try elements.append(self.allocator, first);

        while (self.match(.comma)) {
            if (self.check(.rbracket)) break;
            const elem = try self.parseExpression();
            try elements.append(self.allocator, elem);
        }
    }

    _ = try self.expect(.rbracket, "Expected ']'");
    const end = self.previous().span.end;

    return Expression.init(.{
        .sequence_literal = try elements.toOwnedSlice(self.allocator),
    }, Span.init(start, end));
}

fn parseSetOrMapLiteral(self: *Parser) ParseError!Expression {
    const start = self.peek().span.start;
    _ = try self.expect(.lbrace, "Expected '{'");

    if (self.check(.rbrace)) {
        _ = self.advance();
        const end = self.previous().span.end;
        return Expression.init(.{ .set_literal = &.{} }, Span.init(start, end));
    }

    const first = try self.parseExpression();

    // Check if it's a map (first element followed by colon)
    if (self.match(.colon)) {
        // Map literal
        var entries = std.ArrayListUnmanaged(Ast.MapEntry){};
        errdefer entries.deinit(self.allocator);

        const first_value = try self.parseExpression();
        try entries.append(self.allocator, .{ .key = first, .value = first_value });

        while (self.match(.comma)) {
            if (self.check(.rbrace)) break;
            const key = try self.parseExpression();
            _ = try self.expect(.colon, "Expected ':'");
            const value = try self.parseExpression();
            try entries.append(self.allocator, .{ .key = key, .value = value });
        }

        _ = try self.expect(.rbrace, "Expected '}'");
        const end = self.previous().span.end;

        return Expression.init(.{
            .map_literal = try entries.toOwnedSlice(self.allocator),
        }, Span.init(start, end));
    }

    // Set literal
    var elements = std.ArrayListUnmanaged(Expression){};
    errdefer elements.deinit(self.allocator);
    try elements.append(self.allocator, first);

    while (self.match(.comma)) {
        if (self.check(.rbrace)) break;
        const elem = try self.parseExpression();
        try elements.append(self.allocator, elem);
    }

    _ = try self.expect(.rbrace, "Expected '}'");
    const end = self.previous().span.end;

    return Expression.init(.{
        .set_literal = try elements.toOwnedSlice(self.allocator),
    }, Span.init(start, end));
}

// ============================================================================
// Helper Functions
// ============================================================================

fn parseIdentifier(self: *Parser) ParseError!Identifier {
    const token = try self.expect(.identifier, "Expected identifier");
    return Identifier.init(token.lexeme, token.span);
}

/// Parse an identifier or keyword (for contexts where keywords can be used as names)
fn parseIdentifierOrKeyword(self: *Parser) ParseError!Identifier {
    const token = self.peek();
    if (token.type == .identifier or token.isKeyword()) {
        _ = self.advance();
        return Identifier.init(token.lexeme, token.span);
    }
    try self.addDiagnostic("Expected identifier", token.span, .err);
    return ParseError.UnexpectedToken;
}

fn parseQualifiedName(self: *Parser) ParseError!QualifiedName {
    const start = self.peek().span.start;

    var parts = std.ArrayListUnmanaged(Identifier){};
    errdefer parts.deinit(self.allocator);

    const first = try self.parseIdentifier();
    try parts.append(self.allocator, first);

    while (self.match(.dot)) {
        if (!self.check(.identifier)) break;
        const part = try self.parseIdentifier();
        try parts.append(self.allocator, part);
    }

    const end = self.previous().span.end;
    return QualifiedName.init(try parts.toOwnedSlice(self.allocator), Span.init(start, end));
}

// ============================================================================
// Tests
// ============================================================================

test "parser module declaration" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "module my.test.pkg");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expect(mod.name != null);
    try testing.expectEqual(@as(usize, 3), mod.name.?.parts.len);
    try testing.expectEqualStrings("my", mod.name.?.parts[0].name);
    try testing.expectEqualStrings("test", mod.name.?.parts[1].name);
    try testing.expectEqualStrings("pkg", mod.name.?.parts[2].name);
}

test "parser import declaration" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "import std.collections");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.imports.len);
    try testing.expectEqual(@as(usize, 2), mod.imports[0].path.parts.len);
    try testing.expectEqualStrings("std", mod.imports[0].path.parts[0].name);
    try testing.expectEqualStrings("collections", mod.imports[0].path.parts[1].name);
}

test "parser import with items" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "import std.collections { List, Map as M }");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.imports.len);
    try testing.expectEqual(@as(usize, 2), mod.imports[0].items.len);
    try testing.expectEqualStrings("List", mod.imports[0].items[0].name.name);
    try testing.expect(mod.imports[0].items[0].alias == null);
    try testing.expectEqualStrings("Map", mod.imports[0].items[1].name.name);
    try testing.expectEqualStrings("M", mod.imports[0].items[1].alias.?.name);
}

test "parser type definition alias" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "type Email = string");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const decl = mod.declarations[0];
    try testing.expect(decl.kind == .type_def);
    try testing.expectEqualStrings("Email", decl.kind.type_def.name.name);
}

test "parser type definition product" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "type User = { id: i32, name: string }");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const type_def = mod.declarations[0].kind.type_def;
    try testing.expectEqualStrings("User", type_def.name.name);
    try testing.expect(type_def.body == .product);
    try testing.expectEqual(@as(usize, 2), type_def.body.product.len);
}

test "parser type definition sum" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "type Status = | Active | Inactive { reason: string }");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const type_def = mod.declarations[0].kind.type_def;
    try testing.expect(type_def.body == .sum);
    try testing.expectEqual(@as(usize, 2), type_def.body.sum.len);
    try testing.expectEqualStrings("Active", type_def.body.sum[0].name.name);
    try testing.expectEqualStrings("Inactive", type_def.body.sum[1].name.name);
    try testing.expectEqual(@as(usize, 1), type_def.body.sum[1].fields.len);
}

test "parser function spec" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\spec fn divide(a: i32, b: i32) -> i32
        \\  requires b != 0
        \\  ensures result * b + a % b == a
    ;

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const spec = mod.declarations[0].kind.spec_fn;
    try testing.expectEqualStrings("divide", spec.name.name);
    try testing.expectEqual(@as(usize, 2), spec.params.len);
    try testing.expectEqual(@as(usize, 1), spec.requires.len);
    try testing.expectEqual(@as(usize, 1), spec.ensures.len);
}

test "parser attribute" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "@author(\"human\") type X = i32");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    try testing.expectEqual(@as(usize, 1), mod.declarations[0].attributes.len);
    try testing.expectEqualStrings("author", mod.declarations[0].attributes[0].name.name);
}

test "parser generic type" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "type IntList = List[i32]");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const type_def = mod.declarations[0].kind.type_def;
    try testing.expect(type_def.body == .alias);
    try testing.expect(type_def.body.alias.kind == .generic);
}

test "parser expression literals" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "axiom test: 42 + 3.14 and true");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    try testing.expect(mod.declarations[0].kind == .axiom);
}

test "parser old expression" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\spec fn increment(x: i32) -> i32
        \\  ensures result == old(x) + 1
    ;

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();
    try testing.expect(!parser.hasErrors());
    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
}

test "parser quantifier expression" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "axiom all_positive: forall x: i32: x > 0");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();
    try testing.expect(!parser.hasErrors());
    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    const axiom = mod.declarations[0].kind.axiom;
    try testing.expect(axiom.condition.kind == .forall);
}

test "parser let expression" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "axiom test: let x = 5 in x + 1");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    _ = try parser.parseModule();
    try testing.expect(!parser.hasErrors());
}

test "parser if expression" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "axiom test: if x > 0 then x else 0");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    _ = try parser.parseModule();
    try testing.expect(!parser.hasErrors());
}

// ============================================================================
// Error Recovery Tests
// ============================================================================

test "error recovery: multiple declarations with error in first" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // First declaration has error (missing '=' after type name)
    // Second declaration is valid
    const source =
        \\type Broken string
        \\type Valid = i32
    ;

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    // Should have errors but still parse the second declaration
    try testing.expect(parser.hasErrors());
    // The valid declaration should be parsed
    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
    try testing.expectEqualStrings("Valid", mod.declarations[0].kind.type_def.name.name);
}

test "error recovery: continues after malformed import" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // First import is malformed (missing path after 'import')
    // Type definition should still be parsed
    const source =
        \\import
        \\type Valid = i32
    ;

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    const mod = try parser.parseModule();

    // Should have errors but continue parsing
    try testing.expect(parser.hasErrors());
    // The type definition should be parsed
    try testing.expectEqual(@as(usize, 1), mod.declarations.len);
}

test "error context is included in diagnostics" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Invalid type definition (missing identifier after 'type')
    const source = "type = i32";

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    _ = try parser.parseModule();

    try testing.expect(parser.hasErrors());
    try testing.expect(parser.diagnostics.items.len > 0);

    // First diagnostic should have context about type definition
    const first_diag = parser.diagnostics.items[0];
    try testing.expect(first_diag.context != null);
    try testing.expectEqualStrings("type definition", first_diag.context.?);
}

test "diagnostic has location information" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Error on line 2
    const source =
        \\type A = i32
        \\type = i32
    ;

    var lex = Lexer.init(alloc, source);
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    _ = try parser.parseModule();

    try testing.expect(parser.hasErrors());
    try testing.expect(parser.diagnostics.items.len > 0);

    // Diagnostic should be on line 2
    const diag = parser.diagnostics.items[0];
    try testing.expectEqual(@as(u32, 2), diag.span.start.line);
}

test "errorCount returns correct count" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(alloc, "type A = i32");
    const tokens = try lex.tokenize();
    var parser = Parser.init(alloc, tokens.items);

    _ = try parser.parseModule();

    try testing.expectEqual(@as(usize, 0), parser.errorCount());
}
