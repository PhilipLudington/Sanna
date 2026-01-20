const std = @import("std");

/// Source location for error reporting
pub const Location = struct {
    line: u32,
    column: u32,
    offset: usize,

    pub fn init(line: u32, column: u32, offset: usize) Location {
        return .{
            .line = line,
            .column = column,
            .offset = offset,
        };
    }
};

/// Span of source text
pub const Span = struct {
    start: Location,
    end: Location,

    pub fn init(start: Location, end: Location) Span {
        return .{
            .start = start,
            .end = end,
        };
    }
};

/// Token types for the Sanna specification language
pub const TokenType = enum {
    // ========================================
    // Keywords - Specification
    // ========================================
    kw_spec,
    kw_type,
    kw_model,
    kw_invariant,
    kw_axiom,
    kw_requires,
    kw_ensures,
    kw_modifies,
    kw_pure,
    kw_effect,
    kw_forall,
    kw_exists,
    kw_such_that,
    kw_old,
    kw_result,
    kw_decreases,
    kw_ghost,

    // ========================================
    // Keywords - Verification
    // ========================================
    kw_proven,
    kw_unproven,
    kw_assume,
    kw_assert,
    kw_lemma,
    kw_trusted,
    kw_unsafe,
    kw_admits,

    // ========================================
    // Keywords - Provenance & Confidence
    // ========================================
    kw_author,
    kw_confidence,
    kw_reviewed,
    kw_generated,
    kw_needs_review,
    kw_approved,
    kw_deprecated,

    // ========================================
    // Keywords - Structure
    // ========================================
    kw_import,
    kw_module,
    kw_pub,
    kw_impl,
    kw_for,
    kw_where,
    kw_if,
    kw_then,
    kw_else,
    kw_match,
    kw_let,
    kw_in,
    kw_and,
    kw_or,
    kw_not,
    kw_true,
    kw_false,
    kw_fn,
    kw_interface,
    kw_self,

    // ========================================
    // Operators - Logical
    // ========================================
    op_implies, // =>
    op_iff, // <=>
    // and, or, not are keywords

    // ========================================
    // Operators - Comparison
    // ========================================
    op_eq, // ==
    op_ne, // !=
    op_lt, // <
    op_gt, // >
    op_le, // <=
    op_ge, // >=

    // ========================================
    // Operators - Arithmetic
    // ========================================
    op_plus, // +
    op_minus, // -
    op_star, // *
    op_slash, // /
    op_percent, // %

    // ========================================
    // Operators - Set/Collection
    // ========================================
    // 'in' is a keyword
    kw_union,
    kw_intersect,
    kw_subset,
    kw_empty,

    // ========================================
    // Operators - Specification-specific
    // ========================================
    op_at, // @ (attribute prefix)
    op_pipe, // | (such that in comprehensions, sum type variant)
    op_range, // .. (range)
    op_arrow, // -> (function return type)
    op_question, // ? (optional/error propagation)
    op_assign, // = (type definition, let binding)

    // ========================================
    // Delimiters
    // ========================================
    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }
    lbracket, // [
    rbracket, // ]
    comma, // ,
    colon, // :
    semicolon, // ;
    dot, // .

    // ========================================
    // Literals
    // ========================================
    int_literal,
    float_literal,
    string_literal,
    hex_literal,

    // ========================================
    // Identifiers and Special
    // ========================================
    identifier,
    hole, // ???
    doc_comment, // /// or /** */
    eof,
    invalid,
};

/// A token from the Sanna lexer
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    span: Span,

    pub fn init(token_type: TokenType, lexeme: []const u8, span: Span) Token {
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .span = span,
        };
    }

    pub fn isKeyword(self: Token) bool {
        return switch (self.type) {
            .kw_spec,
            .kw_type,
            .kw_model,
            .kw_invariant,
            .kw_axiom,
            .kw_requires,
            .kw_ensures,
            .kw_modifies,
            .kw_pure,
            .kw_effect,
            .kw_forall,
            .kw_exists,
            .kw_such_that,
            .kw_old,
            .kw_result,
            .kw_decreases,
            .kw_ghost,
            .kw_proven,
            .kw_unproven,
            .kw_assume,
            .kw_assert,
            .kw_lemma,
            .kw_trusted,
            .kw_unsafe,
            .kw_admits,
            .kw_author,
            .kw_confidence,
            .kw_reviewed,
            .kw_generated,
            .kw_needs_review,
            .kw_approved,
            .kw_deprecated,
            .kw_import,
            .kw_module,
            .kw_pub,
            .kw_impl,
            .kw_for,
            .kw_where,
            .kw_if,
            .kw_then,
            .kw_else,
            .kw_match,
            .kw_let,
            .kw_in,
            .kw_and,
            .kw_or,
            .kw_not,
            .kw_true,
            .kw_false,
            .kw_fn,
            .kw_interface,
            .kw_self,
            .kw_union,
            .kw_intersect,
            .kw_subset,
            .kw_empty,
            => true,
            else => false,
        };
    }

    pub fn isOperator(self: Token) bool {
        return switch (self.type) {
            .op_implies,
            .op_iff,
            .op_eq,
            .op_ne,
            .op_lt,
            .op_gt,
            .op_le,
            .op_ge,
            .op_plus,
            .op_minus,
            .op_star,
            .op_slash,
            .op_percent,
            .op_at,
            .op_pipe,
            .op_range,
            .op_arrow,
            .op_question,
            .op_assign,
            => true,
            else => false,
        };
    }

    pub fn isLiteral(self: Token) bool {
        return switch (self.type) {
            .int_literal,
            .float_literal,
            .string_literal,
            .hex_literal,
            => true,
            else => false,
        };
    }
};

/// Keyword lookup table
pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    // Specification keywords
    .{ "spec", .kw_spec },
    .{ "type", .kw_type },
    .{ "model", .kw_model },
    .{ "invariant", .kw_invariant },
    .{ "axiom", .kw_axiom },
    .{ "requires", .kw_requires },
    .{ "ensures", .kw_ensures },
    .{ "modifies", .kw_modifies },
    .{ "pure", .kw_pure },
    .{ "effect", .kw_effect },
    .{ "forall", .kw_forall },
    .{ "exists", .kw_exists },
    .{ "such_that", .kw_such_that },
    .{ "old", .kw_old },
    .{ "result", .kw_result },
    .{ "decreases", .kw_decreases },
    .{ "ghost", .kw_ghost },

    // Verification keywords
    .{ "proven", .kw_proven },
    .{ "unproven", .kw_unproven },
    .{ "assume", .kw_assume },
    .{ "assert", .kw_assert },
    .{ "lemma", .kw_lemma },
    .{ "trusted", .kw_trusted },
    .{ "unsafe", .kw_unsafe },
    .{ "admits", .kw_admits },

    // Provenance keywords
    .{ "author", .kw_author },
    .{ "confidence", .kw_confidence },
    .{ "reviewed", .kw_reviewed },
    .{ "generated", .kw_generated },
    .{ "needs_review", .kw_needs_review },
    .{ "approved", .kw_approved },
    .{ "deprecated", .kw_deprecated },

    // Structure keywords
    .{ "import", .kw_import },
    .{ "module", .kw_module },
    .{ "pub", .kw_pub },
    .{ "impl", .kw_impl },
    .{ "for", .kw_for },
    .{ "where", .kw_where },
    .{ "if", .kw_if },
    .{ "then", .kw_then },
    .{ "else", .kw_else },
    .{ "match", .kw_match },
    .{ "let", .kw_let },
    .{ "in", .kw_in },
    .{ "and", .kw_and },
    .{ "or", .kw_or },
    .{ "not", .kw_not },
    .{ "true", .kw_true },
    .{ "false", .kw_false },
    .{ "fn", .kw_fn },
    .{ "interface", .kw_interface },
    .{ "self", .kw_self },

    // Set/collection keywords
    .{ "union", .kw_union },
    .{ "intersect", .kw_intersect },
    .{ "subset", .kw_subset },
    .{ "empty", .kw_empty },
});

/// Look up an identifier to see if it's a keyword
pub fn lookupKeyword(ident: []const u8) ?TokenType {
    return keywords.get(ident);
}

test "keyword lookup" {
    const testing = std.testing;

    try testing.expectEqual(TokenType.kw_spec, lookupKeyword("spec").?);
    try testing.expectEqual(TokenType.kw_requires, lookupKeyword("requires").?);
    try testing.expectEqual(TokenType.kw_ensures, lookupKeyword("ensures").?);
    try testing.expectEqual(TokenType.kw_forall, lookupKeyword("forall").?);
    try testing.expectEqual(TokenType.kw_true, lookupKeyword("true").?);
    try testing.expectEqual(TokenType.kw_false, lookupKeyword("false").?);
    try testing.expect(lookupKeyword("notakeyword") == null);
}

test "token classification" {
    const testing = std.testing;
    const dummy_span = Span.init(Location.init(1, 1, 0), Location.init(1, 1, 0));

    const kw_token = Token.init(.kw_spec, "spec", dummy_span);
    try testing.expect(kw_token.isKeyword());
    try testing.expect(!kw_token.isOperator());
    try testing.expect(!kw_token.isLiteral());

    const op_token = Token.init(.op_plus, "+", dummy_span);
    try testing.expect(!op_token.isKeyword());
    try testing.expect(op_token.isOperator());
    try testing.expect(!op_token.isLiteral());

    const lit_token = Token.init(.int_literal, "42", dummy_span);
    try testing.expect(!lit_token.isKeyword());
    try testing.expect(!lit_token.isOperator());
    try testing.expect(lit_token.isLiteral());
}
