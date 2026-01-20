const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");
const TokenType = Token.TokenType;
const Location = Token.Location;
const Span = Token.Span;

const Lexer = @This();

/// Lexer error types
pub const Error = error{
    UnterminatedString,
    InvalidCharacter,
    InvalidEscape,
    InvalidNumber,
    OutOfMemory,
};

source: []const u8,
offset: usize,
line: u32,
column: u32,
allocator: Allocator,
errors: std.ArrayListUnmanaged(LexError),

/// A lexer error with location information
pub const LexError = struct {
    message: []const u8,
    location: Location,
};

/// Initialize a new lexer
pub fn init(allocator: Allocator, source: []const u8) Lexer {
    return .{
        .source = source,
        .offset = 0,
        .line = 1,
        .column = 1,
        .allocator = allocator,
        .errors = .{},
    };
}

/// Free lexer resources
pub fn deinit(self: *Lexer) void {
    self.errors.deinit(self.allocator);
}

/// Get the current character without advancing
fn peek(self: *const Lexer) ?u8 {
    if (self.offset >= self.source.len) return null;
    return self.source[self.offset];
}

/// Get the character at offset + n without advancing
fn peekAhead(self: *const Lexer, n: usize) ?u8 {
    const idx = self.offset + n;
    if (idx >= self.source.len) return null;
    return self.source[idx];
}

/// Advance and return the current character
fn advance(self: *Lexer) ?u8 {
    if (self.offset >= self.source.len) return null;
    const c = self.source[self.offset];
    self.offset += 1;
    if (c == '\n') {
        self.line += 1;
        self.column = 1;
    } else {
        self.column += 1;
    }
    return c;
}

/// Check if at end of source
fn isAtEnd(self: *const Lexer) bool {
    return self.offset >= self.source.len;
}

/// Get current location
fn currentLocation(self: *const Lexer) Location {
    return Location.init(self.line, self.column, self.offset);
}

/// Skip whitespace (but not newlines in certain contexts)
fn skipWhitespace(self: *Lexer) void {
    while (self.peek()) |c| {
        switch (c) {
            ' ', '\t', '\r', '\n' => _ = self.advance(),
            else => break,
        }
    }
}

/// Skip a line comment (// ...)
fn skipLineComment(self: *Lexer) void {
    while (self.peek()) |c| {
        if (c == '\n') break;
        _ = self.advance();
    }
}

/// Skip a block comment (/* ... */)
fn skipBlockComment(self: *Lexer) void {
    // Skip the opening /*
    _ = self.advance();
    _ = self.advance();

    var depth: u32 = 1;
    while (depth > 0 and !self.isAtEnd()) {
        const c = self.peek().?;
        if (c == '/' and self.peekAhead(1) == @as(u8, '*')) {
            depth += 1;
            _ = self.advance();
            _ = self.advance();
        } else if (c == '*' and self.peekAhead(1) == @as(u8, '/')) {
            depth -= 1;
            _ = self.advance();
            _ = self.advance();
        } else {
            _ = self.advance();
        }
    }
}

/// Scan an identifier or keyword
fn scanIdentifier(self: *Lexer) Token.Token {
    const start_loc = self.currentLocation();
    const start = self.offset;

    while (self.peek()) |c| {
        if (isAlphanumeric(c) or c == '_') {
            _ = self.advance();
        } else {
            break;
        }
    }

    const lexeme = self.source[start..self.offset];
    const end_loc = self.currentLocation();
    const span = Span.init(start_loc, end_loc);

    // Check if it's a keyword
    if (Token.lookupKeyword(lexeme)) |kw_type| {
        return Token.Token.init(kw_type, lexeme, span);
    }

    return Token.Token.init(.identifier, lexeme, span);
}

/// Scan a number literal (integer, float, or hex)
fn scanNumber(self: *Lexer) Token.Token {
    const start_loc = self.currentLocation();
    const start = self.offset;

    // Check for hex prefix
    if (self.peek() == @as(u8, '0') and self.peekAhead(1) == @as(u8, 'x')) {
        _ = self.advance(); // 0
        _ = self.advance(); // x
        while (self.peek()) |c| {
            if (isHexDigit(c)) {
                _ = self.advance();
            } else {
                break;
            }
        }
        const lexeme = self.source[start..self.offset];
        const end_loc = self.currentLocation();
        return Token.Token.init(.hex_literal, lexeme, Span.init(start_loc, end_loc));
    }

    // Scan integer part
    while (self.peek()) |c| {
        if (isDigit(c) or c == '_') {
            _ = self.advance();
        } else {
            break;
        }
    }

    // Check for decimal point
    var is_float = false;
    if (self.peek() == @as(u8, '.') and self.peekAhead(1) != @as(u8, '.')) {
        // Make sure it's not a range (..)
        if (self.peekAhead(1)) |next| {
            if (isDigit(next)) {
                is_float = true;
                _ = self.advance(); // .
                while (self.peek()) |c| {
                    if (isDigit(c) or c == '_') {
                        _ = self.advance();
                    } else {
                        break;
                    }
                }
            }
        }
    }

    // Check for exponent
    if (self.peek()) |c| {
        if (c == 'e' or c == 'E') {
            is_float = true;
            _ = self.advance();
            if (self.peek()) |sign| {
                if (sign == '+' or sign == '-') {
                    _ = self.advance();
                }
            }
            while (self.peek()) |d| {
                if (isDigit(d)) {
                    _ = self.advance();
                } else {
                    break;
                }
            }
        }
    }

    const lexeme = self.source[start..self.offset];
    const end_loc = self.currentLocation();
    const span = Span.init(start_loc, end_loc);
    const token_type: TokenType = if (is_float) .float_literal else .int_literal;
    return Token.Token.init(token_type, lexeme, span);
}

/// Scan a string literal
fn scanString(self: *Lexer) !Token.Token {
    const start_loc = self.currentLocation();
    const start = self.offset;

    _ = self.advance(); // opening quote

    while (!self.isAtEnd()) {
        const c = self.peek().?;
        if (c == '"') {
            _ = self.advance(); // closing quote
            const lexeme = self.source[start..self.offset];
            const end_loc = self.currentLocation();
            return Token.Token.init(.string_literal, lexeme, Span.init(start_loc, end_loc));
        } else if (c == '\\') {
            _ = self.advance(); // backslash
            if (self.peek()) |_| {
                _ = self.advance(); // escaped char
            }
        } else if (c == '\n') {
            // Unterminated string
            try self.addError("Unterminated string literal", start_loc);
            const lexeme = self.source[start..self.offset];
            const end_loc = self.currentLocation();
            return Token.Token.init(.invalid, lexeme, Span.init(start_loc, end_loc));
        } else {
            _ = self.advance();
        }
    }

    // Reached end of file without closing quote
    try self.addError("Unterminated string literal", start_loc);
    const lexeme = self.source[start..self.offset];
    const end_loc = self.currentLocation();
    return Token.Token.init(.invalid, lexeme, Span.init(start_loc, end_loc));
}

/// Scan a doc comment (/// or /** */)
fn scanDocComment(self: *Lexer) Token.Token {
    const start_loc = self.currentLocation();
    const start = self.offset;

    if (self.peekAhead(2) == @as(u8, '/')) {
        // /// style doc comment
        _ = self.advance(); // /
        _ = self.advance(); // /
        _ = self.advance(); // /

        while (self.peek()) |c| {
            if (c == '\n') break;
            _ = self.advance();
        }
    } else if (self.peekAhead(2) == @as(u8, '*')) {
        // /** */ style doc comment
        _ = self.advance(); // /
        _ = self.advance(); // *
        _ = self.advance(); // *

        while (!self.isAtEnd()) {
            if (self.peek() == @as(u8, '*') and self.peekAhead(1) == @as(u8, '/')) {
                _ = self.advance();
                _ = self.advance();
                break;
            }
            _ = self.advance();
        }
    }

    const lexeme = self.source[start..self.offset];
    const end_loc = self.currentLocation();
    return Token.Token.init(.doc_comment, lexeme, Span.init(start_loc, end_loc));
}

/// Add an error to the error list
fn addError(self: *Lexer, message: []const u8, location: Location) !void {
    try self.errors.append(self.allocator, .{
        .message = message,
        .location = location,
    });
}

/// Make a simple token
fn makeToken(self: *Lexer, token_type: TokenType, start_loc: Location, len: usize) Token.Token {
    const lexeme = self.source[start_loc.offset .. start_loc.offset + len];
    const end_loc = self.currentLocation();
    return Token.Token.init(token_type, lexeme, Span.init(start_loc, end_loc));
}

/// Get the next token
pub fn nextToken(self: *Lexer) !Token.Token {
    self.skipWhitespace();

    if (self.isAtEnd()) {
        const loc = self.currentLocation();
        return Token.Token.init(.eof, "", Span.init(loc, loc));
    }

    const start_loc = self.currentLocation();
    const c = self.peek().?;

    // Identifiers and keywords
    if (isAlpha(c) or c == '_') {
        return self.scanIdentifier();
    }

    // Numbers
    if (isDigit(c)) {
        return self.scanNumber();
    }

    // String literals
    if (c == '"') {
        return try self.scanString();
    }

    // Operators and delimiters
    switch (c) {
        // Single character tokens
        '(' => {
            _ = self.advance();
            return self.makeToken(.lparen, start_loc, 1);
        },
        ')' => {
            _ = self.advance();
            return self.makeToken(.rparen, start_loc, 1);
        },
        '{' => {
            _ = self.advance();
            return self.makeToken(.lbrace, start_loc, 1);
        },
        '}' => {
            _ = self.advance();
            return self.makeToken(.rbrace, start_loc, 1);
        },
        '[' => {
            _ = self.advance();
            return self.makeToken(.lbracket, start_loc, 1);
        },
        ']' => {
            _ = self.advance();
            return self.makeToken(.rbracket, start_loc, 1);
        },
        ',' => {
            _ = self.advance();
            return self.makeToken(.comma, start_loc, 1);
        },
        ':' => {
            _ = self.advance();
            return self.makeToken(.colon, start_loc, 1);
        },
        ';' => {
            _ = self.advance();
            return self.makeToken(.semicolon, start_loc, 1);
        },
        '@' => {
            _ = self.advance();
            return self.makeToken(.op_at, start_loc, 1);
        },
        '|' => {
            _ = self.advance();
            return self.makeToken(.op_pipe, start_loc, 1);
        },
        '+' => {
            _ = self.advance();
            return self.makeToken(.op_plus, start_loc, 1);
        },
        '*' => {
            _ = self.advance();
            return self.makeToken(.op_star, start_loc, 1);
        },
        '%' => {
            _ = self.advance();
            return self.makeToken(.op_percent, start_loc, 1);
        },

        // Two or three character tokens
        '.' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '.')) {
                _ = self.advance();
                return self.makeToken(.op_range, start_loc, 2);
            }
            return self.makeToken(.dot, start_loc, 1);
        },
        '-' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '>')) {
                _ = self.advance();
                return self.makeToken(.op_arrow, start_loc, 2);
            }
            return self.makeToken(.op_minus, start_loc, 1);
        },
        '=' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '=')) {
                _ = self.advance();
                return self.makeToken(.op_eq, start_loc, 2);
            }
            if (self.peek() == @as(u8, '>')) {
                _ = self.advance();
                return self.makeToken(.op_implies, start_loc, 2);
            }
            // Single '=' for type definitions and let bindings
            return self.makeToken(.op_assign, start_loc, 1);
        },
        '!' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '=')) {
                _ = self.advance();
                return self.makeToken(.op_ne, start_loc, 2);
            }
            // Single '!' - not a valid operator in Sanna (use 'not')
            try self.addError("Unexpected '!' - use 'not' for negation", start_loc);
            return self.makeToken(.invalid, start_loc, 1);
        },
        '<' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '=')) {
                _ = self.advance();
                if (self.peek() == @as(u8, '>')) {
                    _ = self.advance();
                    return self.makeToken(.op_iff, start_loc, 3);
                }
                return self.makeToken(.op_le, start_loc, 2);
            }
            return self.makeToken(.op_lt, start_loc, 1);
        },
        '>' => {
            _ = self.advance();
            if (self.peek() == @as(u8, '=')) {
                _ = self.advance();
                return self.makeToken(.op_ge, start_loc, 2);
            }
            return self.makeToken(.op_gt, start_loc, 1);
        },
        '/' => {
            if (self.peekAhead(1) == @as(u8, '/')) {
                // Check for doc comment
                if (self.peekAhead(2) == @as(u8, '/')) {
                    return self.scanDocComment();
                }
                // Regular line comment
                self.skipLineComment();
                return try self.nextToken();
            }
            if (self.peekAhead(1) == @as(u8, '*')) {
                // Check for doc comment
                if (self.peekAhead(2) == @as(u8, '*') and self.peekAhead(3) != @as(u8, '/')) {
                    return self.scanDocComment();
                }
                // Regular block comment
                self.skipBlockComment();
                return try self.nextToken();
            }
            _ = self.advance();
            return self.makeToken(.op_slash, start_loc, 1);
        },
        '?' => {
            _ = self.advance();
            // Check for hole ???
            if (self.peek() == @as(u8, '?') and self.peekAhead(1) == @as(u8, '?')) {
                _ = self.advance();
                _ = self.advance();
                return self.makeToken(.hole, start_loc, 3);
            }
            return self.makeToken(.op_question, start_loc, 1);
        },

        else => {
            _ = self.advance();
            try self.addError("Unexpected character", start_loc);
            return self.makeToken(.invalid, start_loc, 1);
        },
    }
}

/// Tokenize the entire source and return all tokens
pub fn tokenize(self: *Lexer) !std.ArrayListUnmanaged(Token.Token) {
    var tokens = std.ArrayListUnmanaged(Token.Token){};
    errdefer tokens.deinit(self.allocator);

    while (true) {
        const token = try self.nextToken();
        try tokens.append(self.allocator, token);
        if (token.type == .eof) break;
    }

    return tokens;
}

/// Check if there were any lexer errors
pub fn hasErrors(self: *const Lexer) bool {
    return self.errors.items.len > 0;
}

// Helper functions
fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isAlphanumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

// Tests
test "lexer basic tokens" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "spec fn foo()");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.kw_spec, t1.type);
    try testing.expectEqualStrings("spec", t1.lexeme);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.kw_fn, t2.type);

    const t3 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t3.type);
    try testing.expectEqualStrings("foo", t3.lexeme);

    const t4 = try lexer.nextToken();
    try testing.expectEqual(TokenType.lparen, t4.type);

    const t5 = try lexer.nextToken();
    try testing.expectEqual(TokenType.rparen, t5.type);

    const t6 = try lexer.nextToken();
    try testing.expectEqual(TokenType.eof, t6.type);
}

test "lexer operators" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "=> <=> == != <= >= -> ..");
    defer lexer.deinit();

    try testing.expectEqual(TokenType.op_implies, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_iff, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_eq, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_ne, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_le, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_ge, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_arrow, (try lexer.nextToken()).type);
    try testing.expectEqual(TokenType.op_range, (try lexer.nextToken()).type);
}

test "lexer numbers" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "42 3.14 0xff 1_000_000");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.int_literal, t1.type);
    try testing.expectEqualStrings("42", t1.lexeme);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.float_literal, t2.type);
    try testing.expectEqualStrings("3.14", t2.lexeme);

    const t3 = try lexer.nextToken();
    try testing.expectEqual(TokenType.hex_literal, t3.type);
    try testing.expectEqualStrings("0xff", t3.lexeme);

    const t4 = try lexer.nextToken();
    try testing.expectEqual(TokenType.int_literal, t4.type);
    try testing.expectEqualStrings("1_000_000", t4.lexeme);
}

test "lexer strings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "\"hello\" \"world\\n\"");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.string_literal, t1.type);
    try testing.expectEqualStrings("\"hello\"", t1.lexeme);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.string_literal, t2.type);
    try testing.expectEqualStrings("\"world\\n\"", t2.lexeme);
}

test "lexer comments" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "a // comment\nb /* block */ c");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t1.type);
    try testing.expectEqualStrings("a", t1.lexeme);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t2.type);
    try testing.expectEqualStrings("b", t2.lexeme);

    const t3 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t3.type);
    try testing.expectEqualStrings("c", t3.lexeme);
}

test "lexer doc comments" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "/// doc comment\na");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.doc_comment, t1.type);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t2.type);
}

test "lexer hole" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var lexer = Lexer.init(allocator, "??? a");
    defer lexer.deinit();

    const t1 = try lexer.nextToken();
    try testing.expectEqual(TokenType.hole, t1.type);
    try testing.expectEqualStrings("???", t1.lexeme);

    const t2 = try lexer.nextToken();
    try testing.expectEqual(TokenType.identifier, t2.type);
}

test "lexer spec example" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const source =
        \\spec fn divide(a: i32, b: i32) -> i32
        \\  requires:
        \\    b != 0
        \\  ensures:
        \\    result * b + (a % b) == a
    ;

    var lexer = Lexer.init(allocator, source);
    defer lexer.deinit();

    var tokens = try lexer.tokenize();
    defer tokens.deinit(allocator);

    // Verify no errors
    try testing.expect(!lexer.hasErrors());

    // Verify key tokens are present
    try testing.expectEqual(TokenType.kw_spec, tokens.items[0].type);
    try testing.expectEqual(TokenType.kw_fn, tokens.items[1].type);
    try testing.expectEqual(TokenType.identifier, tokens.items[2].type);
    try testing.expectEqualStrings("divide", tokens.items[2].lexeme);
}
