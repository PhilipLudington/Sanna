//! Sanna - A specification language for AI-assisted software development
//!
//! Sanna combines formal specifications, mechanical verification, confidence
//! tracking, and provenance into a unified system.

const std = @import("std");

pub const lexer = @import("lexer/root.zig");
pub const parser = @import("parser/root.zig");

// Re-export commonly used types from lexer
pub const Lexer = lexer.Lexer;
pub const Token = lexer.Token;
pub const TokenType = lexer.TokenType;
pub const Span = lexer.Span;
pub const Location = lexer.Location;

// Re-export commonly used types from parser
pub const Ast = parser.Ast;
pub const Module = parser.Module;
pub const Declaration = parser.Declaration;
pub const TypeExpr = parser.TypeExpr;
pub const Expression = parser.Expression;

test {
    _ = lexer;
    _ = parser;
}
