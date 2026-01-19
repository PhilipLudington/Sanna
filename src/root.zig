//! Sanna - A specification language for AI-assisted software development
//!
//! Sanna combines formal specifications, mechanical verification, confidence
//! tracking, and provenance into a unified system.

const std = @import("std");

pub const lexer = @import("lexer/root.zig");

// Re-export commonly used types
pub const Lexer = lexer.Lexer;
pub const Token = lexer.Token;
pub const TokenType = lexer.TokenType;

test {
    _ = lexer;
}
