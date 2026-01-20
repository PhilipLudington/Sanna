//! Sanna - A specification language for AI-assisted software development
//!
//! Sanna combines formal specifications, mechanical verification, confidence
//! tracking, and provenance into a unified system.

const std = @import("std");

pub const lexer = @import("lexer/root.zig");
pub const parser = @import("parser/root.zig");
pub const types = @import("types/root.zig");
pub const specs = @import("specs/root.zig");
pub const verify = @import("verify/root.zig");

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

// Re-export commonly used types from type system
pub const Type = types.Type;
pub const TypeContext = types.TypeContext;
pub const TypeChecker = types.TypeChecker;
pub const TypeError = types.TypeError;

// Re-export commonly used types from specification semantics
pub const ModuleLoader = specs.ModuleLoader;
pub const ImportResolver = specs.ImportResolver;
pub const SemanticAnalyzer = specs.SemanticAnalyzer;
pub const SpecBinding = specs.SpecBinding;

// Re-export commonly used types from verification engine
pub const VerificationEngine = verify.VerificationEngine;
pub const VerificationReport = verify.VerificationReport;
pub const VerificationStatus = verify.VerificationStatus;
pub const Z3Solver = verify.Z3Solver;

test {
    _ = lexer;
    _ = parser;
    _ = types;
    _ = specs;
    _ = verify;
}
