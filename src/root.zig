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
pub const provenance = @import("provenance/root.zig");
pub const trust = @import("trust/root.zig");
pub const codegen = @import("codegen/root.zig");
pub const cli = @import("cli/root.zig");
pub const stdlib = @import("stdlib/root.zig");
pub const lsp = @import("lsp/root.zig");

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

// Re-export commonly used types from provenance system
pub const ProvenanceMetadata = provenance.ProvenanceMetadata;
pub const ProvenanceExtractor = provenance.ProvenanceExtractor;
pub const ProvenanceStore = provenance.ProvenanceStore;
pub const Author = provenance.Author;
pub const AuthorKind = provenance.AuthorKind;
pub const Confidence = provenance.Confidence;

// Re-export commonly used types from trust system
pub const TrustCalculator = trust.TrustCalculator;
pub const TrustConfig = trust.TrustConfig;
pub const TrustScore = trust.Score;
pub const TrustLevel = trust.Level;
pub const TrustThresholds = trust.TrustThresholds;
pub const TrustReportGenerator = trust.TrustReportGenerator;
pub const TrustReport = trust.Report;
pub const CriticalityLevel = trust.CriticalityLevel;

// Re-export commonly used types from code generation
pub const GenerationRequest = codegen.GenerationRequest;
pub const GenerationResponse = codegen.GenerationResponse;
pub const GeneratedCode = codegen.GeneratedCode;
pub const TargetLanguage = codegen.TargetLanguage;
pub const MockGenerator = codegen.MockGenerator;
pub const GenerationPipeline = codegen.GenerationPipeline;
pub const CheckpointManager = codegen.CheckpointManager;

// Re-export commonly used types from standard library
pub const StandardLibrary = stdlib.StandardLibrary;

// Re-export commonly used types from LSP
pub const LspServer = lsp.Server;
pub const LspProtocol = lsp.Protocol;

test {
    _ = lexer;
    _ = parser;
    _ = types;
    _ = specs;
    _ = verify;
    _ = provenance;
    _ = trust;
    _ = codegen;
    _ = cli;
    _ = stdlib;
    _ = lsp;
}
