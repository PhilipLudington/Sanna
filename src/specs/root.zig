//! Sanna Specification Semantics Module
//!
//! This module provides semantic analysis for Sanna specifications, including:
//! - Module resolution and import handling
//! - Specification binding (connecting specs to implementations)
//! - Precondition and postcondition analysis
//! - Frame condition analysis (modifies clauses)
//! - Pure function detection and enforcement
//! - Axiom and lemma management
//!
//! ## Usage
//!
//! ```zig
//! const specs = @import("specs/root.zig");
//!
//! // Create a module loader
//! var loader = specs.ModuleLoader.init(allocator);
//! defer loader.deinit();
//! try loader.addSearchPath("./specs");
//!
//! // Create a semantic analyzer
//! var analyzer = specs.SemanticAnalyzer.init(allocator, &loader);
//! defer analyzer.deinit();
//!
//! // Analyze a module
//! const result = try analyzer.analyzeModule(module);
//! ```

const std = @import("std");

// Module loading and resolution
pub const ModuleLoader = @import("ModuleLoader.zig").ModuleLoader;

// Import resolution
pub const ImportResolver = @import("ImportResolver.zig").ImportResolver;

// Semantic analyzer (specification semantics)
pub const SemanticAnalyzer = @import("SemanticAnalyzer.zig").SemanticAnalyzer;
pub const AnalysisResult = @import("SemanticAnalyzer.zig").AnalysisResult;
pub const SemanticError = @import("SemanticAnalyzer.zig").SemanticError;

// Specification binding
pub const SpecBinding = @import("SpecBinding.zig").SpecBinding;
pub const BoundSpec = @import("SpecBinding.zig").BoundSpec;

// Tests
test {
    std.testing.refAllDecls(@This());
}
