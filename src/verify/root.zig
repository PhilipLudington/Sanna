//! Sanna Verification Engine Module
//!
//! This module provides the verification engine for Sanna specifications.
//! It transforms specifications into proof obligations and verifies them
//! using Z3 (SMT solver).
//!
//! ## Overview
//!
//! The verification process consists of:
//! 1. **Obligation Generation**: Transform specs into proof obligations
//! 2. **SMT Encoding**: Convert Sanna types/expressions to SMT-LIB
//! 3. **Solver Invocation**: Run Z3 on the encoded obligations
//! 4. **Result Analysis**: Interpret solver output and report results
//!
//! ## Usage
//!
//! ```zig
//! const verify = @import("verify/root.zig");
//!
//! // Create verification engine
//! var engine = verify.VerificationEngine.init(allocator, .{
//!     .timeout_ms = 30000,
//!     .use_cache = true,
//! });
//! defer engine.deinit();
//!
//! // Verify specifications
//! const report = try engine.verifyBinding(spec_binding);
//!
//! // Check results
//! if (report.allPassed()) {
//!     std.debug.print("All specifications verified!\n", .{});
//! } else {
//!     // Handle failures
//!     for (report.results.items) |result| {
//!         if (result.isFailed()) {
//!             std.debug.print("Failed: {s}\n", .{result.spec_name});
//!         }
//!     }
//! }
//! ```
//!
//! ## Components
//!
//! - **SmtTypes**: SMT-LIB AST representation (sorts, expressions, commands)
//! - **SmtEncoder**: Encodes Sanna types and expressions to SMT-LIB
//! - **SmtLibWriter**: Generates SMT-LIB text output
//! - **Z3Solver**: Z3 subprocess integration
//! - **ProofObligation**: Proof obligation data structures
//! - **ObligationGenerator**: Generates obligations from specifications
//! - **VerificationCache**: Caches results for incremental verification
//! - **VerificationEngine**: Main orchestrator
//! - **VerificationResult**: Result types and reporting

const std = @import("std");

// ============================================================================
// Core Types
// ============================================================================

/// SMT-LIB type system
pub const SmtTypes = @import("SmtTypes.zig");
pub const SmtSort = SmtTypes.SmtSort;
pub const SmtExpr = SmtTypes.SmtExpr;
pub const SmtDecl = SmtTypes.SmtDecl;
pub const SmtCommand = SmtTypes.SmtCommand;
pub const SmtScript = SmtTypes.SmtScript;

/// SMT encoder for Sanna to SMT-LIB translation
pub const SmtEncoder = @import("SmtEncoder.zig").SmtEncoder;

/// SMT-LIB writer for text output
pub const SmtLibWriter = @import("SmtLibWriter.zig").SmtLibWriter;

// ============================================================================
// Solver Integration
// ============================================================================

/// Z3 solver integration
pub const Z3Solver = @import("Z3Solver.zig").Z3Solver;
pub const SolverResult = @import("Z3Solver.zig").SolverResult;
pub const SatStatus = @import("Z3Solver.zig").SatStatus;
pub const Model = @import("Z3Solver.zig").Model;

/// Check if Z3 is available on the system
pub const isZ3Available = @import("Z3Solver.zig").isZ3Available;

// ============================================================================
// Proof Obligations
// ============================================================================

/// Proof obligation types
pub const ProofObligation = @import("ProofObligation.zig").ProofObligation;
pub const ObligationKind = @import("ProofObligation.zig").ObligationKind;
pub const ObligationSource = @import("ProofObligation.zig").ObligationSource;
pub const ObligationSet = @import("ProofObligation.zig").ObligationSet;
pub const VerificationStatus = @import("ProofObligation.zig").VerificationStatus;

/// Obligation generator
pub const ObligationGenerator = @import("ObligationGenerator.zig").ObligationGenerator;

// ============================================================================
// Caching
// ============================================================================

/// Verification cache for incremental verification
pub const VerificationCache = @import("VerificationCache.zig").VerificationCache;
pub const CacheEntry = @import("VerificationCache.zig").CacheEntry;
pub const CacheStats = @import("VerificationCache.zig").CacheStats;

// ============================================================================
// Results and Reporting
// ============================================================================

/// Verification result types
pub const VerificationResult = @import("VerificationResult.zig");
pub const VerificationReport = VerificationResult.VerificationReport;
pub const ObligationResult = VerificationResult.ObligationResult;
pub const Counterexample = VerificationResult.Counterexample;
pub const Diagnostic = VerificationResult.Diagnostic;

/// Format a verification report for display
pub const formatReport = VerificationResult.formatReport;

// ============================================================================
// Main Engine
// ============================================================================

/// Main verification engine
pub const VerificationEngine = @import("VerificationEngine.zig").VerificationEngine;
pub const EngineStats = @import("VerificationEngine.zig").EngineStats;

/// Quick verification helper
pub const quickVerify = @import("VerificationEngine.zig").quickVerify;

// ============================================================================
// Convenience Functions
// ============================================================================

/// Create a default verification engine
pub fn createEngine(allocator: std.mem.Allocator) VerificationEngine {
    return VerificationEngine.init(allocator, .{});
}

/// Create a verification engine with custom timeout
pub fn createEngineWithTimeout(allocator: std.mem.Allocator, timeout_ms: u32) VerificationEngine {
    return VerificationEngine.init(allocator, .{ .timeout_ms = timeout_ms });
}

// ============================================================================
// Tests
// ============================================================================

test {
    // Run all module tests
    std.testing.refAllDecls(@This());

    // Import sub-modules to run their tests
    _ = SmtTypes;
    _ = @import("SmtEncoder.zig");
    _ = @import("SmtLibWriter.zig");
    _ = @import("Z3Solver.zig");
    _ = @import("ProofObligation.zig");
    _ = @import("ObligationGenerator.zig");
    _ = @import("VerificationCache.zig");
    _ = @import("VerificationResult.zig");
    _ = @import("VerificationEngine.zig");
}
