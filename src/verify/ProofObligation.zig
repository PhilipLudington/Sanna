//! Proof Obligation Data Structures
//!
//! This module defines the structures for representing proof obligations
//! that need to be verified. A proof obligation is a logical statement
//! that must be proven valid for a specification to be correct.
//!
//! Obligations are generated from:
//! - Function specifications (requires/ensures)
//! - Type invariants
//! - Lemmas
//! - Axioms (assumed without proof)

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtExpr = SmtTypes.SmtExpr;
const SmtSort = SmtTypes.SmtSort;
const SmtDecl = SmtTypes.SmtDecl;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Proof Obligation
// ============================================================================

/// A proof obligation that needs to be verified
pub const ProofObligation = struct {
    /// Unique identifier for this obligation
    id: u64,
    /// Human-readable name for this obligation
    name: []const u8,
    /// The kind of proof obligation
    kind: ObligationKind,
    /// Source of this obligation
    source: ObligationSource,
    /// Source span for error reporting
    span: ?Span,
    /// Assumptions (conditions assumed to be true)
    assumptions: []const *const SmtExpr,
    /// The goal to prove (must be true under assumptions)
    goal: *const SmtExpr,
    /// Free variables (with their sorts) used in this obligation
    free_vars: []const FreeVar,
    /// Declarations needed for this obligation
    declarations: []const SmtDecl,
    /// Current verification status
    status: VerificationStatus,
    /// Optional proof hint
    proof_hint: ?[]const u8,
    /// Dependencies (other obligations that must be proven first)
    dependencies: []const u64,
    /// Content hash for caching
    content_hash: ?u64,

    pub fn init(
        id: u64,
        name: []const u8,
        kind: ObligationKind,
        source: ObligationSource,
        goal: *const SmtExpr,
    ) ProofObligation {
        return .{
            .id = id,
            .name = name,
            .kind = kind,
            .source = source,
            .span = null,
            .assumptions = &.{},
            .goal = goal,
            .free_vars = &.{},
            .declarations = &.{},
            .status = .pending,
            .proof_hint = null,
            .dependencies = &.{},
            .content_hash = null,
        };
    }

    /// Check if this obligation is ready to be verified
    /// (all dependencies are satisfied)
    pub fn isReady(self: *const ProofObligation, verified_ids: []const u64) bool {
        for (self.dependencies) |dep_id| {
            var found = false;
            for (verified_ids) |vid| {
                if (dep_id == vid) {
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }
        return true;
    }

    /// Check if this obligation is terminal (verified, failed, or skipped)
    pub fn isTerminal(self: *const ProofObligation) bool {
        return switch (self.status) {
            .verified, .failed, .trusted, .admitted, .skipped => true,
            .pending, .in_progress, .timeout, .unknown => false,
        };
    }
};

/// A free variable in a proof obligation
pub const FreeVar = struct {
    name: []const u8,
    sort: *const SmtSort,
    /// Whether this is a pre-state variable (e.g., x_pre from old(x))
    is_pre_state: bool = false,
    /// Whether this is the result variable
    is_result: bool = false,
};

// ============================================================================
// Obligation Kind
// ============================================================================

/// The kind of proof obligation
pub const ObligationKind = enum {
    /// Verify that preconditions can be satisfied
    /// (precondition is satisfiable, not always false)
    precondition_satisfiability,

    /// Verify postcondition holds given precondition
    /// requires => ensures
    postcondition_verification,

    /// Verify that invariant is preserved by an operation
    /// (invariant_pre && operation) => invariant_post
    invariant_preservation,

    /// Verify that invariant is established by initialization
    /// initialization => invariant
    invariant_establishment,

    /// Prove a lemma from axioms and other lemmas
    lemma_proof,

    /// Verify frame conditions (unmodified variables unchanged)
    frame_condition,

    /// Verify termination (decreases clause is well-founded)
    termination,

    /// Verify that modifies clauses are respected
    modifies_verification,

    /// Verify type safety (operations don't violate type constraints)
    type_safety,

    /// Custom obligation (user-defined)
    custom,

    pub fn description(self: ObligationKind) []const u8 {
        return switch (self) {
            .precondition_satisfiability => "precondition satisfiability",
            .postcondition_verification => "postcondition verification",
            .invariant_preservation => "invariant preservation",
            .invariant_establishment => "invariant establishment",
            .lemma_proof => "lemma proof",
            .frame_condition => "frame condition",
            .termination => "termination",
            .modifies_verification => "modifies verification",
            .type_safety => "type safety",
            .custom => "custom",
        };
    }
};

// ============================================================================
// Obligation Source
// ============================================================================

/// The source of a proof obligation
pub const ObligationSource = struct {
    kind: Kind,
    /// Fully qualified name of the source element
    name: []const u8,
    /// Optional additional context
    context: ?[]const u8 = null,

    pub const Kind = enum {
        /// From a function specification
        function_spec,
        /// From a type invariant
        type_invariant,
        /// From a lemma
        lemma,
        /// From an axiom (shouldn't generate obligations, but tracked)
        axiom,
        /// From an interface specification
        interface_spec,
        /// From a model definition
        model,
        /// Synthetic (generated by the verifier)
        synthetic,
    };

    pub fn functionSpec(name: []const u8) ObligationSource {
        return .{ .kind = .function_spec, .name = name };
    }

    pub fn typeInvariant(type_name: []const u8) ObligationSource {
        return .{ .kind = .type_invariant, .name = type_name };
    }

    pub fn lemmaSource(name: []const u8) ObligationSource {
        return .{ .kind = .lemma, .name = name };
    }

    pub fn axiomSource(name: []const u8) ObligationSource {
        return .{ .kind = .axiom, .name = name };
    }

    pub fn synthetic(description: []const u8) ObligationSource {
        return .{ .kind = .synthetic, .name = description };
    }
};

// ============================================================================
// Verification Status
// ============================================================================

/// The verification status of a proof obligation
pub const VerificationStatus = enum {
    /// Not yet verified
    pending,
    /// Verification in progress
    in_progress,
    /// Successfully verified (proved valid)
    verified,
    /// Verification failed (counterexample found)
    failed,
    /// Verification timed out
    timeout,
    /// Solver returned unknown
    unknown,
    /// Marked as trusted (not verified)
    trusted,
    /// Admitted (assumed without proof)
    admitted,
    /// Skipped (e.g., due to unsupported features)
    skipped,

    pub fn isSuccess(self: VerificationStatus) bool {
        return self == .verified or self == .trusted or self == .admitted;
    }

    pub fn isFailure(self: VerificationStatus) bool {
        return self == .failed;
    }

    pub fn isTerminal(self: VerificationStatus) bool {
        return switch (self) {
            .pending, .in_progress => false,
            else => true,
        };
    }

    pub fn description(self: VerificationStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .in_progress => "in progress",
            .verified => "verified",
            .failed => "failed",
            .timeout => "timeout",
            .unknown => "unknown",
            .trusted => "trusted",
            .admitted => "admitted",
            .skipped => "skipped",
        };
    }
};

// ============================================================================
// Obligation Set
// ============================================================================

/// A collection of proof obligations for a verification task
pub const ObligationSet = struct {
    allocator: Allocator,
    /// All obligations indexed by ID
    obligations: std.AutoHashMapUnmanaged(u64, *ProofObligation),
    /// Next ID to assign
    next_id: u64,
    /// Obligations in topological order (respecting dependencies)
    ordered: std.ArrayListUnmanaged(*ProofObligation),
    /// IDs of verified obligations
    verified_ids: std.ArrayListUnmanaged(u64),

    pub fn init(allocator: Allocator) ObligationSet {
        return .{
            .allocator = allocator,
            .obligations = .{},
            .next_id = 1,
            .ordered = .{},
            .verified_ids = .{},
        };
    }

    pub fn deinit(self: *ObligationSet) void {
        var iter = self.obligations.iterator();
        while (iter.next()) |entry| {
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.obligations.deinit(self.allocator);
        self.ordered.deinit(self.allocator);
        self.verified_ids.deinit(self.allocator);
    }

    /// Add a new obligation and return its ID
    pub fn add(self: *ObligationSet, obligation: ProofObligation) !u64 {
        const ob = try self.allocator.create(ProofObligation);
        ob.* = obligation;
        ob.id = self.next_id;

        try self.obligations.put(self.allocator, self.next_id, ob);
        try self.ordered.append(self.allocator, ob);

        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    /// Get an obligation by ID
    pub fn get(self: *const ObligationSet, id: u64) ?*ProofObligation {
        return self.obligations.get(id);
    }

    /// Mark an obligation as verified
    pub fn markVerified(self: *ObligationSet, id: u64) !void {
        if (self.obligations.getPtr(id)) |ob| {
            ob.*.status = .verified;
            try self.verified_ids.append(self.allocator, id);
        }
    }

    /// Get all pending obligations that are ready to verify
    pub fn getReady(self: *const ObligationSet) []const *ProofObligation {
        var ready = std.ArrayListUnmanaged(*ProofObligation){};
        for (self.ordered.items) |ob| {
            if (ob.status == .pending and ob.isReady(self.verified_ids.items)) {
                ready.append(self.allocator, ob) catch continue;
            }
        }
        return ready.items;
    }

    /// Get statistics about the obligation set
    pub fn getStats(self: *const ObligationSet) ObligationStats {
        var stats = ObligationStats{};
        for (self.ordered.items) |ob| {
            stats.total += 1;
            switch (ob.status) {
                .pending => stats.pending += 1,
                .in_progress => stats.in_progress += 1,
                .verified => stats.verified += 1,
                .failed => stats.failed += 1,
                .timeout => stats.timeout += 1,
                .unknown => stats.unknown += 1,
                .trusted => stats.trusted += 1,
                .admitted => stats.admitted += 1,
                .skipped => stats.skipped += 1,
            }
        }
        return stats;
    }

    /// Check if all obligations are resolved
    pub fn isComplete(self: *const ObligationSet) bool {
        for (self.ordered.items) |ob| {
            if (!ob.status.isTerminal()) return false;
        }
        return true;
    }

    /// Check if all obligations are successfully verified
    pub fn allVerified(self: *const ObligationSet) bool {
        for (self.ordered.items) |ob| {
            if (!ob.status.isSuccess()) return false;
        }
        return true;
    }
};

/// Statistics about an obligation set
pub const ObligationStats = struct {
    total: usize = 0,
    pending: usize = 0,
    in_progress: usize = 0,
    verified: usize = 0,
    failed: usize = 0,
    timeout: usize = 0,
    unknown: usize = 0,
    trusted: usize = 0,
    admitted: usize = 0,
    skipped: usize = 0,

    pub fn successRate(self: ObligationStats) f64 {
        if (self.total == 0) return 1.0;
        const successful: f64 = @floatFromInt(self.verified + self.trusted + self.admitted);
        const total: f64 = @floatFromInt(self.total);
        return successful / total;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ProofObligation creation" {
    const testing = std.testing;
    const SmtExprs = SmtTypes.SmtExpr;

    const goal = SmtExprs.boolLit(true);
    const ob = ProofObligation.init(
        1,
        "test_obligation",
        .postcondition_verification,
        ObligationSource.functionSpec("test.function"),
        &goal,
    );

    try testing.expectEqual(@as(u64, 1), ob.id);
    try testing.expectEqualStrings("test_obligation", ob.name);
    try testing.expectEqual(ObligationKind.postcondition_verification, ob.kind);
    try testing.expectEqual(VerificationStatus.pending, ob.status);
}

test "VerificationStatus properties" {
    const testing = std.testing;

    try testing.expect(VerificationStatus.verified.isSuccess());
    try testing.expect(VerificationStatus.trusted.isSuccess());
    try testing.expect(VerificationStatus.admitted.isSuccess());
    try testing.expect(!VerificationStatus.failed.isSuccess());
    try testing.expect(!VerificationStatus.pending.isSuccess());

    try testing.expect(VerificationStatus.failed.isFailure());
    try testing.expect(!VerificationStatus.verified.isFailure());

    try testing.expect(VerificationStatus.verified.isTerminal());
    try testing.expect(VerificationStatus.failed.isTerminal());
    try testing.expect(!VerificationStatus.pending.isTerminal());
    try testing.expect(!VerificationStatus.in_progress.isTerminal());
}

test "ObligationSet basic operations" {
    const testing = std.testing;
    const SmtExprs = SmtTypes.SmtExpr;

    var set = ObligationSet.init(testing.allocator);
    defer set.deinit();

    const goal = SmtExprs.boolLit(true);
    const ob = ProofObligation.init(
        0, // Will be assigned by set
        "test",
        .postcondition_verification,
        ObligationSource.functionSpec("test"),
        &goal,
    );

    const id = try set.add(ob);
    try testing.expectEqual(@as(u64, 1), id);

    const retrieved = set.get(id);
    try testing.expect(retrieved != null);
    try testing.expectEqualStrings("test", retrieved.?.name);

    const stats = set.getStats();
    try testing.expectEqual(@as(usize, 1), stats.total);
    try testing.expectEqual(@as(usize, 1), stats.pending);
}

test "ObligationKind descriptions" {
    const testing = std.testing;

    try testing.expectEqualStrings(
        "postcondition verification",
        ObligationKind.postcondition_verification.description(),
    );
    try testing.expectEqualStrings(
        "invariant preservation",
        ObligationKind.invariant_preservation.description(),
    );
}
