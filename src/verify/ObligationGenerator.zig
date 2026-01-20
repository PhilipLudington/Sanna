//! Obligation Generator
//!
//! This module generates proof obligations from Sanna specifications.
//! It transforms function specs, type invariants, and lemmas into
//! SMT formulas that can be verified by Z3.
//!
//! ## Obligation Types Generated
//!
//! From function specifications:
//! - Postcondition verification: `requires => ensures`
//! - Frame conditions: unmodified variables unchanged
//!
//! From type invariants:
//! - Invariant establishment: constructor ensures invariant
//! - Invariant preservation: methods maintain invariant
//!
//! From lemmas:
//! - Lemma proof: prove condition from axioms

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtExpr = SmtTypes.SmtExpr;
const SmtSort = SmtTypes.SmtSort;
const SmtDecl = SmtTypes.SmtDecl;
const SmtEncoder = @import("SmtEncoder.zig").SmtEncoder;
const ProofObligation = @import("ProofObligation.zig");
const Obligation = ProofObligation.ProofObligation;
const ObligationKind = ProofObligation.ObligationKind;
const ObligationSource = ProofObligation.ObligationSource;
const ObligationSet = ProofObligation.ObligationSet;
const FreeVar = ProofObligation.FreeVar;
const specs = @import("../specs/root.zig");
const SpecBinding = specs.SpecBinding;
const BoundSpec = specs.BoundSpec;
const BoundInvariant = specs.BoundInvariant;
const BoundAxiom = specs.BoundAxiom;
const BoundLemma = specs.BoundLemma;
const Ast = @import("../parser/root.zig").Ast;
const Type = @import("../types/root.zig").Type;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Obligation Generator
// ============================================================================

/// Generates proof obligations from specifications
pub const ObligationGenerator = struct {
    allocator: Allocator,
    /// SMT encoder for expression translation
    encoder: SmtEncoder,
    /// Generated obligations
    obligations: ObligationSet,
    /// Axioms available for proofs
    axioms: std.ArrayListUnmanaged(*const SmtExpr),
    /// Errors during generation
    errors: std.ArrayListUnmanaged(GeneratorError),

    pub fn init(allocator: Allocator) ObligationGenerator {
        return .{
            .allocator = allocator,
            .encoder = SmtEncoder.init(allocator),
            .obligations = ObligationSet.init(allocator),
            .axioms = .{},
            .errors = .{},
        };
    }

    pub fn deinit(self: *ObligationGenerator) void {
        self.encoder.deinit();
        self.obligations.deinit();
        self.axioms.deinit(self.allocator);
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    /// Get the generated obligations
    pub fn getObligations(self: *const ObligationGenerator) *const ObligationSet {
        return &self.obligations;
    }

    // ========================================================================
    // Generation from SpecBinding
    // ========================================================================

    /// Generate all obligations from a SpecBinding
    pub fn generateFromBinding(self: *ObligationGenerator, binding: *const SpecBinding) !void {
        // Generate from function specs
        var spec_iter = binding.function_specs.iterator();
        while (spec_iter.next()) |entry| {
            try self.generateFromFunctionSpec(entry.key_ptr.*, entry.value_ptr.*);
        }

        // Generate from type invariants
        var inv_iter = binding.type_invariants.iterator();
        while (inv_iter.next()) |entry| {
            const type_name = entry.key_ptr.*;
            const invariants = entry.value_ptr.items;
            for (invariants) |inv| {
                try self.generateFromInvariant(type_name, inv);
            }
        }

        // Generate from lemmas
        var lemma_iter = binding.lemmas.iterator();
        while (lemma_iter.next()) |entry| {
            try self.generateFromLemma(entry.key_ptr.*, entry.value_ptr.*);
        }

        // Register axioms (don't generate obligations, just add to assumption set)
        var axiom_iter = binding.axioms.iterator();
        while (axiom_iter.next()) |entry| {
            try self.registerAxiom(entry.value_ptr.*);
        }
    }

    /// Generate obligations from a function specification
    pub fn generateFromFunctionSpec(self: *ObligationGenerator, name: []const u8, spec: *const BoundSpec) !void {
        const arena = self.encoder.arenaAllocator();

        // Check for @trusted attribute
        if (self.hasAttribute(spec.ast, "trusted")) {
            // Don't generate obligations for trusted specs
            return;
        }

        // Generate variable declarations for parameters
        var free_vars = std.ArrayListUnmanaged(FreeVar){};
        defer free_vars.deinit(self.allocator);

        for (spec.ast.params) |param| {
            const sort = try self.encoder.encodeTypeExpr(&param.type_expr);
            try free_vars.append(self.allocator, .{
                .name = param.name.name,
                .sort = sort,
                .is_pre_state = false,
                .is_result = false,
            });
        }

        // Generate pre-state variables for old() expressions
        for (spec.ast.params) |param| {
            const sort = try self.encoder.encodeTypeExpr(&param.type_expr);
            const pre_name = try std.fmt.allocPrint(arena, "{s}_pre", .{param.name.name});
            try free_vars.append(self.allocator, .{
                .name = pre_name,
                .sort = sort,
                .is_pre_state = true,
                .is_result = false,
            });
        }

        // Add result variable if non-void return
        if (!isVoidType(&spec.ast.return_type)) {
            const result_sort = try self.encoder.encodeTypeExpr(&spec.ast.return_type);
            try free_vars.append(self.allocator, .{
                .name = "_result_",
                .sort = result_sort,
                .is_pre_state = false,
                .is_result = true,
            });
        }

        // Encode preconditions
        var preconditions = std.ArrayListUnmanaged(*const SmtExpr){};
        defer preconditions.deinit(self.allocator);

        for (spec.requires) |req| {
            const encoded = try self.encoder.encodeExpression(req);
            try preconditions.append(self.allocator, encoded);
        }

        // Encode postconditions
        self.encoder.setPostconditionContext(true);
        defer self.encoder.setPostconditionContext(false);

        var postconditions = std.ArrayListUnmanaged(*const SmtExpr){};
        defer postconditions.deinit(self.allocator);

        for (spec.ensures) |ens| {
            const encoded = try self.encoder.encodeExpression(ens);
            try postconditions.append(self.allocator, encoded);
        }

        // Generate postcondition verification obligation
        // Goal: requires => ensures
        if (postconditions.items.len > 0) {
            const combined_post = try self.combineConjunction(postconditions.items);
            const goal = if (preconditions.items.len > 0) blk: {
                const combined_pre = try self.combineConjunction(preconditions.items);
                const impl = try arena.create(SmtExpr);
                impl.* = SmtExpr.impliesExpr(combined_pre, combined_post);
                break :blk impl;
            } else combined_post;

            const ob_name = try std.fmt.allocPrint(arena, "{s}:postcondition", .{name});
            var ob = Obligation.init(
                0,
                ob_name,
                .postcondition_verification,
                ObligationSource.functionSpec(name),
                goal,
            );
            ob.span = spec.ast.span;
            ob.assumptions = try self.allocator.dupe(*const SmtExpr, self.axioms.items);
            ob.free_vars = try self.allocator.dupe(FreeVar, free_vars.items);

            _ = try self.obligations.add(ob);
        }

        // Generate frame condition obligations if modifies clauses exist
        if (spec.modifies.len > 0) {
            try self.generateFrameConditions(name, spec, &free_vars);
        }
    }

    /// Generate frame condition obligations
    fn generateFrameConditions(
        self: *ObligationGenerator,
        name: []const u8,
        spec: *const BoundSpec,
        free_vars: *std.ArrayListUnmanaged(FreeVar),
    ) !void {
        const arena = self.encoder.arenaAllocator();

        // For each variable NOT in the modifies clause, generate:
        // old(var) == var
        for (free_vars.items) |fv| {
            if (fv.is_pre_state or fv.is_result) continue;

            // Check if this variable is in modifies clause
            var is_modified = false;
            for (spec.modifies) |mod| {
                if (mod.kind == .identifier) {
                    if (std.mem.eql(u8, mod.kind.identifier.name, fv.name)) {
                        is_modified = true;
                        break;
                    }
                }
            }

            if (!is_modified) {
                // Generate frame condition: x_pre == x
                const pre_var = try arena.create(SmtExpr);
                const pre_name = try std.fmt.allocPrint(arena, "{s}_pre", .{fv.name});
                pre_var.* = SmtExpr.constant(pre_name);

                const post_var = try arena.create(SmtExpr);
                post_var.* = SmtExpr.constant(fv.name);

                const eq = try arena.create(SmtExpr);
                eq.* = SmtExpr.eqExpr(pre_var, post_var);

                // Goal: implies preconditions, frame condition holds
                var preconditions = std.ArrayListUnmanaged(*const SmtExpr){};
                defer preconditions.deinit(self.allocator);

                for (spec.requires) |req| {
                    const encoded = try self.encoder.encodeExpression(req);
                    try preconditions.append(self.allocator, encoded);
                }

                const goal = if (preconditions.items.len > 0) blk: {
                    const combined_pre = try self.combineConjunction(preconditions.items);
                    const impl = try arena.create(SmtExpr);
                    impl.* = SmtExpr.impliesExpr(combined_pre, eq);
                    break :blk impl;
                } else eq;

                const ob_name = try std.fmt.allocPrint(arena, "{s}:frame:{s}", .{ name, fv.name });
                var ob = Obligation.init(
                    0,
                    ob_name,
                    .frame_condition,
                    ObligationSource.functionSpec(name),
                    goal,
                );
                ob.span = spec.ast.span;
                ob.free_vars = try self.allocator.dupe(FreeVar, free_vars.items);

                _ = try self.obligations.add(ob);
            }
        }
    }

    /// Generate obligations from a type invariant
    fn generateFromInvariant(self: *ObligationGenerator, type_name: []const u8, inv: *const BoundInvariant) !void {
        const arena = self.encoder.arenaAllocator();

        // Encode the invariant condition
        const inv_expr = try self.encoder.encodeExpression(inv.condition);

        // Generate invariant establishment obligation
        // (for now, just check that invariant is satisfiable)
        const ob_name = try std.fmt.allocPrint(arena, "{s}:invariant:{s}", .{
            type_name,
            inv.name orelse "unnamed",
        });

        var ob = Obligation.init(
            0,
            ob_name,
            .invariant_preservation,
            ObligationSource.typeInvariant(type_name),
            inv_expr,
        );
        ob.span = inv.span;

        _ = try self.obligations.add(ob);
    }

    /// Generate obligations from a lemma
    fn generateFromLemma(self: *ObligationGenerator, name: []const u8, lemma: *const BoundLemma) !void {
        const arena = self.encoder.arenaAllocator();

        // Check for @admitted attribute
        // For now, just generate the proof obligation

        // Encode the lemma condition
        const lemma_expr = try self.encoder.encodeExpression(lemma.condition);

        // Goal: axioms => lemma condition
        const goal = if (self.axioms.items.len > 0) blk: {
            const combined_axioms = try self.combineConjunction(self.axioms.items);
            const impl = try arena.create(SmtExpr);
            impl.* = SmtExpr.impliesExpr(combined_axioms, lemma_expr);
            break :blk impl;
        } else lemma_expr;

        const ob_name = try std.fmt.allocPrint(arena, "{s}:proof", .{name});
        var ob = Obligation.init(
            0,
            ob_name,
            .lemma_proof,
            ObligationSource.lemmaSource(name),
            goal,
        );
        ob.span = lemma.span;
        ob.assumptions = try self.allocator.dupe(*const SmtExpr, self.axioms.items);

        _ = try self.obligations.add(ob);
    }

    /// Register an axiom (doesn't generate obligation, just adds to assumption set)
    fn registerAxiom(self: *ObligationGenerator, axiom: *const BoundAxiom) !void {
        const axiom_expr = try self.encoder.encodeExpression(axiom.condition);
        try self.axioms.append(self.allocator, axiom_expr);
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    /// Combine expressions with conjunction (AND)
    fn combineConjunction(self: *ObligationGenerator, exprs: []const *const SmtExpr) !*const SmtExpr {
        const arena = self.encoder.arenaAllocator();

        if (exprs.len == 0) {
            const true_expr = try arena.create(SmtExpr);
            true_expr.* = SmtExpr.boolLit(true);
            return true_expr;
        }

        if (exprs.len == 1) {
            return exprs[0];
        }

        const result = try arena.create(SmtExpr);
        result.* = SmtExpr.andExpr(exprs);
        return result;
    }

    /// Combine expressions with disjunction (OR)
    fn combineDisjunction(self: *ObligationGenerator, exprs: []const *const SmtExpr) !*const SmtExpr {
        const arena = self.encoder.arenaAllocator();

        if (exprs.len == 0) {
            const false_expr = try arena.create(SmtExpr);
            false_expr.* = SmtExpr.boolLit(false);
            return false_expr;
        }

        if (exprs.len == 1) {
            return exprs[0];
        }

        const result = try arena.create(SmtExpr);
        result.* = SmtExpr.orExpr(exprs);
        return result;
    }

    /// Check if a function spec has a specific attribute
    fn hasAttribute(self: *const ObligationGenerator, spec: *const Ast.FunctionSpec, attr_name: []const u8) bool {
        _ = self;
        _ = spec;
        _ = attr_name;
        // Would need to access attributes through the Declaration wrapper
        // For now, return false
        return false;
    }

    /// Add a generator error
    fn addError(self: *ObligationGenerator, message: []const u8, span: ?Span) !void {
        try self.errors.append(self.allocator, .{
            .message = try self.allocator.dupe(u8, message),
            .span = span,
        });
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const ObligationGenerator) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const ObligationGenerator) []const GeneratorError {
        return self.errors.items;
    }
};

/// Check if a type expression represents void/unit
fn isVoidType(te: *const Ast.TypeExpr) bool {
    switch (te.kind) {
        .named => |qn| {
            if (qn.parts.len == 1) {
                const name = qn.parts[0].name;
                return std.mem.eql(u8, name, "unit") or
                    std.mem.eql(u8, name, "void") or
                    std.mem.eql(u8, name, "Unit") or
                    std.mem.eql(u8, name, "Void");
            }
        },
        else => {},
    }
    return false;
}

/// Error during obligation generation
pub const GeneratorError = struct {
    message: []const u8,
    span: ?Span,
};

// ============================================================================
// Tests
// ============================================================================

test "ObligationGenerator initialization" {
    const testing = std.testing;

    var gen = ObligationGenerator.init(testing.allocator);
    defer gen.deinit();

    try testing.expect(!gen.hasErrors());
    const obs = gen.getObligations();
    try testing.expectEqual(@as(usize, 0), obs.ordered.items.len);
}

test "isVoidType detection" {
    const testing = std.testing;
    const parser = @import("../parser/root.zig");
    const Span_t = @import("../lexer/root.zig").Span;
    const Location = @import("../lexer/root.zig").Location;

    const dummy_span = Span_t.init(
        Location.init(1, 1, 0),
        Location.init(1, 5, 4),
    );

    // Test unit type - use array on stack that outlives the QualifiedName
    const unit_ident = parser.Ast.Identifier.init("unit", dummy_span);
    const unit_parts = [_]parser.Ast.Identifier{unit_ident};
    const unit_qn = parser.Ast.QualifiedName.init(&unit_parts, dummy_span);
    const unit_type = parser.Ast.TypeExpr.init(.{ .named = unit_qn }, dummy_span);
    try testing.expect(isVoidType(&unit_type));

    // Test void type
    const void_ident = parser.Ast.Identifier.init("void", dummy_span);
    const void_parts = [_]parser.Ast.Identifier{void_ident};
    const void_qn = parser.Ast.QualifiedName.init(&void_parts, dummy_span);
    const void_type = parser.Ast.TypeExpr.init(.{ .named = void_qn }, dummy_span);
    try testing.expect(isVoidType(&void_type));

    // Test non-void type
    const int_ident = parser.Ast.Identifier.init("i32", dummy_span);
    const int_parts = [_]parser.Ast.Identifier{int_ident};
    const int_qn = parser.Ast.QualifiedName.init(&int_parts, dummy_span);
    const int_type = parser.Ast.TypeExpr.init(.{ .named = int_qn }, dummy_span);
    try testing.expect(!isVoidType(&int_type));
}
