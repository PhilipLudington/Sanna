const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Type = @import("../types/root.zig").Type;
const TypeContext = @import("../types/root.zig").TypeContext;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Specification Binding
// ============================================================================

/// SpecBinding manages the connection between specifications and their
/// implementations. It tracks:
/// - Function specifications and their bound implementations
/// - Interface specifications and implementing types
/// - Type invariants and their enforcement points
/// - Axioms and lemmas and their verification status
pub const SpecBinding = struct {
    allocator: Allocator,

    /// Function specifications by fully qualified name
    function_specs: std.StringHashMapUnmanaged(*const BoundSpec),

    /// Interface implementations (interface name -> list of implementing types)
    interface_impls: std.StringHashMapUnmanaged(std.ArrayListUnmanaged([]const u8)),

    /// Type invariants by type name
    type_invariants: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(*const BoundInvariant)),

    /// Axioms by name
    axioms: std.StringHashMapUnmanaged(*const BoundAxiom),

    /// Lemmas by name
    lemmas: std.StringHashMapUnmanaged(*const BoundLemma),

    /// Errors during binding
    errors: std.ArrayListUnmanaged(BindingError),

    pub fn init(allocator: Allocator) SpecBinding {
        return .{
            .allocator = allocator,
            .function_specs = .{},
            .interface_impls = .{},
            .type_invariants = .{},
            .axioms = .{},
            .lemmas = .{},
            .errors = .{},
        };
    }

    pub fn deinit(self: *SpecBinding) void {
        // Free function specs
        var spec_iter = self.function_specs.iterator();
        while (spec_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.function_specs.deinit(self.allocator);

        // Free interface impls
        var impl_iter = self.interface_impls.iterator();
        while (impl_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            for (entry.value_ptr.items) |item| {
                self.allocator.free(item);
            }
            entry.value_ptr.deinit(self.allocator);
        }
        self.interface_impls.deinit(self.allocator);

        // Free type invariants
        var inv_iter = self.type_invariants.iterator();
        while (inv_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            for (entry.value_ptr.items) |inv| {
                self.allocator.destroy(inv);
            }
            entry.value_ptr.deinit(self.allocator);
        }
        self.type_invariants.deinit(self.allocator);

        // Free axioms
        var axiom_iter = self.axioms.iterator();
        while (axiom_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.axioms.deinit(self.allocator);

        // Free lemmas
        var lemma_iter = self.lemmas.iterator();
        while (lemma_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.lemmas.deinit(self.allocator);

        // Free errors
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    // ========================================================================
    // Function Specification Binding
    // ========================================================================

    /// Register a function specification
    pub fn registerFunctionSpec(self: *SpecBinding, name: []const u8, spec: *const Ast.FunctionSpec) !void {
        const bound = try self.allocator.create(BoundSpec);
        errdefer self.allocator.destroy(bound);

        bound.* = .{
            .name = try self.allocator.dupe(u8, name),
            .ast = spec,
            .requires = try self.extractRequires(spec),
            .ensures = try self.extractEnsures(spec),
            .modifies = try self.extractModifies(spec),
            .is_pure = spec.pure,
            .implementation = null,
            .verification_status = .pending,
        };

        const key = try self.allocator.dupe(u8, name);
        try self.function_specs.put(self.allocator, key, bound);
    }

    /// Look up a function specification
    pub fn lookupFunctionSpec(self: *const SpecBinding, name: []const u8) ?*const BoundSpec {
        return self.function_specs.get(name);
    }

    /// Bind an implementation to a specification
    pub fn bindImplementation(self: *SpecBinding, spec_name: []const u8, impl_ref: ImplementationRef) !void {
        if (self.function_specs.getPtr(spec_name)) |spec| {
            const mutable_spec = @constCast(spec.*);
            mutable_spec.implementation = impl_ref;
        } else {
            try self.addError(.{
                .kind = .spec_not_found,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "no specification found for '{s}'",
                    .{spec_name},
                ),
            });
        }
    }

    fn extractRequires(self: *SpecBinding, spec: *const Ast.FunctionSpec) ![]const *const Ast.Expression {
        if (spec.requires.len == 0) return &.{};
        const result = try self.allocator.alloc(*const Ast.Expression, spec.requires.len);
        for (spec.requires, 0..) |req, i| {
            result[i] = req;
        }
        return result;
    }

    fn extractEnsures(self: *SpecBinding, spec: *const Ast.FunctionSpec) ![]const *const Ast.Expression {
        if (spec.ensures.len == 0) return &.{};
        const result = try self.allocator.alloc(*const Ast.Expression, spec.ensures.len);
        for (spec.ensures, 0..) |ens, i| {
            result[i] = ens;
        }
        return result;
    }

    fn extractModifies(self: *SpecBinding, spec: *const Ast.FunctionSpec) ![]const *const Ast.Expression {
        if (spec.modifies.len == 0) return &.{};
        const result = try self.allocator.alloc(*const Ast.Expression, spec.modifies.len);
        for (spec.modifies, 0..) |mod, i| {
            result[i] = mod;
        }
        return result;
    }

    // ========================================================================
    // Interface Implementation Binding
    // ========================================================================

    /// Register an interface implementation
    pub fn registerInterfaceImpl(self: *SpecBinding, interface_name: []const u8, implementing_type: []const u8) !void {
        const gop = try self.interface_impls.getOrPut(self.allocator, interface_name);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.allocator.dupe(u8, interface_name);
            gop.value_ptr.* = .{};
        }
        const type_copy = try self.allocator.dupe(u8, implementing_type);
        try gop.value_ptr.append(self.allocator, type_copy);
    }

    /// Get all types implementing an interface
    pub fn getImplementors(self: *const SpecBinding, interface_name: []const u8) ?[]const []const u8 {
        if (self.interface_impls.get(interface_name)) |impls| {
            return impls.items;
        }
        return null;
    }

    // ========================================================================
    // Invariant Binding
    // ========================================================================

    /// Register a type invariant
    pub fn registerInvariant(self: *SpecBinding, type_name: []const u8, invariant: *const Ast.InvariantDecl) !void {
        const gop = try self.type_invariants.getOrPut(self.allocator, type_name);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.allocator.dupe(u8, type_name);
            gop.value_ptr.* = .{};
        }

        const bound = try self.allocator.create(BoundInvariant);
        bound.* = .{
            .name = if (invariant.name) |n| try self.allocator.dupe(u8, n) else null,
            .condition = invariant.condition,
            .span = invariant.span,
            .verification_status = .pending,
        };
        try gop.value_ptr.append(self.allocator, bound);
    }

    /// Get invariants for a type
    pub fn getTypeInvariants(self: *const SpecBinding, type_name: []const u8) ?[]const *const BoundInvariant {
        if (self.type_invariants.get(type_name)) |invs| {
            return invs.items;
        }
        return null;
    }

    // ========================================================================
    // Axiom Binding
    // ========================================================================

    /// Register an axiom
    pub fn registerAxiom(self: *SpecBinding, name: []const u8, axiom: *const Ast.AxiomDecl) !void {
        const bound = try self.allocator.create(BoundAxiom);
        bound.* = .{
            .name = try self.allocator.dupe(u8, name),
            .condition = axiom.condition,
            .type_params = axiom.type_params,
            .span = axiom.span,
            .is_trusted = false,
            .dependencies = &.{},
        };

        const key = try self.allocator.dupe(u8, name);
        try self.axioms.put(self.allocator, key, bound);
    }

    /// Look up an axiom
    pub fn lookupAxiom(self: *const SpecBinding, name: []const u8) ?*const BoundAxiom {
        return self.axioms.get(name);
    }

    // ========================================================================
    // Lemma Binding
    // ========================================================================

    /// Register a lemma
    pub fn registerLemma(self: *SpecBinding, name: []const u8, lemma: *const Ast.LemmaDecl) !void {
        const bound = try self.allocator.create(BoundLemma);
        bound.* = .{
            .name = try self.allocator.dupe(u8, name),
            .condition = lemma.condition,
            .type_params = lemma.type_params,
            .span = lemma.span,
            .verification_status = .pending,
            .dependencies = &.{},
            .proof_hint = null,
        };

        const key = try self.allocator.dupe(u8, name);
        try self.lemmas.put(self.allocator, key, bound);
    }

    /// Look up a lemma
    pub fn lookupLemma(self: *const SpecBinding, name: []const u8) ?*const BoundLemma {
        return self.lemmas.get(name);
    }

    // ========================================================================
    // Error Handling
    // ========================================================================

    fn addError(self: *SpecBinding, err: BindingError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn hasErrors(self: *const SpecBinding) bool {
        return self.errors.items.len > 0;
    }

    pub fn getErrors(self: *const SpecBinding) []const BindingError {
        return self.errors.items;
    }
};

// ============================================================================
// Bound Specification Types
// ============================================================================

/// A bound function specification with analysis results
pub const BoundSpec = struct {
    /// Fully qualified name
    name: []const u8,
    /// Original AST node
    ast: *const Ast.FunctionSpec,
    /// Preconditions (requires clauses)
    requires: []const *const Ast.Expression,
    /// Postconditions (ensures clauses)
    ensures: []const *const Ast.Expression,
    /// Frame conditions (modifies clauses)
    modifies: []const *const Ast.Expression,
    /// Whether this is a pure function
    is_pure: bool,
    /// Bound implementation (if any)
    implementation: ?ImplementationRef,
    /// Verification status
    verification_status: VerificationStatus,
};

/// A reference to an implementation
pub const ImplementationRef = struct {
    /// Language of the implementation (Klar, Kira, etc.)
    language: Language,
    /// Path to the implementation file
    file_path: []const u8,
    /// Source location in the implementation
    span: ?Span = null,

    pub const Language = enum {
        klar,
        kira,
        unknown,
    };
};

/// A bound invariant
pub const BoundInvariant = struct {
    name: ?[]const u8,
    condition: *const Ast.Expression,
    span: ?Span,
    verification_status: VerificationStatus,
};

/// A bound axiom
pub const BoundAxiom = struct {
    name: []const u8,
    condition: *const Ast.Expression,
    type_params: []const Ast.TypeParameter,
    span: ?Span,
    is_trusted: bool,
    dependencies: []const []const u8,
};

/// A bound lemma
pub const BoundLemma = struct {
    name: []const u8,
    condition: *const Ast.Expression,
    type_params: []const Ast.TypeParameter,
    span: ?Span,
    verification_status: VerificationStatus,
    dependencies: []const []const u8,
    proof_hint: ?[]const u8,
};

/// Verification status for specifications
pub const VerificationStatus = enum {
    /// Not yet verified
    pending,
    /// Verification in progress
    in_progress,
    /// Successfully verified
    verified,
    /// Verification failed
    failed,
    /// Verification timed out
    timeout,
    /// Marked as trusted (not verified)
    trusted,
    /// Admitted (assumed without proof)
    admitted,
};

/// An error during specification binding
pub const BindingError = struct {
    kind: Kind,
    message: []const u8,
    span: ?Span = null,

    pub const Kind = enum {
        /// Specification not found for implementation
        spec_not_found,
        /// Duplicate specification
        duplicate_spec,
        /// Implementation signature doesn't match specification
        signature_mismatch,
        /// Missing required specification
        missing_spec,
        /// Invalid specification
        invalid_spec,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "spec binding init/deinit" {
    const testing = std.testing;
    var binding = SpecBinding.init(testing.allocator);
    defer binding.deinit();

    try testing.expect(!binding.hasErrors());
}
