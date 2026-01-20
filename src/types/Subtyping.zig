const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("Type.zig");
const TypeContext = @import("TypeContext.zig").TypeContext;

// ============================================================================
// Subtyping and Type Compatibility
// ============================================================================

/// Subtyping checker for Sanna's type system.
/// Implements structural subtyping with support for:
/// - Refinement types (types with invariants)
/// - Generic type covariance/contravariance
/// - Optional type lifting
pub const Subtyping = struct {
    ctx: *TypeContext,

    pub fn init(ctx: *TypeContext) Subtyping {
        return .{ .ctx = ctx };
    }

    /// Check if `sub` is a subtype of `super`.
    /// Returns true if values of type `sub` can be used where `super` is expected.
    pub fn isSubtypeOf(self: *Subtyping, sub: *const Type.Type, super: *const Type.Type) bool {
        // Identical types are always subtypes
        if (sub.eql(super)) {
            return true;
        }

        // Error type is a subtype of everything (for error recovery)
        if (sub.kind == .err or super.kind == .err) {
            return true;
        }

        // Hole type is compatible with everything (for partial specifications)
        if (sub.kind == .hole or super.kind == .hole) {
            return true;
        }

        return switch (super.kind) {
            // Type variables: depends on bounds
            .type_var => |tv| self.satisfiesBounds(sub, tv.bounds),

            // Optional types: T <: ?T (lift non-optional to optional)
            .optional => |inner| blk: {
                // T <: ?T
                if (self.isSubtypeOf(sub, inner)) {
                    break :blk true;
                }
                // ?S <: ?T if S <: T
                if (sub.unwrapOptional()) |sub_inner| {
                    break :blk self.isSubtypeOf(sub_inner, inner);
                }
                break :blk false;
            },

            // Result types: Result[A, E] <: Result[B, E] if A <: B (covariant in Ok)
            .result => |res| blk: {
                switch (sub.kind) {
                    .result => |sub_res| {
                        // Covariant in ok_type, contravariant in err_type?
                        // For simplicity, use invariant in error type
                        break :blk self.isSubtypeOf(sub_res.ok_type, res.ok_type) and
                            sub_res.err_type.eql(res.err_type);
                    },
                    else => break :blk false,
                }
            },

            // Generic types: structural subtyping with variance
            .generic => |gen| blk: {
                switch (sub.kind) {
                    .generic => |sub_gen| {
                        // Same base type?
                        if (!sub_gen.base.eql(gen.base)) {
                            break :blk false;
                        }
                        // Same arity?
                        if (sub_gen.args.len != gen.args.len) {
                            break :blk false;
                        }
                        // Check type arguments (invariant for now)
                        // TODO: Could add variance annotations to type parameters
                        for (sub_gen.args, gen.args) |sub_arg, super_arg| {
                            if (!sub_arg.eql(super_arg)) {
                                break :blk false;
                            }
                        }
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },

            // Function types: contravariant in params, covariant in return
            .function => |func| blk: {
                switch (sub.kind) {
                    .function => |sub_func| {
                        // Same arity?
                        if (sub_func.params.len != func.params.len) {
                            break :blk false;
                        }
                        // Contravariant in parameters
                        for (func.params, sub_func.params) |super_param, sub_param| {
                            if (!self.isSubtypeOf(super_param, sub_param)) {
                                break :blk false;
                            }
                        }
                        // Covariant in return type
                        break :blk self.isSubtypeOf(sub_func.return_type, func.return_type);
                    },
                    else => break :blk false,
                }
            },

            // Tuple types: structural subtyping (covariant)
            .tuple => |tup| blk: {
                switch (sub.kind) {
                    .tuple => |sub_tup| {
                        if (sub_tup.elements.len != tup.elements.len) {
                            break :blk false;
                        }
                        for (sub_tup.elements, tup.elements) |sub_elem, super_elem| {
                            if (!self.isSubtypeOf(sub_elem, super_elem)) {
                                break :blk false;
                            }
                        }
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },

            // Named types with refinements
            .named => |named| blk: {
                switch (sub.kind) {
                    .named => |sub_named| {
                        // Same name?
                        if (!std.mem.eql(u8, sub_named.name, named.name)) {
                            // Check if sub is an alias of super
                            if (self.ctx.lookupTypeDef(sub_named.name)) |def| {
                                if (def.body == .alias) {
                                    break :blk self.isSubtypeOf(def.body.alias, super);
                                }
                            }
                            break :blk false;
                        }
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },

            // Integer widening: i8 <: i16 <: i32 <: i64
            .int => |int| blk: {
                switch (sub.kind) {
                    .int => |sub_int| {
                        // Same signedness
                        if (sub_int.signed != int.signed) {
                            break :blk false;
                        }
                        // Wider or equal bit width
                        // bits == 0 means arbitrary precision (always accepts)
                        if (int.bits == 0) {
                            break :blk true;
                        }
                        if (sub_int.bits == 0) {
                            // Arbitrary precision is not subtype of fixed width
                            break :blk false;
                        }
                        break :blk sub_int.bits <= int.bits;
                    },
                    else => break :blk false,
                }
            },

            // Float widening: f32 <: f64
            .float => |fl| blk: {
                switch (sub.kind) {
                    .float => |sub_fl| {
                        break :blk sub_fl.bits <= fl.bits;
                    },
                    // Int to float coercion is allowed
                    .int => break :blk true,
                    else => break :blk false,
                }
            },

            // Self type: only self is subtype of Self
            .self_type => sub.kind == .self_type,

            // Model types: same name required
            .model => |mod| blk: {
                switch (sub.kind) {
                    .model => |sub_mod| {
                        break :blk std.mem.eql(u8, sub_mod.name, mod.name);
                    },
                    else => break :blk false,
                }
            },

            // Primitives: exact match only (already handled by eql above)
            .bool_type, .string_type, .char_type, .unit => false,

            .err, .hole => unreachable, // Handled above
        };
    }

    /// Check if a type satisfies all bounds (trait constraints)
    fn satisfiesBounds(self: *Subtyping, ty: *const Type.Type, bounds: []const Type.Bound) bool {
        for (bounds) |bound| {
            if (!self.satisfiesBound(ty, bound)) {
                return false;
            }
        }
        return true;
    }

    /// Check if a type satisfies a single bound
    fn satisfiesBound(self: *Subtyping, ty: *const Type.Type, bound: Type.Bound) bool {
        // Look up the trait/interface
        if (self.ctx.lookupInterfaceDef(bound.trait_name)) |_| {
            // TODO: Check if ty implements the interface
            // For now, assume built-in traits are satisfied by appropriate types
            return self.checkBuiltinBound(ty, bound.trait_name);
        }

        // Unknown bound - be conservative
        return false;
    }

    /// Check built-in trait bounds (Ord, Eq, Hash, etc.)
    fn checkBuiltinBound(_: *Subtyping, ty: *const Type.Type, trait_name: []const u8) bool {
        if (std.mem.eql(u8, trait_name, "Eq")) {
            return ty.isEquatable();
        }
        if (std.mem.eql(u8, trait_name, "Ord")) {
            return ty.isOrdered();
        }
        if (std.mem.eql(u8, trait_name, "Hash")) {
            // Primitive types are hashable
            return ty.isPrimitive();
        }
        // Unknown trait
        return false;
    }

    /// Find the common supertype of two types (least upper bound)
    pub fn commonSupertype(self: *Subtyping, a: *const Type.Type, b: *const Type.Type) ?*const Type.Type {
        if (a.eql(b)) {
            return a;
        }

        if (self.isSubtypeOf(a, b)) {
            return b;
        }
        if (self.isSubtypeOf(b, a)) {
            return a;
        }

        // Special cases for numeric types
        switch (a.kind) {
            .int => |ia| {
                switch (b.kind) {
                    .int => |ib| {
                        // Same signedness: return wider type
                        if (ia.signed == ib.signed) {
                            if (ia.bits >= ib.bits) return a;
                            return b;
                        }
                        // Different signedness: no common supertype (would need signed with more bits)
                        return null;
                    },
                    .float => return b, // Int widened to float
                    else => {},
                }
            },
            .float => |fa| {
                switch (b.kind) {
                    .float => |fb| {
                        if (fa.bits >= fb.bits) return a;
                        return b;
                    },
                    .int => return a, // Int widened to float
                    else => {},
                }
            },
            else => {},
        }

        // Optional types: ?T and ?U -> ?(T join U)
        if (a.unwrapOptional()) |inner_a| {
            if (b.unwrapOptional()) |inner_b| {
                if (self.commonSupertype(inner_a, inner_b)) |_| {
                    // Would need to construct a new optional type
                    // For now, just return the first one if compatible
                    return a;
                }
            }
        }

        return null;
    }
};

// ============================================================================
// Type Unification (for Generic Type Inference)
// ============================================================================

/// Type unification for generic type inference.
/// Finds substitutions that make two types equal.
pub const Unification = struct {
    allocator: Allocator,
    /// Substitution map: type variable ID -> type
    substitutions: std.AutoHashMapUnmanaged(u32, *const Type.Type),

    pub fn init(allocator: Allocator) Unification {
        return .{
            .allocator = allocator,
            .substitutions = .{},
        };
    }

    pub fn deinit(self: *Unification) void {
        self.substitutions.deinit(self.allocator);
    }

    /// Unify two types, updating the substitution map.
    /// Returns true if unification succeeds.
    pub fn unify(self: *Unification, a: *const Type.Type, b: *const Type.Type) !bool {
        // Apply existing substitutions first
        const a_resolved = self.applySubstitutions(a);
        const b_resolved = self.applySubstitutions(b);

        // Already equal?
        if (a_resolved.eql(b_resolved)) {
            return true;
        }

        // Holes unify with anything
        if (a_resolved.kind == .hole or b_resolved.kind == .hole) {
            return true;
        }

        // Error types unify with anything
        if (a_resolved.kind == .err or b_resolved.kind == .err) {
            return true;
        }

        // Type variable on left: bind it
        if (a_resolved.kind == .type_var) {
            const tv = a_resolved.kind.type_var;
            if (self.occursIn(tv, b_resolved)) {
                return false; // Occurs check failed (infinite type)
            }
            try self.substitutions.put(self.allocator, tv.id, b_resolved);
            return true;
        }

        // Type variable on right: bind it
        if (b_resolved.kind == .type_var) {
            const tv = b_resolved.kind.type_var;
            if (self.occursIn(tv, a_resolved)) {
                return false;
            }
            try self.substitutions.put(self.allocator, tv.id, a_resolved);
            return true;
        }

        // Structural unification
        return switch (a_resolved.kind) {
            .generic => |ga| blk: {
                switch (b_resolved.kind) {
                    .generic => |gb| {
                        if (!try self.unify(ga.base, gb.base)) break :blk false;
                        if (ga.args.len != gb.args.len) break :blk false;
                        for (ga.args, gb.args) |arg_a, arg_b| {
                            if (!try self.unify(arg_a, arg_b)) break :blk false;
                        }
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },

            .function => |fa| blk: {
                switch (b_resolved.kind) {
                    .function => |fb| {
                        if (fa.params.len != fb.params.len) break :blk false;
                        for (fa.params, fb.params) |pa, pb| {
                            if (!try self.unify(pa, pb)) break :blk false;
                        }
                        break :blk try self.unify(fa.return_type, fb.return_type);
                    },
                    else => break :blk false,
                }
            },

            .optional => |oa| blk: {
                switch (b_resolved.kind) {
                    .optional => |ob| break :blk try self.unify(oa, ob),
                    else => break :blk false,
                }
            },

            .result => |ra| blk: {
                switch (b_resolved.kind) {
                    .result => |rb| {
                        break :blk try self.unify(ra.ok_type, rb.ok_type) and
                            try self.unify(ra.err_type, rb.err_type);
                    },
                    else => break :blk false,
                }
            },

            .tuple => |ta| blk: {
                switch (b_resolved.kind) {
                    .tuple => |tb| {
                        if (ta.elements.len != tb.elements.len) break :blk false;
                        for (ta.elements, tb.elements) |ea, eb| {
                            if (!try self.unify(ea, eb)) break :blk false;
                        }
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },

            else => false, // No unification possible
        };
    }

    /// Apply current substitutions to a type
    fn applySubstitutions(self: *Unification, ty: *const Type.Type) *const Type.Type {
        switch (ty.kind) {
            .type_var => |tv| {
                if (self.substitutions.get(tv.id)) |resolved| {
                    // Recursively apply
                    return self.applySubstitutions(resolved);
                }
                return ty;
            },
            else => return ty, // TODO: recursively apply to compound types
        }
    }

    /// Check if a type variable occurs in a type (for infinite type check)
    fn occursIn(self: *Unification, tv: Type.TypeVar, ty: *const Type.Type) bool {
        return switch (ty.kind) {
            .type_var => |other_tv| tv.id == other_tv.id,
            .generic => |gen| blk: {
                for (gen.args) |arg| {
                    if (self.occursIn(tv, arg)) break :blk true;
                }
                break :blk false;
            },
            .function => |func| blk: {
                for (func.params) |param| {
                    if (self.occursIn(tv, param)) break :blk true;
                }
                break :blk self.occursIn(tv, func.return_type);
            },
            .optional => |inner| self.occursIn(tv, inner),
            .result => |res| self.occursIn(tv, res.ok_type) or self.occursIn(tv, res.err_type),
            .tuple => |tup| blk: {
                for (tup.elements) |elem| {
                    if (self.occursIn(tv, elem)) break :blk true;
                }
                break :blk false;
            },
            else => false,
        };
    }

    /// Get the substitution for a type variable, or null if not bound
    pub fn getSubstitution(self: *const Unification, tv_id: u32) ?*const Type.Type {
        return self.substitutions.get(tv_id);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "subtyping - identical types" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();
    var sub = Subtyping.init(&ctx);

    try testing.expect(sub.isSubtypeOf(&Type.builtins.i32_type, &Type.builtins.i32_type));
    try testing.expect(sub.isSubtypeOf(&Type.builtins.bool_type, &Type.builtins.bool_type));
}

test "subtyping - integer widening" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();
    var sub = Subtyping.init(&ctx);

    // i8 <: i16 <: i32 <: i64
    try testing.expect(sub.isSubtypeOf(&Type.builtins.i8_type, &Type.builtins.i16_type));
    try testing.expect(sub.isSubtypeOf(&Type.builtins.i16_type, &Type.builtins.i32_type));
    try testing.expect(sub.isSubtypeOf(&Type.builtins.i32_type, &Type.builtins.i64_type));

    // Transitivity
    try testing.expect(sub.isSubtypeOf(&Type.builtins.i8_type, &Type.builtins.i64_type));

    // Not the other way around
    try testing.expect(!sub.isSubtypeOf(&Type.builtins.i64_type, &Type.builtins.i32_type));
}

test "subtyping - float widening" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();
    var sub = Subtyping.init(&ctx);

    // f32 <: f64
    try testing.expect(sub.isSubtypeOf(&Type.builtins.f32_type, &Type.builtins.f64_type));
    try testing.expect(!sub.isSubtypeOf(&Type.builtins.f64_type, &Type.builtins.f32_type));

    // int <: float
    try testing.expect(sub.isSubtypeOf(&Type.builtins.i32_type, &Type.builtins.f64_type));
}

test "unification - basic" {
    const testing = std.testing;
    var unif = Unification.init(testing.allocator);
    defer unif.deinit();

    // Same types unify
    try testing.expect(try unif.unify(&Type.builtins.i32_type, &Type.builtins.i32_type));

    // Different types don't unify
    try testing.expect(!try unif.unify(&Type.builtins.i32_type, &Type.builtins.string_type));
}

test "unification - type variables" {
    const testing = std.testing;
    var unif = Unification.init(testing.allocator);
    defer unif.deinit();

    const tv = Type.TypeVar{ .name = "T", .id = 0 };
    const tv_type = Type.Type{ .kind = .{ .type_var = tv } };

    // Type variable unifies with concrete type
    try testing.expect(try unif.unify(&tv_type, &Type.builtins.i32_type));

    // Check substitution was recorded
    const resolved = unif.getSubstitution(0);
    try testing.expect(resolved != null);
    try testing.expect(resolved.?.eql(&Type.builtins.i32_type));
}
