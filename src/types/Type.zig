const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Core Type Representation
// ============================================================================

/// A type in the Sanna type system.
/// Types are interned (deduplicated) for efficient comparison.
pub const Type = struct {
    kind: Kind,
    /// Optional source span for error reporting
    span: ?Span = null,

    pub const Kind = union(enum) {
        // Primitive types
        bool_type,
        int: IntType,
        float: FloatType,
        string_type,
        char_type,

        // Unit type (void)
        unit,

        // Named type (user-defined or built-in)
        named: NamedType,

        // Generic type application (e.g., List[i32])
        generic: GenericType,

        // Function type (e.g., fn(i32, i32) -> bool)
        function: FunctionType,

        // Optional type (e.g., ?i32)
        optional: *const Type,

        // Result type (e.g., Result[T, E])
        result: ResultType,

        // Tuple type (e.g., (i32, string))
        tuple: TupleType,

        // Self type (used in interface contexts)
        self_type,

        // Model type (mathematical specification-only type)
        model: ModelType,

        // Type variable (for generic type parameters)
        type_var: TypeVar,

        // Type hole (???)
        hole,

        // Error type (for error recovery)
        err,
    };

    /// Create a new type with a specific kind
    pub fn init(kind: Kind) Type {
        return .{ .kind = kind };
    }

    /// Create a type with source location
    pub fn initWithSpan(kind: Kind, span: Span) Type {
        return .{ .kind = kind, .span = span };
    }

    /// Check if two types are structurally equal
    pub fn eql(self: *const Type, other: *const Type) bool {
        return switch (self.kind) {
            .bool_type => other.kind == .bool_type,
            .string_type => other.kind == .string_type,
            .char_type => other.kind == .char_type,
            .unit => other.kind == .unit,
            .self_type => other.kind == .self_type,
            .hole => other.kind == .hole,
            .err => other.kind == .err,

            .int => |i| switch (other.kind) {
                .int => |j| i.bits == j.bits and i.signed == j.signed,
                else => false,
            },

            .float => |f| switch (other.kind) {
                .float => |g| f.bits == g.bits,
                else => false,
            },

            .named => |n| switch (other.kind) {
                .named => |m| std.mem.eql(u8, n.name, m.name) and
                    std.mem.eql(u8, n.module_path orelse "", m.module_path orelse ""),
                else => false,
            },

            .generic => |g| switch (other.kind) {
                .generic => |h| blk: {
                    if (!g.base.eql(h.base)) break :blk false;
                    if (g.args.len != h.args.len) break :blk false;
                    for (g.args, h.args) |a, b| {
                        if (!a.eql(b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },

            .function => |f| switch (other.kind) {
                .function => |g| blk: {
                    if (f.params.len != g.params.len) break :blk false;
                    for (f.params, g.params) |a, b| {
                        if (!a.eql(b)) break :blk false;
                    }
                    break :blk f.return_type.eql(g.return_type);
                },
                else => false,
            },

            .optional => |o| switch (other.kind) {
                .optional => |p| o.eql(p),
                else => false,
            },

            .result => |r| switch (other.kind) {
                .result => |s| r.ok_type.eql(s.ok_type) and r.err_type.eql(s.err_type),
                else => false,
            },

            .tuple => |t| switch (other.kind) {
                .tuple => |u| blk: {
                    if (t.elements.len != u.elements.len) break :blk false;
                    for (t.elements, u.elements) |a, b| {
                        if (!a.eql(b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },

            .model => |m| switch (other.kind) {
                .model => |n| std.mem.eql(u8, m.name, n.name),
                else => false,
            },

            .type_var => |v| switch (other.kind) {
                .type_var => |w| std.mem.eql(u8, v.name, w.name) and v.id == w.id,
                else => false,
            },
        };
    }

    /// Check if this type is a primitive type
    pub fn isPrimitive(self: *const Type) bool {
        return switch (self.kind) {
            .bool_type, .int, .float, .string_type, .char_type, .unit => true,
            else => false,
        };
    }

    /// Check if this type is a numeric type
    pub fn isNumeric(self: *const Type) bool {
        return switch (self.kind) {
            .int, .float => true,
            else => false,
        };
    }

    /// Check if this type is an integer type
    pub fn isInteger(self: *const Type) bool {
        return self.kind == .int;
    }

    /// Check if this type is a floating-point type
    pub fn isFloat(self: *const Type) bool {
        return self.kind == .float;
    }

    /// Check if this type can be compared for equality
    pub fn isEquatable(self: *const Type) bool {
        return switch (self.kind) {
            .bool_type, .int, .float, .string_type, .char_type, .unit => true,
            .named, .generic, .tuple, .optional, .result => true,
            .type_var => true, // Depends on bounds, but optimistically true
            .function, .model, .self_type, .hole, .err => false,
        };
    }

    /// Check if this type can be ordered (supports <, >, <=, >=)
    pub fn isOrdered(self: *const Type) bool {
        return switch (self.kind) {
            .int, .float, .string_type, .char_type => true,
            .type_var => true, // Depends on bounds
            else => false,
        };
    }

    /// Check if this type is a type variable
    pub fn isTypeVar(self: *const Type) bool {
        return self.kind == .type_var;
    }

    /// Check if this type contains any type variables
    pub fn containsTypeVars(self: *const Type) bool {
        return switch (self.kind) {
            .type_var => true,
            .generic => |g| blk: {
                for (g.args) |arg| {
                    if (arg.containsTypeVars()) break :blk true;
                }
                break :blk false;
            },
            .function => |f| blk: {
                for (f.params) |param| {
                    if (param.containsTypeVars()) break :blk true;
                }
                break :blk f.return_type.containsTypeVars();
            },
            .optional => |o| o.containsTypeVars(),
            .result => |r| r.ok_type.containsTypeVars() or r.err_type.containsTypeVars(),
            .tuple => |t| blk: {
                for (t.elements) |elem| {
                    if (elem.containsTypeVars()) break :blk true;
                }
                break :blk false;
            },
            else => false,
        };
    }

    /// Check if this type is complete (no holes)
    pub fn isComplete(self: *const Type) bool {
        return switch (self.kind) {
            .hole => false,
            .generic => |g| blk: {
                for (g.args) |arg| {
                    if (!arg.isComplete()) break :blk false;
                }
                break :blk true;
            },
            .function => |f| blk: {
                for (f.params) |param| {
                    if (!param.isComplete()) break :blk false;
                }
                break :blk f.return_type.isComplete();
            },
            .optional => |o| o.isComplete(),
            .result => |r| r.ok_type.isComplete() and r.err_type.isComplete(),
            .tuple => |t| blk: {
                for (t.elements) |elem| {
                    if (!elem.isComplete()) break :blk false;
                }
                break :blk true;
            },
            else => true,
        };
    }

    /// Get the underlying type for an optional, or null if not optional
    pub fn unwrapOptional(self: *const Type) ?*const Type {
        return switch (self.kind) {
            .optional => |inner| inner,
            else => null,
        };
    }

    /// Check if this is a model type (specification-only)
    pub fn isModelType(self: *const Type) bool {
        return self.kind == .model;
    }

    /// Check if this type can only exist in specification context (ghost/model)
    pub fn isSpecOnly(self: *const Type) bool {
        return switch (self.kind) {
            .model => true,
            .generic => |g| blk: {
                // A generic type is spec-only if any of its arguments are spec-only
                for (g.args) |arg| {
                    if (arg.isSpecOnly()) break :blk true;
                }
                break :blk false;
            },
            .tuple => |t| blk: {
                for (t.elements) |elem| {
                    if (elem.isSpecOnly()) break :blk true;
                }
                break :blk false;
            },
            .function => |f| blk: {
                for (f.params) |param| {
                    if (param.isSpecOnly()) break :blk true;
                }
                break :blk f.return_type.isSpecOnly();
            },
            .optional => |o| o.isSpecOnly(),
            .result => |r| r.ok_type.isSpecOnly() or r.err_type.isSpecOnly(),
            else => false,
        };
    }
};

// ============================================================================
// Integer Types
// ============================================================================

/// Integer type specification
pub const IntType = struct {
    bits: u16, // 8, 16, 32, 64, or 0 for arbitrary precision
    signed: bool,

    pub const i8_type = IntType{ .bits = 8, .signed = true };
    pub const i16_type = IntType{ .bits = 16, .signed = true };
    pub const i32_type = IntType{ .bits = 32, .signed = true };
    pub const i64_type = IntType{ .bits = 64, .signed = true };
    pub const u8_type = IntType{ .bits = 8, .signed = false };
    pub const u16_type = IntType{ .bits = 16, .signed = false };
    pub const u32_type = IntType{ .bits = 32, .signed = false };
    pub const u64_type = IntType{ .bits = 64, .signed = false };
    pub const usize_type = IntType{ .bits = 0, .signed = false }; // Platform-dependent
    pub const isize_type = IntType{ .bits = 0, .signed = true }; // Platform-dependent
    pub const nat_type = IntType{ .bits = 0, .signed = false }; // Natural numbers (arbitrary precision)
    pub const int_type = IntType{ .bits = 0, .signed = true }; // Arbitrary precision integer
};

/// Floating-point type specification
pub const FloatType = struct {
    bits: u16, // 32 or 64

    pub const f32_type = FloatType{ .bits = 32 };
    pub const f64_type = FloatType{ .bits = 64 };
};

// ============================================================================
// Named Types
// ============================================================================

/// A reference to a named type (user-defined or built-in)
pub const NamedType = struct {
    /// The type name
    name: []const u8,
    /// Optional module path (e.g., "std.collections")
    module_path: ?[]const u8 = null,
    /// The resolved type definition (set during type checking)
    resolved: ?*const TypeDefinition = null,
};

// ============================================================================
// Generic Types
// ============================================================================

/// A generic type application (e.g., List[i32], Map[string, User])
pub const GenericType = struct {
    /// The base type (e.g., List, Map)
    base: *const Type,
    /// The type arguments
    args: []const *const Type,
};

// ============================================================================
// Function Types
// ============================================================================

/// A function type (e.g., fn(i32, i32) -> bool)
pub const FunctionType = struct {
    /// Parameter types
    params: []const *const Type,
    /// Return type
    return_type: *const Type,
    /// Whether the function is pure (no side effects)
    is_pure: bool = false,
};

// ============================================================================
// Result Types
// ============================================================================

/// A Result type (e.g., Result[T, E])
pub const ResultType = struct {
    ok_type: *const Type,
    err_type: *const Type,
};

// ============================================================================
// Tuple Types
// ============================================================================

/// A tuple type (e.g., (i32, string, bool))
pub const TupleType = struct {
    elements: []const *const Type,
};

// ============================================================================
// Model Types
// ============================================================================

/// A model type (mathematical specification-only type)
pub const ModelType = struct {
    name: []const u8,
    /// Type parameters for generic models
    type_params: []const TypeVar = &.{},
    /// The resolved model definition (set during type checking)
    resolved: ?*const ModelDefinition = null,
};

// ============================================================================
// Type Variables
// ============================================================================

/// A type variable (for generic type parameters)
pub const TypeVar = struct {
    /// The variable name (e.g., "T", "E")
    name: []const u8,
    /// Unique identifier for this type variable
    id: u32,
    /// Bounds (trait constraints)
    bounds: []const Bound = &.{},
};

/// A bound on a type variable (e.g., T: Ord + Eq)
pub const Bound = struct {
    /// The trait name
    trait_name: []const u8,
    /// Optional module path
    module_path: ?[]const u8 = null,
};

// ============================================================================
// Type Definitions
// ============================================================================

/// A type definition (the declaration of a type)
pub const TypeDefinition = struct {
    /// The type name
    name: []const u8,
    /// Generic type parameters
    type_params: []const TypeParam = &.{},
    /// The body of the type definition
    body: Body,
    /// Invariants (refinement predicates)
    invariants: []const Invariant = &.{},
    /// Source span
    span: ?Span = null,

    pub const Body = union(enum) {
        /// Alias: type Email = string
        alias: *const Type,
        /// Product type (record): type User = { id: i32, name: string }
        product: []const Field,
        /// Sum type: type Status = | Active | Inactive { reason: string }
        sum: []const Variant,
    };
};

/// A type parameter in a type definition
pub const TypeParam = struct {
    name: []const u8,
    bounds: []const Bound = &.{},
};

/// A field in a product type
pub const Field = struct {
    name: []const u8,
    field_type: *const Type,
    default_value: ?*const Ast.Expression = null,
    /// Whether this is a ghost field (specification-only, doesn't exist at runtime)
    is_ghost: bool = false,
    span: ?Span = null,

    /// Check if this field can only be used in specification context
    pub fn isSpecOnly(self: Field) bool {
        return self.is_ghost or self.field_type.isSpecOnly();
    }
};

/// A variant in a sum type
pub const Variant = struct {
    name: []const u8,
    fields: []const Field = &.{}, // Empty for unit variants
    span: ?Span = null,
};

/// An invariant (refinement predicate)
pub const Invariant = struct {
    /// Optional name for the invariant
    name: ?[]const u8 = null,
    /// The invariant condition (AST expression)
    condition: *const Ast.Expression,
    /// Source span
    span: ?Span = null,
};

// ============================================================================
// Model Definitions
// ============================================================================

/// A model definition (mathematical specification-only type)
pub const ModelDefinition = struct {
    /// The model name
    name: []const u8,
    /// Generic type parameters
    type_params: []const TypeParam = &.{},
    /// Model members (signatures and axioms)
    members: []const ModelMember = &.{},
    /// Source span
    span: ?Span = null,
};

/// A member of a model definition
pub const ModelMember = union(enum) {
    /// A function/constant signature
    signature: Signature,
    /// An axiom
    axiom: Axiom,
};

/// A signature in a model (function or constant)
pub const Signature = struct {
    name: []const u8,
    params: []const Param = &.{},
    return_type: *const Type,
    span: ?Span = null,
};

/// A parameter in a signature
pub const Param = struct {
    name: []const u8,
    param_type: *const Type,
};

/// An axiom in a model
pub const Axiom = struct {
    name: []const u8,
    condition: *const Ast.Expression,
    span: ?Span = null,
};

// ============================================================================
// Built-in Types
// ============================================================================

/// Built-in primitive types (statically allocated)
pub const builtins = struct {
    pub const bool_type = Type{ .kind = .bool_type };
    pub const string_type = Type{ .kind = .string_type };
    pub const char_type = Type{ .kind = .char_type };
    pub const unit_type = Type{ .kind = .unit };
    pub const self_type = Type{ .kind = .self_type };
    pub const hole_type = Type{ .kind = .hole };
    pub const error_type = Type{ .kind = .err };

    pub const i8_type = Type{ .kind = .{ .int = IntType.i8_type } };
    pub const i16_type = Type{ .kind = .{ .int = IntType.i16_type } };
    pub const i32_type = Type{ .kind = .{ .int = IntType.i32_type } };
    pub const i64_type = Type{ .kind = .{ .int = IntType.i64_type } };
    pub const u8_type = Type{ .kind = .{ .int = IntType.u8_type } };
    pub const u16_type = Type{ .kind = .{ .int = IntType.u16_type } };
    pub const u32_type = Type{ .kind = .{ .int = IntType.u32_type } };
    pub const u64_type = Type{ .kind = .{ .int = IntType.u64_type } };
    pub const usize_type = Type{ .kind = .{ .int = IntType.usize_type } };
    pub const isize_type = Type{ .kind = .{ .int = IntType.isize_type } };
    pub const nat_type = Type{ .kind = .{ .int = IntType.nat_type } };
    pub const int_type = Type{ .kind = .{ .int = IntType.int_type } };

    pub const f32_type = Type{ .kind = .{ .float = FloatType.f32_type } };
    pub const f64_type = Type{ .kind = .{ .float = FloatType.f64_type } };
};

// ============================================================================
// Tests
// ============================================================================

test "primitive type equality" {
    const testing = std.testing;

    try testing.expect(builtins.bool_type.eql(&builtins.bool_type));
    try testing.expect(builtins.i32_type.eql(&builtins.i32_type));
    try testing.expect(!builtins.i32_type.eql(&builtins.i64_type));
    try testing.expect(!builtins.i32_type.eql(&builtins.bool_type));
}

test "type classification" {
    const testing = std.testing;

    try testing.expect(builtins.bool_type.isPrimitive());
    try testing.expect(builtins.i32_type.isPrimitive());
    try testing.expect(builtins.string_type.isPrimitive());

    try testing.expect(builtins.i32_type.isNumeric());
    try testing.expect(builtins.f64_type.isNumeric());
    try testing.expect(!builtins.bool_type.isNumeric());

    try testing.expect(builtins.i32_type.isInteger());
    try testing.expect(!builtins.f64_type.isInteger());

    try testing.expect(builtins.f64_type.isFloat());
    try testing.expect(!builtins.i32_type.isFloat());
}

test "type is equatable and orderable" {
    const testing = std.testing;

    try testing.expect(builtins.bool_type.isEquatable());
    try testing.expect(builtins.i32_type.isEquatable());
    try testing.expect(builtins.string_type.isEquatable());

    try testing.expect(builtins.i32_type.isOrdered());
    try testing.expect(builtins.f64_type.isOrdered());
    try testing.expect(!builtins.bool_type.isOrdered());
}

test "type completeness" {
    const testing = std.testing;

    try testing.expect(builtins.bool_type.isComplete());
    try testing.expect(builtins.i32_type.isComplete());
    try testing.expect(!builtins.hole_type.isComplete());
}
