const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("Type.zig");
const Ast = @import("../parser/root.zig").Ast;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Type Context (Type Environment)
// ============================================================================

/// The type context manages type definitions, scopes, and name resolution
/// during type checking.
pub const TypeContext = struct {
    allocator: Allocator,

    /// The current scope chain (innermost first)
    scopes: std.ArrayListUnmanaged(Scope),

    /// All type definitions by fully qualified name
    type_defs: std.StringHashMapUnmanaged(*const Type.TypeDefinition),

    /// All model definitions by fully qualified name
    model_defs: std.StringHashMapUnmanaged(*const Type.ModelDefinition),

    /// All interface definitions by fully qualified name
    interface_defs: std.StringHashMapUnmanaged(*const InterfaceDefinition),

    /// Type variable counter for generating unique IDs
    type_var_counter: u32,

    /// Collected errors during type checking
    errors: std.ArrayListUnmanaged(TypeError),

    /// Current module path (for qualified name resolution)
    current_module: ?[]const u8,

    /// Interned types (for deduplication)
    interned_types: std.ArrayListUnmanaged(*Type.Type),

    pub fn init(allocator: Allocator) TypeContext {
        return .{
            .allocator = allocator,
            .scopes = .{},
            .type_defs = .{},
            .model_defs = .{},
            .interface_defs = .{},
            .type_var_counter = 0,
            .errors = .{},
            .current_module = null,
            .interned_types = .{},
        };
    }

    pub fn deinit(self: *TypeContext) void {
        // Free all scopes
        for (self.scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.scopes.deinit(self.allocator);

        // Free hash maps
        self.type_defs.deinit(self.allocator);
        self.model_defs.deinit(self.allocator);
        self.interface_defs.deinit(self.allocator);

        // Free errors
        self.errors.deinit(self.allocator);

        // Free interned types
        for (self.interned_types.items) |ty| {
            self.allocator.destroy(ty);
        }
        self.interned_types.deinit(self.allocator);
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    /// Push a new scope onto the scope stack
    pub fn pushScope(self: *TypeContext, kind: Scope.Kind) !void {
        try self.scopes.append(self.allocator, Scope.init(kind));
    }

    /// Pop the current scope from the stack
    pub fn popScope(self: *TypeContext) void {
        if (self.scopes.pop()) |scope| {
            var s = scope;
            s.deinit(self.allocator);
        }
    }

    /// Get the current scope (innermost)
    pub fn currentScope(self: *TypeContext) ?*Scope {
        if (self.scopes.items.len > 0) {
            return &self.scopes.items[self.scopes.items.len - 1];
        }
        return null;
    }

    /// Define a variable in the current scope
    pub fn defineVariable(self: *TypeContext, name: []const u8, var_type: *const Type.Type) !void {
        if (self.currentScope()) |scope| {
            try scope.variables.put(self.allocator, name, var_type);
        }
    }

    /// Define a type variable in the current scope
    pub fn defineTypeVar(self: *TypeContext, name: []const u8, type_var: Type.TypeVar) !void {
        if (self.currentScope()) |scope| {
            try scope.type_vars.put(self.allocator, name, type_var);
        }
    }

    /// Look up a variable by name in all scopes (innermost to outermost)
    pub fn lookupVariable(self: *const TypeContext, name: []const u8) ?*const Type.Type {
        // Search from innermost to outermost scope
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].variables.get(name)) |var_type| {
                return var_type;
            }
        }
        return null;
    }

    /// Look up a type variable by name in all scopes
    pub fn lookupTypeVar(self: *const TypeContext, name: []const u8) ?Type.TypeVar {
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].type_vars.get(name)) |type_var| {
                return type_var;
            }
        }
        return null;
    }

    // ========================================================================
    // Ghost State Management
    // ========================================================================

    /// Define a ghost variable in the current scope (specification-only)
    pub fn defineGhostVariable(self: *TypeContext, name: []const u8, var_type: *const Type.Type) !void {
        if (self.currentScope()) |scope| {
            try scope.ghost_vars.put(self.allocator, name, var_type);
        }
    }

    /// Look up a ghost variable by name in all scopes
    pub fn lookupGhostVariable(self: *const TypeContext, name: []const u8) ?*const Type.Type {
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].ghost_vars.get(name)) |var_type| {
                return var_type;
            }
        }
        return null;
    }

    /// Check if we are currently in a ghost/specification context
    pub fn isInGhostContext(self: *const TypeContext) bool {
        // Check if any scope in the chain is a ghost context
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].is_ghost_context) {
                return true;
            }
        }
        return false;
    }

    /// Look up a variable, returning it along with whether it's a ghost variable
    pub const VariableLookupResult = struct {
        var_type: *const Type.Type,
        is_ghost: bool,
    };

    pub fn lookupVariableWithGhostInfo(self: *const TypeContext, name: []const u8) ?VariableLookupResult {
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            // Check ghost variables first
            if (self.scopes.items[i].ghost_vars.get(name)) |var_type| {
                return .{ .var_type = var_type, .is_ghost = true };
            }
            // Then regular variables
            if (self.scopes.items[i].variables.get(name)) |var_type| {
                return .{ .var_type = var_type, .is_ghost = false };
            }
        }
        return null;
    }

    // ========================================================================
    // Type Definitions
    // ========================================================================

    /// Register a type definition
    pub fn registerTypeDef(self: *TypeContext, name: []const u8, def: *const Type.TypeDefinition) !void {
        const fqn = try self.fullyQualifiedName(name);
        try self.type_defs.put(self.allocator, fqn, def);
    }

    /// Look up a type definition by name
    pub fn lookupTypeDef(self: *const TypeContext, name: []const u8) ?*const Type.TypeDefinition {
        // First try the name as-is (might be fully qualified)
        if (self.type_defs.get(name)) |def| {
            return def;
        }

        // Try with current module prefix
        if (self.current_module) |module| {
            var buf: [512]u8 = undefined;
            const fqn = std.fmt.bufPrint(&buf, "{s}.{s}", .{ module, name }) catch return null;
            if (self.type_defs.get(fqn)) |def| {
                return def;
            }
        }

        return null;
    }

    /// Register a model definition
    pub fn registerModelDef(self: *TypeContext, name: []const u8, def: *const Type.ModelDefinition) !void {
        const fqn = try self.fullyQualifiedName(name);
        try self.model_defs.put(self.allocator, fqn, def);
    }

    /// Look up a model definition by name
    pub fn lookupModelDef(self: *const TypeContext, name: []const u8) ?*const Type.ModelDefinition {
        if (self.model_defs.get(name)) |def| {
            return def;
        }

        if (self.current_module) |module| {
            var buf: [512]u8 = undefined;
            const fqn = std.fmt.bufPrint(&buf, "{s}.{s}", .{ module, name }) catch return null;
            if (self.model_defs.get(fqn)) |def| {
                return def;
            }
        }

        return null;
    }

    /// Register an interface definition
    pub fn registerInterfaceDef(self: *TypeContext, name: []const u8, def: *const InterfaceDefinition) !void {
        const fqn = try self.fullyQualifiedName(name);
        try self.interface_defs.put(self.allocator, fqn, def);
    }

    /// Look up an interface definition by name
    pub fn lookupInterfaceDef(self: *const TypeContext, name: []const u8) ?*const InterfaceDefinition {
        if (self.interface_defs.get(name)) |def| {
            return def;
        }

        if (self.current_module) |module| {
            var buf: [512]u8 = undefined;
            const fqn = std.fmt.bufPrint(&buf, "{s}.{s}", .{ module, name }) catch return null;
            if (self.interface_defs.get(fqn)) |def| {
                return def;
            }
        }

        return null;
    }

    // ========================================================================
    // Type Variable Management
    // ========================================================================

    /// Generate a fresh type variable with unique ID
    pub fn freshTypeVar(self: *TypeContext, name: []const u8, bounds: []const Type.Bound) Type.TypeVar {
        const id = self.type_var_counter;
        self.type_var_counter += 1;
        return .{
            .name = name,
            .id = id,
            .bounds = bounds,
        };
    }

    // ========================================================================
    // Type Interning
    // ========================================================================

    /// Intern a type to ensure structural equality uses pointer equality
    pub fn intern(self: *TypeContext, ty: Type.Type) !*const Type.Type {
        // Check if we already have this type interned
        for (self.interned_types.items) |existing| {
            if (existing.eql(&ty)) {
                return existing;
            }
        }

        // Allocate and store the new type
        const interned = try self.allocator.create(Type.Type);
        interned.* = ty;
        try self.interned_types.append(self.allocator, interned);
        return interned;
    }

    // ========================================================================
    // Built-in Type Resolution
    // ========================================================================

    /// Resolve a type name to a built-in type, or null if not a built-in
    pub fn resolveBuiltin(name: []const u8) ?*const Type.Type {
        return builtins.get(name);
    }

    // ========================================================================
    // Error Handling
    // ========================================================================

    /// Add a type error
    pub fn addError(self: *TypeContext, err: TypeError) !void {
        try self.errors.append(self.allocator, err);
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const TypeContext) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const TypeContext) []const TypeError {
        return self.errors.items;
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    fn fullyQualifiedName(self: *TypeContext, name: []const u8) ![]const u8 {
        if (self.current_module) |module| {
            const len = module.len + 1 + name.len;
            const buf = try self.allocator.alloc(u8, len);
            @memcpy(buf[0..module.len], module);
            buf[module.len] = '.';
            @memcpy(buf[module.len + 1 ..], name);
            return buf;
        }
        return name;
    }
};

// ============================================================================
// Scope
// ============================================================================

/// A scope in the type environment
pub const Scope = struct {
    kind: Kind,
    /// Variables defined in this scope
    variables: std.StringHashMapUnmanaged(*const Type.Type),
    /// Type variables defined in this scope
    type_vars: std.StringHashMapUnmanaged(Type.TypeVar),
    /// Ghost variables defined in this scope (specification-only)
    ghost_vars: std.StringHashMapUnmanaged(*const Type.Type),
    /// The return type for function scopes
    return_type: ?*const Type.Type = null,
    /// The self type for interface/impl scopes
    self_type: ?*const Type.Type = null,
    /// Whether we are in a ghost context (specification code)
    is_ghost_context: bool = false,

    pub const Kind = enum {
        /// Global/module scope
        module,
        /// Function body scope
        function,
        /// Block scope (if, match arm, etc.)
        block,
        /// Interface definition scope
        interface,
        /// Type definition scope
        type_def,
        /// Let binding scope
        let_binding,
        /// Quantifier scope (forall, exists)
        quantifier,
        /// Requires clause (specification context)
        requires,
        /// Ensures clause (specification context)
        ensures,
        /// Invariant (specification context)
        invariant,
    };

    pub fn init(kind: Kind) Scope {
        return .{
            .kind = kind,
            .variables = .{},
            .type_vars = .{},
            .ghost_vars = .{},
            .is_ghost_context = isSpecificationContext(kind),
        };
    }

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.variables.deinit(allocator);
        self.type_vars.deinit(allocator);
        self.ghost_vars.deinit(allocator);
    }

    /// Check if a scope kind is a specification context (where ghost state is allowed)
    fn isSpecificationContext(kind: Kind) bool {
        return switch (kind) {
            .requires, .ensures, .invariant, .quantifier => true,
            else => false,
        };
    }
};

// ============================================================================
// Interface Definition
// ============================================================================

/// An interface definition
pub const InterfaceDefinition = struct {
    /// The interface name
    name: []const u8,
    /// Generic type parameters
    type_params: []const Type.TypeParam = &.{},
    /// Super-traits (interfaces this extends)
    super_traits: []const []const u8 = &.{},
    /// Associated types
    associated_types: []const AssociatedType = &.{},
    /// Method signatures
    methods: []const MethodSignature = &.{},
    /// Interface-level invariants
    invariants: []const Type.Invariant = &.{},
    /// Source span
    span: ?Span = null,
};

/// An associated type in an interface
pub const AssociatedType = struct {
    name: []const u8,
    bounds: []const Type.Bound = &.{},
    span: ?Span = null,
};

/// A method signature in an interface
pub const MethodSignature = struct {
    name: []const u8,
    type_params: []const Type.TypeParam = &.{},
    params: []const Type.Param = &.{},
    return_type: *const Type.Type,
    is_pure: bool = false,
    requires: []const *const Ast.Expression = &.{},
    ensures: []const *const Ast.Expression = &.{},
    modifies: []const *const Ast.Expression = &.{},
    span: ?Span = null,
};

// ============================================================================
// Type Errors
// ============================================================================

/// A type error
pub const TypeError = struct {
    kind: Kind,
    message: []const u8,
    span: ?Span = null,
    notes: []const Note = &.{},

    pub const Kind = enum {
        /// Type mismatch (expected X, found Y)
        type_mismatch,
        /// Undefined type reference
        undefined_type,
        /// Undefined variable reference
        undefined_variable,
        /// Undefined function reference
        undefined_function,
        /// Wrong number of type arguments
        wrong_arity,
        /// Type parameter bounds not satisfied
        bounds_not_satisfied,
        /// Recursive type without indirection
        recursive_type,
        /// Invariant violation
        invariant_violation,
        /// Invalid operation for type
        invalid_operation,
        /// Ambiguous type (inference failure)
        ambiguous_type,
        /// Duplicate definition
        duplicate_definition,
        /// Missing return type
        missing_return_type,
        /// Ghost state used in non-ghost context
        ghost_in_non_ghost,
        /// Model type used in runtime code
        model_in_runtime,
    };

    pub const Note = struct {
        message: []const u8,
        span: ?Span = null,
    };
};

// ============================================================================
// Built-in Types Map
// ============================================================================

const builtins = std.StaticStringMap(*const Type.Type).initComptime(.{
    .{ "bool", &Type.builtins.bool_type },
    .{ "string", &Type.builtins.string_type },
    .{ "char", &Type.builtins.char_type },
    .{ "void", &Type.builtins.unit_type },
    .{ "unit", &Type.builtins.unit_type },
    .{ "i8", &Type.builtins.i8_type },
    .{ "i16", &Type.builtins.i16_type },
    .{ "i32", &Type.builtins.i32_type },
    .{ "i64", &Type.builtins.i64_type },
    .{ "u8", &Type.builtins.u8_type },
    .{ "u16", &Type.builtins.u16_type },
    .{ "u32", &Type.builtins.u32_type },
    .{ "u64", &Type.builtins.u64_type },
    .{ "usize", &Type.builtins.usize_type },
    .{ "isize", &Type.builtins.isize_type },
    .{ "Nat", &Type.builtins.nat_type },
    .{ "Int", &Type.builtins.int_type },
    .{ "f32", &Type.builtins.f32_type },
    .{ "f64", &Type.builtins.f64_type },
    .{ "Self", &Type.builtins.self_type },
});

// ============================================================================
// Tests
// ============================================================================

test "scope push and pop" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    try ctx.pushScope(.module);
    try testing.expectEqual(@as(usize, 1), ctx.scopes.items.len);

    try ctx.pushScope(.function);
    try testing.expectEqual(@as(usize, 2), ctx.scopes.items.len);

    ctx.popScope();
    try testing.expectEqual(@as(usize, 1), ctx.scopes.items.len);

    ctx.popScope();
    try testing.expectEqual(@as(usize, 0), ctx.scopes.items.len);
}

test "variable lookup respects scope" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    try ctx.pushScope(.module);
    try ctx.defineVariable("x", &Type.builtins.i32_type);

    try ctx.pushScope(.function);
    try ctx.defineVariable("y", &Type.builtins.string_type);

    // Can find variable in current scope
    const y = ctx.lookupVariable("y");
    try testing.expect(y != null);
    try testing.expect(y.?.eql(&Type.builtins.string_type));

    // Can find variable in outer scope
    const x = ctx.lookupVariable("x");
    try testing.expect(x != null);
    try testing.expect(x.?.eql(&Type.builtins.i32_type));

    // Cannot find undefined variable
    try testing.expect(ctx.lookupVariable("z") == null);

    ctx.popScope();

    // Variable from inner scope is now gone
    try testing.expect(ctx.lookupVariable("y") == null);

    // Variable from outer scope still accessible
    try testing.expect(ctx.lookupVariable("x") != null);
}

test "builtin type resolution" {
    const testing = std.testing;

    const bool_type = TypeContext.resolveBuiltin("bool");
    try testing.expect(bool_type != null);
    try testing.expect(bool_type.?.eql(&Type.builtins.bool_type));

    const i32_type = TypeContext.resolveBuiltin("i32");
    try testing.expect(i32_type != null);
    try testing.expect(i32_type.?.eql(&Type.builtins.i32_type));

    const unknown = TypeContext.resolveBuiltin("UnknownType");
    try testing.expect(unknown == null);
}

test "fresh type variables have unique IDs" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const tv1 = ctx.freshTypeVar("T", &.{});
    const tv2 = ctx.freshTypeVar("T", &.{});
    const tv3 = ctx.freshTypeVar("U", &.{});

    try testing.expect(tv1.id != tv2.id);
    try testing.expect(tv2.id != tv3.id);
    try testing.expect(tv1.id != tv3.id);
}

test "type interning" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const t1 = try ctx.intern(Type.builtins.i32_type);
    const t2 = try ctx.intern(Type.builtins.i32_type);
    const t3 = try ctx.intern(Type.builtins.string_type);

    // Same type should return same pointer
    try testing.expectEqual(t1, t2);

    // Different types should return different pointers
    try testing.expect(t1 != t3);
}
