const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Type = @import("../types/root.zig").Type;
const TypeContext = @import("../types/root.zig").TypeContext;
const TypeChecker = @import("../types/root.zig").TypeChecker;
const TypeError = @import("../types/root.zig").TypeError;
const Span = @import("../lexer/root.zig").Span;
const ModuleLoader = @import("ModuleLoader.zig").ModuleLoader;
const ImportResolver = @import("ImportResolver.zig").ImportResolver;
const SpecBinding = @import("SpecBinding.zig").SpecBinding;
const BoundSpec = @import("SpecBinding.zig").BoundSpec;
const BindingError = @import("SpecBinding.zig").BindingError;

// ============================================================================
// Semantic Analyzer
// ============================================================================

/// The SemanticAnalyzer performs complete semantic analysis of Sanna specifications.
/// It orchestrates module loading, import resolution, type checking, and
/// specification-specific analysis including:
/// - Precondition (requires) semantic analysis
/// - Postcondition (ensures) semantic analysis with old() and result
/// - Modifies clause analysis for frame conditions
/// - Pure function detection and enforcement
/// - Axiom registration and consistency checking
/// - Lemma dependency tracking
pub const SemanticAnalyzer = struct {
    allocator: Allocator,

    /// Module loader for resolving imports
    loader: *ModuleLoader,

    /// Type context for type checking
    ctx: TypeContext,

    /// Type checker
    type_checker: TypeChecker,

    /// Import resolver
    import_resolver: ImportResolver,

    /// Specification bindings
    spec_binding: SpecBinding,

    /// Collected semantic errors
    errors: std.ArrayListUnmanaged(SemanticError),

    /// Currently analyzing specification (for context in error messages)
    current_spec: ?[]const u8,

    /// Whether we are in an ensures clause (affects old() and result availability)
    in_ensures_clause: bool,

    /// Whether we are in a requires clause
    in_requires_clause: bool,

    /// The result type for the current function (for 'result' keyword)
    current_result_type: ?*const Type.Type,

    /// Variables available via old() in ensures clauses
    old_bindings: std.StringHashMapUnmanaged(*const Type.Type),

    pub fn init(allocator: Allocator, loader: *ModuleLoader) SemanticAnalyzer {
        var ctx = TypeContext.init(allocator);
        return .{
            .allocator = allocator,
            .loader = loader,
            .ctx = ctx,
            .type_checker = TypeChecker.init(&ctx),
            .import_resolver = ImportResolver.init(allocator, loader, &ctx),
            .spec_binding = SpecBinding.init(allocator),
            .errors = .{},
            .current_spec = null,
            .in_ensures_clause = false,
            .in_requires_clause = false,
            .current_result_type = null,
            .old_bindings = .{},
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        self.ctx.deinit();
        self.import_resolver.deinit();
        self.spec_binding.deinit();
        self.old_bindings.deinit(self.allocator);

        for (self.errors.items) |err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    // ========================================================================
    // Module Analysis
    // ========================================================================

    /// Analyze a complete module
    pub fn analyzeModule(self: *SemanticAnalyzer, module: *const Ast.Module) !AnalysisResult {
        // Step 1: Resolve imports
        try self.import_resolver.resolveImports(module.imports);

        // Step 2: Type check the module
        _ = try self.type_checker.checkModule(module);

        // Step 3: Analyze specifications
        for (module.declarations) |decl| {
            try self.analyzeDeclaration(decl);
        }

        return AnalysisResult{
            .success = !self.hasErrors(),
            .type_errors = self.ctx.getErrors(),
            .import_errors = self.import_resolver.getErrors(),
            .semantic_errors = self.errors.items,
            .binding_errors = self.spec_binding.getErrors(),
        };
    }

    /// Analyze a single declaration
    fn analyzeDeclaration(self: *SemanticAnalyzer, decl: Ast.Declaration) !void {
        switch (decl.kind) {
            .spec_fn => |spec_fn| try self.analyzeFunctionSpec(spec_fn, decl.attributes),
            .spec_interface => |spec_interface| try self.analyzeInterfaceSpec(spec_interface),
            .invariant => |invariant| try self.analyzeInvariant(invariant),
            .axiom => |axiom| try self.analyzeAxiom(axiom),
            .lemma => |lemma| try self.analyzeLemma(lemma),
            .type_def => |type_def| try self.analyzeTypeInvariants(type_def),
            .model_def => {}, // Models are handled during type checking
        }
    }

    // ========================================================================
    // Function Specification Analysis
    // ========================================================================

    /// Analyze a function specification
    fn analyzeFunctionSpec(self: *SemanticAnalyzer, spec: Ast.FunctionSpec, attributes: []const Ast.Attribute) !void {
        self.current_spec = spec.name.name;
        defer self.current_spec = null;

        // Register the specification
        const fqn = try self.fullyQualifiedName(spec.name.name);
        defer self.allocator.free(fqn);
        try self.spec_binding.registerFunctionSpec(fqn, &spec);

        // Push function scope
        try self.ctx.pushScope(.function);
        defer self.ctx.popScope();

        // Register type parameters
        for (spec.type_params) |param| {
            const tv = self.ctx.freshTypeVar(param.name.name, &.{});
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Register parameters
        for (spec.params) |param| {
            const param_type = try self.type_checker.resolveTypeExpr(param.type_expr);
            try self.ctx.defineVariable(param.name.name, param_type);
        }

        // Set up result type for ensures clause analysis
        if (spec.return_type) |ret_type| {
            self.current_result_type = try self.type_checker.resolveTypeExpr(ret_type);
        } else {
            self.current_result_type = &Type.builtins.unit_type;
        }
        defer self.current_result_type = null;

        // Analyze requires clauses
        for (spec.requires) |req| {
            try self.analyzeRequiresClause(req);
        }

        // Store old bindings for ensures analysis
        try self.captureOldBindings();
        defer self.old_bindings.clearRetainingCapacity();

        // Analyze ensures clauses
        for (spec.ensures) |ens| {
            try self.analyzeEnsuresClause(ens);
        }

        // Analyze modifies clauses
        for (spec.modifies) |mod| {
            try self.analyzeModifiesClause(mod);
        }

        // Check pure function constraints
        if (spec.pure) {
            try self.checkPureFunction(spec);
        }

        // Analyze decreases clause if present
        if (spec.decreases) |dec| {
            try self.analyzeDecreasesClause(dec);
        }

        // Check for @trusted attribute
        for (attributes) |attr| {
            if (std.mem.eql(u8, attr.name.name, "trusted")) {
                // Mark as trusted
                if (self.spec_binding.lookupFunctionSpec(fqn)) |bound| {
                    const mutable = @constCast(bound);
                    mutable.verification_status = .trusted;
                }
            }
        }
    }

    /// Analyze a requires clause (precondition)
    fn analyzeRequiresClause(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        self.in_requires_clause = true;
        defer self.in_requires_clause = false;

        // Push requires scope (specification context)
        try self.ctx.pushScope(.requires);
        defer self.ctx.popScope();

        // Type check the expression - must be boolean
        const expr_type = try self.type_checker.inferExpression(expr);
        if (!expr_type.eql(&Type.builtins.bool_type)) {
            try self.addError(.{
                .kind = .type_mismatch,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "requires clause must be boolean, found {s}",
                    .{self.typeName(expr_type)},
                ),
                .span = expr.span,
            });
        }

        // Check for invalid constructs in requires
        try self.checkRequiresConstraints(expr);
    }

    /// Check constraints on requires clauses
    fn checkRequiresConstraints(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        // Requires clauses cannot use 'result' or 'old()'
        switch (expr.kind) {
            .result => {
                try self.addError(.{
                    .kind = .invalid_in_requires,
                    .message = try self.allocator.dupe(u8, "'result' cannot be used in requires clause"),
                    .span = expr.span,
                });
            },
            .old => {
                try self.addError(.{
                    .kind = .invalid_in_requires,
                    .message = try self.allocator.dupe(u8, "'old()' cannot be used in requires clause"),
                    .span = expr.span,
                });
            },
            .binary => |bin| {
                try self.checkRequiresConstraints(bin.left);
                try self.checkRequiresConstraints(bin.right);
            },
            .unary => |un| {
                try self.checkRequiresConstraints(un.operand);
            },
            .call => |call| {
                for (call.args) |arg| {
                    try self.checkRequiresConstraints(arg);
                }
            },
            .if_expr => |if_e| {
                try self.checkRequiresConstraints(if_e.condition);
                try self.checkRequiresConstraints(if_e.then_branch);
                if (if_e.else_branch) |else_b| {
                    try self.checkRequiresConstraints(else_b);
                }
            },
            .quantifier => |q| {
                try self.checkRequiresConstraints(q.body);
            },
            else => {},
        }
    }

    /// Capture variable bindings for old() in ensures clauses
    fn captureOldBindings(self: *SemanticAnalyzer) !void {
        // Capture all variables from the current scope for old() references
        if (self.ctx.currentScope()) |scope| {
            var iter = scope.variables.iterator();
            while (iter.next()) |entry| {
                try self.old_bindings.put(self.allocator, entry.key_ptr.*, entry.value_ptr.*);
            }
        }
    }

    /// Analyze an ensures clause (postcondition)
    fn analyzeEnsuresClause(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        self.in_ensures_clause = true;
        defer self.in_ensures_clause = false;

        // Push ensures scope (specification context)
        try self.ctx.pushScope(.ensures);
        defer self.ctx.popScope();

        // Register 'result' in the scope if return type is non-void
        if (self.current_result_type) |ret_type| {
            if (!ret_type.eql(&Type.builtins.unit_type)) {
                try self.ctx.defineVariable("result", ret_type);
            }
        }

        // Type check the expression - must be boolean
        const expr_type = try self.type_checker.inferExpression(expr);
        if (!expr_type.eql(&Type.builtins.bool_type)) {
            try self.addError(.{
                .kind = .type_mismatch,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "ensures clause must be boolean, found {s}",
                    .{self.typeName(expr_type)},
                ),
                .span = expr.span,
            });
        }

        // Validate old() usages
        try self.validateOldUsages(expr);
    }

    /// Validate old() expressions in ensures clauses
    fn validateOldUsages(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        switch (expr.kind) {
            .old => |old| {
                // Check that the inner expression refers to pre-state variables
                try self.validateOldExpression(old.expr);
            },
            .binary => |bin| {
                try self.validateOldUsages(bin.left);
                try self.validateOldUsages(bin.right);
            },
            .unary => |un| {
                try self.validateOldUsages(un.operand);
            },
            .call => |call| {
                for (call.args) |arg| {
                    try self.validateOldUsages(arg);
                }
            },
            .if_expr => |if_e| {
                try self.validateOldUsages(if_e.condition);
                try self.validateOldUsages(if_e.then_branch);
                if (if_e.else_branch) |else_b| {
                    try self.validateOldUsages(else_b);
                }
            },
            .quantifier => |q| {
                try self.validateOldUsages(q.body);
            },
            else => {},
        }
    }

    /// Validate the expression inside old()
    fn validateOldExpression(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        switch (expr.kind) {
            .identifier => |ident| {
                // Must refer to a parameter or pre-state variable
                if (!self.old_bindings.contains(ident.name)) {
                    try self.addError(.{
                        .kind = .invalid_old_reference,
                        .message = try std.fmt.allocPrint(
                            self.allocator,
                            "'{s}' is not available in old() - must be a parameter or pre-state variable",
                            .{ident.name},
                        ),
                        .span = expr.span,
                    });
                }
            },
            .result => {
                try self.addError(.{
                    .kind = .invalid_old_reference,
                    .message = try self.allocator.dupe(u8, "'result' cannot be used inside old()"),
                    .span = expr.span,
                });
            },
            .field_access => |fa| {
                try self.validateOldExpression(fa.object);
            },
            .index => |idx| {
                try self.validateOldExpression(idx.object);
                try self.validateOldExpression(idx.index);
            },
            else => {},
        }
    }

    // ========================================================================
    // Modifies Clause Analysis (Frame Conditions)
    // ========================================================================

    /// Analyze a modifies clause
    fn analyzeModifiesClause(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        // Modifies clauses specify which state can be modified
        // Valid forms:
        // - Identifier (e.g., `self`)
        // - Field access (e.g., `self.balance`)
        // - Index (e.g., `arr[i]`)
        // - Special: `nothing` or `*` (everything)

        switch (expr.kind) {
            .identifier => {
                // Valid - modifies a variable
            },
            .field_access => {
                // Valid - modifies a specific field
            },
            .index => {
                // Valid - modifies a specific index
            },
            else => {
                try self.addError(.{
                    .kind = .invalid_modifies,
                    .message = try self.allocator.dupe(u8, "modifies clause must be an identifier, field access, or index expression"),
                    .span = expr.span,
                });
            },
        }

        // Type check the expression
        _ = try self.type_checker.inferExpression(expr);
    }

    // ========================================================================
    // Pure Function Analysis
    // ========================================================================

    /// Check pure function constraints
    fn checkPureFunction(self: *SemanticAnalyzer, spec: Ast.FunctionSpec) !void {
        // Pure functions cannot have modifies clauses (except 'nothing')
        if (spec.modifies.len > 0) {
            // Check if it's just 'nothing'
            var has_real_modifies = false;
            for (spec.modifies) |mod| {
                switch (mod.kind) {
                    .identifier => |ident| {
                        if (!std.mem.eql(u8, ident.name, "nothing")) {
                            has_real_modifies = true;
                            break;
                        }
                    },
                    else => {
                        has_real_modifies = true;
                        break;
                    },
                }
            }

            if (has_real_modifies) {
                try self.addError(.{
                    .kind = .pure_function_modifies,
                    .message = try self.allocator.dupe(u8, "pure function cannot have modifies clauses"),
                    .span = spec.span,
                });
            }
        }
    }

    // ========================================================================
    // Decreases Clause Analysis
    // ========================================================================

    /// Analyze a decreases clause (for termination)
    fn analyzeDecreasesClause(self: *SemanticAnalyzer, expr: *const Ast.Expression) !void {
        // Decreases clause must be a well-founded measure (typically natural number)
        const expr_type = try self.type_checker.inferExpression(expr);

        // Check that it's a numeric or tuple of numerics
        if (!isWellFoundedType(expr_type)) {
            try self.addError(.{
                .kind = .invalid_decreases,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "decreases clause must have well-founded type, found {s}",
                    .{self.typeName(expr_type)},
                ),
                .span = expr.span,
            });
        }
    }

    fn isWellFoundedType(ty: *const Type.Type) bool {
        return switch (ty.kind) {
            .int => |i| !i.signed or i.bits == 0, // Nat or unsigned
            .tuple => |t| blk: {
                for (t.elements) |elem| {
                    if (!isWellFoundedType(elem)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        };
    }

    // ========================================================================
    // Interface Specification Analysis
    // ========================================================================

    /// Analyze an interface specification
    fn analyzeInterfaceSpec(self: *SemanticAnalyzer, spec: Ast.InterfaceSpec) !void {
        // Push interface scope
        try self.ctx.pushScope(.interface);
        defer self.ctx.popScope();

        // Register type parameters
        for (spec.type_params) |param| {
            const tv = self.ctx.freshTypeVar(param.name.name, &.{});
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Register Self type
        try self.ctx.defineVariable("Self", &Type.builtins.self_type);

        // Analyze method specifications
        for (spec.members) |member| {
            switch (member) {
                .method => |method| {
                    try self.analyzeMethodSpec(method);
                },
                .associated_type => {
                    // Associated types are handled during type checking
                },
            }
        }

        // Analyze interface invariants
        for (spec.invariants) |inv| {
            const inv_type = try self.type_checker.inferExpression(inv);
            if (!inv_type.eql(&Type.builtins.bool_type)) {
                try self.addError(.{
                    .kind = .type_mismatch,
                    .message = try self.allocator.dupe(u8, "interface invariant must be boolean"),
                    .span = inv.span,
                });
            }
        }
    }

    /// Analyze a method specification within an interface
    fn analyzeMethodSpec(self: *SemanticAnalyzer, method: Ast.MethodSpec) !void {
        // Push method scope
        try self.ctx.pushScope(.function);
        defer self.ctx.popScope();

        // Register parameters
        for (method.params) |param| {
            const param_type = try self.type_checker.resolveTypeExpr(param.type_expr);
            try self.ctx.defineVariable(param.name.name, param_type);
        }

        // Analyze requires/ensures if present
        if (method.requires) |req| {
            try self.analyzeRequiresClause(req);
        }

        if (method.ensures) |ens| {
            if (method.return_type) |ret| {
                self.current_result_type = try self.type_checker.resolveTypeExpr(ret);
            }
            defer self.current_result_type = null;
            try self.analyzeEnsuresClause(ens);
        }
    }

    // ========================================================================
    // Invariant Analysis
    // ========================================================================

    /// Analyze a standalone invariant declaration
    fn analyzeInvariant(self: *SemanticAnalyzer, invariant: Ast.InvariantDecl) !void {
        try self.ctx.pushScope(.invariant);
        defer self.ctx.popScope();

        // Type check the condition
        const cond_type = try self.type_checker.inferExpression(invariant.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.addError(.{
                .kind = .type_mismatch,
                .message = try self.allocator.dupe(u8, "invariant must be boolean"),
                .span = invariant.condition.span,
            });
        }

        // Register in spec binding if it belongs to a type
        if (invariant.for_type) |type_name| {
            try self.spec_binding.registerInvariant(type_name, &invariant);
        }
    }

    /// Analyze type invariants within a type definition
    fn analyzeTypeInvariants(self: *SemanticAnalyzer, type_def: Ast.TypeDefinition) !void {
        if (type_def.invariants.len == 0) return;

        try self.ctx.pushScope(.type_def);
        defer self.ctx.popScope();

        // For product types, register fields as variables
        switch (type_def.body) {
            .product => |fields| {
                for (fields) |field| {
                    const field_type = try self.type_checker.resolveTypeExpr(field.type_expr);
                    try self.ctx.defineVariable(field.name.name, field_type);
                }
            },
            else => {},
        }

        // Check each invariant
        for (type_def.invariants) |inv| {
            const inv_type = try self.type_checker.inferExpression(inv);
            if (!inv_type.eql(&Type.builtins.bool_type)) {
                try self.addError(.{
                    .kind = .type_mismatch,
                    .message = try self.allocator.dupe(u8, "type invariant must be boolean"),
                    .span = inv.span,
                });
            }
        }
    }

    // ========================================================================
    // Axiom Analysis
    // ========================================================================

    /// Analyze an axiom declaration
    fn analyzeAxiom(self: *SemanticAnalyzer, axiom: Ast.AxiomDecl) !void {
        // Register the axiom
        const fqn = try self.fullyQualifiedName(axiom.name.name);
        defer self.allocator.free(fqn);
        try self.spec_binding.registerAxiom(fqn, &axiom);

        // Push scope for type parameters
        try self.ctx.pushScope(.quantifier);
        defer self.ctx.popScope();

        // Register type parameters
        for (axiom.type_params) |param| {
            const tv = self.ctx.freshTypeVar(param.name.name, &.{});
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Type check the axiom condition
        const cond_type = try self.type_checker.inferExpression(axiom.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.addError(.{
                .kind = .type_mismatch,
                .message = try self.allocator.dupe(u8, "axiom condition must be boolean"),
                .span = axiom.condition.span,
            });
        }

        // Check for consistency (basic checks only - full consistency requires SMT)
        try self.checkAxiomConsistency(axiom);
    }

    /// Basic consistency checking for axioms
    fn checkAxiomConsistency(self: *SemanticAnalyzer, axiom: Ast.AxiomDecl) !void {
        // Check for obvious contradictions like `false`
        switch (axiom.condition.kind) {
            .literal => |lit| {
                if (lit == .bool_lit and !lit.bool_lit) {
                    try self.addError(.{
                        .kind = .inconsistent_axiom,
                        .message = try self.allocator.dupe(u8, "axiom is trivially false"),
                        .span = axiom.condition.span,
                    });
                }
            },
            else => {},
        }
    }

    // ========================================================================
    // Lemma Analysis
    // ========================================================================

    /// Analyze a lemma declaration
    fn analyzeLemma(self: *SemanticAnalyzer, lemma: Ast.LemmaDecl) !void {
        // Register the lemma
        const fqn = try self.fullyQualifiedName(lemma.name.name);
        defer self.allocator.free(fqn);
        try self.spec_binding.registerLemma(fqn, &lemma);

        // Push scope for type parameters
        try self.ctx.pushScope(.quantifier);
        defer self.ctx.popScope();

        // Register type parameters
        for (lemma.type_params) |param| {
            const tv = self.ctx.freshTypeVar(param.name.name, &.{});
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Type check the lemma condition
        const cond_type = try self.type_checker.inferExpression(lemma.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.addError(.{
                .kind = .type_mismatch,
                .message = try self.allocator.dupe(u8, "lemma condition must be boolean"),
                .span = lemma.condition.span,
            });
        }

        // Track dependencies on axioms/other lemmas
        try self.trackLemmaDependencies(lemma);
    }

    /// Track dependencies of a lemma on axioms and other lemmas
    fn trackLemmaDependencies(self: *SemanticAnalyzer, lemma: Ast.LemmaDecl) !void {
        _ = self;
        _ = lemma;
        // TODO: Analyze the lemma body/proof to find dependencies
        // This will be fully implemented when we have proof hints
    }

    // ========================================================================
    // Error Handling
    // ========================================================================

    fn addError(self: *SemanticAnalyzer, err: SemanticError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn hasErrors(self: *const SemanticAnalyzer) bool {
        return self.errors.items.len > 0 or
            self.ctx.hasErrors() or
            self.import_resolver.hasErrors() or
            self.spec_binding.hasErrors() or
            self.loader.hasErrors();
    }

    pub fn getErrors(self: *const SemanticAnalyzer) []const SemanticError {
        return self.errors.items;
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn fullyQualifiedName(self: *SemanticAnalyzer, name: []const u8) ![]const u8 {
        if (self.ctx.current_module) |module| {
            const len = module.len + 1 + name.len;
            const buf = try self.allocator.alloc(u8, len);
            @memcpy(buf[0..module.len], module);
            buf[module.len] = '.';
            @memcpy(buf[module.len + 1 ..], name);
            return buf;
        }
        return self.allocator.dupe(u8, name);
    }

    fn typeName(self: *SemanticAnalyzer, ty: *const Type.Type) []const u8 {
        _ = self;
        return switch (ty.kind) {
            .bool_type => "bool",
            .int => "int",
            .float => "float",
            .string_type => "string",
            .char_type => "char",
            .unit => "unit",
            .named => |n| n.name,
            .generic => "<generic>",
            .function => "<function>",
            .optional => "<optional>",
            .result => "<result>",
            .tuple => "<tuple>",
            .self_type => "Self",
            .model => |m| m.name,
            .type_var => |v| v.name,
            .hole => "???",
            .err => "<error>",
        };
    }
};

// ============================================================================
// Analysis Result Types
// ============================================================================

/// Result of semantic analysis
pub const AnalysisResult = struct {
    success: bool,
    type_errors: []const TypeError,
    import_errors: []const ImportResolver.ImportError,
    semantic_errors: []const SemanticError,
    binding_errors: []const BindingError,

    pub fn totalErrors(self: AnalysisResult) usize {
        return self.type_errors.len +
            self.import_errors.len +
            self.semantic_errors.len +
            self.binding_errors.len;
    }
};

/// A semantic error
pub const SemanticError = struct {
    kind: Kind,
    message: []const u8,
    span: ?Span = null,
    notes: []const Note = &.{},

    pub const Kind = enum {
        /// Type mismatch
        type_mismatch,
        /// Invalid use of result in requires
        invalid_in_requires,
        /// Invalid old() reference
        invalid_old_reference,
        /// Invalid modifies clause
        invalid_modifies,
        /// Pure function has modifies
        pure_function_modifies,
        /// Invalid decreases clause
        invalid_decreases,
        /// Inconsistent axiom
        inconsistent_axiom,
        /// Missing specification
        missing_spec,
        /// Specification violation
        spec_violation,
    };

    pub const Note = struct {
        message: []const u8,
        span: ?Span = null,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "semantic analyzer init/deinit" {
    const testing = std.testing;
    var loader = ModuleLoader.init(testing.allocator);
    defer loader.deinit();

    var analyzer = SemanticAnalyzer.init(testing.allocator, &loader);
    defer analyzer.deinit();

    try testing.expect(!analyzer.hasErrors());
}
