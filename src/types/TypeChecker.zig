const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("Type.zig");
const TypeContext = @import("TypeContext.zig").TypeContext;
const TypeError = @import("TypeContext.zig").TypeError;
const Scope = @import("TypeContext.zig").Scope;
const InterfaceDefinition = @import("TypeContext.zig").InterfaceDefinition;
const Ast = @import("../parser/root.zig").Ast;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Type Checker
// ============================================================================

/// The type checker performs type checking and inference on Sanna AST nodes.
pub const TypeChecker = struct {
    ctx: *TypeContext,
    allocator: Allocator,

    pub fn init(ctx: *TypeContext) TypeChecker {
        return .{
            .ctx = ctx,
            .allocator = ctx.allocator,
        };
    }

    // ========================================================================
    // Module Type Checking
    // ========================================================================

    /// Type check an entire module
    pub fn checkModule(self: *TypeChecker, module: *const Ast.Module) !TypeCheckResult {
        // Set current module
        if (module.name) |name| {
            self.ctx.current_module = try self.qualifiedNameToString(name);
        }

        // Push module scope
        try self.ctx.pushScope(.module);
        defer self.ctx.popScope();

        // First pass: register all type definitions
        for (module.declarations) |decl| {
            try self.registerDeclaration(decl);
        }

        // Second pass: type check all declarations
        for (module.declarations) |decl| {
            try self.checkDeclaration(decl);
        }

        return TypeCheckResult{
            .success = !self.ctx.hasErrors(),
            .errors = self.ctx.getErrors(),
        };
    }

    /// Register a declaration (first pass)
    fn registerDeclaration(self: *TypeChecker, decl: Ast.Declaration) !void {
        switch (decl.kind) {
            .type_def => |type_def| {
                const def = try self.convertTypeDefinition(type_def);
                try self.ctx.registerTypeDef(type_def.name.name, def);
            },
            .model_def => |model_def| {
                const def = try self.convertModelDefinition(model_def);
                try self.ctx.registerModelDef(model_def.name.name, def);
            },
            .spec_interface => |interface_spec| {
                const def = try self.convertInterfaceDefinition(interface_spec);
                try self.ctx.registerInterfaceDef(interface_spec.name.name, def);
            },
            else => {},
        }
    }

    /// Type check a declaration (second pass)
    fn checkDeclaration(self: *TypeChecker, decl: Ast.Declaration) !void {
        switch (decl.kind) {
            .type_def => |type_def| try self.checkTypeDefinition(type_def),
            .model_def => |model_def| try self.checkModelDefinition(model_def),
            .spec_fn => |spec_fn| try self.checkFunctionSpec(spec_fn),
            .spec_interface => |interface_spec| try self.checkInterfaceSpec(interface_spec),
            .invariant => |inv| try self.checkInvariant(inv),
            .axiom => |axiom| try self.checkAxiom(axiom),
            .lemma => |lemma| try self.checkLemma(lemma),
        }
    }

    // ========================================================================
    // Type Definition Checking
    // ========================================================================

    fn checkTypeDefinition(self: *TypeChecker, type_def: Ast.TypeDefinition) !void {
        // Push a scope for type parameters
        try self.ctx.pushScope(.type_def);
        defer self.ctx.popScope();

        // Register type parameters
        for (type_def.type_params) |param| {
            const bounds = try self.convertBounds(param.bounds);
            const tv = self.ctx.freshTypeVar(param.name.name, bounds);
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Check the type body
        switch (type_def.body) {
            .alias => |type_expr| {
                _ = try self.resolveTypeExpr(type_expr);
            },
            .product => |fields| {
                for (fields) |field| {
                    _ = try self.resolveTypeExpr(field.type_expr);
                    if (field.default_value) |default| {
                        _ = try self.inferExpression(default);
                    }
                }
            },
            .sum => |variants| {
                for (variants) |variant| {
                    for (variant.fields) |field| {
                        _ = try self.resolveTypeExpr(field.type_expr);
                    }
                }
            },
        }

        // Check invariants
        for (type_def.invariants) |inv_expr| {
            const inv_type = try self.inferExpression(inv_expr);
            if (!inv_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "invariant must be a boolean expression",
                    .span = inv_expr.span,
                });
            }
        }
    }

    fn checkModelDefinition(self: *TypeChecker, model_def: Ast.ModelDefinition) !void {
        try self.ctx.pushScope(.type_def);
        defer self.ctx.popScope();

        // Register type parameters
        for (model_def.type_params) |param| {
            const bounds = try self.convertBounds(param.bounds);
            const tv = self.ctx.freshTypeVar(param.name.name, bounds);
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Check model members
        for (model_def.members) |member| {
            switch (member) {
                .signature => |sig| {
                    for (sig.params) |param| {
                        _ = try self.resolveTypeExpr(param.type_expr);
                    }
                    _ = try self.resolveTypeExpr(sig.return_type);
                },
                .axiom => |axiom| {
                    try self.checkAxiom(axiom);
                },
            }
        }
    }

    // ========================================================================
    // Function Specification Checking
    // ========================================================================

    fn checkFunctionSpec(self: *TypeChecker, spec: Ast.FunctionSpec) !void {
        try self.ctx.pushScope(.function);
        defer self.ctx.popScope();

        // Register type parameters
        for (spec.type_params) |param| {
            const bounds = try self.convertBounds(param.bounds);
            const tv = self.ctx.freshTypeVar(param.name.name, bounds);
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Resolve and register parameters
        for (spec.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try self.ctx.defineVariable(param.name.name, param_type);
        }

        // Resolve return type and set in scope
        const return_type = try self.resolveTypeExpr(spec.return_type);
        if (self.ctx.currentScope()) |scope| {
            scope.return_type = return_type;
        }

        // Check preconditions
        for (spec.requires) |req| {
            const req_type = try self.inferExpression(req);
            if (!req_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "requires clause must be a boolean expression",
                    .span = req.span,
                });
            }
        }

        // Check postconditions (with 'result' in scope)
        try self.ctx.defineVariable("result", return_type);
        for (spec.ensures) |ens| {
            const ens_type = try self.inferExpression(ens);
            if (!ens_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "ensures clause must be a boolean expression",
                    .span = ens.span,
                });
            }
        }

        // Check modifies clauses
        for (spec.modifies) |mod| {
            _ = try self.inferExpression(mod);
        }

        // Check decreases clause if present
        if (spec.decreases) |dec| {
            const dec_type = try self.inferExpression(dec);
            // Decreases should be a natural number or tuple of natural numbers
            if (!dec_type.isNumeric()) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "decreases clause must be a numeric expression",
                    .span = dec.span,
                });
            }
        }
    }

    // ========================================================================
    // Interface Specification Checking
    // ========================================================================

    fn checkInterfaceSpec(self: *TypeChecker, spec: Ast.InterfaceSpec) !void {
        try self.ctx.pushScope(.interface);
        defer self.ctx.popScope();

        // Register type parameters
        for (spec.type_params) |param| {
            const bounds = try self.convertBounds(param.bounds);
            const tv = self.ctx.freshTypeVar(param.name.name, bounds);
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Set Self type in scope
        if (self.ctx.currentScope()) |scope| {
            scope.self_type = &Type.builtins.self_type;
        }

        // Check super-traits exist
        for (spec.super_traits) |super_trait| {
            const name = try self.qualifiedNameToString(super_trait);
            if (self.ctx.lookupInterfaceDef(name) == null) {
                try self.ctx.addError(.{
                    .kind = .undefined_type,
                    .message = "undefined super-trait",
                    .span = super_trait.span,
                });
            }
        }

        // Check interface members
        for (spec.members) |member| {
            switch (member) {
                .associated_type => |assoc| {
                    // Register associated type as a type variable
                    const bounds = try self.convertBounds(assoc.bounds);
                    const tv = self.ctx.freshTypeVar(assoc.name.name, bounds);
                    try self.ctx.defineTypeVar(assoc.name.name, tv);
                },
                .function => |func| {
                    try self.checkFunctionSpec(func);
                },
            }
        }

        // Check interface invariants
        for (spec.invariants) |inv| {
            const inv_type = try self.inferExpression(inv);
            if (!inv_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "interface invariant must be a boolean expression",
                    .span = inv.span,
                });
            }
        }
    }

    // ========================================================================
    // Invariant, Axiom, Lemma Checking
    // ========================================================================

    fn checkInvariant(self: *TypeChecker, inv: Ast.InvariantDecl) !void {
        const cond_type = try self.inferExpression(inv.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "invariant condition must be a boolean expression",
                .span = inv.span,
            });
        }
    }

    fn checkAxiom(self: *TypeChecker, axiom: Ast.AxiomDecl) !void {
        const cond_type = try self.inferExpression(axiom.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "axiom condition must be a boolean expression",
                .span = axiom.span,
            });
        }
    }

    fn checkLemma(self: *TypeChecker, lemma: Ast.LemmaDecl) !void {
        try self.ctx.pushScope(.function);
        defer self.ctx.popScope();

        // Register type parameters
        for (lemma.type_params) |param| {
            const bounds = try self.convertBounds(param.bounds);
            const tv = self.ctx.freshTypeVar(param.name.name, bounds);
            try self.ctx.defineTypeVar(param.name.name, tv);
        }

        // Register parameters
        for (lemma.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try self.ctx.defineVariable(param.name.name, param_type);
        }

        // Check condition
        const cond_type = try self.inferExpression(lemma.condition);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "lemma condition must be a boolean expression",
                .span = lemma.span,
            });
        }
    }

    // ========================================================================
    // Type Expression Resolution
    // ========================================================================

    /// Resolve a type expression to a concrete type
    pub fn resolveTypeExpr(self: *TypeChecker, type_expr: Ast.TypeExpr) !*const Type.Type {
        return switch (type_expr.kind) {
            .named => |qname| try self.resolveNamedType(qname, type_expr.span),
            .generic => |gen| try self.resolveGenericType(gen, type_expr.span),
            .function => |func| try self.resolveFunctionType(func, type_expr.span),
            .optional => |inner| try self.resolveOptionalType(inner, type_expr.span),
            .result => |res| try self.resolveResultType(res, type_expr.span),
            .tuple => |elems| try self.resolveTupleType(elems, type_expr.span),
            .self_type => &Type.builtins.self_type,
            .hole => &Type.builtins.hole_type,
        };
    }

    fn resolveNamedType(self: *TypeChecker, qname: Ast.QualifiedName, span: Span) !*const Type.Type {
        const name = try self.qualifiedNameToString(qname);

        // Check for builtin types first
        if (TypeContext.resolveBuiltin(name)) |builtin| {
            return builtin;
        }

        // Check for type variables in scope
        if (self.ctx.lookupTypeVar(name)) |tv| {
            return try self.ctx.intern(Type.Type{
                .kind = .{ .type_var = tv },
                .span = span,
            });
        }

        // Check for user-defined types
        if (self.ctx.lookupTypeDef(name)) |_| {
            return try self.ctx.intern(Type.Type{
                .kind = .{ .named = .{ .name = name } },
                .span = span,
            });
        }

        // Check for model types
        if (self.ctx.lookupModelDef(name)) |_| {
            return try self.ctx.intern(Type.Type{
                .kind = .{ .model = .{ .name = name } },
                .span = span,
            });
        }

        // Undefined type
        try self.ctx.addError(.{
            .kind = .undefined_type,
            .message = "undefined type",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn resolveGenericType(self: *TypeChecker, gen: Ast.GenericType, span: Span) !*const Type.Type {
        const base_name = try self.qualifiedNameToString(gen.base);

        // Resolve base type
        const base = try self.resolveNamedType(gen.base, span);

        // Resolve type arguments
        const args = try self.allocator.alloc(*const Type.Type, gen.args.len);
        for (gen.args, 0..) |arg, i| {
            args[i] = try self.resolveTypeExpr(arg);
        }

        // Check arity if we have definition
        if (self.ctx.lookupTypeDef(base_name)) |def| {
            if (def.type_params.len != args.len) {
                try self.ctx.addError(.{
                    .kind = .wrong_arity,
                    .message = "wrong number of type arguments",
                    .span = span,
                });
            }
        }

        return try self.ctx.intern(Type.Type{
            .kind = .{ .generic = .{ .base = base, .args = args } },
            .span = span,
        });
    }

    fn resolveFunctionType(self: *TypeChecker, func: Ast.FunctionType, span: Span) !*const Type.Type {
        const params = try self.allocator.alloc(*const Type.Type, func.params.len);
        for (func.params, 0..) |param, i| {
            params[i] = try self.resolveTypeExpr(param);
        }
        const return_type = try self.resolveTypeExpr(func.return_type.*);

        return try self.ctx.intern(Type.Type{
            .kind = .{ .function = .{ .params = params, .return_type = return_type } },
            .span = span,
        });
    }

    fn resolveOptionalType(self: *TypeChecker, inner: *const Ast.TypeExpr, span: Span) !*const Type.Type {
        const inner_type = try self.resolveTypeExpr(inner.*);

        return try self.ctx.intern(Type.Type{
            .kind = .{ .optional = inner_type },
            .span = span,
        });
    }

    fn resolveResultType(self: *TypeChecker, res: Ast.ResultType, span: Span) !*const Type.Type {
        const ok_type = try self.resolveTypeExpr(res.ok_type.*);
        const err_type = try self.resolveTypeExpr(res.err_type.*);

        return try self.ctx.intern(Type.Type{
            .kind = .{ .result = .{ .ok_type = ok_type, .err_type = err_type } },
            .span = span,
        });
    }

    fn resolveTupleType(self: *TypeChecker, elems: []const Ast.TypeExpr, span: Span) !*const Type.Type {
        const elements = try self.allocator.alloc(*const Type.Type, elems.len);
        for (elems, 0..) |elem, i| {
            elements[i] = try self.resolveTypeExpr(elem);
        }

        return try self.ctx.intern(Type.Type{
            .kind = .{ .tuple = .{ .elements = elements } },
            .span = span,
        });
    }

    // ========================================================================
    // Expression Type Inference
    // ========================================================================

    /// Infer the type of an expression
    pub fn inferExpression(self: *TypeChecker, expr: Ast.Expression) !*const Type.Type {
        return switch (expr.kind) {
            // Literals
            .int_literal => &Type.builtins.i64_type,
            .float_literal => &Type.builtins.f64_type,
            .string_literal => &Type.builtins.string_type,
            .bool_literal => &Type.builtins.bool_type,

            // Identifiers
            .identifier => |ident| self.inferIdentifier(ident, expr.span),
            .qualified => |qname| self.inferQualifiedName(qname, expr.span),

            // Access
            .field_access => |access| self.inferFieldAccess(access, expr.span),
            .index_access => |access| self.inferIndexAccess(access, expr.span),
            .method_call => |call| self.inferMethodCall(call, expr.span),

            // Operators
            .binary => |bin| self.inferBinaryOp(bin, expr.span),
            .unary => |un| self.inferUnaryOp(un, expr.span),

            // Logical
            .and_expr => |and_e| self.inferLogical(and_e.left, and_e.right, expr.span),
            .or_expr => |or_e| self.inferLogical(or_e.left, or_e.right, expr.span),
            .not_expr => |not_e| self.inferNot(not_e.operand, expr.span),
            .implies => |impl| self.inferLogical(impl.antecedent, impl.consequent, expr.span),
            .iff => |iff_e| self.inferLogical(iff_e.left, iff_e.right, expr.span),

            // Quantifiers
            .forall, .exists => |quant| self.inferQuantifier(quant, expr.span),

            // Control flow
            .if_expr => |if_e| self.inferIfExpr(if_e, expr.span),
            .match_expr => |match_e| self.inferMatchExpr(match_e, expr.span),
            .let_expr => |let_e| self.inferLetExpr(let_e, expr.span),

            // Function-related
            .call => |call| self.inferCall(call, expr.span),
            .lambda => |lambda| self.inferLambda(lambda, expr.span),

            // Specification-specific
            .old => |inner| self.inferExpression(inner.*),
            .result => self.inferResult(expr.span),
            .self_expr => self.inferSelf(expr.span),

            // Collections
            .sequence_literal => |elems| self.inferSequenceLiteral(elems, expr.span),
            .set_literal => |elems| self.inferSetLiteral(elems, expr.span),
            .map_literal => |entries| self.inferMapLiteral(entries, expr.span),
            .set_comprehension => |comp| self.inferSetComprehension(comp, expr.span),

            // Range
            .range => |range| self.inferRange(range, expr.span),

            // Hole
            .hole => &Type.builtins.hole_type,
            .typed_hole => |ty| self.resolveTypeExpr(ty.*),
        };
    }

    fn inferIdentifier(self: *TypeChecker, ident: Ast.Identifier, span: Span) !*const Type.Type {
        if (self.ctx.lookupVariable(ident.name)) |var_type| {
            return var_type;
        }

        try self.ctx.addError(.{
            .kind = .undefined_variable,
            .message = "undefined variable",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn inferQualifiedName(self: *TypeChecker, qname: Ast.QualifiedName, span: Span) !*const Type.Type {
        const name = try self.qualifiedNameToString(qname);

        // Try as a type reference
        if (self.ctx.lookupTypeDef(name)) |_| {
            return try self.ctx.intern(Type.Type{
                .kind = .{ .named = .{ .name = name } },
                .span = span,
            });
        }

        try self.ctx.addError(.{
            .kind = .undefined_variable,
            .message = "undefined name",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn inferFieldAccess(self: *TypeChecker, access: Ast.FieldAccess, span: Span) !*const Type.Type {
        const obj_type = try self.inferExpression(access.object.*);

        // For named types, look up the field
        switch (obj_type.kind) {
            .named => |named| {
                if (self.ctx.lookupTypeDef(named.name)) |def| {
                    if (def.body == .product) {
                        for (def.body.product) |field| {
                            if (std.mem.eql(u8, field.name, access.field.name)) {
                                return field.field_type;
                            }
                        }
                    }
                }
            },
            else => {},
        }

        try self.ctx.addError(.{
            .kind = .invalid_operation,
            .message = "field not found",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn inferIndexAccess(self: *TypeChecker, access: Ast.IndexAccess, span: Span) !*const Type.Type {
        const obj_type = try self.inferExpression(access.object.*);
        const idx_type = try self.inferExpression(access.index.*);

        // Index should be an integer
        if (!idx_type.isInteger()) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "index must be an integer",
                .span = span,
            });
        }

        // For generic types like List[T], return T
        switch (obj_type.kind) {
            .generic => |gen| {
                if (gen.args.len > 0) {
                    return gen.args[0];
                }
            },
            else => {},
        }

        // Default to returning an unknown type
        return &Type.builtins.error_type;
    }

    fn inferMethodCall(self: *TypeChecker, call: Ast.MethodCall, span: Span) !*const Type.Type {
        const obj_type = try self.inferExpression(call.object.*);

        // Infer argument types (for error checking)
        for (call.args) |arg| {
            _ = try self.inferExpression(arg);
        }

        // Look up method in type's interface
        _ = obj_type;
        _ = span;

        // For now, return error type (proper method resolution would need interface lookup)
        return &Type.builtins.error_type;
    }

    fn inferBinaryOp(self: *TypeChecker, bin: Ast.BinaryExpr, span: Span) !*const Type.Type {
        const left_type = try self.inferExpression(bin.left.*);
        const right_type = try self.inferExpression(bin.right.*);

        return switch (bin.op) {
            // Arithmetic operators return the common numeric type
            .add, .sub, .mul, .div, .mod => blk: {
                if (!left_type.isNumeric() or !right_type.isNumeric()) {
                    try self.ctx.addError(.{
                        .kind = .type_mismatch,
                        .message = "arithmetic operator requires numeric operands",
                        .span = span,
                    });
                    break :blk &Type.builtins.error_type;
                }
                // Return the "wider" type
                if (left_type.isFloat() or right_type.isFloat()) {
                    break :blk &Type.builtins.f64_type;
                }
                break :blk left_type;
            },

            // Comparison operators return bool
            .eq, .ne, .lt, .le, .gt, .ge => &Type.builtins.bool_type,

            // Set operations
            .in_op => &Type.builtins.bool_type,
            .union_op, .intersect => left_type, // Returns same type as operands
            .subset => &Type.builtins.bool_type,
        };
    }

    fn inferUnaryOp(self: *TypeChecker, un: Ast.UnaryExpr, span: Span) !*const Type.Type {
        const operand_type = try self.inferExpression(un.operand.*);

        return switch (un.op) {
            .neg => blk: {
                if (!operand_type.isNumeric()) {
                    try self.ctx.addError(.{
                        .kind = .type_mismatch,
                        .message = "negation requires numeric operand",
                        .span = span,
                    });
                    break :blk &Type.builtins.error_type;
                }
                break :blk operand_type;
            },
            .not => blk: {
                if (!operand_type.eql(&Type.builtins.bool_type)) {
                    try self.ctx.addError(.{
                        .kind = .type_mismatch,
                        .message = "logical not requires boolean operand",
                        .span = span,
                    });
                    break :blk &Type.builtins.error_type;
                }
                break :blk &Type.builtins.bool_type;
            },
        };
    }

    fn inferLogical(self: *TypeChecker, left: *const Ast.Expression, right: *const Ast.Expression, span: Span) !*const Type.Type {
        const left_type = try self.inferExpression(left.*);
        const right_type = try self.inferExpression(right.*);

        if (!left_type.eql(&Type.builtins.bool_type) or !right_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "logical operator requires boolean operands",
                .span = span,
            });
        }

        return &Type.builtins.bool_type;
    }

    fn inferNot(self: *TypeChecker, operand: *const Ast.Expression, span: Span) !*const Type.Type {
        const operand_type = try self.inferExpression(operand.*);

        if (!operand_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "logical not requires boolean operand",
                .span = span,
            });
        }

        return &Type.builtins.bool_type;
    }

    fn inferQuantifier(self: *TypeChecker, quant: Ast.QuantifierExpr, span: Span) !*const Type.Type {
        try self.ctx.pushScope(.quantifier);
        defer self.ctx.popScope();

        // Bind quantified variables
        for (quant.variables) |qvar| {
            const var_type = switch (qvar.domain) {
                .type_domain => |type_expr| try self.resolveTypeExpr(type_expr),
                .range => &Type.builtins.i64_type,
                .collection => |coll| blk: {
                    const coll_type = try self.inferExpression(coll.*);
                    // Extract element type from collection
                    switch (coll_type.kind) {
                        .generic => |gen| {
                            if (gen.args.len > 0) {
                                break :blk gen.args[0];
                            }
                        },
                        else => {},
                    }
                    break :blk &Type.builtins.error_type;
                },
            };
            try self.ctx.defineVariable(qvar.name.name, var_type);
        }

        // Check optional condition
        if (quant.condition) |cond| {
            const cond_type = try self.inferExpression(cond.*);
            if (!cond_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "quantifier condition must be boolean",
                    .span = span,
                });
            }
        }

        // Check body
        const body_type = try self.inferExpression(quant.body.*);
        if (!body_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "quantifier body must be boolean",
                .span = span,
            });
        }

        return &Type.builtins.bool_type;
    }

    fn inferIfExpr(self: *TypeChecker, if_e: Ast.IfExpr, span: Span) !*const Type.Type {
        const cond_type = try self.inferExpression(if_e.condition.*);
        if (!cond_type.eql(&Type.builtins.bool_type)) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "if condition must be boolean",
                .span = span,
            });
        }

        const then_type = try self.inferExpression(if_e.then_branch.*);

        if (if_e.else_branch) |else_branch| {
            const else_type = try self.inferExpression(else_branch.*);
            if (!then_type.eql(else_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "if branches must have same type",
                    .span = span,
                });
            }
        }

        return then_type;
    }

    fn inferMatchExpr(self: *TypeChecker, match_e: Ast.MatchExpr, span: Span) !*const Type.Type {
        _ = try self.inferExpression(match_e.scrutinee.*);

        var result_type: ?*const Type.Type = null;

        for (match_e.arms) |arm| {
            try self.ctx.pushScope(.block);
            defer self.ctx.popScope();

            // Bind pattern variables
            try self.bindPattern(arm.pattern);

            // Check guard if present
            if (arm.guard) |guard| {
                const guard_type = try self.inferExpression(guard.*);
                if (!guard_type.eql(&Type.builtins.bool_type)) {
                    try self.ctx.addError(.{
                        .kind = .type_mismatch,
                        .message = "match guard must be boolean",
                        .span = span,
                    });
                }
            }

            // Infer body type
            const body_type = try self.inferExpression(arm.body.*);

            if (result_type) |rt| {
                if (!rt.eql(body_type)) {
                    try self.ctx.addError(.{
                        .kind = .type_mismatch,
                        .message = "match arms must have same type",
                        .span = span,
                    });
                }
            } else {
                result_type = body_type;
            }
        }

        return result_type orelse &Type.builtins.error_type;
    }

    fn inferLetExpr(self: *TypeChecker, let_e: Ast.LetExpr, span: Span) !*const Type.Type {
        _ = span;

        const value_type = try self.inferExpression(let_e.value.*);

        // Check type annotation if present
        if (let_e.type_annotation) |annot| {
            const annot_type = try self.resolveTypeExpr(annot);
            if (!value_type.eql(annot_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "let value type doesn't match annotation",
                    .span = let_e.value.span,
                });
            }
        }

        try self.ctx.pushScope(.let_binding);
        defer self.ctx.popScope();

        try self.ctx.defineVariable(let_e.name.name, value_type);
        return self.inferExpression(let_e.body.*);
    }

    fn inferCall(self: *TypeChecker, call: Ast.CallExpr, span: Span) !*const Type.Type {
        const callee_type = try self.inferExpression(call.callee.*);

        // Infer argument types
        for (call.args) |arg| {
            _ = try self.inferExpression(arg);
        }

        switch (callee_type.kind) {
            .function => |func| {
                // Check argument count
                if (call.args.len != func.params.len) {
                    try self.ctx.addError(.{
                        .kind = .wrong_arity,
                        .message = "wrong number of arguments",
                        .span = span,
                    });
                }
                return func.return_type;
            },
            else => {
                try self.ctx.addError(.{
                    .kind = .invalid_operation,
                    .message = "cannot call non-function type",
                    .span = span,
                });
                return &Type.builtins.error_type;
            },
        }
    }

    fn inferLambda(self: *TypeChecker, lambda: Ast.LambdaExpr, span: Span) !*const Type.Type {
        try self.ctx.pushScope(.function);
        defer self.ctx.popScope();

        const params = try self.allocator.alloc(*const Type.Type, lambda.params.len);
        for (lambda.params, 0..) |param, i| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            params[i] = param_type;
            try self.ctx.defineVariable(param.name.name, param_type);
        }

        const body_type = try self.inferExpression(lambda.body.*);

        return try self.ctx.intern(Type.Type{
            .kind = .{ .function = .{ .params = params, .return_type = body_type } },
            .span = span,
        });
    }

    fn inferResult(self: *TypeChecker, span: Span) !*const Type.Type {
        // Look up return type from function scope
        var i: usize = self.ctx.scopes.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.ctx.scopes.items[i];
            if (scope.kind == .function) {
                if (scope.return_type) |rt| {
                    return rt;
                }
            }
        }

        try self.ctx.addError(.{
            .kind = .invalid_operation,
            .message = "'result' used outside of postcondition",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn inferSelf(self: *TypeChecker, span: Span) !*const Type.Type {
        // Look up self type from interface/impl scope
        var i: usize = self.ctx.scopes.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.ctx.scopes.items[i];
            if (scope.self_type) |st| {
                return st;
            }
        }

        try self.ctx.addError(.{
            .kind = .invalid_operation,
            .message = "'self' used outside of interface context",
            .span = span,
        });
        return &Type.builtins.error_type;
    }

    fn inferSequenceLiteral(self: *TypeChecker, elems: []const Ast.Expression, span: Span) !*const Type.Type {
        if (elems.len == 0) {
            // Empty sequence needs type annotation
            return &Type.builtins.error_type;
        }

        const elem_type = try self.inferExpression(elems[0]);

        // Check all elements have same type
        for (elems[1..]) |elem| {
            const t = try self.inferExpression(elem);
            if (!t.eql(elem_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "sequence elements must have same type",
                    .span = span,
                });
            }
        }

        // Return List[T] type
        const base = try self.ctx.intern(Type.Type{
            .kind = .{ .named = .{ .name = "List" } },
        });
        const args = try self.allocator.alloc(*const Type.Type, 1);
        args[0] = elem_type;

        return try self.ctx.intern(Type.Type{
            .kind = .{ .generic = .{ .base = base, .args = args } },
            .span = span,
        });
    }

    fn inferSetLiteral(self: *TypeChecker, elems: []const Ast.Expression, span: Span) !*const Type.Type {
        if (elems.len == 0) {
            return &Type.builtins.error_type;
        }

        const elem_type = try self.inferExpression(elems[0]);

        for (elems[1..]) |elem| {
            const t = try self.inferExpression(elem);
            if (!t.eql(elem_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "set elements must have same type",
                    .span = span,
                });
            }
        }

        const base = try self.ctx.intern(Type.Type{
            .kind = .{ .named = .{ .name = "Set" } },
        });
        const args = try self.allocator.alloc(*const Type.Type, 1);
        args[0] = elem_type;

        return try self.ctx.intern(Type.Type{
            .kind = .{ .generic = .{ .base = base, .args = args } },
            .span = span,
        });
    }

    fn inferMapLiteral(self: *TypeChecker, entries: []const Ast.MapEntry, span: Span) !*const Type.Type {
        if (entries.len == 0) {
            return &Type.builtins.error_type;
        }

        const key_type = try self.inferExpression(entries[0].key);
        const val_type = try self.inferExpression(entries[0].value);

        for (entries[1..]) |entry| {
            const kt = try self.inferExpression(entry.key);
            const vt = try self.inferExpression(entry.value);
            if (!kt.eql(key_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "map keys must have same type",
                    .span = span,
                });
            }
            if (!vt.eql(val_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "map values must have same type",
                    .span = span,
                });
            }
        }

        const base = try self.ctx.intern(Type.Type{
            .kind = .{ .named = .{ .name = "Map" } },
        });
        const args = try self.allocator.alloc(*const Type.Type, 2);
        args[0] = key_type;
        args[1] = val_type;

        return try self.ctx.intern(Type.Type{
            .kind = .{ .generic = .{ .base = base, .args = args } },
            .span = span,
        });
    }

    fn inferSetComprehension(self: *TypeChecker, comp: Ast.SetComprehension, span: Span) !*const Type.Type {
        try self.ctx.pushScope(.quantifier);
        defer self.ctx.popScope();

        // Infer domain type
        const domain_type = try self.inferExpression(comp.domain.*);

        // Extract element type from domain
        const var_type: *const Type.Type = switch (domain_type.kind) {
            .generic => |gen| if (gen.args.len > 0) gen.args[0] else &Type.builtins.error_type,
            else => &Type.builtins.error_type,
        };

        try self.ctx.defineVariable(comp.variable.name, var_type);

        // Check condition if present
        if (comp.condition) |cond| {
            const cond_type = try self.inferExpression(cond.*);
            if (!cond_type.eql(&Type.builtins.bool_type)) {
                try self.ctx.addError(.{
                    .kind = .type_mismatch,
                    .message = "comprehension condition must be boolean",
                    .span = span,
                });
            }
        }

        // Infer element expression type
        const elem_type = try self.inferExpression(comp.element.*);

        const base = try self.ctx.intern(Type.Type{
            .kind = .{ .named = .{ .name = "Set" } },
        });
        const args = try self.allocator.alloc(*const Type.Type, 1);
        args[0] = elem_type;

        return try self.ctx.intern(Type.Type{
            .kind = .{ .generic = .{ .base = base, .args = args } },
            .span = span,
        });
    }

    fn inferRange(self: *TypeChecker, range: Ast.RangeExpr, span: Span) !*const Type.Type {
        const start_type = try self.inferExpression(range.start.*);
        const end_type = try self.inferExpression(range.end.*);

        if (!start_type.isInteger() or !end_type.isInteger()) {
            try self.ctx.addError(.{
                .kind = .type_mismatch,
                .message = "range bounds must be integers",
                .span = span,
            });
        }

        // Range is iterable, returns the element type for context
        return start_type;
    }

    // ========================================================================
    // Pattern Binding
    // ========================================================================

    fn bindPattern(self: *TypeChecker, pattern: Ast.Pattern) !void {
        switch (pattern.kind) {
            .wildcard => {},
            .binding => |ident| {
                // Bind to error type for now (proper pattern typing needs more context)
                try self.ctx.defineVariable(ident.name, &Type.builtins.error_type);
            },
            .literal => {},
            .constructor => |ctor| {
                for (ctor.args) |arg| {
                    try self.bindPattern(arg);
                }
            },
            .tuple => |elems| {
                for (elems) |elem| {
                    try self.bindPattern(elem);
                }
            },
            .record => |fields| {
                for (fields) |field| {
                    if (field.pattern) |p| {
                        try self.bindPattern(p.*);
                    } else {
                        try self.ctx.defineVariable(field.name.name, &Type.builtins.error_type);
                    }
                }
            },
        }
    }

    // ========================================================================
    // Conversion Helpers
    // ========================================================================

    fn convertTypeDefinition(self: *TypeChecker, ast_def: Ast.TypeDefinition) !*const Type.TypeDefinition {
        const def = try self.allocator.create(Type.TypeDefinition);

        const type_params = try self.allocator.alloc(Type.TypeParam, ast_def.type_params.len);
        for (ast_def.type_params, 0..) |param, i| {
            type_params[i] = .{
                .name = param.name.name,
                .bounds = try self.convertBounds(param.bounds),
            };
        }

        def.* = .{
            .name = ast_def.name.name,
            .type_params = type_params,
            .body = switch (ast_def.body) {
                .alias => |type_expr| .{ .alias = try self.resolveTypeExpr(type_expr) },
                .product => |fields| blk: {
                    const converted = try self.allocator.alloc(Type.Field, fields.len);
                    for (fields, 0..) |field, i| {
                        converted[i] = .{
                            .name = field.name.name,
                            .field_type = try self.resolveTypeExpr(field.type_expr),
                            .span = field.span,
                        };
                    }
                    break :blk .{ .product = converted };
                },
                .sum => |variants| blk: {
                    const converted = try self.allocator.alloc(Type.Variant, variants.len);
                    for (variants, 0..) |variant, i| {
                        const variant_fields = try self.allocator.alloc(Type.Field, variant.fields.len);
                        for (variant.fields, 0..) |field, j| {
                            variant_fields[j] = .{
                                .name = field.name.name,
                                .field_type = try self.resolveTypeExpr(field.type_expr),
                                .span = field.span,
                            };
                        }
                        converted[i] = .{
                            .name = variant.name.name,
                            .fields = variant_fields,
                            .span = variant.span,
                        };
                    }
                    break :blk .{ .sum = converted };
                },
            },
            .span = ast_def.span,
        };

        return def;
    }

    fn convertModelDefinition(self: *TypeChecker, ast_def: Ast.ModelDefinition) !*const Type.ModelDefinition {
        const def = try self.allocator.create(Type.ModelDefinition);

        const type_params = try self.allocator.alloc(Type.TypeParam, ast_def.type_params.len);
        for (ast_def.type_params, 0..) |param, i| {
            type_params[i] = .{
                .name = param.name.name,
                .bounds = try self.convertBounds(param.bounds),
            };
        }

        const members = try self.allocator.alloc(Type.ModelMember, ast_def.members.len);
        for (ast_def.members, 0..) |member, i| {
            members[i] = switch (member) {
                .signature => |sig| .{
                    .signature = .{
                        .name = sig.name.name,
                        .return_type = try self.resolveTypeExpr(sig.return_type),
                        .span = sig.span,
                    },
                },
                .axiom => |axiom| .{
                    .axiom = .{
                        .name = axiom.name.name,
                        .condition = &axiom.condition,
                        .span = axiom.span,
                    },
                },
            };
        }

        def.* = .{
            .name = ast_def.name.name,
            .type_params = type_params,
            .members = members,
            .span = ast_def.span,
        };

        return def;
    }

    fn convertInterfaceDefinition(self: *TypeChecker, ast_spec: Ast.InterfaceSpec) !*const InterfaceDefinition {
        const def = try self.allocator.create(InterfaceDefinition);

        const type_params = try self.allocator.alloc(Type.TypeParam, ast_spec.type_params.len);
        for (ast_spec.type_params, 0..) |param, i| {
            type_params[i] = .{
                .name = param.name.name,
                .bounds = try self.convertBounds(param.bounds),
            };
        }

        const super_traits = try self.allocator.alloc([]const u8, ast_spec.super_traits.len);
        for (ast_spec.super_traits, 0..) |st, i| {
            super_traits[i] = try self.qualifiedNameToString(st);
        }

        def.* = .{
            .name = ast_spec.name.name,
            .type_params = type_params,
            .super_traits = super_traits,
            .span = ast_spec.span,
        };

        return def;
    }

    fn convertBounds(self: *TypeChecker, ast_bounds: []const Ast.QualifiedName) ![]const Type.Bound {
        const bounds = try self.allocator.alloc(Type.Bound, ast_bounds.len);
        for (ast_bounds, 0..) |qname, i| {
            bounds[i] = .{
                .trait_name = try self.qualifiedNameToString(qname),
            };
        }
        return bounds;
    }

    fn qualifiedNameToString(self: *TypeChecker, qname: Ast.QualifiedName) ![]const u8 {
        if (qname.parts.len == 1) {
            return qname.parts[0].name;
        }

        var len: usize = 0;
        for (qname.parts) |part| {
            len += part.name.len;
        }
        len += qname.parts.len - 1; // dots

        const buf = try self.allocator.alloc(u8, len);
        var pos: usize = 0;

        for (qname.parts, 0..) |part, i| {
            if (i > 0) {
                buf[pos] = '.';
                pos += 1;
            }
            @memcpy(buf[pos .. pos + part.name.len], part.name);
            pos += part.name.len;
        }

        return buf;
    }
};

// ============================================================================
// Type Check Result
// ============================================================================

/// Result of type checking
pub const TypeCheckResult = struct {
    success: bool,
    errors: []const TypeError,
};

// ============================================================================
// Tests
// ============================================================================

test "type checker initialization" {
    const testing = std.testing;
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const checker = TypeChecker.init(&ctx);
    _ = checker;
}
