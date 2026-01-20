//! SMT Encoder
//!
//! This module encodes Sanna types and expressions into SMT-LIB format.
//! It handles the translation from the Sanna type system and AST
//! to the SMT-LIB representation that can be sent to Z3.
//!
//! ## Type Mapping
//!
//! | Sanna Type | SMT-LIB Sort |
//! |------------|--------------|
//! | bool       | Bool         |
//! | i8-i64     | Int          |
//! | u8-u64     | Int          |
//! | int, nat   | Int          |
//! | f32, f64   | Real         |
//! | string     | String       |
//! | List[T]    | (Array Int T) + length |
//! | Set[T]     | (Array T Bool) |
//! | Option[T]  | ADT          |
//!
//! ## Special Handling
//!
//! - `old(x)`: Renamed to `x_pre` to represent pre-state
//! - `result`: Maps to `_result_` variable
//! - Bounded integers: Constraints added (0 <= x < 2^n for unsigned)

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtSort = SmtTypes.SmtSort;
const SmtExpr = SmtTypes.SmtExpr;
const SmtDecl = SmtTypes.SmtDecl;
const BoundVar = SmtTypes.BoundVar;
const Ast = @import("../parser/root.zig").Ast;
const types = @import("../types/root.zig");
const Type = types.Type.Type;
const IntType = types.IntType;
const FloatType = types.FloatType;

// ============================================================================
// SMT Encoder
// ============================================================================

/// Encoder for translating Sanna to SMT-LIB
pub const SmtEncoder = struct {
    allocator: Allocator,
    /// Arena for SMT expression allocation
    arena: std.heap.ArenaAllocator,
    /// Variable name mappings (for old() renaming, etc.)
    var_mappings: std.StringHashMapUnmanaged([]const u8),
    /// Generated declarations
    declarations: std.ArrayListUnmanaged(SmtDecl),
    /// Whether we're in a pre-state context (inside old())
    in_pre_state: bool,
    /// Whether we're in a postcondition context (result available)
    in_postcondition: bool,
    /// Errors encountered during encoding
    errors: std.ArrayListUnmanaged(EncodingError),

    pub fn init(allocator: Allocator) SmtEncoder {
        return .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .var_mappings = .{},
            .declarations = .{},
            .in_pre_state = false,
            .in_postcondition = false,
            .errors = .{},
        };
    }

    pub fn deinit(self: *SmtEncoder) void {
        self.arena.deinit();
        self.var_mappings.deinit(self.allocator);
        self.declarations.deinit(self.allocator);
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    /// Get the arena allocator for SMT node allocation
    pub fn arenaAllocator(self: *SmtEncoder) Allocator {
        return self.arena.allocator();
    }

    // ========================================================================
    // Type Encoding
    // ========================================================================

    /// Encode a Sanna type to an SMT sort
    pub fn encodeType(self: *SmtEncoder, sanna_type: *const Type) !*const SmtSort {
        const arena = self.arenaAllocator();
        const sort = try arena.create(SmtSort);

        sort.* = switch (sanna_type.kind) {
            .bool_type => SmtSort.boolean(),
            .int => |int_type| blk: {
                // All integers map to Int sort (with constraints for bounded)
                _ = int_type;
                break :blk SmtSort.integer();
            },
            .float => SmtSort.real(),
            .string_type => SmtSort.string(),
            .char_type => SmtSort.integer(), // Char as Int
            .unit => SmtSort.boolean(), // Unit as trivially true
            .optional => |inner| blk: {
                // Option[T] as (Maybe T) - using ADT
                const inner_sort = try self.encodeType(inner);
                const params = try arena.alloc(*const SmtSort, 1);
                params[0] = inner_sort;
                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = "Maybe",
                            .params = params,
                        },
                    },
                };
            },
            .tuple => |tuple| blk: {
                // Tuple as a parameterized sort
                const params = try arena.alloc(*const SmtSort, tuple.elements.len);
                for (tuple.elements, 0..) |elem, i| {
                    params[i] = try self.encodeType(elem);
                }
                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = "Tuple",
                            .params = params,
                        },
                    },
                };
            },
            .named => |named| blk: {
                // Handle known named types
                if (std.mem.eql(u8, named.name, "List")) {
                    // List[T] -> (Array Int T)
                    break :blk SmtSort.named("List");
                } else if (std.mem.eql(u8, named.name, "Set")) {
                    // Set[T] -> (Array T Bool)
                    break :blk SmtSort.named("Set");
                } else if (std.mem.eql(u8, named.name, "Map")) {
                    break :blk SmtSort.named("Map");
                } else {
                    // Other named types
                    break :blk SmtSort.named(named.name);
                }
            },
            .generic => |generic| blk: {
                // Handle generic type applications
                const base_sort = try self.encodeType(generic.base);
                const params = try arena.alloc(*const SmtSort, generic.args.len);
                for (generic.args, 0..) |arg, i| {
                    params[i] = try self.encodeType(arg);
                }

                // Check for special types
                if (base_sort.kind == .named) {
                    const name = base_sort.kind.named;
                    if (std.mem.eql(u8, name, "List")) {
                        // List[T] -> (Array Int T)
                        const int_sort = try arena.create(SmtSort);
                        int_sort.* = SmtSort.integer();
                        break :blk SmtSort.array(int_sort, params[0]);
                    } else if (std.mem.eql(u8, name, "Set")) {
                        // Set[T] -> (Array T Bool)
                        const bool_sort = try arena.create(SmtSort);
                        bool_sort.* = SmtSort.boolean();
                        break :blk SmtSort.array(params[0], bool_sort);
                    }
                }

                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = if (base_sort.kind == .named) base_sort.kind.named else "Unknown",
                            .params = params,
                        },
                    },
                };
            },
            .type_var => |tv| SmtSort.named(tv.name),
            .model => |m| SmtSort.named(m.name),
            .function => SmtSort.named("Function"),
            .result => SmtSort.named("Result"),
            .self_type => SmtSort.named("Self"),
            .hole, .err => SmtSort.named("Unknown"),
        };

        return sort;
    }

    /// Get integer bounds constraints for a bounded integer type
    pub fn getIntegerBounds(self: *SmtEncoder, int_type: IntType, var_expr: *const SmtExpr) !?*const SmtExpr {
        if (int_type.bits == 0) {
            // Arbitrary precision - no bounds needed
            // But for nat, we need non-negativity
            if (!int_type.signed) {
                const arena = self.arenaAllocator();
                const zero = try arena.create(SmtExpr);
                zero.* = SmtExpr.intLit(0);
                const constraint = try arena.create(SmtExpr);
                constraint.* = SmtExpr.geExpr(var_expr, zero);
                return constraint;
            }
            return null;
        }

        const arena = self.arenaAllocator();

        if (int_type.signed) {
            // Signed: -2^(n-1) <= x < 2^(n-1)
            const half_range: i64 = @as(i64, 1) << @intCast(int_type.bits - 1);
            const min_val = try arena.create(SmtExpr);
            min_val.* = SmtExpr.intLit(-half_range);
            const max_val = try arena.create(SmtExpr);
            max_val.* = SmtExpr.intLit(half_range);

            const lower = try arena.create(SmtExpr);
            lower.* = SmtExpr.geExpr(var_expr, min_val);
            const upper = try arena.create(SmtExpr);
            upper.* = SmtExpr.ltExpr(var_expr, max_val);

            const operands = try arena.alloc(*const SmtExpr, 2);
            operands[0] = lower;
            operands[1] = upper;

            const result = try arena.create(SmtExpr);
            result.* = SmtExpr.andExpr(operands);
            return result;
        } else {
            // Unsigned: 0 <= x < 2^n
            const max_val_int: i64 = @as(i64, 1) << @intCast(int_type.bits);
            const zero = try arena.create(SmtExpr);
            zero.* = SmtExpr.intLit(0);
            const max_val = try arena.create(SmtExpr);
            max_val.* = SmtExpr.intLit(max_val_int);

            const lower = try arena.create(SmtExpr);
            lower.* = SmtExpr.geExpr(var_expr, zero);
            const upper = try arena.create(SmtExpr);
            upper.* = SmtExpr.ltExpr(var_expr, max_val);

            const operands = try arena.alloc(*const SmtExpr, 2);
            operands[0] = lower;
            operands[1] = upper;

            const result = try arena.create(SmtExpr);
            result.* = SmtExpr.andExpr(operands);
            return result;
        }
    }

    // ========================================================================
    // Expression Encoding
    // ========================================================================

    /// Encode a Sanna expression to an SMT expression
    pub fn encodeExpression(self: *SmtEncoder, expr: *const Ast.Expression) !*const SmtExpr {
        const arena = self.arenaAllocator();
        const result = try arena.create(SmtExpr);

        result.* = switch (expr.kind) {
            // Literals
            .int_literal => |v| SmtExpr.intLit(v),
            .float_literal => |v| SmtExpr.realLit(v),
            .string_literal => |v| SmtExpr.stringLit(v),
            .bool_literal => |v| SmtExpr.boolLit(v),

            // Identifiers
            .identifier => |ident| blk: {
                const name = self.mapVariableName(ident.name);
                // We need a sort for the variable - use Int as default
                // In practice, this should be looked up from context
                break :blk SmtExpr.constant(name);
            },

            .qualified => |qn| blk: {
                // Build qualified name string
                var name_buf = std.ArrayListUnmanaged(u8){};
                for (qn.parts, 0..) |part, i| {
                    if (i > 0) try name_buf.append(arena, '.');
                    try name_buf.appendSlice(arena, part.name);
                }
                break :blk SmtExpr.constant(try name_buf.toOwnedSlice(arena));
            },

            // Binary operations
            .binary => |bin| blk: {
                const left = try self.encodeExpression(bin.left);
                const right = try self.encodeExpression(bin.right);
                break :blk try self.encodeBinaryOp(bin.op, left, right);
            },

            // Unary operations
            .unary => |un| blk: {
                const operand = try self.encodeExpression(un.operand);
                break :blk switch (un.op) {
                    .neg => blk2: {
                        const neg_result = try arena.create(SmtExpr);
                        neg_result.* = SmtExpr{ .kind = .{ .neg = operand } };
                        break :blk2 neg_result.*;
                    },
                    .not => SmtExpr.notExpr(operand),
                };
            },

            // Logical operations
            .and_expr => |and_e| blk: {
                const left = try self.encodeExpression(and_e.left);
                const right = try self.encodeExpression(and_e.right);
                const operands = try arena.alloc(*const SmtExpr, 2);
                operands[0] = left;
                operands[1] = right;
                break :blk SmtExpr.andExpr(operands);
            },

            .or_expr => |or_e| blk: {
                const left = try self.encodeExpression(or_e.left);
                const right = try self.encodeExpression(or_e.right);
                const operands = try arena.alloc(*const SmtExpr, 2);
                operands[0] = left;
                operands[1] = right;
                break :blk SmtExpr.orExpr(operands);
            },

            .not_expr => |not_e| SmtExpr.notExpr(try self.encodeExpression(not_e.operand)),

            .implies => |imp| SmtExpr.impliesExpr(
                try self.encodeExpression(imp.antecedent),
                try self.encodeExpression(imp.consequent),
            ),

            .iff => |iff_e| SmtExpr.iffExpr(
                try self.encodeExpression(iff_e.left),
                try self.encodeExpression(iff_e.right),
            ),

            // Quantifiers
            .forall => |q| blk: {
                const bound_vars = try self.encodeQuantifierVars(q.variables);
                const body = try self.encodeExpression(q.body);
                break :blk SmtExpr.forallExpr(bound_vars, body);
            },

            .exists => |q| blk: {
                const bound_vars = try self.encodeQuantifierVars(q.variables);
                const body = try self.encodeExpression(q.body);
                break :blk SmtExpr.existsExpr(bound_vars, body);
            },

            // Conditional
            .if_expr => |if_e| blk: {
                const cond = try self.encodeExpression(if_e.condition);
                const then_branch = try self.encodeExpression(if_e.then_branch);
                const else_branch = if (if_e.else_branch) |eb|
                    try self.encodeExpression(eb)
                else blk2: {
                    // Default else branch - false for bool, 0 for int
                    const default = try arena.create(SmtExpr);
                    default.* = SmtExpr.boolLit(false);
                    break :blk2 default;
                };
                break :blk SmtExpr.iteExpr(cond, then_branch, else_branch);
            },

            // Specification-specific
            .old => |inner| blk: {
                // Enter pre-state context
                const was_in_pre_state = self.in_pre_state;
                self.in_pre_state = true;
                defer self.in_pre_state = was_in_pre_state;

                break :blk (try self.encodeExpression(inner)).*;
            },

            .result => SmtExpr.constant("_result_"),

            .self_expr => SmtExpr.constant("self"),

            // Function call
            .call => |call| blk: {
                const func_name = switch (call.callee.kind) {
                    .identifier => |id| id.name,
                    .qualified => |qn| if (qn.parts.len > 0) qn.parts[qn.parts.len - 1].name else "unknown",
                    else => "unknown",
                };

                const args = try arena.alloc(*const SmtExpr, call.args.len);
                for (call.args, 0..) |arg, i| {
                    args[i] = try self.encodeExpression(&arg);
                }
                break :blk SmtExpr.applyExpr(func_name, args);
            },

            // Method call
            .method_call => |mc| blk: {
                // Encode as function call with receiver as first argument
                const receiver = try self.encodeExpression(mc.object);
                const args = try arena.alloc(*const SmtExpr, mc.args.len + 1);
                args[0] = receiver;
                for (mc.args, 0..) |arg, i| {
                    args[i + 1] = try self.encodeExpression(&arg);
                }
                break :blk SmtExpr.applyExpr(mc.method.name, args);
            },

            // Field access
            .field_access => |fa| blk: {
                const obj = try self.encodeExpression(fa.object);
                const field_name = try std.fmt.allocPrint(arena, "{s}.{s}", .{
                    if (obj.kind == .constant) obj.kind.constant else "obj",
                    fa.field.name,
                });
                break :blk SmtExpr.constant(field_name);
            },

            // Index access
            .index_access => |ia| blk: {
                const arr = try self.encodeExpression(ia.object);
                const idx = try self.encodeExpression(ia.index);
                break :blk SmtExpr.selectExpr(arr, idx);
            },

            // Let expression
            .let_expr => |let_e| blk: {
                const value = try self.encodeExpression(let_e.value);
                const body = try self.encodeExpression(let_e.body);

                const bindings = try arena.alloc(SmtTypes.LetBinding, 1);
                bindings[0] = .{
                    .name = let_e.name.name,
                    .value = value,
                };

                break :blk SmtExpr{
                    .kind = .{
                        .let_expr = .{
                            .bindings = bindings,
                            .body = body,
                        },
                    },
                };
            },

            // Collection literals
            .sequence_literal, .set_literal => |elements| blk: {
                // Encode as a constant array with known elements
                // This is a simplification - full encoding would need more work
                if (elements.len == 0) {
                    break :blk SmtExpr.constant("empty_collection");
                }
                // For non-empty, we'd build a store chain
                // store(store(empty, 0, e0), 1, e1)...
                var current = try arena.create(SmtExpr);
                current.* = SmtExpr.constant("empty_array");

                for (elements, 0..) |elem, i| {
                    const idx = try arena.create(SmtExpr);
                    idx.* = SmtExpr.intLit(@intCast(i));
                    const val = try self.encodeExpression(&elem);
                    const store = try arena.create(SmtExpr);
                    store.* = SmtExpr.storeExpr(current, idx, val);
                    current = store;
                }
                break :blk current.*;
            },

            // Range
            .range => |r| blk: {
                // Encode range as a pair (start, end)
                const start = try self.encodeExpression(r.start);
                const end = try self.encodeExpression(r.end);
                const args = try arena.alloc(*const SmtExpr, 2);
                args[0] = start;
                args[1] = end;
                break :blk SmtExpr.applyExpr("range", args);
            },

            // Match expression - encode as nested ite
            .match_expr => |m| blk: {
                const scrutinee = try self.encodeExpression(m.scrutinee);
                break :blk try self.encodeMatchExpr(scrutinee, m.arms);
            },

            // Holes - should not appear in verification
            .hole, .typed_hole => SmtExpr.constant("_hole_"),

            // Lambda - encode as uninterpreted function
            .lambda => SmtExpr.constant("_lambda_"),

            // Map literal
            .map_literal => SmtExpr.constant("_map_literal_"),

            // Set comprehension
            .set_comprehension => SmtExpr.constant("_set_comprehension_"),
        };

        return result;
    }

    /// Encode a binary operation
    fn encodeBinaryOp(self: *SmtEncoder, op: Ast.BinaryOp, left: *const SmtExpr, right: *const SmtExpr) !SmtExpr {
        const arena = self.arenaAllocator();

        return switch (op) {
            .add => blk: {
                const operands = try arena.alloc(*const SmtExpr, 2);
                operands[0] = left;
                operands[1] = right;
                break :blk SmtExpr.addExpr(operands);
            },
            .sub => SmtExpr.subExpr(left, right),
            .mul => blk: {
                const operands = try arena.alloc(*const SmtExpr, 2);
                operands[0] = left;
                operands[1] = right;
                break :blk SmtExpr.mulExpr(operands);
            },
            .div => SmtExpr.divExpr(left, right),
            .mod => SmtExpr.modExpr(left, right),
            .eq => SmtExpr.eqExpr(left, right),
            .ne => blk: {
                const eq = try arena.create(SmtExpr);
                eq.* = SmtExpr.eqExpr(left, right);
                break :blk SmtExpr.notExpr(eq);
            },
            .lt => SmtExpr.ltExpr(left, right),
            .le => SmtExpr.leExpr(left, right),
            .gt => SmtExpr.gtExpr(left, right),
            .ge => SmtExpr.geExpr(left, right),
            .in_op => blk: {
                // x in S encoded as (select S x)
                break :blk SmtExpr.selectExpr(right, left);
            },
            .union_op, .intersect, .subset => blk: {
                const op_name = switch (op) {
                    .union_op => "set_union",
                    .intersect => "set_intersect",
                    .subset => "set_subset",
                    else => unreachable,
                };
                const args = try arena.alloc(*const SmtExpr, 2);
                args[0] = left;
                args[1] = right;
                break :blk SmtExpr.applyExpr(op_name, args);
            },
        };
    }

    /// Encode quantifier variables
    fn encodeQuantifierVars(self: *SmtEncoder, vars: []const Ast.QuantifierVar) ![]const BoundVar {
        const arena = self.arenaAllocator();
        const result = try arena.alloc(BoundVar, vars.len);

        for (vars, 0..) |v, i| {
            // Default to Int sort if no domain specified
            const sort = switch (v.domain) {
                .type_domain => |te| try self.encodeTypeExpr(&te),
                .range, .collection => blk: {
                    const int_sort = try arena.create(SmtSort);
                    int_sort.* = SmtSort.integer();
                    break :blk int_sort;
                },
            };

            result[i] = .{
                .name = v.name.name,
                .sort = sort,
            };
        }

        return result;
    }

    /// Encode a type expression to SMT sort
    pub fn encodeTypeExpr(self: *SmtEncoder, te: *const Ast.TypeExpr) !*const SmtSort {
        const arena = self.arenaAllocator();
        const sort = try arena.create(SmtSort);

        sort.* = switch (te.kind) {
            .named => |qn| blk: {
                if (qn.parts.len == 1) {
                    const name = qn.parts[0].name;
                    if (std.mem.eql(u8, name, "bool")) {
                        break :blk SmtSort.boolean();
                    } else if (std.mem.eql(u8, name, "int") or
                        std.mem.eql(u8, name, "nat") or
                        std.mem.eql(u8, name, "i32") or
                        std.mem.eql(u8, name, "i64") or
                        std.mem.eql(u8, name, "u32") or
                        std.mem.eql(u8, name, "u64"))
                    {
                        break :blk SmtSort.integer();
                    } else if (std.mem.eql(u8, name, "real") or
                        std.mem.eql(u8, name, "f32") or
                        std.mem.eql(u8, name, "f64"))
                    {
                        break :blk SmtSort.real();
                    } else if (std.mem.eql(u8, name, "string")) {
                        break :blk SmtSort.string();
                    } else {
                        break :blk SmtSort.named(name);
                    }
                } else {
                    break :blk SmtSort.named("qualified");
                }
            },
            .generic => |gt| blk: {
                const params = try arena.alloc(*const SmtSort, gt.args.len);
                for (gt.args, 0..) |arg, i| {
                    params[i] = try self.encodeTypeExpr(&arg);
                }
                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = if (gt.base.parts.len > 0) gt.base.parts[0].name else "Unknown",
                            .params = params,
                        },
                    },
                };
            },
            .function => SmtSort.named("Function"),
            .optional => |inner| blk: {
                const inner_sort = try self.encodeTypeExpr(inner);
                const params = try arena.alloc(*const SmtSort, 1);
                params[0] = inner_sort;
                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = "Maybe",
                            .params = params,
                        },
                    },
                };
            },
            .result => SmtSort.named("Result"),
            .tuple => |elems| blk: {
                const params = try arena.alloc(*const SmtSort, elems.len);
                for (elems, 0..) |elem, i| {
                    params[i] = try self.encodeTypeExpr(&elem);
                }
                break :blk SmtSort{
                    .kind = .{
                        .parameterized = .{
                            .name = "Tuple",
                            .params = params,
                        },
                    },
                };
            },
            .self_type => SmtSort.named("Self"),
            .hole => SmtSort.named("Unknown"),
        };

        return sort;
    }

    /// Encode a match expression as nested if-then-else
    fn encodeMatchExpr(self: *SmtEncoder, scrutinee: *const SmtExpr, arms: []const Ast.MatchArm) Allocator.Error!SmtExpr {
        const arena = self.arenaAllocator();

        if (arms.len == 0) {
            return SmtExpr.constant("_match_empty_");
        }

        if (arms.len == 1) {
            return (try self.encodeExpression(arms[0].body)).*;
        }

        // Build nested ite from last to first
        var result = try self.encodeExpression(arms[arms.len - 1].body);

        var i: usize = arms.len - 1;
        while (i > 0) {
            i -= 1;
            const arm = arms[i];

            // Encode pattern match condition
            const cond = try self.encodePatternMatch(scrutinee, &arm.pattern);
            const then_branch = try self.encodeExpression(arm.body);

            const ite = try arena.create(SmtExpr);
            ite.* = SmtExpr.iteExpr(cond, then_branch, result);
            result = ite;
        }

        return result.*;
    }

    /// Encode a pattern match as a condition
    fn encodePatternMatch(self: *SmtEncoder, scrutinee: *const SmtExpr, pattern: *const Ast.Pattern) !*const SmtExpr {
        const arena = self.arenaAllocator();
        const result = try arena.create(SmtExpr);

        result.* = switch (pattern.kind) {
            .wildcard => SmtExpr.boolLit(true),
            .binding => SmtExpr.boolLit(true), // Binding always matches
            .literal => |lit| blk: {
                const lit_expr = try arena.create(SmtExpr);
                lit_expr.* = switch (lit) {
                    .int => |v| SmtExpr.intLit(v),
                    .float => |v| SmtExpr.realLit(v),
                    .string => |v| SmtExpr.stringLit(v),
                    .bool => |v| SmtExpr.boolLit(v),
                };
                break :blk SmtExpr.eqExpr(scrutinee, lit_expr);
            },
            .constructor => |ctor| blk: {
                // Check if scrutinee is of this constructor
                const ctor_name = if (ctor.name.parts.len > 0)
                    ctor.name.parts[ctor.name.parts.len - 1].name
                else
                    "Unknown";
                const args = try arena.alloc(*const SmtExpr, 1);
                args[0] = scrutinee;
                break :blk SmtExpr.applyExpr(
                    try std.fmt.allocPrint(arena, "is_{s}", .{ctor_name}),
                    args,
                );
            },
            .tuple, .record => SmtExpr.boolLit(true), // Simplified
        };

        return result;
    }

    /// Map a variable name, handling old() context
    fn mapVariableName(self: *SmtEncoder, name: []const u8) []const u8 {
        if (self.in_pre_state) {
            // In old() context, append _pre suffix
            const mapped = std.fmt.allocPrint(self.arenaAllocator(), "{s}_pre", .{name}) catch name;
            return mapped;
        }

        // Check for explicit mappings
        if (self.var_mappings.get(name)) |mapped| {
            return mapped;
        }

        return name;
    }

    /// Add a variable mapping
    pub fn addVarMapping(self: *SmtEncoder, from: []const u8, to: []const u8) !void {
        try self.var_mappings.put(self.allocator, from, to);
    }

    /// Set postcondition context
    pub fn setPostconditionContext(self: *SmtEncoder, in_postcondition: bool) void {
        self.in_postcondition = in_postcondition;
    }

    /// Add an encoding error
    fn addError(self: *SmtEncoder, message: []const u8) !void {
        try self.errors.append(self.allocator, .{
            .message = try self.allocator.dupe(u8, message),
        });
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const SmtEncoder) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const SmtEncoder) []const EncodingError {
        return self.errors.items;
    }
};

/// An error during SMT encoding
pub const EncodingError = struct {
    message: []const u8,
};

// ============================================================================
// Tests
// ============================================================================

test "SmtEncoder type encoding" {
    const testing = std.testing;
    const types_mod = @import("../types/root.zig");
    const builtins = types_mod.builtins;

    var encoder = SmtEncoder.init(testing.allocator);
    defer encoder.deinit();

    // Test bool type
    const bool_sort = try encoder.encodeType(&builtins.bool_type);
    try testing.expect(bool_sort.kind == .bool_sort);

    // Test int type
    const int_sort = try encoder.encodeType(&builtins.i32_type);
    try testing.expect(int_sort.kind == .int_sort);

    // Test float type
    const float_sort = try encoder.encodeType(&builtins.f64_type);
    try testing.expect(float_sort.kind == .real_sort);

    // Test string type
    const string_sort = try encoder.encodeType(&builtins.string_type);
    try testing.expect(string_sort.kind == .string_sort);
}

test "SmtEncoder integer bounds" {
    const testing = std.testing;

    var encoder = SmtEncoder.init(testing.allocator);
    defer encoder.deinit();

    const arena = encoder.arenaAllocator();
    const var_expr = try arena.create(SmtExpr);
    var_expr.* = SmtExpr.constant("x");

    // Test u8 bounds (0 <= x < 256)
    const u8_bounds = try encoder.getIntegerBounds(IntType.u8_type, var_expr);
    try testing.expect(u8_bounds != null);
    try testing.expect(u8_bounds.?.kind == .and_expr);

    // Test arbitrary precision nat (x >= 0)
    const nat_bounds = try encoder.getIntegerBounds(IntType.nat_type, var_expr);
    try testing.expect(nat_bounds != null);
    try testing.expect(nat_bounds.?.kind == .ge);

    // Test arbitrary precision int (no bounds)
    const int_bounds = try encoder.getIntegerBounds(IntType.int_type, var_expr);
    try testing.expect(int_bounds == null);
}

test "SmtEncoder variable mapping for old()" {
    const testing = std.testing;

    var encoder = SmtEncoder.init(testing.allocator);
    defer encoder.deinit();

    // Without pre-state context
    const normal_name = encoder.mapVariableName("x");
    try testing.expectEqualStrings("x", normal_name);

    // With pre-state context
    encoder.in_pre_state = true;
    const pre_name = encoder.mapVariableName("x");
    try testing.expect(std.mem.endsWith(u8, pre_name, "_pre"));
}
