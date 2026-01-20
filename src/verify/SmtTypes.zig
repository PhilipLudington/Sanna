//! SMT-LIB AST Representation
//!
//! This module defines the Abstract Syntax Tree for SMT-LIB 2.6 format,
//! which is used to communicate with SMT solvers like Z3.
//!
//! The representation focuses on the subset of SMT-LIB needed for
//! verifying Sanna specifications:
//! - Core theory (Bool, equality, ite)
//! - Ints theory (integer arithmetic)
//! - Reals theory (real arithmetic)
//! - ArraysEx theory (arrays with extensionality)
//! - Quantifiers (forall, exists)

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// SMT Sorts (Types)
// ============================================================================

/// An SMT-LIB sort (type)
pub const SmtSort = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        /// Boolean sort
        bool_sort,
        /// Integer sort (mathematical integers, unbounded)
        int_sort,
        /// Real sort (mathematical reals)
        real_sort,
        /// String sort
        string_sort,
        /// Fixed-width bit-vector sort
        bitvec: u32,
        /// Array sort (Array Index Element)
        array: ArraySort,
        /// Named sort (user-defined or from theory)
        named: []const u8,
        /// Parameterized sort (e.g., List Int)
        parameterized: ParameterizedSort,
    };

    pub fn init(kind: Kind) SmtSort {
        return .{ .kind = kind };
    }

    /// Create a boolean sort
    pub fn boolean() SmtSort {
        return .{ .kind = .bool_sort };
    }

    /// Create an integer sort
    pub fn integer() SmtSort {
        return .{ .kind = .int_sort };
    }

    /// Create a real sort
    pub fn real() SmtSort {
        return .{ .kind = .real_sort };
    }

    /// Create a string sort
    pub fn string() SmtSort {
        return .{ .kind = .string_sort };
    }

    /// Create a bit-vector sort with given width
    pub fn bitvec(width: u32) SmtSort {
        return .{ .kind = .{ .bitvec = width } };
    }

    /// Create an array sort
    pub fn array(index_sort: *const SmtSort, element_sort: *const SmtSort) SmtSort {
        return .{ .kind = .{ .array = .{
            .index_sort = index_sort,
            .element_sort = element_sort,
        } } };
    }

    /// Create a named sort
    pub fn named(name: []const u8) SmtSort {
        return .{ .kind = .{ .named = name } };
    }

    /// Check if this sort is equal to another
    pub fn eql(self: *const SmtSort, other: *const SmtSort) bool {
        return switch (self.kind) {
            .bool_sort => other.kind == .bool_sort,
            .int_sort => other.kind == .int_sort,
            .real_sort => other.kind == .real_sort,
            .string_sort => other.kind == .string_sort,
            .bitvec => |w1| switch (other.kind) {
                .bitvec => |w2| w1 == w2,
                else => false,
            },
            .array => |a1| switch (other.kind) {
                .array => |a2| a1.index_sort.eql(a2.index_sort) and
                    a1.element_sort.eql(a2.element_sort),
                else => false,
            },
            .named => |n1| switch (other.kind) {
                .named => |n2| std.mem.eql(u8, n1, n2),
                else => false,
            },
            .parameterized => |p1| switch (other.kind) {
                .parameterized => |p2| blk: {
                    if (!std.mem.eql(u8, p1.name, p2.name)) break :blk false;
                    if (p1.params.len != p2.params.len) break :blk false;
                    for (p1.params, p2.params) |a, b| {
                        if (!a.eql(b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },
        };
    }
};

/// Array sort with index and element sorts
pub const ArraySort = struct {
    index_sort: *const SmtSort,
    element_sort: *const SmtSort,
};

/// Parameterized sort (e.g., List Int, Pair Int Bool)
pub const ParameterizedSort = struct {
    name: []const u8,
    params: []const *const SmtSort,
};

// ============================================================================
// SMT Expressions
// ============================================================================

/// An SMT-LIB expression
pub const SmtExpr = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        // Literals
        bool_lit: bool,
        int_lit: i64,
        real_lit: f64,
        string_lit: []const u8,
        bitvec_lit: BitVecLit,

        // Variables and constants
        variable: Variable,
        constant: []const u8,

        // Core operations
        not: *const SmtExpr,
        and_expr: []const *const SmtExpr,
        or_expr: []const *const SmtExpr,
        implies: BinaryExpr,
        iff: BinaryExpr,
        xor: BinaryExpr,
        ite: IteExpr,

        // Equality and comparison
        eq: BinaryExpr,
        distinct: []const *const SmtExpr,
        lt: BinaryExpr,
        le: BinaryExpr,
        gt: BinaryExpr,
        ge: BinaryExpr,

        // Arithmetic
        neg: *const SmtExpr,
        add: []const *const SmtExpr,
        sub: BinaryExpr,
        mul: []const *const SmtExpr,
        div: BinaryExpr,
        mod: BinaryExpr,
        abs: *const SmtExpr,

        // Array operations
        select: SelectExpr,
        store: StoreExpr,
        const_array: ConstArrayExpr,

        // Quantifiers
        forall: QuantifierExpr,
        exists: QuantifierExpr,

        // Let binding
        let_expr: LetExpr,

        // Function application
        apply: ApplyExpr,

        // Annotated expression (with :named, :pattern, etc.)
        annotated: AnnotatedExpr,
    };

    pub fn init(kind: Kind) SmtExpr {
        return .{ .kind = kind };
    }

    // ========================================================================
    // Convenience constructors
    // ========================================================================

    /// Create a boolean literal
    pub fn boolLit(value: bool) SmtExpr {
        return .{ .kind = .{ .bool_lit = value } };
    }

    /// Create an integer literal
    pub fn intLit(value: i64) SmtExpr {
        return .{ .kind = .{ .int_lit = value } };
    }

    /// Create a real literal
    pub fn realLit(value: f64) SmtExpr {
        return .{ .kind = .{ .real_lit = value } };
    }

    /// Create a string literal
    pub fn stringLit(value: []const u8) SmtExpr {
        return .{ .kind = .{ .string_lit = value } };
    }

    /// Create a variable reference
    pub fn variable(name: []const u8, sort: *const SmtSort) SmtExpr {
        return .{ .kind = .{ .variable = .{ .name = name, .sort = sort } } };
    }

    /// Create a constant reference
    pub fn constant(name: []const u8) SmtExpr {
        return .{ .kind = .{ .constant = name } };
    }

    /// Create a negation
    pub fn notExpr(operand: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .not = operand } };
    }

    /// Create a conjunction
    pub fn andExpr(operands: []const *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .and_expr = operands } };
    }

    /// Create a disjunction
    pub fn orExpr(operands: []const *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .or_expr = operands } };
    }

    /// Create an implication
    pub fn impliesExpr(antecedent: *const SmtExpr, consequent: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .implies = .{ .left = antecedent, .right = consequent } } };
    }

    /// Create an if-and-only-if
    pub fn iffExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .iff = .{ .left = left, .right = right } } };
    }

    /// Create an if-then-else
    pub fn iteExpr(condition: *const SmtExpr, then_expr: *const SmtExpr, else_expr: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .ite = .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_expr,
        } } };
    }

    /// Create an equality
    pub fn eqExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .eq = .{ .left = left, .right = right } } };
    }

    /// Create a less-than comparison
    pub fn ltExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .lt = .{ .left = left, .right = right } } };
    }

    /// Create a less-than-or-equal comparison
    pub fn leExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .le = .{ .left = left, .right = right } } };
    }

    /// Create a greater-than comparison
    pub fn gtExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .gt = .{ .left = left, .right = right } } };
    }

    /// Create a greater-than-or-equal comparison
    pub fn geExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .ge = .{ .left = left, .right = right } } };
    }

    /// Create an addition
    pub fn addExpr(operands: []const *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .add = operands } };
    }

    /// Create a subtraction
    pub fn subExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .sub = .{ .left = left, .right = right } } };
    }

    /// Create a multiplication
    pub fn mulExpr(operands: []const *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .mul = operands } };
    }

    /// Create a division
    pub fn divExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .div = .{ .left = left, .right = right } } };
    }

    /// Create a modulo
    pub fn modExpr(left: *const SmtExpr, right: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .mod = .{ .left = left, .right = right } } };
    }

    /// Create an array select
    pub fn selectExpr(arr: *const SmtExpr, index: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .select = .{ .array = arr, .index = index } } };
    }

    /// Create an array store
    pub fn storeExpr(arr: *const SmtExpr, index: *const SmtExpr, value: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .store = .{ .array = arr, .index = index, .value = value } } };
    }

    /// Create a universal quantifier
    pub fn forallExpr(bound_vars: []const BoundVar, body: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .forall = .{ .bound_vars = bound_vars, .body = body } } };
    }

    /// Create an existential quantifier
    pub fn existsExpr(bound_vars: []const BoundVar, body: *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .exists = .{ .bound_vars = bound_vars, .body = body } } };
    }

    /// Create a function application
    pub fn applyExpr(func_name: []const u8, args: []const *const SmtExpr) SmtExpr {
        return .{ .kind = .{ .apply = .{ .func_name = func_name, .args = args } } };
    }
};

/// A variable with its sort
pub const Variable = struct {
    name: []const u8,
    sort: *const SmtSort,
};

/// A bound variable in a quantifier
pub const BoundVar = struct {
    name: []const u8,
    sort: *const SmtSort,
};

/// Bit-vector literal
pub const BitVecLit = struct {
    value: u64,
    width: u32,
};

/// Binary expression
pub const BinaryExpr = struct {
    left: *const SmtExpr,
    right: *const SmtExpr,
};

/// If-then-else expression
pub const IteExpr = struct {
    condition: *const SmtExpr,
    then_branch: *const SmtExpr,
    else_branch: *const SmtExpr,
};

/// Array select expression
pub const SelectExpr = struct {
    array: *const SmtExpr,
    index: *const SmtExpr,
};

/// Array store expression
pub const StoreExpr = struct {
    array: *const SmtExpr,
    index: *const SmtExpr,
    value: *const SmtExpr,
};

/// Constant array expression (array where all elements have the same value)
pub const ConstArrayExpr = struct {
    sort: *const SmtSort,
    value: *const SmtExpr,
};

/// Quantifier expression
pub const QuantifierExpr = struct {
    bound_vars: []const BoundVar,
    body: *const SmtExpr,
    /// Optional patterns for triggering
    patterns: []const []const *const SmtExpr = &.{},
};

/// Let binding expression
pub const LetExpr = struct {
    bindings: []const LetBinding,
    body: *const SmtExpr,
};

/// A single let binding
pub const LetBinding = struct {
    name: []const u8,
    value: *const SmtExpr,
};

/// Function application expression
pub const ApplyExpr = struct {
    func_name: []const u8,
    args: []const *const SmtExpr,
};

/// Annotated expression
pub const AnnotatedExpr = struct {
    expr: *const SmtExpr,
    annotations: []const Annotation,
};

/// An annotation (e.g., :named, :pattern)
pub const Annotation = struct {
    keyword: []const u8,
    value: ?AnnotationValue = null,
};

/// Annotation value
pub const AnnotationValue = union(enum) {
    symbol: []const u8,
    string: []const u8,
    expr: *const SmtExpr,
    exprs: []const *const SmtExpr,
};

// ============================================================================
// SMT Declarations
// ============================================================================

/// An SMT-LIB declaration
pub const SmtDecl = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        /// Sort declaration: (declare-sort Name Arity)
        sort_decl: SortDecl,
        /// Constant declaration: (declare-const Name Sort)
        const_decl: ConstDecl,
        /// Function declaration: (declare-fun Name (Sorts...) ReturnSort)
        func_decl: FuncDecl,
        /// Sort definition: (define-sort Name (Params...) Sort)
        sort_def: SortDef,
        /// Function definition: (define-fun Name (Params...) ReturnSort Body)
        func_def: FuncDef,
    };

    pub fn init(kind: Kind) SmtDecl {
        return .{ .kind = kind };
    }

    /// Create a sort declaration
    pub fn sortDecl(name: []const u8, arity: u32) SmtDecl {
        return .{ .kind = .{ .sort_decl = .{ .name = name, .arity = arity } } };
    }

    /// Create a constant declaration
    pub fn constDecl(name: []const u8, sort: *const SmtSort) SmtDecl {
        return .{ .kind = .{ .const_decl = .{ .name = name, .sort = sort } } };
    }

    /// Create a function declaration
    pub fn funcDecl(name: []const u8, param_sorts: []const *const SmtSort, return_sort: *const SmtSort) SmtDecl {
        return .{ .kind = .{ .func_decl = .{
            .name = name,
            .param_sorts = param_sorts,
            .return_sort = return_sort,
        } } };
    }

    /// Create a function definition
    pub fn funcDef(
        name: []const u8,
        params: []const BoundVar,
        return_sort: *const SmtSort,
        body: *const SmtExpr,
    ) SmtDecl {
        return .{ .kind = .{ .func_def = .{
            .name = name,
            .params = params,
            .return_sort = return_sort,
            .body = body,
        } } };
    }
};

/// Sort declaration
pub const SortDecl = struct {
    name: []const u8,
    arity: u32,
};

/// Constant declaration
pub const ConstDecl = struct {
    name: []const u8,
    sort: *const SmtSort,
};

/// Function declaration
pub const FuncDecl = struct {
    name: []const u8,
    param_sorts: []const *const SmtSort,
    return_sort: *const SmtSort,
};

/// Sort definition
pub const SortDef = struct {
    name: []const u8,
    params: []const []const u8,
    body: *const SmtSort,
};

/// Function definition
pub const FuncDef = struct {
    name: []const u8,
    params: []const BoundVar,
    return_sort: *const SmtSort,
    body: *const SmtExpr,
};

// ============================================================================
// SMT Commands
// ============================================================================

/// An SMT-LIB command
pub const SmtCommand = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        /// Set logic: (set-logic LOGIC)
        set_logic: []const u8,
        /// Set option: (set-option :keyword value)
        set_option: SetOption,
        /// Declaration
        declaration: SmtDecl,
        /// Assert: (assert expr)
        assert: *const SmtExpr,
        /// Check satisfiability: (check-sat)
        check_sat,
        /// Get model: (get-model)
        get_model,
        /// Get value: (get-value (terms...))
        get_value: []const *const SmtExpr,
        /// Get unsat core: (get-unsat-core)
        get_unsat_core,
        /// Push: (push n)
        push: u32,
        /// Pop: (pop n)
        pop: u32,
        /// Reset: (reset)
        reset,
        /// Exit: (exit)
        exit,
        /// Echo: (echo string)
        echo: []const u8,
    };

    pub fn init(kind: Kind) SmtCommand {
        return .{ .kind = kind };
    }

    /// Create a set-logic command
    pub fn setLogic(logic: []const u8) SmtCommand {
        return .{ .kind = .{ .set_logic = logic } };
    }

    /// Create an assertion
    pub fn assert(expr: *const SmtExpr) SmtCommand {
        return .{ .kind = .{ .assert = expr } };
    }

    /// Create a check-sat command
    pub fn checkSat() SmtCommand {
        return .{ .kind = .check_sat };
    }

    /// Create a get-model command
    pub fn getModel() SmtCommand {
        return .{ .kind = .get_model };
    }

    /// Create a push command
    pub fn pushCmd(n: u32) SmtCommand {
        return .{ .kind = .{ .push = n } };
    }

    /// Create a pop command
    pub fn popCmd(n: u32) SmtCommand {
        return .{ .kind = .{ .pop = n } };
    }
};

/// Set option command
pub const SetOption = struct {
    keyword: []const u8,
    value: OptionValue,
};

/// Option value
pub const OptionValue = union(enum) {
    bool_val: bool,
    int_val: i64,
    string_val: []const u8,
    symbol_val: []const u8,
};

// ============================================================================
// SMT Script
// ============================================================================

/// A complete SMT-LIB script
pub const SmtScript = struct {
    commands: []const SmtCommand,

    pub fn init(commands: []const SmtCommand) SmtScript {
        return .{ .commands = commands };
    }
};

// ============================================================================
// Built-in Sorts
// ============================================================================

/// Pre-allocated built-in sorts for common use
pub const builtin_sorts = struct {
    pub const bool_sort = SmtSort{ .kind = .bool_sort };
    pub const int_sort = SmtSort{ .kind = .int_sort };
    pub const real_sort = SmtSort{ .kind = .real_sort };
    pub const string_sort = SmtSort{ .kind = .string_sort };
};

// ============================================================================
// Tests
// ============================================================================

test "SmtSort equality" {
    const testing = std.testing;

    try testing.expect(builtin_sorts.bool_sort.eql(&builtin_sorts.bool_sort));
    try testing.expect(builtin_sorts.int_sort.eql(&builtin_sorts.int_sort));
    try testing.expect(!builtin_sorts.bool_sort.eql(&builtin_sorts.int_sort));

    const bv32 = SmtSort.bitvec(32);
    const bv64 = SmtSort.bitvec(64);
    try testing.expect(bv32.eql(&bv32));
    try testing.expect(!bv32.eql(&bv64));
}

test "SmtExpr construction" {
    const testing = std.testing;

    const true_lit = SmtExpr.boolLit(true);
    try testing.expect(true_lit.kind.bool_lit == true);

    const int_lit = SmtExpr.intLit(42);
    try testing.expect(int_lit.kind.int_lit == 42);

    const var_expr = SmtExpr.variable("x", &builtin_sorts.int_sort);
    try testing.expectEqualStrings("x", var_expr.kind.variable.name);
}

test "SmtDecl construction" {
    const testing = std.testing;

    const sort_decl = SmtDecl.sortDecl("MySort", 0);
    try testing.expectEqualStrings("MySort", sort_decl.kind.sort_decl.name);

    const const_decl = SmtDecl.constDecl("x", &builtin_sorts.int_sort);
    try testing.expectEqualStrings("x", const_decl.kind.const_decl.name);
}

test "SmtCommand construction" {
    const cmd1 = SmtCommand.setLogic("QF_LIA");
    try std.testing.expectEqualStrings("QF_LIA", cmd1.kind.set_logic);

    const cmd2 = SmtCommand.checkSat();
    try std.testing.expect(cmd2.kind == .check_sat);
}
