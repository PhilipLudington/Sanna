//! SMT-LIB Writer
//!
//! This module generates SMT-LIB 2.6 format text from the SMT AST.
//! The output can be sent to SMT solvers like Z3.
//!
//! ## Example Output
//!
//! ```smt2
//! (set-logic ALL)
//! (declare-const x Int)
//! (declare-const y Int)
//! (assert (>= x 0))
//! (assert (=> (> x 0) (> y 0)))
//! (check-sat)
//! (get-model)
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const SmtTypes = @import("SmtTypes.zig");
const SmtSort = SmtTypes.SmtSort;
const SmtExpr = SmtTypes.SmtExpr;
const SmtDecl = SmtTypes.SmtDecl;
const SmtCommand = SmtTypes.SmtCommand;
const SmtScript = SmtTypes.SmtScript;
const BoundVar = SmtTypes.BoundVar;

// ============================================================================
// SMT-LIB Writer
// ============================================================================

/// Writer for generating SMT-LIB format output
pub const SmtLibWriter = struct {
    allocator: Allocator,
    /// Output buffer
    buffer: std.ArrayListUnmanaged(u8),
    /// Indentation level
    indent: u32,
    /// Use pretty printing (multi-line with indentation)
    pretty: bool,

    pub fn init(allocator: Allocator) SmtLibWriter {
        return .{
            .allocator = allocator,
            .buffer = .{},
            .indent = 0,
            .pretty = false,
        };
    }

    pub fn deinit(self: *SmtLibWriter) void {
        self.buffer.deinit(self.allocator);
    }

    /// Get the generated SMT-LIB string
    pub fn getOutput(self: *const SmtLibWriter) []const u8 {
        return self.buffer.items;
    }

    /// Clear the output buffer
    pub fn reset(self: *SmtLibWriter) void {
        self.buffer.clearRetainingCapacity();
    }

    /// Enable pretty printing
    pub fn setPretty(self: *SmtLibWriter, pretty: bool) void {
        self.pretty = pretty;
    }

    // ========================================================================
    // High-Level Writers
    // ========================================================================

    /// Write a complete SMT script
    pub fn writeScript(self: *SmtLibWriter, script: *const SmtScript) !void {
        for (script.commands) |cmd| {
            try self.writeCommand(&cmd);
            try self.buffer.append(self.allocator, '\n');
        }
    }

    /// Write a command
    pub fn writeCommand(self: *SmtLibWriter, cmd: *const SmtCommand) !void {
        switch (cmd.kind) {
            .set_logic => |logic| {
                try self.buffer.appendSlice(self.allocator, "(set-logic ");
                try self.buffer.appendSlice(self.allocator,logic);
                try self.buffer.append(self.allocator, ')');
            },
            .set_option => |opt| {
                try self.buffer.appendSlice(self.allocator, "(set-option :");
                try self.buffer.appendSlice(self.allocator,opt.keyword);
                try self.buffer.append(self.allocator, ' ');
                try self.writeOptionValue(&opt.value);
                try self.buffer.append(self.allocator, ')');
            },
            .declaration => |decl| try self.writeDeclaration(&decl),
            .assert => |expr| {
                try self.buffer.appendSlice(self.allocator, "(assert ");
                try self.writeExpr(expr);
                try self.buffer.append(self.allocator, ')');
            },
            .check_sat => try self.buffer.appendSlice(self.allocator, "(check-sat)"),
            .get_model => try self.buffer.appendSlice(self.allocator, "(get-model)"),
            .get_value => |exprs| {
                try self.buffer.appendSlice(self.allocator, "(get-value (");
                for (exprs, 0..) |expr, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.writeExpr(expr);
                }
                try self.buffer.appendSlice(self.allocator, "))");
            },
            .get_unsat_core => try self.buffer.appendSlice(self.allocator, "(get-unsat-core)"),
            .push => |n| {
                try self.buffer.appendSlice(self.allocator, "(push ");
                try self.writeInt(@intCast(n));
                try self.buffer.append(self.allocator, ')');
            },
            .pop => |n| {
                try self.buffer.appendSlice(self.allocator, "(pop ");
                try self.writeInt(@intCast(n));
                try self.buffer.append(self.allocator, ')');
            },
            .reset => try self.buffer.appendSlice(self.allocator, "(reset)"),
            .exit => try self.buffer.appendSlice(self.allocator, "(exit)"),
            .echo => |s| {
                try self.buffer.appendSlice(self.allocator, "(echo \"");
                try self.writeEscapedString(s);
                try self.buffer.appendSlice(self.allocator, "\")");
            },
        }
    }

    /// Write a declaration
    pub fn writeDeclaration(self: *SmtLibWriter, decl: *const SmtDecl) !void {
        switch (decl.kind) {
            .sort_decl => |sd| {
                try self.buffer.appendSlice(self.allocator, "(declare-sort ");
                try self.writeSymbol(sd.name);
                try self.buffer.append(self.allocator, ' ');
                try self.writeInt(@intCast(sd.arity));
                try self.buffer.append(self.allocator, ')');
            },
            .const_decl => |cd| {
                try self.buffer.appendSlice(self.allocator, "(declare-const ");
                try self.writeSymbol(cd.name);
                try self.buffer.append(self.allocator, ' ');
                try self.writeSort(cd.sort);
                try self.buffer.append(self.allocator, ')');
            },
            .func_decl => |fd| {
                try self.buffer.appendSlice(self.allocator, "(declare-fun ");
                try self.writeSymbol(fd.name);
                try self.buffer.appendSlice(self.allocator, " (");
                for (fd.param_sorts, 0..) |sort, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.writeSort(sort);
                }
                try self.buffer.appendSlice(self.allocator, ") ");
                try self.writeSort(fd.return_sort);
                try self.buffer.append(self.allocator, ')');
            },
            .sort_def => |sd| {
                try self.buffer.appendSlice(self.allocator, "(define-sort ");
                try self.writeSymbol(sd.name);
                try self.buffer.appendSlice(self.allocator, " (");
                for (sd.params, 0..) |param, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.writeSymbol(param);
                }
                try self.buffer.appendSlice(self.allocator, ") ");
                try self.writeSort(sd.body);
                try self.buffer.append(self.allocator, ')');
            },
            .func_def => |fd| {
                try self.buffer.appendSlice(self.allocator, "(define-fun ");
                try self.writeSymbol(fd.name);
                try self.buffer.appendSlice(self.allocator, " (");
                for (fd.params, 0..) |param, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.buffer.append(self.allocator, '(');
                    try self.writeSymbol(param.name);
                    try self.buffer.append(self.allocator, ' ');
                    try self.writeSort(param.sort);
                    try self.buffer.append(self.allocator, ')');
                }
                try self.buffer.appendSlice(self.allocator, ") ");
                try self.writeSort(fd.return_sort);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(fd.body);
                try self.buffer.append(self.allocator, ')');
            },
        }
    }

    /// Write a sort
    pub fn writeSort(self: *SmtLibWriter, sort: *const SmtSort) !void {
        switch (sort.kind) {
            .bool_sort => try self.buffer.appendSlice(self.allocator, "Bool"),
            .int_sort => try self.buffer.appendSlice(self.allocator, "Int"),
            .real_sort => try self.buffer.appendSlice(self.allocator, "Real"),
            .string_sort => try self.buffer.appendSlice(self.allocator, "String"),
            .bitvec => |width| {
                try self.buffer.appendSlice(self.allocator, "(_ BitVec ");
                try self.writeInt(@intCast(width));
                try self.buffer.append(self.allocator, ')');
            },
            .array => |arr| {
                try self.buffer.appendSlice(self.allocator, "(Array ");
                try self.writeSort(arr.index_sort);
                try self.buffer.append(self.allocator, ' ');
                try self.writeSort(arr.element_sort);
                try self.buffer.append(self.allocator, ')');
            },
            .named => |name| try self.writeSymbol(name),
            .parameterized => |p| {
                try self.buffer.append(self.allocator, '(');
                try self.writeSymbol(p.name);
                for (p.params) |param| {
                    try self.buffer.append(self.allocator, ' ');
                    try self.writeSort(param);
                }
                try self.buffer.append(self.allocator, ')');
            },
        }
    }

    /// Write an expression
    pub fn writeExpr(self: *SmtLibWriter, expr: *const SmtExpr) Allocator.Error!void {
        switch (expr.kind) {
            // Literals
            .bool_lit => |v| try self.buffer.appendSlice(self.allocator,if (v) "true" else "false"),
            .int_lit => |v| try self.writeInt(v),
            .real_lit => |v| try self.writeReal(v),
            .string_lit => |v| {
                try self.buffer.append(self.allocator, '"');
                try self.writeEscapedString(v);
                try self.buffer.append(self.allocator, '"');
            },
            .bitvec_lit => |bv| {
                try self.buffer.appendSlice(self.allocator, "#x");
                // Write hex representation
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{x}", .{bv.value}) catch "0";
                try self.buffer.appendSlice(self.allocator, formatted);
            },

            // Variables and constants
            .variable => |v| try self.writeSymbol(v.name),
            .constant => |name| try self.writeSymbol(name),

            // Core operations
            .not => |operand| {
                try self.buffer.appendSlice(self.allocator, "(not ");
                try self.writeExpr(operand);
                try self.buffer.append(self.allocator, ')');
            },
            .and_expr => |operands| try self.writeNaryOp("and", operands),
            .or_expr => |operands| try self.writeNaryOp("or", operands),
            .implies => |bin| try self.writeBinaryOp("=>", bin.left, bin.right),
            .iff => |bin| try self.writeBinaryOp("=", bin.left, bin.right),
            .xor => |bin| try self.writeBinaryOp("xor", bin.left, bin.right),
            .ite => |ite| {
                try self.buffer.appendSlice(self.allocator, "(ite ");
                try self.writeExpr(ite.condition);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(ite.then_branch);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(ite.else_branch);
                try self.buffer.append(self.allocator, ')');
            },

            // Equality and comparison
            .eq => |bin| try self.writeBinaryOp("=", bin.left, bin.right),
            .distinct => |operands| try self.writeNaryOp("distinct", operands),
            .lt => |bin| try self.writeBinaryOp("<", bin.left, bin.right),
            .le => |bin| try self.writeBinaryOp("<=", bin.left, bin.right),
            .gt => |bin| try self.writeBinaryOp(">", bin.left, bin.right),
            .ge => |bin| try self.writeBinaryOp(">=", bin.left, bin.right),

            // Arithmetic
            .neg => |operand| {
                try self.buffer.appendSlice(self.allocator, "(- ");
                try self.writeExpr(operand);
                try self.buffer.append(self.allocator, ')');
            },
            .add => |operands| try self.writeNaryOp("+", operands),
            .sub => |bin| try self.writeBinaryOp("-", bin.left, bin.right),
            .mul => |operands| try self.writeNaryOp("*", operands),
            .div => |bin| try self.writeBinaryOp("div", bin.left, bin.right),
            .mod => |bin| try self.writeBinaryOp("mod", bin.left, bin.right),
            .abs => |operand| {
                try self.buffer.appendSlice(self.allocator, "(abs ");
                try self.writeExpr(operand);
                try self.buffer.append(self.allocator, ')');
            },

            // Array operations
            .select => |sel| {
                try self.buffer.appendSlice(self.allocator, "(select ");
                try self.writeExpr(sel.array);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(sel.index);
                try self.buffer.append(self.allocator, ')');
            },
            .store => |st| {
                try self.buffer.appendSlice(self.allocator, "(store ");
                try self.writeExpr(st.array);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(st.index);
                try self.buffer.append(self.allocator, ' ');
                try self.writeExpr(st.value);
                try self.buffer.append(self.allocator, ')');
            },
            .const_array => |ca| {
                try self.buffer.appendSlice(self.allocator, "((as const ");
                try self.writeSort(ca.sort);
                try self.buffer.appendSlice(self.allocator, ") ");
                try self.writeExpr(ca.value);
                try self.buffer.append(self.allocator, ')');
            },

            // Quantifiers
            .forall => |q| try self.writeQuantifier("forall", &q),
            .exists => |q| try self.writeQuantifier("exists", &q),

            // Let binding
            .let_expr => |let_e| {
                try self.buffer.appendSlice(self.allocator, "(let (");
                for (let_e.bindings, 0..) |binding, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.buffer.append(self.allocator, '(');
                    try self.writeSymbol(binding.name);
                    try self.buffer.append(self.allocator, ' ');
                    try self.writeExpr(binding.value);
                    try self.buffer.append(self.allocator, ')');
                }
                try self.buffer.appendSlice(self.allocator, ") ");
                try self.writeExpr(let_e.body);
                try self.buffer.append(self.allocator, ')');
            },

            // Function application
            .apply => |app| {
                try self.buffer.append(self.allocator, '(');
                try self.writeSymbol(app.func_name);
                for (app.args) |arg| {
                    try self.buffer.append(self.allocator, ' ');
                    try self.writeExpr(arg);
                }
                try self.buffer.append(self.allocator, ')');
            },

            // Annotated expression
            .annotated => |ann| {
                try self.buffer.appendSlice(self.allocator, "(! ");
                try self.writeExpr(ann.expr);
                for (ann.annotations) |annot| {
                    try self.buffer.append(self.allocator, ' ');
                    try self.buffer.append(self.allocator, ':');
                    try self.buffer.appendSlice(self.allocator,annot.keyword);
                    if (annot.value) |val| {
                        try self.buffer.append(self.allocator, ' ');
                        try self.writeAnnotationValue(&val);
                    }
                }
                try self.buffer.append(self.allocator, ')');
            },
        }
    }

    // ========================================================================
    // Helper Writers
    // ========================================================================

    fn writeBinaryOp(self: *SmtLibWriter, op: []const u8, left: *const SmtExpr, right: *const SmtExpr) !void {
        try self.buffer.append(self.allocator, '(');
        try self.buffer.appendSlice(self.allocator,op);
        try self.buffer.append(self.allocator, ' ');
        try self.writeExpr(left);
        try self.buffer.append(self.allocator, ' ');
        try self.writeExpr(right);
        try self.buffer.append(self.allocator, ')');
    }

    fn writeNaryOp(self: *SmtLibWriter, op: []const u8, operands: []const *const SmtExpr) !void {
        if (operands.len == 0) {
            // Identity for and is true, for or is false
            if (std.mem.eql(u8, op, "and")) {
                try self.buffer.appendSlice(self.allocator, "true");
            } else if (std.mem.eql(u8, op, "or")) {
                try self.buffer.appendSlice(self.allocator, "false");
            } else if (std.mem.eql(u8, op, "+")) {
                try self.buffer.append(self.allocator, '0');
            } else if (std.mem.eql(u8, op, "*")) {
                try self.buffer.append(self.allocator, '1');
            }
            return;
        }

        if (operands.len == 1) {
            try self.writeExpr(operands[0]);
            return;
        }

        try self.buffer.append(self.allocator, '(');
        try self.buffer.appendSlice(self.allocator,op);
        for (operands) |operand| {
            try self.buffer.append(self.allocator, ' ');
            try self.writeExpr(operand);
        }
        try self.buffer.append(self.allocator, ')');
    }

    fn writeQuantifier(self: *SmtLibWriter, kind: []const u8, quant: *const SmtTypes.QuantifierExpr) !void {
        try self.buffer.append(self.allocator, '(');
        try self.buffer.appendSlice(self.allocator,kind);
        try self.buffer.appendSlice(self.allocator, " (");

        for (quant.bound_vars, 0..) |bv, i| {
            if (i > 0) try self.buffer.append(self.allocator, ' ');
            try self.buffer.append(self.allocator, '(');
            try self.writeSymbol(bv.name);
            try self.buffer.append(self.allocator, ' ');
            try self.writeSort(bv.sort);
            try self.buffer.append(self.allocator, ')');
        }

        try self.buffer.appendSlice(self.allocator, ") ");

        // Write patterns if any
        if (quant.patterns.len > 0) {
            try self.buffer.appendSlice(self.allocator, "(! ");
            try self.writeExpr(quant.body);
            for (quant.patterns) |pattern| {
                try self.buffer.appendSlice(self.allocator, " :pattern (");
                for (pattern, 0..) |p, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.writeExpr(p);
                }
                try self.buffer.append(self.allocator, ')');
            }
            try self.buffer.append(self.allocator, ')');
        } else {
            try self.writeExpr(quant.body);
        }

        try self.buffer.append(self.allocator, ')');
    }

    fn writeOptionValue(self: *SmtLibWriter, val: *const SmtTypes.OptionValue) !void {
        switch (val.*) {
            .bool_val => |v| try self.buffer.appendSlice(self.allocator,if (v) "true" else "false"),
            .int_val => |v| try self.writeInt(v),
            .string_val => |v| {
                try self.buffer.append(self.allocator, '"');
                try self.writeEscapedString(v);
                try self.buffer.append(self.allocator, '"');
            },
            .symbol_val => |v| try self.writeSymbol(v),
        }
    }

    fn writeAnnotationValue(self: *SmtLibWriter, val: *const SmtTypes.AnnotationValue) !void {
        switch (val.*) {
            .symbol => |s| try self.writeSymbol(s),
            .string => |s| {
                try self.buffer.append(self.allocator, '"');
                try self.writeEscapedString(s);
                try self.buffer.append(self.allocator, '"');
            },
            .expr => |e| try self.writeExpr(e),
            .exprs => |es| {
                try self.buffer.append(self.allocator, '(');
                for (es, 0..) |e, i| {
                    if (i > 0) try self.buffer.append(self.allocator, ' ');
                    try self.writeExpr(e);
                }
                try self.buffer.append(self.allocator, ')');
            },
        }
    }

    fn writeSymbol(self: *SmtLibWriter, name: []const u8) !void {
        // Check if name needs quoting
        const needs_quote = blk: {
            if (name.len == 0) break :blk true;
            for (name) |c| {
                if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '.' and c != '-') {
                    break :blk true;
                }
            }
            break :blk false;
        };

        if (needs_quote) {
            try self.buffer.append(self.allocator, '|');
            try self.buffer.appendSlice(self.allocator,name);
            try self.buffer.append(self.allocator, '|');
        } else {
            try self.buffer.appendSlice(self.allocator,name);
        }
    }

    fn writeInt(self: *SmtLibWriter, value: i64) !void {
        if (value < 0) {
            try self.buffer.appendSlice(self.allocator, "(- ");
            var buf: [20]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{-value}) catch "0";
            try self.buffer.appendSlice(self.allocator, formatted);
            try self.buffer.append(self.allocator, ')');
        } else {
            var buf: [20]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{value}) catch "0";
            try self.buffer.appendSlice(self.allocator, formatted);
        }
    }

    fn writeReal(self: *SmtLibWriter, value: f64) !void {
        if (value < 0) {
            try self.buffer.appendSlice(self.allocator, "(- ");
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{-value}) catch "0.0";
            try self.buffer.appendSlice(self.allocator,formatted);
            try self.buffer.append(self.allocator, ')');
        } else {
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{value}) catch "0.0";
            try self.buffer.appendSlice(self.allocator,formatted);
        }
    }

    fn writeEscapedString(self: *SmtLibWriter, s: []const u8) !void {
        for (s) |c| {
            switch (c) {
                '"' => try self.buffer.appendSlice(self.allocator, "\"\""),
                '\\' => try self.buffer.appendSlice(self.allocator, "\\\\"),
                '\n' => try self.buffer.appendSlice(self.allocator, "\\n"),
                '\r' => try self.buffer.appendSlice(self.allocator, "\\r"),
                '\t' => try self.buffer.appendSlice(self.allocator, "\\t"),
                else => try self.buffer.append(self.allocator, c),
            }
        }
    }

    fn writeNewline(self: *SmtLibWriter) !void {
        if (self.pretty) {
            try self.buffer.append(self.allocator, '\n');
            var i: u32 = 0;
            while (i < self.indent) : (i += 1) {
                try self.buffer.appendSlice(self.allocator, "  ");
            }
        }
    }
};

// ============================================================================
// Convenience Functions
// ============================================================================

/// Generate SMT-LIB string for an expression
pub fn exprToString(allocator: Allocator, expr: *const SmtExpr) ![]u8 {
    var writer = SmtLibWriter.init(allocator);
    defer writer.deinit();

    try writer.writeExpr(expr);
    return allocator.dupe(u8, writer.getOutput());
}

/// Generate SMT-LIB string for a sort
pub fn sortToString(allocator: Allocator, sort: *const SmtSort) ![]u8 {
    var writer = SmtLibWriter.init(allocator);
    defer writer.deinit();

    try writer.writeSort(sort);
    return allocator.dupe(u8, writer.getOutput());
}

/// Generate SMT-LIB string for a declaration
pub fn declToString(allocator: Allocator, decl: *const SmtDecl) ![]u8 {
    var writer = SmtLibWriter.init(allocator);
    defer writer.deinit();

    try writer.writeDeclaration(decl);
    return allocator.dupe(u8, writer.getOutput());
}

// ============================================================================
// Tests
// ============================================================================

test "SmtLibWriter sort output" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    // Boolean sort
    const bool_sort = SmtSort.boolean();
    try writer.writeSort(&bool_sort);
    try testing.expectEqualStrings("Bool", writer.getOutput());

    // Integer sort
    writer.reset();
    const int_sort = SmtSort.integer();
    try writer.writeSort(&int_sort);
    try testing.expectEqualStrings("Int", writer.getOutput());

    // BitVec sort
    writer.reset();
    const bv_sort = SmtSort.bitvec(32);
    try writer.writeSort(&bv_sort);
    try testing.expectEqualStrings("(_ BitVec 32)", writer.getOutput());

    // Array sort
    writer.reset();
    const arr_sort = SmtSort.array(&int_sort, &bool_sort);
    try writer.writeSort(&arr_sort);
    try testing.expectEqualStrings("(Array Int Bool)", writer.getOutput());
}

test "SmtLibWriter expression output" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    // Boolean literal
    const true_lit = SmtExpr.boolLit(true);
    try writer.writeExpr(&true_lit);
    try testing.expectEqualStrings("true", writer.getOutput());

    // Integer literal
    writer.reset();
    const int_lit = SmtExpr.intLit(42);
    try writer.writeExpr(&int_lit);
    try testing.expectEqualStrings("42", writer.getOutput());

    // Negative integer
    writer.reset();
    const neg_lit = SmtExpr.intLit(-5);
    try writer.writeExpr(&neg_lit);
    try testing.expectEqualStrings("(- 5)", writer.getOutput());

    // Variable
    writer.reset();
    const int_sort = SmtSort.integer();
    const var_expr = SmtExpr.variable("x", &int_sort);
    try writer.writeExpr(&var_expr);
    try testing.expectEqualStrings("x", writer.getOutput());
}

test "SmtLibWriter binary operations" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    const x = SmtExpr.constant("x");
    const y = SmtExpr.constant("y");

    // Less than
    const lt = SmtExpr.ltExpr(&x, &y);
    try writer.writeExpr(&lt);
    try testing.expectEqualStrings("(< x y)", writer.getOutput());

    // Addition
    writer.reset();
    const operands = [_]*const SmtExpr{ &x, &y };
    const add = SmtExpr.addExpr(&operands);
    try writer.writeExpr(&add);
    try testing.expectEqualStrings("(+ x y)", writer.getOutput());

    // Implication
    writer.reset();
    const impl = SmtExpr.impliesExpr(&x, &y);
    try writer.writeExpr(&impl);
    try testing.expectEqualStrings("(=> x y)", writer.getOutput());
}

test "SmtLibWriter declaration output" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    // Constant declaration
    const int_sort = SmtSort.integer();
    const const_decl = SmtDecl.constDecl("x", &int_sort);
    try writer.writeDeclaration(&const_decl);
    try testing.expectEqualStrings("(declare-const x Int)", writer.getOutput());

    // Sort declaration
    writer.reset();
    const sort_decl = SmtDecl.sortDecl("MySort", 0);
    try writer.writeDeclaration(&sort_decl);
    try testing.expectEqualStrings("(declare-sort MySort 0)", writer.getOutput());
}

test "SmtLibWriter command output" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    // Set logic
    const set_logic = SmtCommand.setLogic("QF_LIA");
    try writer.writeCommand(&set_logic);
    try testing.expectEqualStrings("(set-logic QF_LIA)", writer.getOutput());

    // Check sat
    writer.reset();
    const check_sat = SmtCommand.checkSat();
    try writer.writeCommand(&check_sat);
    try testing.expectEqualStrings("(check-sat)", writer.getOutput());

    // Assert
    writer.reset();
    const expr = SmtExpr.boolLit(true);
    const assert_cmd = SmtCommand.assert(&expr);
    try writer.writeCommand(&assert_cmd);
    try testing.expectEqualStrings("(assert true)", writer.getOutput());
}

test "SmtLibWriter quantifier output" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    const int_sort = SmtSort.integer();
    const bound_vars = [_]BoundVar{.{ .name = "x", .sort = &int_sort }};
    const body = SmtExpr.boolLit(true);
    const forall = SmtExpr.forallExpr(&bound_vars, &body);

    try writer.writeExpr(&forall);
    try testing.expectEqualStrings("(forall ((x Int)) true)", writer.getOutput());
}

test "SmtLibWriter special symbol handling" {
    const testing = std.testing;

    var writer = SmtLibWriter.init(testing.allocator);
    defer writer.deinit();

    // Symbol with special characters needs quoting
    const var_expr = SmtExpr.constant("x+y");
    try writer.writeExpr(&var_expr);
    try testing.expectEqualStrings("|x+y|", writer.getOutput());
}
