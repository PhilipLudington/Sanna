//! AST Pretty-Printer for Sanna
//!
//! Provides formatted output of the Abstract Syntax Tree for debugging
//! and visualization purposes. The output format shows the hierarchical
//! structure with indentation and source location information.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");
const Module = Ast.Module;
const Import = Ast.Import;
const ImportItem = Ast.ImportItem;
const Declaration = Ast.Declaration;
const DeclarationKind = Ast.DeclarationKind;
const Identifier = Ast.Identifier;
const QualifiedName = Ast.QualifiedName;
const TypeParameter = Ast.TypeParameter;
const Attribute = Ast.Attribute;
const AttributeArg = Ast.AttributeArg;
const AttributeValue = Ast.AttributeValue;
const Visibility = Ast.Visibility;
const TypeDefinition = Ast.TypeDefinition;
const TypeBody = Ast.TypeBody;
const TypeExpr = Ast.TypeExpr;
const TypeExprKind = Ast.TypeExprKind;
const Field = Ast.Field;
const Variant = Ast.Variant;
const ModelDefinition = Ast.ModelDefinition;
const FunctionSpec = Ast.FunctionSpec;
const InterfaceSpec = Ast.InterfaceSpec;
const InvariantDecl = Ast.InvariantDecl;
const AxiomDecl = Ast.AxiomDecl;
const LemmaDecl = Ast.LemmaDecl;
const Parameter = Ast.Parameter;
const Expression = Ast.Expression;
const ExpressionKind = Ast.ExpressionKind;

const lexer = @import("../lexer/root.zig");
const Span = lexer.Span;

/// Configuration options for the pretty printer
pub const PrintOptions = struct {
    /// Show source locations (line:column)
    show_locations: bool = true,
    /// Initial indentation level
    initial_indent: usize = 0,
    /// Characters to use for indentation
    indent_string: []const u8 = "  ",
    /// Maximum depth to print (0 = unlimited)
    max_depth: usize = 0,
};

/// Pretty printer state
pub const PrettyPrinter = struct {
    options: PrintOptions,
    depth: usize,

    const Self = @This();

    pub fn init(options: PrintOptions) Self {
        return .{
            .options = options,
            .depth = options.initial_indent,
        };
    }

    /// Write indentation for current depth
    fn writeIndent(self: *Self, writer: anytype) !void {
        for (0..self.depth) |_| {
            try writer.writeAll(self.options.indent_string);
        }
    }

    /// Write a location annotation
    fn writeLocation(self: *Self, writer: anytype, span: Span) !void {
        if (self.options.show_locations) {
            try writer.print(" @{d}:{d}", .{ span.start.line, span.start.column });
        }
    }

    /// Check if we've exceeded max depth
    fn shouldTruncate(self: *Self) bool {
        return self.options.max_depth > 0 and self.depth >= self.options.max_depth;
    }

    // =========================================================================
    // Module-level printing
    // =========================================================================

    pub fn printModule(self: *Self, writer: anytype, module: Module) !void {
        try self.writeIndent(writer);
        try writer.writeAll("Module");
        try self.writeLocation(writer, module.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        if (self.shouldTruncate()) {
            try self.writeIndent(writer);
            try writer.writeAll("...\n");
            return;
        }

        // Module name
        if (module.name) |name| {
            try self.writeIndent(writer);
            try writer.writeAll("name: ");
            try self.printQualifiedName(writer, name);
            try writer.writeByte('\n');
        }

        // Imports
        if (module.imports.len > 0) {
            try self.writeIndent(writer);
            try writer.print("imports: ({d})\n", .{module.imports.len});
            self.depth += 1;
            for (module.imports) |import_decl| {
                try self.printImport(writer, import_decl);
            }
            self.depth -= 1;
        }

        // Declarations
        if (module.declarations.len > 0) {
            try self.writeIndent(writer);
            try writer.print("declarations: ({d})\n", .{module.declarations.len});
            self.depth += 1;
            for (module.declarations) |decl| {
                try self.printDeclaration(writer, decl);
            }
            self.depth -= 1;
        }
    }

    fn printQualifiedName(self: *Self, writer: anytype, name: QualifiedName) !void {
        for (name.parts, 0..) |part, i| {
            if (i > 0) try writer.writeByte('.');
            try writer.writeAll(part.name);
        }
        _ = self;
    }

    fn printImport(self: *Self, writer: anytype, import_decl: Import) !void {
        try self.writeIndent(writer);
        try writer.writeAll("Import: ");
        try self.printQualifiedName(writer, import_decl.path);
        try self.writeLocation(writer, import_decl.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        if (import_decl.alias) |alias| {
            try self.writeIndent(writer);
            try writer.print("alias: {s}\n", .{alias.name});
        }

        if (import_decl.items.len > 0) {
            try self.writeIndent(writer);
            try writer.writeAll("items: [");
            for (import_decl.items, 0..) |item, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(item.name.name);
                if (item.alias) |item_alias| {
                    try writer.print(" as {s}", .{item_alias.name});
                }
            }
            try writer.writeAll("]\n");
        }
    }

    // =========================================================================
    // Declaration printing
    // =========================================================================

    fn printDeclaration(self: *Self, writer: anytype, decl: Declaration) !void {
        try self.writeIndent(writer);
        try writer.writeAll("Declaration");
        try self.writeLocation(writer, decl.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        if (self.shouldTruncate()) {
            try self.writeIndent(writer);
            try writer.writeAll("...\n");
            return;
        }

        // Visibility
        if (decl.visibility == .public) {
            try self.writeIndent(writer);
            try writer.writeAll("visibility: public\n");
        }

        // Attributes
        if (decl.attributes.len > 0) {
            try self.writeIndent(writer);
            try writer.print("attributes: ({d})\n", .{decl.attributes.len});
            self.depth += 1;
            for (decl.attributes) |attr| {
                try self.printAttribute(writer, attr);
            }
            self.depth -= 1;
        }

        // Doc comment
        if (decl.doc_comment) |doc| {
            try self.writeIndent(writer);
            try writer.print("doc: \"{s}\"\n", .{doc});
        }

        // Declaration kind
        try self.printDeclarationKind(writer, decl.kind);
    }

    fn printAttribute(self: *Self, writer: anytype, attr: Attribute) !void {
        try self.writeIndent(writer);
        try writer.print("@{s}", .{attr.name.name});

        if (attr.args.len > 0) {
            try writer.writeByte('(');
            for (attr.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                if (arg.name) |arg_name| {
                    try writer.print("{s}: ", .{arg_name.name});
                }
                try self.printAttributeValue(writer, arg.value);
            }
            try writer.writeByte(')');
        }
        try writer.writeByte('\n');
    }

    fn printAttributeValue(self: *Self, writer: anytype, value: AttributeValue) !void {
        _ = self;
        switch (value) {
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .int => |i| try writer.print("{d}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .bool => |b| try writer.print("{}", .{b}),
            .identifier => |id| try writer.print("{s}", .{id.name}),
        }
    }

    fn printDeclarationKind(self: *Self, writer: anytype, kind: DeclarationKind) !void {
        switch (kind) {
            .type_def => |td| try self.printTypeDefinition(writer, td),
            .model_def => |md| try self.printModelDefinition(writer, md),
            .spec_fn => |sf| try self.printFunctionSpec(writer, sf),
            .spec_interface => |si| try self.printInterfaceSpec(writer, si),
            .invariant => |inv| try self.printInvariantDecl(writer, inv),
            .axiom => |ax| try self.printAxiomDecl(writer, ax),
            .lemma => |lem| try self.printLemmaDecl(writer, lem),
        }
    }

    // =========================================================================
    // Type definition printing
    // =========================================================================

    fn printTypeDefinition(self: *Self, writer: anytype, td: TypeDefinition) !void {
        try self.writeIndent(writer);
        try writer.print("TypeDefinition: {s}", .{td.name.name});
        try self.writeLocation(writer, td.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        // Type parameters
        if (td.type_params.len > 0) {
            try self.writeIndent(writer);
            try writer.writeAll("type_params: [");
            for (td.type_params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(param.name.name);
                if (param.bounds.len > 0) {
                    try writer.writeAll(": ");
                    for (param.bounds, 0..) |bound, j| {
                        if (j > 0) try writer.writeAll(" + ");
                        try self.printQualifiedName(writer, bound);
                    }
                }
            }
            try writer.writeAll("]\n");
        }

        // Body
        try self.printTypeBody(writer, td.body);

        // Invariants
        if (td.invariants.len > 0) {
            try self.writeIndent(writer);
            try writer.print("invariants: ({d})\n", .{td.invariants.len});
            self.depth += 1;
            for (td.invariants) |inv| {
                try self.printExpression(writer, inv);
            }
            self.depth -= 1;
        }
    }

    fn printTypeBody(self: *Self, writer: anytype, body: TypeBody) !void {
        try self.writeIndent(writer);
        switch (body) {
            .alias => |type_expr| {
                try writer.writeAll("body: alias = ");
                try self.printTypeExprInline(writer, type_expr);
                try writer.writeByte('\n');
            },
            .product => |fields| {
                try writer.print("body: product ({d} fields)\n", .{fields.len});
                self.depth += 1;
                for (fields) |field| {
                    try self.printField(writer, field);
                }
                self.depth -= 1;
            },
            .sum => |variants| {
                try writer.print("body: sum ({d} variants)\n", .{variants.len});
                self.depth += 1;
                for (variants) |variant| {
                    try self.printVariant(writer, variant);
                }
                self.depth -= 1;
            },
        }
    }

    fn printField(self: *Self, writer: anytype, field: Field) !void {
        try self.writeIndent(writer);
        try writer.print("{s}: ", .{field.name.name});
        try self.printTypeExprInline(writer, field.type_expr);
        if (field.default_value) |_| {
            try writer.writeAll(" = <expr>");
        }
        try writer.writeByte('\n');
    }

    fn printVariant(self: *Self, writer: anytype, variant: Variant) !void {
        try self.writeIndent(writer);
        try writer.print("| {s}", .{variant.name.name});
        if (variant.fields.len > 0) {
            try writer.print(" ({d} fields)", .{variant.fields.len});
        }
        try writer.writeByte('\n');

        if (variant.fields.len > 0) {
            self.depth += 1;
            for (variant.fields) |field| {
                try self.printField(writer, field);
            }
            self.depth -= 1;
        }
    }

    fn printTypeExprInline(self: *Self, writer: anytype, type_expr: TypeExpr) !void {
        switch (type_expr.kind) {
            .named => |name| try self.printQualifiedName(writer, name),
            .generic => |gen| {
                try self.printQualifiedName(writer, gen.base);
                try writer.writeByte('[');
                for (gen.args, 0..) |_, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.writeAll("_");
                }
                try writer.writeByte(']');
            },
            .function => try writer.writeAll("fn(...)"),
            .tuple => try writer.writeAll("(...)"),
            .optional => try writer.writeAll("?_"),
            .result => try writer.writeAll("Result[_, _]"),
            .self_type => try writer.writeAll("Self"),
            .hole => try writer.writeAll("???"),
        }
    }

    // =========================================================================
    // Specification printing
    // =========================================================================

    fn printModelDefinition(self: *Self, writer: anytype, md: ModelDefinition) !void {
        try self.writeIndent(writer);
        try writer.print("ModelDefinition: {s}", .{md.name.name});
        try self.writeLocation(writer, md.span);
        try writer.writeByte('\n');
    }

    fn printFunctionSpec(self: *Self, writer: anytype, spec: FunctionSpec) !void {
        try self.writeIndent(writer);
        try writer.writeAll("FunctionSpec: ");
        if (spec.is_pure) try writer.writeAll("pure ");
        try writer.print("{s}", .{spec.name.name});
        try self.writeLocation(writer, spec.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        // Parameters
        if (spec.params.len > 0) {
            try self.writeIndent(writer);
            try writer.writeAll("params: (");
            for (spec.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                if (param.is_self) {
                    try writer.writeAll("self");
                } else {
                    try writer.print("{s}: ", .{param.name.name});
                    try self.printTypeExprInline(writer, param.type_expr);
                }
            }
            try writer.writeAll(")\n");
        }

        // Return type
        try self.writeIndent(writer);
        try writer.writeAll("returns: ");
        try self.printTypeExprInline(writer, spec.return_type);
        try writer.writeByte('\n');

        // Requires
        if (spec.requires.len > 0) {
            try self.writeIndent(writer);
            try writer.print("requires: ({d})\n", .{spec.requires.len});
            self.depth += 1;
            for (spec.requires) |req| {
                try self.printExpression(writer, req);
            }
            self.depth -= 1;
        }

        // Ensures
        if (spec.ensures.len > 0) {
            try self.writeIndent(writer);
            try writer.print("ensures: ({d})\n", .{spec.ensures.len});
            self.depth += 1;
            for (spec.ensures) |ens| {
                try self.printExpression(writer, ens);
            }
            self.depth -= 1;
        }

        // Modifies
        if (spec.modifies.len > 0) {
            try self.writeIndent(writer);
            try writer.print("modifies: ({d})\n", .{spec.modifies.len});
        }

        // Decreases
        if (spec.decreases) |dec| {
            try self.writeIndent(writer);
            try writer.writeAll("decreases:\n");
            self.depth += 1;
            try self.printExpression(writer, dec);
            self.depth -= 1;
        }
    }

    fn printInterfaceSpec(self: *Self, writer: anytype, spec: InterfaceSpec) !void {
        try self.writeIndent(writer);
        try writer.print("InterfaceSpec: {s}", .{spec.name.name});
        try self.writeLocation(writer, spec.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        // Members
        if (spec.members.len > 0) {
            try self.writeIndent(writer);
            try writer.print("members: ({d})\n", .{spec.members.len});
        }

        // Invariants
        if (spec.invariants.len > 0) {
            try self.writeIndent(writer);
            try writer.print("invariants: ({d})\n", .{spec.invariants.len});
        }
    }

    fn printInvariantDecl(self: *Self, writer: anytype, inv: InvariantDecl) !void {
        try self.writeIndent(writer);
        try writer.writeAll("InvariantDecl");
        if (inv.name) |name| {
            try writer.print(": {s}", .{name.name});
        }
        try self.writeLocation(writer, inv.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        try self.printExpression(writer, inv.condition);
    }

    fn printAxiomDecl(self: *Self, writer: anytype, ax: AxiomDecl) !void {
        try self.writeIndent(writer);
        try writer.print("AxiomDecl: {s}", .{ax.name.name});
        try self.writeLocation(writer, ax.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        try self.printExpression(writer, ax.condition);
    }

    fn printLemmaDecl(self: *Self, writer: anytype, lem: LemmaDecl) !void {
        try self.writeIndent(writer);
        try writer.print("LemmaDecl: {s}", .{lem.name.name});
        try self.writeLocation(writer, lem.span);
        try writer.writeByte('\n');

        self.depth += 1;
        defer self.depth -= 1;

        try self.printExpression(writer, lem.condition);
    }

    // =========================================================================
    // Expression printing
    // =========================================================================

    fn printExpression(self: *Self, writer: anytype, expr: Expression) !void {
        try self.writeIndent(writer);
        try self.printExpressionKind(writer, expr.kind);
        try self.writeLocation(writer, expr.span);
        try writer.writeByte('\n');
    }

    fn printExpressionKind(_: *Self, writer: anytype, kind: ExpressionKind) !void {
        switch (kind) {
            // Literals
            .int_literal => |val| try writer.print("IntLiteral({d})", .{val}),
            .float_literal => |val| try writer.print("FloatLiteral({d})", .{val}),
            .string_literal => |val| try writer.print("StringLiteral(\"{s}\")", .{val}),
            .bool_literal => |val| try writer.print("BoolLiteral({s})", .{if (val) "true" else "false"}),

            // Identifiers and access
            .identifier => |id| try writer.print("Identifier({s})", .{id.name}),
            .qualified => try writer.writeAll("Qualified(...)"),
            .field_access => |fa| try writer.print("FieldAccess(..., {s})", .{fa.field.name}),
            .index_access => try writer.writeAll("IndexAccess(...)"),
            .method_call => |mc| try writer.print("MethodCall(..., {s}, ...)", .{mc.method.name}),

            // Operators
            .binary => |op| try writer.print("Binary(..., {s}, ...)", .{@tagName(op.op)}),
            .unary => |op| try writer.print("Unary({s}, ...)", .{@tagName(op.op)}),

            // Logical expressions
            .and_expr => try writer.writeAll("And(...)"),
            .or_expr => try writer.writeAll("Or(...)"),
            .not_expr => try writer.writeAll("Not(...)"),
            .implies => try writer.writeAll("Implies(...)"),
            .iff => try writer.writeAll("Iff(...)"),

            // Quantifiers
            .forall => try writer.writeAll("Forall(...)"),
            .exists => try writer.writeAll("Exists(...)"),

            // Control flow
            .if_expr => try writer.writeAll("If(...)"),
            .match_expr => try writer.writeAll("Match(...)"),
            .let_expr => try writer.writeAll("Let(...)"),

            // Function-related
            .call => try writer.writeAll("Call(...)"),
            .lambda => try writer.writeAll("Lambda(...)"),

            // Specification-specific
            .old => try writer.writeAll("Old(...)"),
            .result => try writer.writeAll("Result"),
            .self_expr => try writer.writeAll("Self"),

            // Collections
            .sequence_literal => |seq| try writer.print("SequenceLiteral({d} elements)", .{seq.len}),
            .set_literal => |set| try writer.print("SetLiteral({d} elements)", .{set.len}),
            .map_literal => |map| try writer.print("MapLiteral({d} entries)", .{map.len}),
            .set_comprehension => try writer.writeAll("SetComprehension(...)"),

            // Range
            .range => try writer.writeAll("Range(...)"),

            // Hole
            .hole => try writer.writeAll("Hole(???)"),
            .typed_hole => try writer.writeAll("TypedHole(???: T)"),
        }
    }
};

// ============================================================================
// Convenience functions
// ============================================================================

/// Print a module to a writer with default options
pub fn printModule(writer: anytype, module: Module) !void {
    var printer = PrettyPrinter.init(.{});
    try printer.printModule(writer, module);
}

/// Print a module to a string buffer
pub fn printModuleToString(allocator: Allocator, module: Module) ![]u8 {
    var list = std.ArrayListUnmanaged(u8){};
    errdefer list.deinit(allocator);

    var printer = PrettyPrinter.init(.{});
    try printer.printModule(list.writer(allocator), module);

    return list.toOwnedSlice(allocator);
}

/// Print a module to stderr for debugging
pub fn debugPrintModule(module: Module) void {
    const stderr = std.io.getStdErr().writer();
    var printer = PrettyPrinter.init(.{});
    printer.printModule(stderr, module) catch {};
}

// ============================================================================
// Tests
// ============================================================================

test "pretty print empty module" {
    const testing = std.testing;

    const loc = lexer.Location.init(1, 1, 0);
    const span = Span.init(loc, loc);

    const module = Module.init(null, &.{}, &.{}, span);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(testing.allocator);

    var printer = PrettyPrinter.init(.{ .show_locations = false });
    try printer.printModule(output.writer(testing.allocator), module);

    try testing.expectEqualStrings("Module\n", output.items);
}

test "pretty print module with name" {
    const testing = std.testing;

    const loc = lexer.Location.init(1, 1, 0);
    const span = Span.init(loc, loc);

    const parts = [_]Identifier{
        Identifier.init("my", span),
        Identifier.init("module", span),
    };

    const name = QualifiedName.init(&parts, span);
    const module = Module.init(name, &.{}, &.{}, span);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(testing.allocator);

    var printer = PrettyPrinter.init(.{ .show_locations = false });
    try printer.printModule(output.writer(testing.allocator), module);

    try testing.expect(std.mem.indexOf(u8, output.items, "name: my.module") != null);
}

test "pretty print with max depth" {
    const testing = std.testing;

    const loc = lexer.Location.init(1, 1, 0);
    const span = Span.init(loc, loc);

    const module = Module.init(null, &.{}, &.{}, span);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(testing.allocator);

    var printer = PrettyPrinter.init(.{
        .show_locations = false,
        .max_depth = 1,
    });
    try printer.printModule(output.writer(testing.allocator), module);

    try testing.expect(std.mem.indexOf(u8, output.items, "...") != null);
}
