const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("../lexer/root.zig").Token;
const Span = @import("../lexer/root.zig").Span;
const Location = @import("../lexer/root.zig").Location;

// ============================================================================
// Core Types
// ============================================================================

/// An identifier with source location
pub const Identifier = struct {
    name: []const u8,
    span: Span,

    pub fn init(name: []const u8, span: Span) Identifier {
        return .{ .name = name, .span = span };
    }
};

/// A qualified name (e.g., std.collections.List)
pub const QualifiedName = struct {
    parts: []const Identifier,
    span: Span,

    pub fn init(parts: []const Identifier, span: Span) QualifiedName {
        return .{ .parts = parts, .span = span };
    }

    pub fn simple(ident: Identifier) QualifiedName {
        return .{
            .parts = &[_]Identifier{ident},
            .span = ident.span,
        };
    }
};

/// Generic type parameter with optional bounds
pub const TypeParameter = struct {
    name: Identifier,
    bounds: []const QualifiedName, // e.g., Ord, Eq, Hash
    span: Span,

    pub fn init(name: Identifier, bounds: []const QualifiedName, span: Span) TypeParameter {
        return .{ .name = name, .bounds = bounds, .span = span };
    }
};

// ============================================================================
// Module and Imports
// ============================================================================

/// A complete Sanna module
pub const Module = struct {
    /// Module declaration (module name)
    name: ?QualifiedName,
    /// Import declarations
    imports: []const Import,
    /// Top-level declarations
    declarations: []const Declaration,
    /// Source span
    span: Span,

    pub fn init(
        name: ?QualifiedName,
        imports: []const Import,
        declarations: []const Declaration,
        span: Span,
    ) Module {
        return .{
            .name = name,
            .imports = imports,
            .declarations = declarations,
            .span = span,
        };
    }
};

/// An import declaration
pub const Import = struct {
    /// The module path (e.g., std.collections)
    path: QualifiedName,
    /// Specific items to import (empty means import all)
    items: []const ImportItem,
    /// Alias for the import (e.g., import std.collections as col)
    alias: ?Identifier,
    /// Source span
    span: Span,

    pub fn init(
        path: QualifiedName,
        items: []const ImportItem,
        alias: ?Identifier,
        span: Span,
    ) Import {
        return .{
            .path = path,
            .items = items,
            .alias = alias,
            .span = span,
        };
    }
};

/// An individual item in an import list
pub const ImportItem = struct {
    name: Identifier,
    alias: ?Identifier,
    span: Span,

    pub fn init(name: Identifier, alias: ?Identifier, span: Span) ImportItem {
        return .{ .name = name, .alias = alias, .span = span };
    }
};

// ============================================================================
// Top-Level Declarations
// ============================================================================

/// Visibility modifier
pub const Visibility = enum {
    private,
    public,
};

/// A top-level declaration
pub const Declaration = struct {
    kind: DeclarationKind,
    visibility: Visibility,
    attributes: []const Attribute,
    doc_comment: ?[]const u8,
    span: Span,

    pub fn init(
        kind: DeclarationKind,
        visibility: Visibility,
        attributes: []const Attribute,
        doc_comment: ?[]const u8,
        span: Span,
    ) Declaration {
        return .{
            .kind = kind,
            .visibility = visibility,
            .attributes = attributes,
            .doc_comment = doc_comment,
            .span = span,
        };
    }
};

/// The kind of declaration
pub const DeclarationKind = union(enum) {
    type_def: TypeDefinition,
    model_def: ModelDefinition,
    spec_fn: FunctionSpec,
    spec_interface: InterfaceSpec,
    invariant: InvariantDecl,
    axiom: AxiomDecl,
    lemma: LemmaDecl,
};

// ============================================================================
// Type Definitions
// ============================================================================

/// A type definition
pub const TypeDefinition = struct {
    name: Identifier,
    type_params: []const TypeParameter,
    body: TypeBody,
    invariants: []const Expression,
    span: Span,

    pub fn init(
        name: Identifier,
        type_params: []const TypeParameter,
        body: TypeBody,
        invariants: []const Expression,
        span: Span,
    ) TypeDefinition {
        return .{
            .name = name,
            .type_params = type_params,
            .body = body,
            .invariants = invariants,
            .span = span,
        };
    }
};

/// The body of a type definition
pub const TypeBody = union(enum) {
    /// Alias: type Email = string
    alias: TypeExpr,
    /// Product type (record): type User = { id: i32, name: string }
    product: []const Field,
    /// Sum type: type Status = | Active | Inactive { reason: string }
    sum: []const Variant,
};

/// A field in a product type
pub const Field = struct {
    name: Identifier,
    type_expr: TypeExpr,
    default_value: ?Expression,
    span: Span,

    pub fn init(
        name: Identifier,
        type_expr: TypeExpr,
        default_value: ?Expression,
        span: Span,
    ) Field {
        return .{
            .name = name,
            .type_expr = type_expr,
            .default_value = default_value,
            .span = span,
        };
    }
};

/// A variant in a sum type
pub const Variant = struct {
    name: Identifier,
    fields: []const Field, // Empty for unit variants
    span: Span,

    pub fn init(name: Identifier, fields: []const Field, span: Span) Variant {
        return .{ .name = name, .fields = fields, .span = span };
    }
};

// ============================================================================
// Model Definitions
// ============================================================================

/// A model definition (mathematical model for specification)
pub const ModelDefinition = struct {
    name: Identifier,
    type_params: []const TypeParameter,
    members: []const ModelMember,
    span: Span,

    pub fn init(
        name: Identifier,
        type_params: []const TypeParameter,
        members: []const ModelMember,
        span: Span,
    ) ModelDefinition {
        return .{
            .name = name,
            .type_params = type_params,
            .members = members,
            .span = span,
        };
    }
};

/// A member of a model definition
pub const ModelMember = union(enum) {
    /// A constant or function signature
    signature: ModelSignature,
    /// An axiom within the model
    axiom: AxiomDecl,
};

/// A signature in a model (constant or function)
pub const ModelSignature = struct {
    name: Identifier,
    params: []const Parameter,
    return_type: TypeExpr,
    span: Span,

    pub fn init(
        name: Identifier,
        params: []const Parameter,
        return_type: TypeExpr,
        span: Span,
    ) ModelSignature {
        return .{
            .name = name,
            .params = params,
            .return_type = return_type,
            .span = span,
        };
    }
};

// ============================================================================
// Function Specifications
// ============================================================================

/// A function specification
pub const FunctionSpec = struct {
    name: Identifier,
    type_params: []const TypeParameter,
    params: []const Parameter,
    return_type: TypeExpr,
    requires: []const Expression,
    ensures: []const Expression,
    modifies: []const Expression,
    decreases: ?Expression,
    is_pure: bool,
    span: Span,

    pub fn init(
        name: Identifier,
        type_params: []const TypeParameter,
        params: []const Parameter,
        return_type: TypeExpr,
        span: Span,
    ) FunctionSpec {
        return .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .requires = &.{},
            .ensures = &.{},
            .modifies = &.{},
            .decreases = null,
            .is_pure = false,
            .span = span,
        };
    }
};

/// A function parameter
pub const Parameter = struct {
    name: Identifier,
    type_expr: TypeExpr,
    is_self: bool, // True for 'self' parameter
    span: Span,

    pub fn init(name: Identifier, type_expr: TypeExpr, span: Span) Parameter {
        return .{
            .name = name,
            .type_expr = type_expr,
            .is_self = false,
            .span = span,
        };
    }

    pub fn selfParam(type_expr: TypeExpr, span: Span) Parameter {
        return .{
            .name = Identifier.init("self", span),
            .type_expr = type_expr,
            .is_self = true,
            .span = span,
        };
    }
};

// ============================================================================
// Interface Specifications
// ============================================================================

/// An interface specification
pub const InterfaceSpec = struct {
    name: Identifier,
    type_params: []const TypeParameter,
    super_traits: []const QualifiedName,
    members: []const InterfaceMember,
    invariants: []const Expression,
    span: Span,

    pub fn init(
        name: Identifier,
        type_params: []const TypeParameter,
        super_traits: []const QualifiedName,
        members: []const InterfaceMember,
        invariants: []const Expression,
        span: Span,
    ) InterfaceSpec {
        return .{
            .name = name,
            .type_params = type_params,
            .super_traits = super_traits,
            .members = members,
            .invariants = invariants,
            .span = span,
        };
    }
};

/// A member of an interface
pub const InterfaceMember = union(enum) {
    /// Associated type
    associated_type: AssociatedType,
    /// Function specification
    function: FunctionSpec,
};

/// An associated type in an interface
pub const AssociatedType = struct {
    name: Identifier,
    bounds: []const QualifiedName,
    span: Span,

    pub fn init(name: Identifier, bounds: []const QualifiedName, span: Span) AssociatedType {
        return .{ .name = name, .bounds = bounds, .span = span };
    }
};

// ============================================================================
// Invariants, Axioms, and Lemmas
// ============================================================================

/// An invariant declaration
pub const InvariantDecl = struct {
    name: ?Identifier,
    condition: Expression,
    span: Span,

    pub fn init(name: ?Identifier, condition: Expression, span: Span) InvariantDecl {
        return .{ .name = name, .condition = condition, .span = span };
    }
};

/// An axiom declaration
pub const AxiomDecl = struct {
    name: Identifier,
    condition: Expression,
    span: Span,

    pub fn init(name: Identifier, condition: Expression, span: Span) AxiomDecl {
        return .{ .name = name, .condition = condition, .span = span };
    }
};

/// A lemma declaration
pub const LemmaDecl = struct {
    name: Identifier,
    type_params: []const TypeParameter,
    params: []const Parameter,
    condition: Expression,
    span: Span,

    pub fn init(
        name: Identifier,
        type_params: []const TypeParameter,
        params: []const Parameter,
        condition: Expression,
        span: Span,
    ) LemmaDecl {
        return .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .condition = condition,
            .span = span,
        };
    }
};

// ============================================================================
// Type Expressions
// ============================================================================

/// A type expression
pub const TypeExpr = struct {
    kind: TypeExprKind,
    span: Span,

    pub fn init(kind: TypeExprKind, span: Span) TypeExpr {
        return .{ .kind = kind, .span = span };
    }
};

/// The kind of type expression
pub const TypeExprKind = union(enum) {
    /// Named type (e.g., i32, User, List)
    named: QualifiedName,
    /// Generic type application (e.g., List[i32], Map[string, User])
    generic: GenericType,
    /// Function type (e.g., fn(i32) -> bool)
    function: FunctionType,
    /// Optional type (e.g., ?i32)
    optional: *const TypeExpr,
    /// Result type (e.g., Result[T, E])
    result: ResultType,
    /// Tuple type (e.g., (i32, string))
    tuple: []const TypeExpr,
    /// Self type (in interface contexts)
    self_type,
    /// Hole type (???)
    hole,
};

/// A generic type application
pub const GenericType = struct {
    base: QualifiedName,
    args: []const TypeExpr,

    pub fn init(base: QualifiedName, args: []const TypeExpr) GenericType {
        return .{ .base = base, .args = args };
    }
};

/// A function type
pub const FunctionType = struct {
    params: []const TypeExpr,
    return_type: *const TypeExpr,

    pub fn init(params: []const TypeExpr, return_type: *const TypeExpr) FunctionType {
        return .{ .params = params, .return_type = return_type };
    }
};

/// A Result type
pub const ResultType = struct {
    ok_type: *const TypeExpr,
    err_type: *const TypeExpr,

    pub fn init(ok_type: *const TypeExpr, err_type: *const TypeExpr) ResultType {
        return .{ .ok_type = ok_type, .err_type = err_type };
    }
};

// ============================================================================
// Expressions
// ============================================================================

/// An expression
pub const Expression = struct {
    kind: ExpressionKind,
    span: Span,

    pub fn init(kind: ExpressionKind, span: Span) Expression {
        return .{ .kind = kind, .span = span };
    }
};

/// The kind of expression
pub const ExpressionKind = union(enum) {
    // Literals
    int_literal: i64,
    float_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,

    // Identifiers and access
    identifier: Identifier,
    qualified: QualifiedName,
    field_access: FieldAccess,
    index_access: IndexAccess,
    method_call: MethodCall,

    // Operators
    binary: BinaryExpr,
    unary: UnaryExpr,

    // Logical expressions
    and_expr: AndExpr,
    or_expr: OrExpr,
    not_expr: NotExpr,
    implies: ImpliesExpr,
    iff: IffExpr,

    // Quantifiers
    forall: QuantifierExpr,
    exists: QuantifierExpr,

    // Control flow
    if_expr: IfExpr,
    match_expr: MatchExpr,
    let_expr: LetExpr,

    // Function-related
    call: CallExpr,
    lambda: LambdaExpr,

    // Specification-specific
    old: *const Expression,
    result,
    self_expr,

    // Collections
    sequence_literal: []const Expression,
    set_literal: []const Expression,
    map_literal: []const MapEntry,
    set_comprehension: SetComprehension,

    // Range
    range: RangeExpr,

    // Hole
    hole,
    typed_hole: *const TypeExpr,
};

/// Binary expression
pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *const Expression,
    right: *const Expression,

    pub fn init(op: BinaryOp, left: *const Expression, right: *const Expression) BinaryExpr {
        return .{ .op = op, .left = left, .right = right };
    }
};

/// Binary operators
pub const BinaryOp = enum {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,

    // Comparison
    eq,
    ne,
    lt,
    le,
    gt,
    ge,

    // Set operations
    in_op, // 'in'
    union_op,
    intersect,
    subset,
};

/// Unary expression
pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *const Expression,

    pub fn init(op: UnaryOp, operand: *const Expression) UnaryExpr {
        return .{ .op = op, .operand = operand };
    }
};

/// Unary operators
pub const UnaryOp = enum {
    neg,
    not,
};

/// And expression (separate from binary for short-circuit semantics)
pub const AndExpr = struct {
    left: *const Expression,
    right: *const Expression,
};

/// Or expression
pub const OrExpr = struct {
    left: *const Expression,
    right: *const Expression,
};

/// Not expression
pub const NotExpr = struct {
    operand: *const Expression,
};

/// Implies expression (=>)
pub const ImpliesExpr = struct {
    antecedent: *const Expression,
    consequent: *const Expression,
};

/// If-and-only-if expression (<=>)
pub const IffExpr = struct {
    left: *const Expression,
    right: *const Expression,
};

/// Field access expression (e.g., user.name)
pub const FieldAccess = struct {
    object: *const Expression,
    field: Identifier,
};

/// Index access expression (e.g., list[0])
pub const IndexAccess = struct {
    object: *const Expression,
    index: *const Expression,
};

/// Method call expression (e.g., list.append(x))
pub const MethodCall = struct {
    object: *const Expression,
    method: Identifier,
    args: []const Expression,
};

/// Quantifier expression (forall/exists)
pub const QuantifierExpr = struct {
    variables: []const QuantifierVar,
    condition: ?*const Expression, // Optional such_that condition
    body: *const Expression,
};

/// A variable in a quantifier
pub const QuantifierVar = struct {
    name: Identifier,
    domain: QuantifierDomain,
    span: Span,
};

/// The domain of a quantified variable
pub const QuantifierDomain = union(enum) {
    /// Type domain (x: T)
    type_domain: TypeExpr,
    /// Range domain (x in 0..n)
    range: RangeExpr,
    /// Collection domain (x in collection)
    collection: *const Expression,
};

/// If expression
pub const IfExpr = struct {
    condition: *const Expression,
    then_branch: *const Expression,
    else_branch: ?*const Expression,
};

/// Match expression
pub const MatchExpr = struct {
    scrutinee: *const Expression,
    arms: []const MatchArm,
};

/// A match arm
pub const MatchArm = struct {
    pattern: Pattern,
    guard: ?*const Expression,
    body: *const Expression,
    span: Span,
};

/// A pattern in a match expression
pub const Pattern = struct {
    kind: PatternKind,
    span: Span,
};

/// The kind of pattern
pub const PatternKind = union(enum) {
    /// Wildcard pattern (_)
    wildcard,
    /// Binding pattern (x)
    binding: Identifier,
    /// Literal pattern (42, "hello", true)
    literal: LiteralPattern,
    /// Constructor pattern (Some(x), Ok(value))
    constructor: ConstructorPattern,
    /// Tuple pattern ((a, b))
    tuple: []const Pattern,
    /// Record pattern ({ x, y: z })
    record: []const FieldPattern,
};

/// A literal in a pattern
pub const LiteralPattern = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    bool: bool,
};

/// A constructor pattern
pub const ConstructorPattern = struct {
    name: QualifiedName,
    args: []const Pattern,
};

/// A field pattern
pub const FieldPattern = struct {
    name: Identifier,
    pattern: ?*const Pattern, // null means bind to same name
    span: Span,
};

/// Let expression
pub const LetExpr = struct {
    name: Identifier,
    type_annotation: ?TypeExpr,
    value: *const Expression,
    body: *const Expression,
};

/// Call expression
pub const CallExpr = struct {
    callee: *const Expression,
    type_args: []const TypeExpr,
    args: []const Expression,
};

/// Lambda expression
pub const LambdaExpr = struct {
    params: []const Parameter,
    body: *const Expression,
};

/// Map entry
pub const MapEntry = struct {
    key: Expression,
    value: Expression,
};

/// Set comprehension
pub const SetComprehension = struct {
    element: *const Expression,
    variable: Identifier,
    domain: *const Expression,
    condition: ?*const Expression,
};

/// Range expression
pub const RangeExpr = struct {
    start: *const Expression,
    end: *const Expression,
    inclusive: bool,
};

// ============================================================================
// Attributes
// ============================================================================

/// An attribute (e.g., @author, @confidence)
pub const Attribute = struct {
    name: Identifier,
    args: []const AttributeArg,
    span: Span,

    pub fn init(name: Identifier, args: []const AttributeArg, span: Span) Attribute {
        return .{ .name = name, .args = args, .span = span };
    }
};

/// An argument to an attribute
pub const AttributeArg = struct {
    name: ?Identifier, // Named argument (null for positional)
    value: AttributeValue,
    span: Span,
};

/// The value of an attribute argument
pub const AttributeValue = union(enum) {
    string: []const u8,
    int: i64,
    float: f64,
    bool: bool,
    identifier: Identifier,
};

// ============================================================================
// Tests
// ============================================================================

test "identifier creation" {
    const span = Span.init(Location.init(1, 1, 0), Location.init(1, 5, 4));
    const ident = Identifier.init("test", span);
    try std.testing.expectEqualStrings("test", ident.name);
}

test "qualified name creation" {
    const span = Span.init(Location.init(1, 1, 0), Location.init(1, 20, 19));
    const parts = [_]Identifier{
        Identifier.init("std", span),
        Identifier.init("collections", span),
        Identifier.init("List", span),
    };
    const qname = QualifiedName.init(&parts, span);
    try std.testing.expectEqual(@as(usize, 3), qname.parts.len);
    try std.testing.expectEqualStrings("std", qname.parts[0].name);
}

test "type parameter with bounds" {
    const span = Span.init(Location.init(1, 1, 0), Location.init(1, 10, 9));
    const name = Identifier.init("T", span);
    const bounds = [_]QualifiedName{
        QualifiedName.simple(Identifier.init("Ord", span)),
        QualifiedName.simple(Identifier.init("Eq", span)),
    };
    const param = TypeParameter.init(name, &bounds, span);
    try std.testing.expectEqualStrings("T", param.name.name);
    try std.testing.expectEqual(@as(usize, 2), param.bounds.len);
}
