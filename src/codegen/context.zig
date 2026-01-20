//! Generation Context Preparation
//!
//! Prepares the context for AI code generation by gathering related types,
//! specifications, examples, and conventions from the Sanna codebase.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/Ast.zig");
const types_mod = @import("../types/root.zig");
const TypeContext = types_mod.TypeContext;
const Provenance = @import("../provenance/Provenance.zig");
const SpecBinding = @import("../specs/SpecBinding.zig");
const types = @import("types.zig");

const GenerationContext = types.GenerationContext;
const SpecificationRef = types.SpecificationRef;
const TargetLanguage = types.TargetLanguage;

/// Builds generation context from Sanna specifications.
pub const ContextBuilder = struct {
    allocator: Allocator,
    modules: std.StringHashMap(*Ast.Module),
    spec_binding: ?*SpecBinding,
    type_context: ?*TypeContext,
    examples: std.ArrayListUnmanaged(GenerationContext.ExampleCode),
    related_types: std.ArrayListUnmanaged(GenerationContext.TypeContext),
    related_specs: std.ArrayListUnmanaged(GenerationContext.SpecContext),
    imports: std.ArrayListUnmanaged([]const u8),
    conventions: ?GenerationContext.ConventionGuide,
    max_depth: u32,
    visited_types: std.StringHashMap(void),
    visited_specs: std.StringHashMap(void),

    pub fn init(allocator: Allocator) ContextBuilder {
        return .{
            .allocator = allocator,
            .modules = std.StringHashMap(*Ast.Module).init(allocator),
            .spec_binding = null,
            .type_context = null,
            .examples = .{},
            .related_types = .{},
            .related_specs = .{},
            .imports = .{},
            .conventions = null,
            .max_depth = 3,
            .visited_types = std.StringHashMap(void).init(allocator),
            .visited_specs = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *ContextBuilder) void {
        self.modules.deinit();
        self.examples.deinit(self.allocator);
        self.related_types.deinit(self.allocator);
        self.related_specs.deinit(self.allocator);
        self.imports.deinit(self.allocator);
        self.visited_types.deinit();
        self.visited_specs.deinit();
    }

    pub fn setSpecBinding(self: *ContextBuilder, binding: *SpecBinding) void {
        self.spec_binding = binding;
    }

    pub fn setTypeContext(self: *ContextBuilder, ctx: *TypeContext) void {
        self.type_context = ctx;
    }

    pub fn addModule(self: *ContextBuilder, path: []const u8, module: *Ast.Module) !void {
        try self.modules.put(path, module);
    }

    pub fn setConventions(self: *ContextBuilder, conventions: GenerationContext.ConventionGuide) void {
        self.conventions = conventions;
    }

    pub fn addExample(self: *ContextBuilder, example: GenerationContext.ExampleCode) !void {
        try self.examples.append(self.allocator, example);
    }

    pub fn setMaxDepth(self: *ContextBuilder, depth: u32) void {
        self.max_depth = depth;
    }

    pub fn buildContext(self: *ContextBuilder, spec_ref: SpecificationRef) GenerationContext {
        _ = spec_ref;
        self.related_types.clearRetainingCapacity();
        self.related_specs.clearRetainingCapacity();
        self.imports.clearRetainingCapacity();
        self.visited_types.clearRetainingCapacity();
        self.visited_specs.clearRetainingCapacity();

        return GenerationContext{
            .related_types = self.related_types.items,
            .related_specs = self.related_specs.items,
            .examples = self.examples.items,
            .conventions = self.conventions,
            .existing_code = null,
            .imports = self.imports.items,
        };
    }
};

/// Formats specifications for AI consumption.
pub const SpecFormatter = struct {
    allocator: Allocator,
    target: TargetLanguage,
    include_docs: bool,
    include_attributes: bool,

    pub fn init(allocator: Allocator, target: TargetLanguage) SpecFormatter {
        return .{
            .allocator = allocator,
            .target = target,
            .include_docs = true,
            .include_attributes = true,
        };
    }

    pub fn formatFunctionSpec(self: *SpecFormatter, spec: *const Ast.FunctionSpec) ![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        try buf.appendSlice(self.allocator, "spec fn ");
        try buf.appendSlice(self.allocator, spec.name);
        try buf.appendSlice(self.allocator, "(...)");

        return buf.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ContextBuilder.init" {
    var builder = ContextBuilder.init(std.testing.allocator);
    defer builder.deinit();

    try std.testing.expectEqual(@as(u32, 3), builder.max_depth);
}

test "SpecFormatter basic" {
    const formatter = SpecFormatter.init(std.testing.allocator, .klar);
    try std.testing.expectEqual(TargetLanguage.klar, formatter.target);
    try std.testing.expect(formatter.include_docs);
}
