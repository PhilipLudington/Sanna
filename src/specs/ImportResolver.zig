const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Type = @import("../types/root.zig").Type;
const TypeContext = @import("../types/root.zig").TypeContext;
const TypeError = @import("../types/root.zig").TypeError;
const Span = @import("../lexer/root.zig").Span;
const ModuleLoader = @import("ModuleLoader.zig").ModuleLoader;

// ============================================================================
// Import Resolver
// ============================================================================

/// The ImportResolver processes import declarations and binds imported names
/// to the current type context. It handles:
/// - Resolving import paths to loaded modules
/// - Binding imported types, models, interfaces, and specs
/// - Handling selective imports (import { Item1, Item2 })
/// - Handling aliased imports (import X as Y)
/// - Detecting shadowing and ambiguous references
/// - Enforcing visibility rules (pub vs private)
pub const ImportResolver = struct {
    allocator: Allocator,
    loader: *ModuleLoader,
    ctx: *TypeContext,

    /// Imported names mapped to their source modules and definitions
    imported_names: std.StringHashMapUnmanaged(ImportedItem),

    /// Module aliases (import X as alias)
    module_aliases: std.StringHashMapUnmanaged([]const u8),

    /// Collected errors during import resolution
    errors: std.ArrayListUnmanaged(ImportError),

    /// An imported item with its origin
    pub const ImportedItem = struct {
        kind: Kind,
        /// The fully qualified name in the source module
        source_name: []const u8,
        /// The source module name
        source_module: []const u8,
        /// The local name (possibly aliased)
        local_name: []const u8,
        /// The span of the import declaration
        span: ?Span = null,
        /// Visibility of the imported item
        visibility: Ast.Visibility = .public,

        pub const Kind = enum {
            type_def,
            model_def,
            interface_def,
            function_spec,
            axiom,
            lemma,
            invariant,
        };
    };

    /// An error during import resolution
    pub const ImportError = struct {
        kind: Kind,
        message: []const u8,
        span: ?Span = null,
        notes: []const Note = &.{},

        pub const Kind = enum {
            /// Module not found
            module_not_found,
            /// Item not found in module
            item_not_found,
            /// Item is private
            item_not_public,
            /// Name collision with existing import
            import_collision,
            /// Name shadows local definition
            shadows_local,
            /// Ambiguous import (multiple sources)
            ambiguous_import,
            /// Invalid import path
            invalid_import,
        };

        pub const Note = struct {
            message: []const u8,
            span: ?Span = null,
        };
    };

    pub fn init(allocator: Allocator, loader: *ModuleLoader, ctx: *TypeContext) ImportResolver {
        return .{
            .allocator = allocator,
            .loader = loader,
            .ctx = ctx,
            .imported_names = .{},
            .module_aliases = .{},
            .errors = .{},
        };
    }

    pub fn deinit(self: *ImportResolver) void {
        // Free imported names
        var iter = self.imported_names.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.imported_names.deinit(self.allocator);

        // Free module aliases
        var alias_iter = self.module_aliases.iterator();
        while (alias_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.module_aliases.deinit(self.allocator);

        // Free errors
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    // ========================================================================
    // Import Resolution
    // ========================================================================

    /// Process all imports in a module
    pub fn resolveImports(self: *ImportResolver, imports: []const Ast.Import) !void {
        for (imports) |import_decl| {
            try self.resolveImport(import_decl);
        }
    }

    /// Process a single import declaration
    pub fn resolveImport(self: *ImportResolver, import_decl: Ast.Import) !void {
        // Convert the import path to a module name
        const module_name = try self.loader.qualifiedNameToString(import_decl.path);
        defer self.allocator.free(module_name);

        // Load the module
        const loaded = try self.loader.loadModule(module_name) orelse {
            try self.addError(.{
                .kind = .module_not_found,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "module not found: {s}",
                    .{module_name},
                ),
                .span = import_decl.span,
            });
            return;
        };

        // Handle module alias: import std.collections as col
        if (import_decl.alias) |alias| {
            const alias_copy = try self.allocator.dupe(u8, alias.name);
            errdefer self.allocator.free(alias_copy);
            const module_copy = try self.allocator.dupe(u8, module_name);
            errdefer self.allocator.free(module_copy);
            try self.module_aliases.put(self.allocator, alias_copy, module_copy);
        }

        // Handle selective imports: import std.collections { List, Map }
        if (import_decl.items.len > 0) {
            for (import_decl.items) |item| {
                try self.resolveSelectiveImport(loaded, item);
            }
        } else {
            // Import all public items from the module
            try self.resolveWildcardImport(loaded, import_decl);
        }
    }

    /// Resolve a selective import (import path { Item })
    fn resolveSelectiveImport(self: *ImportResolver, loaded: *const ModuleLoader.LoadedModule, item: Ast.ImportItem) !void {
        const item_name = item.name.name;
        const local_name = if (item.alias) |alias| alias.name else item_name;

        // Look for the item in the loaded module's declarations
        var found = false;
        for (loaded.ast.declarations) |decl| {
            const decl_name = getDeclName(decl);
            if (decl_name != null and std.mem.eql(u8, decl_name.?, item_name)) {
                // Check visibility
                if (decl.visibility != .public) {
                    try self.addError(.{
                        .kind = .item_not_public,
                        .message = try std.fmt.allocPrint(
                            self.allocator,
                            "'{s}' is not public in module '{s}'",
                            .{ item_name, loaded.name },
                        ),
                        .span = item.span,
                    });
                    return;
                }

                // Check for collision
                if (self.imported_names.contains(local_name)) {
                    try self.addError(.{
                        .kind = .import_collision,
                        .message = try std.fmt.allocPrint(
                            self.allocator,
                            "import collision: '{s}' is already imported",
                            .{local_name},
                        ),
                        .span = item.span,
                    });
                    return;
                }

                // Register the import
                const imported = ImportedItem{
                    .kind = declKindToImportKind(decl.kind),
                    .source_name = try self.allocator.dupe(u8, item_name),
                    .source_module = try self.allocator.dupe(u8, loaded.name),
                    .local_name = try self.allocator.dupe(u8, local_name),
                    .span = item.span,
                    .visibility = decl.visibility,
                };

                const key = try self.allocator.dupe(u8, local_name);
                try self.imported_names.put(self.allocator, key, imported);
                found = true;
                break;
            }
        }

        if (!found) {
            try self.addError(.{
                .kind = .item_not_found,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "'{s}' not found in module '{s}'",
                    .{ item_name, loaded.name },
                ),
                .span = item.span,
            });
        }
    }

    /// Resolve a wildcard import (import all public items)
    fn resolveWildcardImport(self: *ImportResolver, loaded: *const ModuleLoader.LoadedModule, import_decl: Ast.Import) !void {
        for (loaded.ast.declarations) |decl| {
            // Only import public items
            if (decl.visibility != .public) continue;

            const decl_name = getDeclName(decl) orelse continue;

            // Check for collision (skip silently for wildcard imports)
            if (self.imported_names.contains(decl_name)) {
                continue;
            }

            // Register the import
            const imported = ImportedItem{
                .kind = declKindToImportKind(decl.kind),
                .source_name = try self.allocator.dupe(u8, decl_name),
                .source_module = try self.allocator.dupe(u8, loaded.name),
                .local_name = try self.allocator.dupe(u8, decl_name),
                .span = import_decl.span,
                .visibility = decl.visibility,
            };

            const key = try self.allocator.dupe(u8, decl_name);
            try self.imported_names.put(self.allocator, key, imported);
        }
    }

    // ========================================================================
    // Name Resolution
    // ========================================================================

    /// Look up an imported name
    pub fn lookupImport(self: *const ImportResolver, name: []const u8) ?*const ImportedItem {
        return self.imported_names.getPtr(name);
    }

    /// Look up a module alias
    pub fn lookupModuleAlias(self: *const ImportResolver, alias: []const u8) ?[]const u8 {
        return self.module_aliases.get(alias);
    }

    /// Resolve a potentially qualified name (e.g., "col.List" or "List")
    pub fn resolveQualifiedName(self: *const ImportResolver, qname: Ast.QualifiedName) ?ResolvedName {
        if (qname.parts.len == 0) return null;

        // Single identifier - look up directly
        if (qname.parts.len == 1) {
            const name = qname.parts[0].name;
            if (self.lookupImport(name)) |imported| {
                return .{
                    .module = imported.source_module,
                    .name = imported.source_name,
                    .kind = imported.kind,
                };
            }
            return null;
        }

        // Qualified name - check if first part is a module alias
        const first = qname.parts[0].name;
        if (self.lookupModuleAlias(first)) |module_name| {
            // Build the full qualified name within that module
            var name_buf: [512]u8 = undefined;
            var pos: usize = 0;
            for (qname.parts[1..], 0..) |part, i| {
                if (i > 0) {
                    name_buf[pos] = '.';
                    pos += 1;
                }
                @memcpy(name_buf[pos..][0..part.name.len], part.name);
                pos += part.name.len;
            }

            return .{
                .module = module_name,
                .name = name_buf[0..pos],
                .kind = null, // Unknown until looked up
            };
        }

        // Build full qualified name and look up
        var name_buf: [512]u8 = undefined;
        var pos: usize = 0;
        for (qname.parts, 0..) |part, i| {
            if (i > 0) {
                name_buf[pos] = '.';
                pos += 1;
            }
            @memcpy(name_buf[pos..][0..part.name.len], part.name);
            pos += part.name.len;
        }

        // Check if this full name is imported
        if (self.lookupImport(name_buf[0..pos])) |imported| {
            return .{
                .module = imported.source_module,
                .name = imported.source_name,
                .kind = imported.kind,
            };
        }

        return null;
    }

    pub const ResolvedName = struct {
        module: []const u8,
        name: []const u8,
        kind: ?ImportedItem.Kind,
    };

    // ========================================================================
    // Error Handling
    // ========================================================================

    fn addError(self: *ImportResolver, err: ImportError) !void {
        try self.errors.append(self.allocator, err);
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const ImportResolver) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const ImportResolver) []const ImportError {
        return self.errors.items;
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Get the name of a declaration
    fn getDeclName(decl: Ast.Declaration) ?[]const u8 {
        return switch (decl.kind) {
            .type_def => |td| td.name.name,
            .model_def => |md| md.name.name,
            .spec_fn => |sf| sf.name.name,
            .spec_interface => |si| si.name.name,
            .invariant => |inv| inv.name,
            .axiom => |ax| ax.name.name,
            .lemma => |lm| lm.name.name,
        };
    }

    /// Convert declaration kind to import kind
    fn declKindToImportKind(kind: Ast.DeclarationKind) ImportedItem.Kind {
        return switch (kind) {
            .type_def => .type_def,
            .model_def => .model_def,
            .spec_fn => .function_spec,
            .spec_interface => .interface_def,
            .invariant => .invariant,
            .axiom => .axiom,
            .lemma => .lemma,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "import resolver init/deinit" {
    const testing = std.testing;
    var loader = ModuleLoader.init(testing.allocator);
    defer loader.deinit();

    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    var resolver = ImportResolver.init(testing.allocator, &loader, &ctx);
    defer resolver.deinit();

    try testing.expect(!resolver.hasErrors());
}
