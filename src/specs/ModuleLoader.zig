const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Parser = @import("../parser/root.zig").Parser;
const Lexer = @import("../lexer/root.zig").Lexer;
const Span = @import("../lexer/root.zig").Span;

// ============================================================================
// Module Loader
// ============================================================================

/// The ModuleLoader is responsible for resolving module paths to files,
/// loading and parsing modules, caching parsed modules, and detecting
/// circular imports.
pub const ModuleLoader = struct {
    allocator: Allocator,

    /// Search paths for resolving module imports
    search_paths: std.ArrayListUnmanaged([]const u8),

    /// Cache of loaded modules by their canonical path
    loaded_modules: std.StringHashMapUnmanaged(LoadedModule),

    /// Modules currently being loaded (for circular import detection)
    loading_stack: std.ArrayListUnmanaged([]const u8),

    /// Collected errors during loading
    errors: std.ArrayListUnmanaged(LoadError),

    /// A successfully loaded module
    pub const LoadedModule = struct {
        /// The canonical path to the module file
        path: []const u8,
        /// The fully qualified module name (e.g., "std.collections")
        name: []const u8,
        /// The parsed AST
        ast: *const Ast.Module,
        /// Source code (kept for error reporting)
        source: []const u8,
    };

    /// An error that occurred during module loading
    pub const LoadError = struct {
        kind: Kind,
        message: []const u8,
        module_path: ?[]const u8 = null,
        span: ?Span = null,
        import_chain: []const []const u8 = &.{},

        pub const Kind = enum {
            /// Module file not found
            module_not_found,
            /// Circular import detected
            circular_import,
            /// File read error
            io_error,
            /// Parse error in module
            parse_error,
            /// Invalid module path format
            invalid_path,
        };
    };

    pub fn init(allocator: Allocator) ModuleLoader {
        return .{
            .allocator = allocator,
            .search_paths = .{},
            .loaded_modules = .{},
            .loading_stack = .{},
            .errors = .{},
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        // Free search paths
        for (self.search_paths.items) |path| {
            self.allocator.free(path);
        }
        self.search_paths.deinit(self.allocator);

        // Free loaded modules
        var iter = self.loaded_modules.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            // Note: AST and source owned by module, freed by parser
        }
        self.loaded_modules.deinit(self.allocator);

        // Free loading stack
        self.loading_stack.deinit(self.allocator);

        // Free errors
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
            if (err.module_path) |p| self.allocator.free(p);
        }
        self.errors.deinit(self.allocator);
    }

    // ========================================================================
    // Configuration
    // ========================================================================

    /// Add a search path for module resolution
    pub fn addSearchPath(self: *ModuleLoader, path: []const u8) !void {
        const owned = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned);
        try self.search_paths.append(self.allocator, owned);
    }

    // ========================================================================
    // Module Loading
    // ========================================================================

    /// Load a module by its qualified name (e.g., "std.collections")
    /// Returns the loaded module or null if not found/error occurred.
    pub fn loadModule(self: *ModuleLoader, qualified_name: []const u8) !?*const LoadedModule {
        // Check cache first
        if (self.loaded_modules.getPtr(qualified_name)) |cached| {
            return cached;
        }

        // Check for circular import
        for (self.loading_stack.items) |loading| {
            if (std.mem.eql(u8, loading, qualified_name)) {
                try self.addError(.{
                    .kind = .circular_import,
                    .message = try std.fmt.allocPrint(
                        self.allocator,
                        "circular import detected: {s}",
                        .{qualified_name},
                    ),
                    .module_path = try self.allocator.dupe(u8, qualified_name),
                    .import_chain = try self.allocator.dupe([]const u8, self.loading_stack.items),
                });
                return null;
            }
        }

        // Resolve the module path first (before allocating name_copy)
        const file_path = self.resolveModulePath(qualified_name) orelse {
            try self.addError(.{
                .kind = .module_not_found,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "module not found: {s}",
                    .{qualified_name},
                ),
                .module_path = try self.allocator.dupe(u8, qualified_name),
            });
            return null;
        };
        defer self.allocator.free(file_path);

        // Add to loading stack (after we know the file exists)
        const name_copy = try self.allocator.dupe(u8, qualified_name);
        errdefer self.allocator.free(name_copy);
        try self.loading_stack.append(self.allocator, name_copy);

        // Read the file
        const source = std.fs.cwd().readFileAlloc(self.allocator, file_path, 10 * 1024 * 1024) catch |err| {
            try self.addError(.{
                .kind = .io_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "failed to read module file {s}: {}",
                    .{ file_path, err },
                ),
                .module_path = try self.allocator.dupe(u8, qualified_name),
            });
            _ = self.loading_stack.pop();
            return null;
        };
        errdefer self.allocator.free(source);

        // Tokenize the source
        var lexer = Lexer.init(self.allocator, source);
        defer lexer.deinit();
        var tokens = lexer.tokenize() catch |err| {
            try self.addError(.{
                .kind = .parse_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "tokenization error in module {s}: {}",
                    .{ qualified_name, err },
                ),
                .module_path = try self.allocator.dupe(u8, qualified_name),
            });
            _ = self.loading_stack.pop();
            return null;
        };
        defer tokens.deinit(self.allocator);

        // Parse the module
        var parser = Parser.init(self.allocator, tokens.items);
        defer parser.deinit();
        const ast = parser.parseModule() catch |err| {
            try self.addError(.{
                .kind = .parse_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "parse error in module {s}: {}",
                    .{ qualified_name, err },
                ),
                .module_path = try self.allocator.dupe(u8, qualified_name),
            });
            _ = self.loading_stack.pop();
            return null;
        };

        // Allocate the module AST on the heap
        const ast_ptr = try self.allocator.create(Ast.Module);
        ast_ptr.* = ast;

        // Create the loaded module
        const loaded = LoadedModule{
            .path = try self.allocator.dupe(u8, file_path),
            .name = name_copy,
            .ast = ast_ptr,
            .source = source,
        };

        // Cache the module
        try self.loaded_modules.put(self.allocator, name_copy, loaded);

        // Remove from loading stack
        _ = self.loading_stack.pop();

        return self.loaded_modules.getPtr(name_copy);
    }

    /// Load a module from a specific file path
    pub fn loadModuleFromFile(self: *ModuleLoader, file_path: []const u8, module_name: []const u8) !?*const LoadedModule {
        // Check cache first
        if (self.loaded_modules.getPtr(module_name)) |cached| {
            return cached;
        }

        // Read the file
        const source = std.fs.cwd().readFileAlloc(self.allocator, file_path, 10 * 1024 * 1024) catch |err| {
            try self.addError(.{
                .kind = .io_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "failed to read module file {s}: {}",
                    .{ file_path, err },
                ),
                .module_path = try self.allocator.dupe(u8, module_name),
            });
            return null;
        };
        errdefer self.allocator.free(source);

        // Tokenize the source
        var lexer = Lexer.init(self.allocator, source);
        defer lexer.deinit();
        var tokens = lexer.tokenize() catch |err| {
            try self.addError(.{
                .kind = .parse_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "tokenization error in module {s}: {}",
                    .{ module_name, err },
                ),
                .module_path = try self.allocator.dupe(u8, module_name),
            });
            return null;
        };
        defer tokens.deinit(self.allocator);

        // Parse the module
        var parser = Parser.init(self.allocator, tokens.items);
        defer parser.deinit();
        const ast = parser.parseModule() catch |err| {
            try self.addError(.{
                .kind = .parse_error,
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "parse error in module {s}: {}",
                    .{ module_name, err },
                ),
                .module_path = try self.allocator.dupe(u8, module_name),
            });
            return null;
        };

        // Allocate the module AST on the heap
        const ast_ptr = try self.allocator.create(Ast.Module);
        ast_ptr.* = ast;

        // Create the loaded module
        const name_copy = try self.allocator.dupe(u8, module_name);
        const loaded = LoadedModule{
            .path = try self.allocator.dupe(u8, file_path),
            .name = name_copy,
            .ast = ast_ptr,
            .source = source,
        };

        // Cache the module
        try self.loaded_modules.put(self.allocator, name_copy, loaded);

        return self.loaded_modules.getPtr(name_copy);
    }

    // ========================================================================
    // Path Resolution
    // ========================================================================

    /// Convert a qualified module name to a file path.
    /// e.g., "std.collections" -> "std/collections.sanna"
    fn qualifiedNameToPath(self: *ModuleLoader, qualified_name: []const u8) ![]const u8 {
        // Count dots to determine path segments
        var dot_count: usize = 0;
        for (qualified_name) |c| {
            if (c == '.') dot_count += 1;
        }

        // Allocate path buffer (name length + dots become slashes + .sanna extension)
        const path_len = qualified_name.len + 6; // ".sanna" is 6 chars
        const path = try self.allocator.alloc(u8, path_len);
        errdefer self.allocator.free(path);

        // Replace dots with path separator
        var i: usize = 0;
        for (qualified_name) |c| {
            if (c == '.') {
                path[i] = std.fs.path.sep;
            } else {
                path[i] = c;
            }
            i += 1;
        }

        // Add extension
        @memcpy(path[i..][0..6], ".sanna");

        return path;
    }

    /// Resolve a qualified module name to an actual file path by searching
    /// all configured search paths.
    fn resolveModulePath(self: *ModuleLoader, qualified_name: []const u8) ?[]const u8 {
        const relative_path = self.qualifiedNameToPath(qualified_name) catch return null;
        defer self.allocator.free(relative_path);

        // Try each search path
        for (self.search_paths.items) |search_path| {
            const full_path = std.fs.path.join(self.allocator, &.{ search_path, relative_path }) catch continue;

            // Check if file exists
            if (std.fs.cwd().access(full_path, .{})) |_| {
                return full_path;
            } else |_| {
                self.allocator.free(full_path);
            }
        }

        // Try current directory as fallback
        const cwd_path = self.allocator.dupe(u8, relative_path) catch return null;
        if (std.fs.cwd().access(cwd_path, .{})) |_| {
            return cwd_path;
        } else |_| {
            self.allocator.free(cwd_path);
        }

        return null;
    }

    // ========================================================================
    // Error Handling
    // ========================================================================

    fn addError(self: *ModuleLoader, err: LoadError) !void {
        try self.errors.append(self.allocator, err);
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const ModuleLoader) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const ModuleLoader) []const LoadError {
        return self.errors.items;
    }

    /// Clear all errors
    pub fn clearErrors(self: *ModuleLoader) void {
        for (self.errors.items) |err| {
            self.allocator.free(err.message);
            if (err.module_path) |p| self.allocator.free(p);
        }
        self.errors.clearRetainingCapacity();
    }

    // ========================================================================
    // Utility
    // ========================================================================

    /// Get a cached module by name
    pub fn getModule(self: *const ModuleLoader, name: []const u8) ?*const LoadedModule {
        return self.loaded_modules.getPtr(name);
    }

    /// Convert AST qualified name to string
    pub fn qualifiedNameToString(self: *ModuleLoader, qname: Ast.QualifiedName) ![]const u8 {
        if (qname.parts.len == 0) return "";
        if (qname.parts.len == 1) return self.allocator.dupe(u8, qname.parts[0].name);

        // Calculate total length
        var total_len: usize = 0;
        for (qname.parts, 0..) |part, i| {
            total_len += part.name.len;
            if (i < qname.parts.len - 1) total_len += 1; // dot
        }

        // Build the string
        const result = try self.allocator.alloc(u8, total_len);
        var pos: usize = 0;
        for (qname.parts, 0..) |part, i| {
            @memcpy(result[pos..][0..part.name.len], part.name);
            pos += part.name.len;
            if (i < qname.parts.len - 1) {
                result[pos] = '.';
                pos += 1;
            }
        }

        return result;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "qualified name to path conversion" {
    const testing = std.testing;
    var loader = ModuleLoader.init(testing.allocator);
    defer loader.deinit();

    const path1 = try loader.qualifiedNameToPath("std.collections");
    defer loader.allocator.free(path1);
    try testing.expectEqualStrings("std/collections.sanna", path1);

    const path2 = try loader.qualifiedNameToPath("mymodule");
    defer loader.allocator.free(path2);
    try testing.expectEqualStrings("mymodule.sanna", path2);

    const path3 = try loader.qualifiedNameToPath("a.b.c.d");
    defer loader.allocator.free(path3);
    try testing.expectEqualStrings("a/b/c/d.sanna", path3);
}

test "search path management" {
    const testing = std.testing;
    var loader = ModuleLoader.init(testing.allocator);
    defer loader.deinit();

    try loader.addSearchPath("/usr/lib/sanna");
    try loader.addSearchPath("./specs");

    try testing.expectEqual(@as(usize, 2), loader.search_paths.items.len);
    try testing.expectEqualStrings("/usr/lib/sanna", loader.search_paths.items[0]);
    try testing.expectEqualStrings("./specs", loader.search_paths.items[1]);
}

test "module not found error" {
    const testing = std.testing;
    var loader = ModuleLoader.init(testing.allocator);
    defer loader.deinit();

    const result = try loader.loadModule("nonexistent.module");
    try testing.expect(result == null);
    try testing.expect(loader.hasErrors());
    try testing.expectEqual(ModuleLoader.LoadError.Kind.module_not_found, loader.getErrors()[0].kind);
}
