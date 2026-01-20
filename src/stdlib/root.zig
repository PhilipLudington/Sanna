const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Parser = @import("../parser/root.zig").Parser;
const Lexer = @import("../lexer/root.zig").Lexer;
const ModuleLoader = @import("../specs/root.zig").ModuleLoader;

// ============================================================================
// Standard Library Module Loader
// ============================================================================

/// The StandardLibrary provides built-in specifications for common types
/// and patterns. These specifications are compiled into the binary and
/// made available to all Sanna programs.
pub const StandardLibrary = struct {
    allocator: Allocator,

    /// Parsed standard library modules
    modules: std.StringHashMapUnmanaged(*const Ast.Module),

    /// Errors encountered during initialization
    errors: std.ArrayListUnmanaged([]const u8),

    pub fn init(allocator: Allocator) StandardLibrary {
        return .{
            .allocator = allocator,
            .modules = .{},
            .errors = .{},
        };
    }

    pub fn deinit(self: *StandardLibrary) void {
        var iter = self.modules.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.modules.deinit(self.allocator);

        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit(self.allocator);
    }

    /// Load all standard library modules
    pub fn loadAll(self: *StandardLibrary) !void {
        // Load each standard library module from embedded sources
        inline for (stdlib_modules) |mod| {
            try self.loadEmbeddedModule(mod.name, mod.source);
        }
    }

    /// Load a single embedded module
    fn loadEmbeddedModule(self: *StandardLibrary, name: []const u8, source: []const u8) !void {
        // Tokenize the source
        var lexer = Lexer.init(self.allocator, source);
        defer lexer.deinit();

        var tokens = lexer.tokenize() catch |err| {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Failed to tokenize stdlib module {s}: {}",
                .{ name, err },
            );
            try self.errors.append(self.allocator, msg);
            return;
        };
        defer tokens.deinit(self.allocator);

        // Parse the module
        var parser = Parser.init(self.allocator, tokens.items);
        defer parser.deinit();

        const ast = parser.parseModule() catch |err| {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Failed to parse stdlib module {s}: {}",
                .{ name, err },
            );
            try self.errors.append(self.allocator, msg);
            return;
        };

        // Store the parsed module
        const ast_ptr = try self.allocator.create(Ast.Module);
        ast_ptr.* = ast;

        const name_copy = try self.allocator.dupe(u8, name);
        try self.modules.put(self.allocator, name_copy, ast_ptr);
    }

    /// Get a standard library module by name
    pub fn getModule(self: *const StandardLibrary, name: []const u8) ?*const Ast.Module {
        return self.modules.get(name);
    }

    /// Check if a module name is a standard library module
    pub fn isStdlibModule(name: []const u8) bool {
        return std.mem.startsWith(u8, name, "std.");
    }

    /// Register standard library modules with a ModuleLoader
    pub fn registerWithLoader(self: *const StandardLibrary, loader: *ModuleLoader) !void {
        var iter = self.modules.iterator();
        while (iter.next()) |entry| {
            // Create a LoadedModule for each stdlib module
            const loaded = ModuleLoader.LoadedModule{
                .path = entry.key_ptr.*,
                .name = entry.key_ptr.*,
                .ast = entry.value_ptr.*,
                .source = getEmbeddedSource(entry.key_ptr.*) orelse "",
            };
            try loader.loaded_modules.put(loader.allocator, entry.key_ptr.*, loaded);
        }
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *const StandardLibrary) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *const StandardLibrary) []const []const u8 {
        return self.errors.items;
    }

    /// Get all module names
    pub fn getModuleNames(self: *const StandardLibrary) []const []const u8 {
        const keys = self.modules.keys();
        return keys;
    }
};

// ============================================================================
// Embedded Standard Library Sources
// ============================================================================

const StdlibModule = struct {
    name: []const u8,
    source: []const u8,
};

/// All embedded standard library modules
const stdlib_modules = [_]StdlibModule{
    .{ .name = "std.collections", .source = @embedFile("specs/collections.sanna") },
    .{ .name = "std.result", .source = @embedFile("specs/result.sanna") },
    .{ .name = "std.option", .source = @embedFile("specs/option.sanna") },
    .{ .name = "std.ordering", .source = @embedFile("specs/ordering.sanna") },
    .{ .name = "std.money", .source = @embedFile("specs/money.sanna") },
    .{ .name = "std.time", .source = @embedFile("specs/time.sanna") },
    .{ .name = "std.crypto", .source = @embedFile("specs/crypto.sanna") },
};

/// Get the embedded source for a module by name
fn getEmbeddedSource(name: []const u8) ?[]const u8 {
    inline for (stdlib_modules) |mod| {
        if (std.mem.eql(u8, mod.name, name)) {
            return mod.source;
        }
    }
    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "stdlib initialization" {
    // Use an arena allocator since parsed AST contains many small allocations
    // that are expected to live for the lifetime of the AST
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var stdlib = StandardLibrary.init(arena.allocator());
    // No need to call deinit - arena will clean up everything

    try stdlib.loadAll();

    // Check that modules are loaded without errors
    if (stdlib.hasErrors()) {
        for (stdlib.getErrors()) |err| {
            std.debug.print("Error: {s}\n", .{err});
        }
        return error.StdlibLoadFailed;
    }

    // Verify we can access the modules
    const collections = stdlib.getModule("std.collections");
    try std.testing.expect(collections != null);

    const result_mod = stdlib.getModule("std.result");
    try std.testing.expect(result_mod != null);

    const option_mod = stdlib.getModule("std.option");
    try std.testing.expect(option_mod != null);

    const ordering_mod = stdlib.getModule("std.ordering");
    try std.testing.expect(ordering_mod != null);

    const money_mod = stdlib.getModule("std.money");
    try std.testing.expect(money_mod != null);

    const time_mod = stdlib.getModule("std.time");
    try std.testing.expect(time_mod != null);

    const crypto_mod = stdlib.getModule("std.crypto");
    try std.testing.expect(crypto_mod != null);
}

test "isStdlibModule" {
    const testing = std.testing;
    try testing.expect(StandardLibrary.isStdlibModule("std.collections"));
    try testing.expect(StandardLibrary.isStdlibModule("std.result"));
    try testing.expect(!StandardLibrary.isStdlibModule("mymodule"));
    try testing.expect(!StandardLibrary.isStdlibModule("collections"));
}
