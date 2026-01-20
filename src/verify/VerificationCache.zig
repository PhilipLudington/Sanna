//! Verification Cache
//!
//! This module provides caching for verification results to enable
//! incremental verification. Results are cached based on content hash,
//! allowing unchanged specifications to skip re-verification.
//!
//! ## Cache Strategy
//!
//! - Key: Content hash of (goal + assumptions + solver version)
//! - Value: Verification result + timestamp
//! - Storage: File-based in `proofs/.sanna-cache/`
//! - Invalidation: On spec/dependency changes

const std = @import("std");
const Allocator = std.mem.Allocator;
const ProofObligation = @import("ProofObligation.zig");
const VerificationStatus = ProofObligation.VerificationStatus;
const SmtTypes = @import("SmtTypes.zig");
const SmtExpr = SmtTypes.SmtExpr;
const SmtLibWriter = @import("SmtLibWriter.zig").SmtLibWriter;

// ============================================================================
// Verification Cache
// ============================================================================

/// Cache for verification results
pub const VerificationCache = struct {
    allocator: Allocator,
    /// In-memory cache
    entries: std.StringHashMapUnmanaged(CacheEntry),
    /// Cache directory path
    cache_dir: ?[]const u8,
    /// Solver version for cache key
    solver_version: []const u8,
    /// Cache statistics
    stats: CacheStats,
    /// Whether to persist to disk
    persist: bool,

    pub fn init(allocator: Allocator) VerificationCache {
        return .{
            .allocator = allocator,
            .entries = .{},
            .cache_dir = null,
            .solver_version = "unknown",
            .stats = .{},
            .persist = false,
        };
    }

    pub fn deinit(self: *VerificationCache) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            if (entry.value_ptr.smt_hash) |h| {
                self.allocator.free(h);
            }
        }
        self.entries.deinit(self.allocator);
        if (self.cache_dir) |dir| {
            self.allocator.free(dir);
        }
    }

    /// Configure the cache directory
    pub fn setCacheDir(self: *VerificationCache, dir: []const u8) !void {
        if (self.cache_dir) |old| {
            self.allocator.free(old);
        }
        self.cache_dir = try self.allocator.dupe(u8, dir);
        self.persist = true;
    }

    /// Set the solver version (used in cache key)
    pub fn setSolverVersion(self: *VerificationCache, version: []const u8) void {
        self.solver_version = version;
    }

    /// Look up a cached result
    pub fn lookup(self: *VerificationCache, obligation_id: []const u8) ?CacheEntry {
        self.stats.lookups += 1;

        if (self.entries.get(obligation_id)) |entry| {
            self.stats.hits += 1;
            return entry;
        }

        // Try loading from disk
        if (self.persist) {
            if (self.loadFromDisk(obligation_id)) |entry| {
                self.stats.disk_hits += 1;
                return entry;
            } else |_| {}
        }

        self.stats.misses += 1;
        return null;
    }

    /// Store a result in the cache
    pub fn store(self: *VerificationCache, obligation_id: []const u8, entry: CacheEntry) !void {
        const key = try self.allocator.dupe(u8, obligation_id);
        errdefer self.allocator.free(key);

        try self.entries.put(self.allocator, key, entry);
        self.stats.stores += 1;

        // Persist to disk if enabled
        if (self.persist) {
            self.saveToDisk(obligation_id, &entry) catch |err| {
                // Log but don't fail on disk write errors
                std.log.warn("Failed to persist cache entry: {}", .{err});
            };
        }
    }

    /// Invalidate a cached entry
    pub fn invalidate(self: *VerificationCache, obligation_id: []const u8) void {
        if (self.entries.fetchRemove(obligation_id)) |kv| {
            self.allocator.free(kv.key);
            if (kv.value.smt_hash) |h| {
                self.allocator.free(h);
            }
            self.stats.invalidations += 1;
        }

        // Remove from disk
        if (self.persist and self.cache_dir != null) {
            self.deleteFromDisk(obligation_id) catch {};
        }
    }

    /// Clear all cached entries
    pub fn clear(self: *VerificationCache) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            if (entry.value_ptr.smt_hash) |h| {
                self.allocator.free(h);
            }
        }
        self.entries.clearRetainingCapacity();
        self.stats.clears += 1;
    }

    /// Compute content hash for an obligation
    pub fn computeHash(self: *VerificationCache, goal: *const SmtExpr, assumptions: []const *const SmtExpr) !u64 {
        var writer = SmtLibWriter.init(self.allocator);
        defer writer.deinit();

        // Hash the goal
        try writer.writeExpr(goal);

        // Hash assumptions
        for (assumptions) |assumption| {
            try writer.buffer.append(';');
            try writer.writeExpr(assumption);
        }

        // Include solver version
        try writer.buffer.appendSlice(self.solver_version);

        // Compute hash
        return std.hash.Wyhash.hash(0, writer.getOutput());
    }

    /// Get cache statistics
    pub fn getStats(self: *const VerificationCache) CacheStats {
        return self.stats;
    }

    /// Reset statistics
    pub fn resetStats(self: *VerificationCache) void {
        self.stats = .{};
    }

    // ========================================================================
    // Disk Persistence
    // ========================================================================

    fn loadFromDisk(self: *VerificationCache, obligation_id: []const u8) !CacheEntry {
        // Ensure cache is enabled
        _ = self.cache_dir orelse return error.NoCacheDir;

        const path = try self.getCachePath(obligation_id);
        defer self.allocator.free(path);

        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 4096);
        defer self.allocator.free(content);

        return self.parseCacheEntry(content);
    }

    fn saveToDisk(self: *VerificationCache, obligation_id: []const u8, entry: *const CacheEntry) !void {
        const cache_dir = self.cache_dir orelse return error.NoCacheDir;

        // Ensure directory exists
        std.fs.cwd().makePath(cache_dir) catch {};

        const path = try self.getCachePath(obligation_id);
        defer self.allocator.free(path);

        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        // Write cache entry as simple text format
        var buf: [256]u8 = undefined;
        const content = std.fmt.bufPrint(&buf, "{s}\n{d}\n{d}\n{s}\n", .{
            @tagName(entry.status),
            entry.timestamp,
            entry.verification_time_ms,
            entry.smt_hash orelse "",
        }) catch return error.FormatError;

        try file.writeAll(content);
    }

    fn deleteFromDisk(self: *VerificationCache, obligation_id: []const u8) !void {
        const path = try self.getCachePath(obligation_id);
        defer self.allocator.free(path);

        std.fs.cwd().deleteFile(path) catch {};
    }

    fn getCachePath(self: *VerificationCache, obligation_id: []const u8) ![]u8 {
        const dir = self.cache_dir orelse return error.NoCacheDir;

        // Sanitize obligation ID for filename
        var sanitized = std.ArrayListUnmanaged(u8){};
        defer sanitized.deinit(self.allocator);

        for (obligation_id) |c| {
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '-') {
                try sanitized.append(self.allocator, c);
            } else {
                try sanitized.append(self.allocator, '_');
            }
        }

        return std.fmt.allocPrint(self.allocator, "{s}/{s}.cache", .{
            dir,
            sanitized.items,
        });
    }

    fn parseCacheEntry(self: *VerificationCache, content: []const u8) !CacheEntry {
        var lines = std.mem.splitScalar(u8, content, '\n');

        const status_str = lines.next() orelse return error.InvalidCache;
        const timestamp_str = lines.next() orelse return error.InvalidCache;
        const time_str = lines.next() orelse return error.InvalidCache;
        const hash_str = lines.next();

        const status = std.meta.stringToEnum(VerificationStatus, status_str) orelse return error.InvalidCache;
        const timestamp = std.fmt.parseInt(i64, timestamp_str, 10) catch return error.InvalidCache;
        const time_ms = std.fmt.parseInt(u64, time_str, 10) catch return error.InvalidCache;

        return CacheEntry{
            .status = status,
            .timestamp = timestamp,
            .verification_time_ms = time_ms,
            .smt_hash = if (hash_str) |h| if (h.len > 0) try self.allocator.dupe(u8, h) else null else null,
        };
    }
};

/// A cache entry
pub const CacheEntry = struct {
    /// Verification status
    status: VerificationStatus,
    /// When this entry was created (Unix timestamp ms)
    timestamp: i64,
    /// How long verification took
    verification_time_ms: u64,
    /// Hash of the SMT formula (for validation)
    smt_hash: ?[]const u8,

    pub fn init(status: VerificationStatus, time_ms: u64) CacheEntry {
        return .{
            .status = status,
            .timestamp = std.time.milliTimestamp(),
            .verification_time_ms = time_ms,
            .smt_hash = null,
        };
    }

    /// Check if the entry is still valid (not too old)
    pub fn isValid(self: *const CacheEntry, max_age_ms: i64) bool {
        const now = std.time.milliTimestamp();
        return (now - self.timestamp) < max_age_ms;
    }
};

/// Cache statistics
pub const CacheStats = struct {
    /// Total lookups
    lookups: u64 = 0,
    /// Cache hits (in memory)
    hits: u64 = 0,
    /// Cache hits (from disk)
    disk_hits: u64 = 0,
    /// Cache misses
    misses: u64 = 0,
    /// Stores
    stores: u64 = 0,
    /// Invalidations
    invalidations: u64 = 0,
    /// Full clears
    clears: u64 = 0,

    pub fn hitRate(self: CacheStats) f64 {
        if (self.lookups == 0) return 0.0;
        const total_hits: f64 = @floatFromInt(self.hits + self.disk_hits);
        const total_lookups: f64 = @floatFromInt(self.lookups);
        return total_hits / total_lookups;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "VerificationCache basic operations" {
    const testing = std.testing;

    var cache = VerificationCache.init(testing.allocator);
    defer cache.deinit();

    // Initially empty
    try testing.expect(cache.lookup("test_id") == null);

    // Store an entry
    const entry = CacheEntry.init(.verified, 100);
    try cache.store("test_id", entry);

    // Should find it now
    const found = cache.lookup("test_id");
    try testing.expect(found != null);
    try testing.expectEqual(VerificationStatus.verified, found.?.status);
    try testing.expectEqual(@as(u64, 100), found.?.verification_time_ms);

    // Invalidate
    cache.invalidate("test_id");
    try testing.expect(cache.lookup("test_id") == null);
}

test "CacheEntry validity" {
    const entry = CacheEntry.init(.verified, 100);

    // Should be valid immediately
    try std.testing.expect(entry.isValid(1000));

    // Should be valid for reasonable time
    try std.testing.expect(entry.isValid(3600 * 1000));
}

test "CacheStats hit rate" {
    var stats = CacheStats{};

    // No lookups = 0% hit rate
    try std.testing.expectEqual(@as(f64, 0.0), stats.hitRate());

    // 2 hits out of 4 lookups = 50%
    stats.lookups = 4;
    stats.hits = 2;
    try std.testing.expectEqual(@as(f64, 0.5), stats.hitRate());

    // Include disk hits
    stats.disk_hits = 1;
    try std.testing.expectEqual(@as(f64, 0.75), stats.hitRate());
}

test "VerificationCache clear" {
    const testing = std.testing;

    var cache = VerificationCache.init(testing.allocator);
    defer cache.deinit();

    // Add some entries
    try cache.store("id1", CacheEntry.init(.verified, 100));
    try cache.store("id2", CacheEntry.init(.failed, 200));

    // Clear
    cache.clear();

    // All entries gone
    try testing.expect(cache.lookup("id1") == null);
    try testing.expect(cache.lookup("id2") == null);
}

test "VerificationCache statistics" {
    const testing = std.testing;

    var cache = VerificationCache.init(testing.allocator);
    defer cache.deinit();

    // Store
    try cache.store("test", CacheEntry.init(.verified, 100));
    try testing.expectEqual(@as(u64, 1), cache.stats.stores);

    // Lookup hit
    _ = cache.lookup("test");
    try testing.expectEqual(@as(u64, 1), cache.stats.lookups);
    try testing.expectEqual(@as(u64, 1), cache.stats.hits);

    // Lookup miss
    _ = cache.lookup("nonexistent");
    try testing.expectEqual(@as(u64, 2), cache.stats.lookups);
    try testing.expectEqual(@as(u64, 1), cache.stats.misses);

    // Invalidate
    cache.invalidate("test");
    try testing.expectEqual(@as(u64, 1), cache.stats.invalidations);
}
