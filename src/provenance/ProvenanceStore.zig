//! Provenance Store
//!
//! This module provides in-memory storage and querying of provenance metadata.
//! It supports looking up provenance by specification name, filtering by
//! various criteria, and generating reports.
//!
//! ## Usage
//!
//! ```zig
//! var store = ProvenanceStore.init(allocator);
//! defer store.deinit();
//!
//! try store.register("my_module.my_spec", metadata);
//! if (store.lookup("my_module.my_spec")) |prov| {
//!     // Use provenance data
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const Provenance = @import("Provenance.zig");
const ProvenanceMetadata = Provenance.ProvenanceMetadata;
const AuthorKind = Provenance.AuthorKind;
const Confidence = Provenance.Confidence;
const VerifiedStatus = Provenance.VerifiedStatus;
const toJson = Provenance.toJson;

// ============================================================================
// Provenance Store
// ============================================================================

/// In-memory storage for provenance metadata
pub const ProvenanceStore = struct {
    allocator: Allocator,
    /// Provenance by fully qualified name
    entries: std.StringHashMapUnmanaged(*ProvenanceMetadata),
    /// Index by author kind for fast filtering
    by_author_kind: std.AutoHashMapUnmanaged(AuthorKind, std.ArrayListUnmanaged([]const u8)),
    /// Statistics
    stats: Statistics,

    pub fn init(allocator: Allocator) ProvenanceStore {
        return .{
            .allocator = allocator,
            .entries = .{},
            .by_author_kind = .{},
            .stats = .{},
        };
    }

    pub fn deinit(self: *ProvenanceStore) void {
        // Free entries
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.entries.deinit(self.allocator);

        // Free index lists
        var kind_iter = self.by_author_kind.iterator();
        while (kind_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.by_author_kind.deinit(self.allocator);
    }

    // ========================================================================
    // Registration
    // ========================================================================

    /// Register provenance metadata for a specification
    pub fn register(self: *ProvenanceStore, name: []const u8, metadata: *ProvenanceMetadata) !void {
        // Update name in metadata
        if (metadata.name) |n| {
            self.allocator.free(n);
        }
        metadata.name = try self.allocator.dupe(u8, name);

        // Store entry
        const key = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(key);

        // Check for existing entry and remove it
        if (self.entries.get(name)) |existing| {
            self.removeFromIndices(name, existing);
            existing.deinit();
            self.allocator.destroy(existing);
            self.stats.updated += 1;
        } else {
            self.stats.registered += 1;
        }

        try self.entries.put(self.allocator, key, metadata);

        // Update indices
        try self.updateIndices(key, metadata);

        // Update statistics
        self.updateStats(metadata);
    }

    /// Register metadata and take ownership (metadata will be freed on deinit)
    pub fn registerOwned(self: *ProvenanceStore, name: []const u8, metadata: *ProvenanceMetadata) !void {
        try self.register(name, metadata);
    }

    fn updateIndices(self: *ProvenanceStore, name: []const u8, metadata: *const ProvenanceMetadata) !void {
        for (metadata.authors.items) |author| {
            const gop = try self.by_author_kind.getOrPut(self.allocator, author.kind);
            if (!gop.found_existing) {
                gop.value_ptr.* = .{};
            }
            try gop.value_ptr.append(self.allocator, name);
        }
    }

    fn removeFromIndices(self: *ProvenanceStore, name: []const u8, metadata: *const ProvenanceMetadata) void {
        for (metadata.authors.items) |author| {
            if (self.by_author_kind.getPtr(author.kind)) |list| {
                // Find and remove name from list
                for (list.items, 0..) |item, i| {
                    if (std.mem.eql(u8, item, name)) {
                        _ = list.swapRemove(i);
                        break;
                    }
                }
            }
        }
    }

    fn updateStats(self: *ProvenanceStore, metadata: *const ProvenanceMetadata) void {
        if (metadata.isHumanAuthored()) {
            self.stats.human_authored += 1;
        }
        if (metadata.isAiGenerated()) {
            self.stats.ai_generated += 1;
        }
        if (metadata.confidence) |c| {
            if (c.value < Confidence.Threshold.low) {
                self.stats.low_confidence += 1;
            }
        }
        if (metadata.needsReview()) {
            self.stats.needs_review += 1;
        }
    }

    // ========================================================================
    // Lookup
    // ========================================================================

    /// Look up provenance by name
    pub fn lookup(self: *const ProvenanceStore, name: []const u8) ?*const ProvenanceMetadata {
        return self.entries.get(name);
    }

    /// Check if provenance exists for a name
    pub fn contains(self: *const ProvenanceStore, name: []const u8) bool {
        return self.entries.contains(name);
    }

    /// Get count of registered entries
    pub fn count(self: *const ProvenanceStore) usize {
        return self.entries.count();
    }

    // ========================================================================
    // Filtering
    // ========================================================================

    /// Get all entries by author kind
    pub fn getByAuthorKind(self: *const ProvenanceStore, kind: AuthorKind) ?[]const []const u8 {
        if (self.by_author_kind.get(kind)) |list| {
            return list.items;
        }
        return null;
    }

    /// Filter entries by a predicate
    pub fn filter(
        self: *const ProvenanceStore,
        allocator: Allocator,
        predicate: *const fn (*const ProvenanceMetadata) bool,
    ) ![]const []const u8 {
        var results = std.ArrayListUnmanaged([]const u8){};
        errdefer results.deinit(allocator);

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            if (predicate(entry.value_ptr.*)) {
                try results.append(allocator, entry.key_ptr.*);
            }
        }

        return results.toOwnedSlice(allocator);
    }

    /// Get entries that need review
    pub fn getNeedsReview(self: *const ProvenanceStore, allocator: Allocator) ![]const []const u8 {
        return self.filter(allocator, &needsReviewPredicate);
    }

    fn needsReviewPredicate(metadata: *const ProvenanceMetadata) bool {
        return metadata.needsReview();
    }

    /// Get entries with low confidence (below threshold)
    pub fn getLowConfidence(self: *const ProvenanceStore, allocator: Allocator, threshold: f64) ![]const []const u8 {
        var results = std.ArrayListUnmanaged([]const u8){};
        errdefer results.deinit(allocator);

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            const metadata = entry.value_ptr.*;
            if (metadata.confidence) |c| {
                if (c.value < threshold) {
                    try results.append(allocator, entry.key_ptr.*);
                }
            }
        }

        return results.toOwnedSlice(allocator);
    }

    /// Get unverified entries
    pub fn getUnverified(self: *const ProvenanceStore, allocator: Allocator) ![]const []const u8 {
        var results = std.ArrayListUnmanaged([]const u8){};
        errdefer results.deinit(allocator);

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            const metadata = entry.value_ptr.*;
            if (metadata.verification) |v| {
                if (v.status == .unproven or v.status == .pending) {
                    try results.append(allocator, entry.key_ptr.*);
                }
            }
        }

        return results.toOwnedSlice(allocator);
    }

    /// Get AI-generated entries that are not approved
    pub fn getUnapprovedAi(self: *const ProvenanceStore, allocator: Allocator) ![]const []const u8 {
        var results = std.ArrayListUnmanaged([]const u8){};
        errdefer results.deinit(allocator);

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            const metadata = entry.value_ptr.*;
            if (metadata.isAiGenerated() and !metadata.isApproved()) {
                try results.append(allocator, entry.key_ptr.*);
            }
        }

        return results.toOwnedSlice(allocator);
    }

    // ========================================================================
    // Iteration
    // ========================================================================

    /// Iterator over all entries
    pub fn iterator(self: *const ProvenanceStore) Iterator {
        return Iterator{ .inner = self.entries.iterator() };
    }

    pub const Iterator = struct {
        inner: std.StringHashMapUnmanaged(*ProvenanceMetadata).Iterator,

        pub fn next(self: *Iterator) ?Entry {
            if (self.inner.next()) |kv| {
                return Entry{
                    .name = kv.key_ptr.*,
                    .metadata = kv.value_ptr.*,
                };
            }
            return null;
        }
    };

    pub const Entry = struct {
        name: []const u8,
        metadata: *ProvenanceMetadata,
    };

    // ========================================================================
    // Export
    // ========================================================================

    /// Export all provenance data to JSON
    pub fn exportJson(self: *const ProvenanceStore, allocator: Allocator) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(allocator);

        try buf.appendSlice(allocator, "{\"entries\":[");

        var first = true;
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            if (!first) try buf.appendSlice(allocator, ",");
            first = false;

            const entry_json = try toJson(allocator, entry.value_ptr.*);
            defer allocator.free(entry_json);
            try buf.appendSlice(allocator, entry_json);
        }

        try buf.appendSlice(allocator, "],\"statistics\":");
        try self.appendStatsJson(&buf, allocator);
        try buf.appendSlice(allocator, "}");

        return buf.toOwnedSlice(allocator);
    }

    fn appendStatsJson(self: *const ProvenanceStore, buf: *std.ArrayListUnmanaged(u8), allocator: Allocator) !void {
        const stats_json = try std.fmt.allocPrint(
            allocator,
            "{{\"total\":{d},\"human_authored\":{d},\"ai_generated\":{d},\"low_confidence\":{d},\"needs_review\":{d}}}",
            .{
                self.count(),
                self.stats.human_authored,
                self.stats.ai_generated,
                self.stats.low_confidence,
                self.stats.needs_review,
            },
        );
        defer allocator.free(stats_json);
        try buf.appendSlice(allocator, stats_json);
    }

    // ========================================================================
    // Statistics
    // ========================================================================

    pub const Statistics = struct {
        registered: usize = 0,
        updated: usize = 0,
        human_authored: usize = 0,
        ai_generated: usize = 0,
        low_confidence: usize = 0,
        needs_review: usize = 0,
    };

    pub fn getStatistics(self: *const ProvenanceStore) Statistics {
        return self.stats;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ProvenanceStore init/deinit" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    try std.testing.expectEqual(@as(usize, 0), store.count());
}

test "ProvenanceStore register and lookup" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    // Create metadata
    var metadata = try testing_alloc.create(ProvenanceMetadata);
    metadata.* = ProvenanceMetadata.init(testing_alloc);
    try metadata.addAuthor(.{ .kind = .human, .identity = null });

    try store.register("test.spec", metadata);

    try std.testing.expectEqual(@as(usize, 1), store.count());
    try std.testing.expect(store.contains("test.spec"));

    const looked_up = store.lookup("test.spec");
    try std.testing.expect(looked_up != null);
    try std.testing.expect(looked_up.?.isHumanAuthored());
}

test "ProvenanceStore getByAuthorKind" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    // Register human-authored spec
    var metadata1 = try testing_alloc.create(ProvenanceMetadata);
    metadata1.* = ProvenanceMetadata.init(testing_alloc);
    try metadata1.addAuthor(.{ .kind = .human });
    try store.register("human.spec", metadata1);

    // Register AI-generated spec
    var metadata2 = try testing_alloc.create(ProvenanceMetadata);
    metadata2.* = ProvenanceMetadata.init(testing_alloc);
    try metadata2.addAuthor(.{ .kind = .ai });
    try store.register("ai.spec", metadata2);

    const human_specs = store.getByAuthorKind(.human);
    try std.testing.expect(human_specs != null);
    try std.testing.expectEqual(@as(usize, 1), human_specs.?.len);

    const ai_specs = store.getByAuthorKind(.ai);
    try std.testing.expect(ai_specs != null);
    try std.testing.expectEqual(@as(usize, 1), ai_specs.?.len);
}

test "ProvenanceStore getLowConfidence" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    // Register spec with low confidence
    var metadata1 = try testing_alloc.create(ProvenanceMetadata);
    metadata1.* = ProvenanceMetadata.init(testing_alloc);
    metadata1.confidence = .{ .value = 0.3 };
    try store.register("low.spec", metadata1);

    // Register spec with high confidence
    var metadata2 = try testing_alloc.create(ProvenanceMetadata);
    metadata2.* = ProvenanceMetadata.init(testing_alloc);
    metadata2.confidence = .{ .value = 0.9 };
    try store.register("high.spec", metadata2);

    const low_conf = try store.getLowConfidence(testing_alloc, 0.5);
    defer testing_alloc.free(low_conf);

    try std.testing.expectEqual(@as(usize, 1), low_conf.len);
}

test "ProvenanceStore iterator" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    const metadata = try testing_alloc.create(ProvenanceMetadata);
    metadata.* = ProvenanceMetadata.init(testing_alloc);
    try store.register("test.spec", metadata);

    var iter = store.iterator();
    var count: usize = 0;
    while (iter.next()) |_| {
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

test "ProvenanceStore exportJson" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    var metadata = try testing_alloc.create(ProvenanceMetadata);
    metadata.* = ProvenanceMetadata.init(testing_alloc);
    try metadata.addAuthor(.{ .kind = .human });
    metadata.confidence = .{ .value = 0.85 };
    try store.register("test.spec", metadata);

    const json = try store.exportJson(testing_alloc);
    defer testing_alloc.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "\"entries\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"statistics\"") != null);
}

test "ProvenanceStore statistics" {
    const testing_alloc = std.testing.allocator;
    var store = ProvenanceStore.init(testing_alloc);
    defer store.deinit();

    var metadata = try testing_alloc.create(ProvenanceMetadata);
    metadata.* = ProvenanceMetadata.init(testing_alloc);
    try metadata.addAuthor(.{ .kind = .ai });
    metadata.confidence = .{ .value = 0.3 };
    metadata.needs_review = .{};
    try store.register("test.spec", metadata);

    const stats = store.getStatistics();
    try std.testing.expectEqual(@as(usize, 1), stats.registered);
    try std.testing.expectEqual(@as(usize, 1), stats.ai_generated);
    try std.testing.expectEqual(@as(usize, 1), stats.low_confidence);
    try std.testing.expectEqual(@as(usize, 1), stats.needs_review);
}
