//! Checkpoint Serialization
//!
//! Implements checkpointing for resumable code generation sessions.
//! Allows generation to be paused and resumed across sessions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("types.zig");

const Checkpoint = types.Checkpoint;
const CheckpointId = types.CheckpointId;
const GenerationRequest = types.GenerationRequest;
const TypedHole = types.TypedHole;

/// Manages checkpoints for resumable generation.
pub const CheckpointManager = struct {
    allocator: Allocator,
    storage_path: []const u8,
    checkpoints: std.StringHashMap(StoredCheckpoint),
    max_checkpoints: usize,
    auto_checkpoint_interval: u64,
    last_checkpoint_time: i64,

    const StoredCheckpoint = struct {
        checkpoint: Checkpoint,
        metadata: CheckpointMetadata,
    };

    pub fn init(allocator: Allocator, storage_path: []const u8) CheckpointManager {
        return .{
            .allocator = allocator,
            .storage_path = storage_path,
            .checkpoints = std.StringHashMap(StoredCheckpoint).init(allocator),
            .max_checkpoints = 100,
            .auto_checkpoint_interval = 30_000,
            .last_checkpoint_time = 0,
        };
    }

    pub fn deinit(self: *CheckpointManager) void {
        self.checkpoints.deinit();
    }

    /// Create a new checkpoint.
    pub fn createCheckpoint(
        self: *CheckpointManager,
        request_id: GenerationRequest.RequestId,
        partial_code: []const u8,
        remaining: []const []const u8,
        state: []const u8,
    ) !Checkpoint {
        const id = CheckpointId.generate();
        const checkpoint = Checkpoint{
            .id = id,
            .request_id = request_id,
            .partial_code = partial_code,
            .remaining = remaining,
            .state = state,
            .created_at = std.time.timestamp(),
        };

        const metadata = CheckpointMetadata{
            .version = 1,
            .code_size = partial_code.len,
            .remaining_count = remaining.len,
            .is_complete = remaining.len == 0,
        };

        const id_str = &id.format();
        try self.checkpoints.put(id_str, .{
            .checkpoint = checkpoint,
            .metadata = metadata,
        });

        self.last_checkpoint_time = std.time.timestamp();

        return checkpoint;
    }

    /// Resume from a checkpoint.
    pub fn resumeFromCheckpoint(self: *CheckpointManager, id: CheckpointId) ?Checkpoint {
        const id_str = &id.format();
        if (self.checkpoints.get(id_str)) |stored| {
            return stored.checkpoint;
        }
        return null;
    }

    /// Check if a checkpoint exists.
    pub fn hasCheckpoint(self: *CheckpointManager, id: CheckpointId) bool {
        const id_str = &id.format();
        return self.checkpoints.contains(id_str);
    }

    /// Delete a checkpoint.
    pub fn deleteCheckpoint(self: *CheckpointManager, id: CheckpointId) void {
        const id_str = &id.format();
        _ = self.checkpoints.remove(id_str);
    }

    /// Check if auto-checkpoint is needed.
    pub fn shouldAutoCheckpoint(self: *CheckpointManager) bool {
        const now = std.time.timestamp();
        const elapsed_ms: u64 = @intCast((now - self.last_checkpoint_time) * 1000);
        return elapsed_ms >= self.auto_checkpoint_interval;
    }
};

/// Metadata about a checkpoint.
pub const CheckpointMetadata = struct {
    version: u32,
    code_size: usize,
    remaining_count: usize,
    is_complete: bool,
};

/// Builder for creating checkpoints incrementally.
pub const CheckpointBuilder = struct {
    allocator: Allocator,
    request_id: GenerationRequest.RequestId,
    code_parts: std.ArrayListUnmanaged([]const u8),
    remaining_items: std.ArrayListUnmanaged([]const u8),
    state_data: std.ArrayListUnmanaged(u8),
    holes: std.ArrayListUnmanaged(TypedHole),

    pub fn init(allocator: Allocator, request_id: GenerationRequest.RequestId) CheckpointBuilder {
        return .{
            .allocator = allocator,
            .request_id = request_id,
            .code_parts = .{},
            .remaining_items = .{},
            .state_data = .{},
            .holes = .{},
        };
    }

    pub fn deinit(self: *CheckpointBuilder) void {
        self.code_parts.deinit(self.allocator);
        self.remaining_items.deinit(self.allocator);
        self.state_data.deinit(self.allocator);
        self.holes.deinit(self.allocator);
    }

    pub fn addCodePart(self: *CheckpointBuilder, code: []const u8) !void {
        try self.code_parts.append(self.allocator, code);
    }

    pub fn addRemainingItem(self: *CheckpointBuilder, item: []const u8) !void {
        try self.remaining_items.append(self.allocator, item);
    }

    pub fn completeItem(self: *CheckpointBuilder, item: []const u8) void {
        for (self.remaining_items.items, 0..) |existing, i| {
            if (std.mem.eql(u8, existing, item)) {
                _ = self.remaining_items.orderedRemove(i);
                break;
            }
        }
    }

    pub fn addHole(self: *CheckpointBuilder, hole: TypedHole) !void {
        try self.holes.append(self.allocator, hole);
    }

    pub fn build(self: *CheckpointBuilder) !Checkpoint {
        var total_len: usize = 0;
        for (self.code_parts.items) |part| {
            total_len += part.len;
        }

        var combined_code = try self.allocator.alloc(u8, total_len);
        var offset: usize = 0;
        for (self.code_parts.items) |part| {
            @memcpy(combined_code[offset .. offset + part.len], part);
            offset += part.len;
        }

        return Checkpoint{
            .id = CheckpointId.generate(),
            .request_id = self.request_id,
            .partial_code = combined_code,
            .remaining = self.remaining_items.items,
            .state = self.state_data.items,
            .created_at = std.time.timestamp(),
        };
    }

    pub fn getProgress(self: *CheckpointBuilder, total_items: usize) f32 {
        if (total_items == 0) return 1.0;
        const completed = total_items - self.remaining_items.items.len;
        return @as(f32, @floatFromInt(completed)) / @as(f32, @floatFromInt(total_items));
    }

    pub fn isComplete(self: *CheckpointBuilder) bool {
        return self.remaining_items.items.len == 0 and self.holes.items.len == 0;
    }
};

/// Session state for tracking generation progress.
pub const GenerationSession = struct {
    allocator: Allocator,
    request_id: GenerationRequest.RequestId,
    checkpoint_manager: *CheckpointManager,
    current_builder: CheckpointBuilder,
    last_checkpoint: ?Checkpoint,
    items_generated: u32,
    items_total: u32,
    start_time: i64,
    status: Status,

    pub const Status = enum {
        not_started,
        in_progress,
        paused,
        completed,
        failed,
    };

    pub fn init(
        allocator: Allocator,
        request_id: GenerationRequest.RequestId,
        checkpoint_manager: *CheckpointManager,
    ) GenerationSession {
        return .{
            .allocator = allocator,
            .request_id = request_id,
            .checkpoint_manager = checkpoint_manager,
            .current_builder = CheckpointBuilder.init(allocator, request_id),
            .last_checkpoint = null,
            .items_generated = 0,
            .items_total = 0,
            .start_time = std.time.timestamp(),
            .status = .not_started,
        };
    }

    pub fn deinit(self: *GenerationSession) void {
        self.current_builder.deinit();
        // Free any checkpoint memory we own
        if (self.last_checkpoint) |checkpoint| {
            if (checkpoint.partial_code.len > 0) {
                self.allocator.free(checkpoint.partial_code);
            }
        }
    }

    pub fn start(self: *GenerationSession, items: []const []const u8) !void {
        self.status = .in_progress;
        self.items_total = @intCast(items.len);
        self.items_generated = 0;

        for (items) |item| {
            try self.current_builder.addRemainingItem(item);
        }
    }

    pub fn resumeFrom(self: *GenerationSession, checkpoint: Checkpoint) !void {
        self.status = .in_progress;
        self.last_checkpoint = checkpoint;
        try self.current_builder.addCodePart(checkpoint.partial_code);
        for (checkpoint.remaining) |item| {
            try self.current_builder.addRemainingItem(item);
        }
    }

    pub fn recordProgress(self: *GenerationSession, item: []const u8, code: []const u8) !void {
        try self.current_builder.addCodePart(code);
        self.current_builder.completeItem(item);
        self.items_generated += 1;

        if (self.checkpoint_manager.shouldAutoCheckpoint()) {
            _ = try self.saveCheckpoint();
        }
    }

    pub fn saveCheckpoint(self: *GenerationSession) !Checkpoint {
        const checkpoint = try self.current_builder.build();
        _ = try self.checkpoint_manager.createCheckpoint(
            self.request_id,
            checkpoint.partial_code,
            checkpoint.remaining,
            checkpoint.state,
        );
        self.last_checkpoint = checkpoint;
        return checkpoint;
    }

    pub fn pause(self: *GenerationSession) !Checkpoint {
        self.status = .paused;
        return self.saveCheckpoint();
    }

    pub fn complete(self: *GenerationSession) void {
        self.status = .completed;
    }

    pub fn fail(self: *GenerationSession) void {
        self.status = .failed;
    }

    pub fn getProgress(self: *GenerationSession) f32 {
        if (self.items_total == 0) return 1.0;
        return @as(f32, @floatFromInt(self.items_generated)) /
            @as(f32, @floatFromInt(self.items_total));
    }

    pub fn getElapsedSeconds(self: *GenerationSession) i64 {
        return std.time.timestamp() - self.start_time;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "CheckpointBuilder basic" {
    const testing = std.testing;
    const request_id = GenerationRequest.RequestId.generate();

    var builder = CheckpointBuilder.init(testing.allocator, request_id);
    defer builder.deinit();

    try builder.addCodePart("fn foo() {}");
    try builder.addCodePart("\nfn bar() {}");
    try builder.addRemainingItem("baz");
    try builder.addRemainingItem("qux");

    try testing.expectEqual(@as(f32, 0.0), builder.getProgress(2));
    try testing.expect(!builder.isComplete());

    builder.completeItem("baz");
    try testing.expectEqual(@as(f32, 0.5), builder.getProgress(2));
}

test "CheckpointBuilder builds checkpoint" {
    const testing = std.testing;
    const request_id = GenerationRequest.RequestId.generate();

    var builder = CheckpointBuilder.init(testing.allocator, request_id);
    defer builder.deinit();

    try builder.addCodePart("part1");
    try builder.addCodePart("part2");

    const checkpoint = try builder.build();
    defer testing.allocator.free(checkpoint.partial_code);

    try testing.expectEqualStrings("part1part2", checkpoint.partial_code);
}

test "GenerationSession progress" {
    const testing = std.testing;
    var mgr = CheckpointManager.init(testing.allocator, "/tmp/test_checkpoints");
    defer mgr.deinit();

    const request_id = GenerationRequest.RequestId.generate();
    var session = GenerationSession.init(testing.allocator, request_id, &mgr);
    defer session.deinit();

    const items = [_][]const u8{ "item1", "item2", "item3" };
    try session.start(&items);

    try testing.expectEqual(GenerationSession.Status.in_progress, session.status);
    try testing.expectEqual(@as(f32, 0.0), session.getProgress());

    try session.recordProgress("item1", "code1");
    try testing.expect(session.getProgress() > 0.3);
}

test "CheckpointId format" {
    const id = CheckpointId.generate();
    const formatted = id.format();

    const testing = std.testing;
    try testing.expectEqual(@as(usize, 32), formatted.len);
}
