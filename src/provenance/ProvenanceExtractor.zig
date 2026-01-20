//! Provenance Attribute Extractor
//!
//! This module extracts and validates provenance attributes from AST declarations.
//! It processes @author, @confidence, @needs_review, @approved, @verified,
//! @generation_checkpoint, and @generating attributes.
//!
//! ## Supported Attributes
//!
//! - @author(kind, identity = "...", model = "...", prompt_hash = "...", reviewed_by = "...")
//! - @confidence(0.0 - 1.0)
//! - @needs_review(reason = "...", priority = "...")
//! - @approved(by = "...", at = "...", note = "...")
//! - @verified(status, obligations = [...])
//! - @generation_checkpoint(model = "...", timestamp = "...", context_hash = "...")
//! - @generating(model = "...", started_at = "...")

const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/root.zig").Ast;
const Span = @import("../lexer/root.zig").Span;
const Provenance = @import("Provenance.zig");
const AuthorKind = Provenance.AuthorKind;
const Author = Provenance.Author;
const Confidence = Provenance.Confidence;
const NeedsReview = Provenance.NeedsReview;
const Approval = Provenance.Approval;
const VerifiedStatus = Provenance.VerifiedStatus;
const VerificationInfo = Provenance.VerificationInfo;
const GenerationCheckpoint = Provenance.GenerationCheckpoint;
const GeneratingMarker = Provenance.GeneratingMarker;
const ProvenanceMetadata = Provenance.ProvenanceMetadata;

// ============================================================================
// Extractor
// ============================================================================

/// Extracts provenance metadata from AST attributes
pub const ProvenanceExtractor = struct {
    allocator: Allocator,
    /// Extraction errors
    errors: std.ArrayListUnmanaged(ExtractionError),

    pub fn init(allocator: Allocator) ProvenanceExtractor {
        return .{
            .allocator = allocator,
            .errors = .{},
        };
    }

    pub fn deinit(self: *ProvenanceExtractor) void {
        for (self.errors.items) |*err| {
            self.allocator.free(err.message);
        }
        self.errors.deinit(self.allocator);
    }

    /// Extract provenance metadata from a declaration's attributes
    pub fn extractFromDeclaration(self: *ProvenanceExtractor, decl: *const Ast.Declaration) !?*ProvenanceMetadata {
        const attributes = decl.attributes;
        if (attributes.len == 0) return null;

        return try self.extractFromAttributes(attributes);
    }

    /// Extract provenance metadata from a slice of attributes
    pub fn extractFromAttributes(self: *ProvenanceExtractor, attributes: []const Ast.Attribute) !?*ProvenanceMetadata {
        var metadata = ProvenanceMetadata.init(self.allocator);
        errdefer metadata.deinit();

        var has_provenance = false;

        for (attributes) |attr| {
            const name = attr.name.name;

            if (std.mem.eql(u8, name, "author")) {
                has_provenance = true;
                try self.extractAuthor(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "confidence")) {
                has_provenance = true;
                try self.extractConfidence(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "needs_review")) {
                has_provenance = true;
                try self.extractNeedsReview(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "approved")) {
                has_provenance = true;
                try self.extractApproval(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "verified")) {
                has_provenance = true;
                try self.extractVerified(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "generation_checkpoint")) {
                has_provenance = true;
                try self.extractCheckpoint(&metadata, &attr);
            } else if (std.mem.eql(u8, name, "generating")) {
                has_provenance = true;
                try self.extractGenerating(&metadata, &attr);
            }
            // Other attributes are ignored (not provenance-related)
        }

        if (!has_provenance) {
            metadata.deinit();
            return null;
        }

        // Allocate and return
        const result = try self.allocator.create(ProvenanceMetadata);
        result.* = metadata;
        return result;
    }

    // ========================================================================
    // Attribute Extraction
    // ========================================================================

    fn extractAuthor(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var author = Author{ .kind = .unknown, .span = attr.span };

        // First positional argument should be the author kind
        if (attr.args.len > 0) {
            const first_arg = &attr.args[0];
            if (first_arg.name == null) {
                // Positional argument - should be author kind identifier
                switch (first_arg.value) {
                    .identifier => |id| {
                        if (AuthorKind.fromString(id.name)) |kind| {
                            author.kind = kind;
                        } else {
                            try self.addError(.{
                                .kind = .invalid_author_kind,
                                .message = try std.fmt.allocPrint(
                                    self.allocator,
                                    "invalid author kind: '{s}'",
                                    .{id.name},
                                ),
                                .span = attr.span,
                            });
                        }
                    },
                    .string => |s| {
                        if (AuthorKind.fromString(s)) |kind| {
                            author.kind = kind;
                        } else {
                            try self.addError(.{
                                .kind = .invalid_author_kind,
                                .message = try std.fmt.allocPrint(
                                    self.allocator,
                                    "invalid author kind: '{s}'",
                                    .{s},
                                ),
                                .span = attr.span,
                            });
                        }
                    },
                    else => {},
                }
            }
        }

        // Process named arguments
        for (attr.args) |arg| {
            if (arg.name) |name| {
                const value = try self.getStringValue(&arg);
                if (value) |v| {
                    if (std.mem.eql(u8, name.name, "identity")) {
                        author.identity = try self.allocator.dupe(u8, v);
                    } else if (std.mem.eql(u8, name.name, "model")) {
                        author.model = try self.allocator.dupe(u8, v);
                    } else if (std.mem.eql(u8, name.name, "prompt_hash")) {
                        author.prompt_hash = try self.allocator.dupe(u8, v);
                    } else if (std.mem.eql(u8, name.name, "reviewed_by")) {
                        author.reviewed_by = try self.allocator.dupe(u8, v);
                    }
                }
            }
        }

        try metadata.addAuthor(author);
    }

    fn extractConfidence(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        if (attr.args.len == 0) {
            try self.addError(.{
                .kind = .missing_confidence_value,
                .message = try self.allocator.dupe(u8, "@confidence requires a value between 0.0 and 1.0"),
                .span = attr.span,
            });
            return;
        }

        const first_arg = &attr.args[0];
        var value: ?f64 = null;

        switch (first_arg.value) {
            .float => |f| value = f,
            .int => |i| value = @floatFromInt(i),
            else => {
                try self.addError(.{
                    .kind = .invalid_confidence_value,
                    .message = try self.allocator.dupe(u8, "@confidence value must be a number"),
                    .span = attr.span,
                });
                return;
            },
        }

        if (value) |v| {
            if (!Confidence.isValid(v)) {
                try self.addError(.{
                    .kind = .confidence_out_of_range,
                    .message = try std.fmt.allocPrint(
                        self.allocator,
                        "@confidence value {d:.2} is out of range [0.0, 1.0]",
                        .{v},
                    ),
                    .span = attr.span,
                });
            }
            metadata.confidence = .{ .value = std.math.clamp(v, 0.0, 1.0), .span = attr.span };
        }
    }

    fn extractNeedsReview(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var needs_review = NeedsReview{ .span = attr.span };

        for (attr.args) |arg| {
            if (arg.name) |name| {
                if (std.mem.eql(u8, name.name, "reason")) {
                    if (try self.getStringValue(&arg)) |v| {
                        needs_review.reason = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "priority")) {
                    if (try self.getStringValue(&arg)) |v| {
                        if (NeedsReview.Priority.fromString(v)) |p| {
                            needs_review.priority = p;
                        }
                    }
                }
            } else {
                // Positional argument - assume it's the reason
                if (try self.getStringValue(&arg)) |v| {
                    needs_review.reason = try self.allocator.dupe(u8, v);
                }
            }
        }

        metadata.needs_review = needs_review;
    }

    fn extractApproval(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var by: ?[]const u8 = null;
        var at: ?[]const u8 = null;
        var note: ?[]const u8 = null;

        for (attr.args) |arg| {
            if (arg.name) |name| {
                if (std.mem.eql(u8, name.name, "by")) {
                    if (try self.getStringValue(&arg)) |v| {
                        by = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "at")) {
                    if (try self.getStringValue(&arg)) |v| {
                        at = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "note")) {
                    if (try self.getStringValue(&arg)) |v| {
                        note = try self.allocator.dupe(u8, v);
                    }
                }
            }
        }

        if (by == null) {
            try self.addError(.{
                .kind = .missing_approval_by,
                .message = try self.allocator.dupe(u8, "@approved requires 'by' field"),
                .span = attr.span,
            });
            return;
        }

        try metadata.addApproval(.{
            .by = by.?,
            .at = at,
            .note = note,
            .span = attr.span,
        });
    }

    fn extractVerified(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var status: VerifiedStatus = .pending;
        var obligations = std.ArrayListUnmanaged([]const u8){};
        errdefer {
            for (obligations.items) |ob| {
                self.allocator.free(ob);
            }
            obligations.deinit(self.allocator);
        }

        // First positional argument should be the status
        if (attr.args.len > 0) {
            const first_arg = &attr.args[0];
            if (first_arg.name == null) {
                switch (first_arg.value) {
                    .identifier => |id| {
                        if (VerifiedStatus.fromString(id.name)) |s| {
                            status = s;
                        } else {
                            try self.addError(.{
                                .kind = .invalid_verified_status,
                                .message = try std.fmt.allocPrint(
                                    self.allocator,
                                    "invalid verification status: '{s}'",
                                    .{id.name},
                                ),
                                .span = attr.span,
                            });
                        }
                    },
                    .string => |s| {
                        if (VerifiedStatus.fromString(s)) |st| {
                            status = st;
                        }
                    },
                    else => {},
                }
            }
        }

        // Process named arguments for obligations
        // Note: The current AST doesn't support array values directly,
        // so obligations would need to be passed as multiple arguments
        // or a comma-separated string. For now, we support string format.
        for (attr.args) |arg| {
            if (arg.name) |name| {
                if (std.mem.eql(u8, name.name, "obligations")) {
                    // Parse as comma-separated string
                    if (try self.getStringValue(&arg)) |v| {
                        var iter = std.mem.splitScalar(u8, v, ',');
                        while (iter.next()) |ob| {
                            const trimmed = std.mem.trim(u8, ob, " \t");
                            if (trimmed.len > 0) {
                                try obligations.append(self.allocator, try self.allocator.dupe(u8, trimmed));
                            }
                        }
                    }
                }
            }
        }

        metadata.verification = .{
            .status = status,
            .obligations = try obligations.toOwnedSlice(self.allocator),
            .span = attr.span,
        };
    }

    fn extractCheckpoint(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var model: ?[]const u8 = null;
        var timestamp: ?[]const u8 = null;
        var context_hash: ?[]const u8 = null;
        var version: ?u32 = null;

        for (attr.args) |arg| {
            if (arg.name) |name| {
                if (std.mem.eql(u8, name.name, "model")) {
                    if (try self.getStringValue(&arg)) |v| {
                        model = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "timestamp")) {
                    if (try self.getStringValue(&arg)) |v| {
                        timestamp = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "context_hash")) {
                    if (try self.getStringValue(&arg)) |v| {
                        context_hash = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "version")) {
                    switch (arg.value) {
                        .int => |i| {
                            if (i >= 0 and i <= std.math.maxInt(u32)) {
                                version = @intCast(i);
                            }
                        },
                        else => {},
                    }
                }
            }
        }

        if (model == null) {
            try self.addError(.{
                .kind = .missing_checkpoint_model,
                .message = try self.allocator.dupe(u8, "@generation_checkpoint requires 'model' field"),
                .span = attr.span,
            });
            return;
        }

        metadata.checkpoint = .{
            .model = model.?,
            .timestamp = timestamp,
            .context_hash = context_hash,
            .version = version,
            .span = attr.span,
        };
    }

    fn extractGenerating(self: *ProvenanceExtractor, metadata: *ProvenanceMetadata, attr: *const Ast.Attribute) !void {
        var generating = GeneratingMarker{ .span = attr.span };

        for (attr.args) |arg| {
            if (arg.name) |name| {
                if (std.mem.eql(u8, name.name, "model")) {
                    if (try self.getStringValue(&arg)) |v| {
                        generating.model = try self.allocator.dupe(u8, v);
                    }
                } else if (std.mem.eql(u8, name.name, "started_at")) {
                    if (try self.getStringValue(&arg)) |v| {
                        generating.started_at = try self.allocator.dupe(u8, v);
                    }
                }
            }
        }

        metadata.generating = generating;
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn getStringValue(self: *ProvenanceExtractor, arg: *const Ast.AttributeArg) !?[]const u8 {
        _ = self;
        return switch (arg.value) {
            .string => |s| s,
            .identifier => |id| id.name,
            else => null,
        };
    }

    fn addError(self: *ProvenanceExtractor, err: ExtractionError) !void {
        try self.errors.append(self.allocator, err);
    }

    // ========================================================================
    // Error Access
    // ========================================================================

    pub fn hasErrors(self: *const ProvenanceExtractor) bool {
        return self.errors.items.len > 0;
    }

    pub fn getErrors(self: *const ProvenanceExtractor) []const ExtractionError {
        return self.errors.items;
    }
};

// ============================================================================
// Extraction Errors
// ============================================================================

pub const ExtractionError = struct {
    kind: Kind,
    message: []const u8,
    span: ?Span = null,

    pub const Kind = enum {
        invalid_author_kind,
        missing_confidence_value,
        invalid_confidence_value,
        confidence_out_of_range,
        missing_approval_by,
        invalid_verified_status,
        missing_checkpoint_model,
        invalid_timestamp,
        general,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "ProvenanceExtractor init/deinit" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    try std.testing.expect(!extractor.hasErrors());
}

test "extractFromAttributes returns null for empty" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    const result = try extractor.extractFromAttributes(&.{});
    try std.testing.expect(result == null);
}

test "extractFromAttributes with @author" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    // Create a simple @author(human) attribute
    const span = Span.init(
        @import("../lexer/root.zig").Location.init(1, 1, 0),
        @import("../lexer/root.zig").Location.init(1, 15, 14),
    );
    const name_ident = Ast.Identifier.init("author", span);
    const human_ident = Ast.Identifier.init("human", span);

    const args = [_]Ast.AttributeArg{
        .{
            .name = null,
            .value = .{ .identifier = human_ident },
            .span = span,
        },
    };

    const attrs = [_]Ast.Attribute{
        Ast.Attribute.init(name_ident, &args, span),
    };

    const result = try extractor.extractFromAttributes(&attrs);
    try std.testing.expect(result != null);

    if (result) |metadata| {
        defer {
            metadata.deinit();
            testing_alloc.destroy(metadata);
        }
        try std.testing.expectEqual(@as(usize, 1), metadata.authors.items.len);
        try std.testing.expectEqual(AuthorKind.human, metadata.authors.items[0].kind);
    }
}

test "extractFromAttributes with @confidence" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    const span = Span.init(
        @import("../lexer/root.zig").Location.init(1, 1, 0),
        @import("../lexer/root.zig").Location.init(1, 18, 17),
    );
    const name_ident = Ast.Identifier.init("confidence", span);

    const args = [_]Ast.AttributeArg{
        .{
            .name = null,
            .value = .{ .float = 0.85 },
            .span = span,
        },
    };

    const attrs = [_]Ast.Attribute{
        Ast.Attribute.init(name_ident, &args, span),
    };

    const result = try extractor.extractFromAttributes(&attrs);
    try std.testing.expect(result != null);

    if (result) |metadata| {
        defer {
            metadata.deinit();
            testing_alloc.destroy(metadata);
        }
        try std.testing.expect(metadata.confidence != null);
        try std.testing.expectApproxEqAbs(@as(f64, 0.85), metadata.confidence.?.value, 0.001);
    }
}

test "extractFromAttributes with invalid confidence" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    const span = Span.init(
        @import("../lexer/root.zig").Location.init(1, 1, 0),
        @import("../lexer/root.zig").Location.init(1, 18, 17),
    );
    const name_ident = Ast.Identifier.init("confidence", span);

    const args = [_]Ast.AttributeArg{
        .{
            .name = null,
            .value = .{ .float = 1.5 }, // Out of range
            .span = span,
        },
    };

    const attrs = [_]Ast.Attribute{
        Ast.Attribute.init(name_ident, &args, span),
    };

    const result = try extractor.extractFromAttributes(&attrs);
    try std.testing.expect(result != null);
    try std.testing.expect(extractor.hasErrors());
    try std.testing.expectEqual(ExtractionError.Kind.confidence_out_of_range, extractor.getErrors()[0].kind);

    if (result) |metadata| {
        metadata.deinit();
        testing_alloc.destroy(metadata);
    }
}

test "extractFromAttributes with @verified" {
    const testing_alloc = std.testing.allocator;
    var extractor = ProvenanceExtractor.init(testing_alloc);
    defer extractor.deinit();

    const span = Span.init(
        @import("../lexer/root.zig").Location.init(1, 1, 0),
        @import("../lexer/root.zig").Location.init(1, 20, 19),
    );
    const name_ident = Ast.Identifier.init("verified", span);
    const proven_ident = Ast.Identifier.init("proven", span);

    const args = [_]Ast.AttributeArg{
        .{
            .name = null,
            .value = .{ .identifier = proven_ident },
            .span = span,
        },
    };

    const attrs = [_]Ast.Attribute{
        Ast.Attribute.init(name_ident, &args, span),
    };

    const result = try extractor.extractFromAttributes(&attrs);
    try std.testing.expect(result != null);

    if (result) |metadata| {
        defer {
            metadata.deinit();
            testing_alloc.destroy(metadata);
        }
        try std.testing.expect(metadata.verification != null);
        try std.testing.expectEqual(VerifiedStatus.proven, metadata.verification.?.status);
    }
}
