//! Annotation Inserter
//!
//! Automatically inserts provenance annotations (@author, @confidence, etc.)
//! into generated code based on the generation context.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Provenance = @import("../provenance/Provenance.zig");
const types = @import("types.zig");

const Annotation = types.Annotation;
const AnnotationArg = types.Annotation.AnnotationArg;
const GeneratedCode = types.GeneratedCode;
const CodeComponent = types.GeneratedCode.CodeComponent;
const AIModel = types.AIModel;
const TargetLanguage = types.TargetLanguage;

/// Annotator for inserting provenance metadata into generated code.
pub const Annotator = struct {
    allocator: Allocator,
    target: TargetLanguage,
    annotations: std.ArrayListUnmanaged(Annotation),
    model: ?AIModel,
    confidence_threshold: f32,
    review_threshold: f32,
    include_timestamps: bool,
    include_model_info: bool,

    pub fn init(allocator: Allocator, target: TargetLanguage) Annotator {
        return .{
            .allocator = allocator,
            .target = target,
            .annotations = .{},
            .model = null,
            .confidence_threshold = 0.7,
            .review_threshold = 0.5,
            .include_timestamps = true,
            .include_model_info = true,
        };
    }

    pub fn deinit(self: *Annotator) void {
        self.annotations.deinit(self.allocator);
    }

    /// Set the AI model used for generation.
    pub fn setModel(self: *Annotator, model: AIModel) void {
        self.model = model;
    }

    /// Set confidence thresholds.
    pub fn setThresholds(self: *Annotator, confidence: f32, review: f32) void {
        self.confidence_threshold = confidence;
        self.review_threshold = review;
    }

    /// Generate annotations for a code component.
    pub fn annotateComponent(
        self: *Annotator,
        component: *const CodeComponent,
        line: u32,
    ) ![]const Annotation {
        self.annotations.clearRetainingCapacity();

        // @author annotation
        try self.addAuthorAnnotation(component, line);

        // @confidence annotation
        try self.addConfidenceAnnotation(component, line);

        // @needs_review annotation (if needed)
        if (component.needs_review or component.confidence < self.review_threshold) {
            try self.addNeedsReviewAnnotation(component, line);
        }

        return self.annotations.items;
    }

    /// Add @author annotation.
    fn addAuthorAnnotation(self: *Annotator, component: *const CodeComponent, line: u32) !void {
        _ = component;

        // Build args inline
        const model_str: []const u8 = if (self.model) |m| m.name else "unknown";

        try self.annotations.append(self.allocator, .{
            .name = "author",
            .args = &[_]AnnotationArg{
                .{ .name = "kind", .value = .{ .string = "ai" } },
                .{ .name = "model", .value = .{ .string = model_str } },
            },
            .target = .{
                .kind = .function,
                .name = "",
            },
            .insert_location = .{
                .line = line,
            },
        });
    }

    /// Add @confidence annotation.
    fn addConfidenceAnnotation(self: *Annotator, component: *const CodeComponent, line: u32) !void {
        const level = confidenceLevel(component.confidence);

        try self.annotations.append(self.allocator, .{
            .name = "confidence",
            .args = &[_]AnnotationArg{
                .{ .name = null, .value = .{ .float = component.confidence } },
                .{ .name = "level", .value = .{ .string = level } },
            },
            .target = .{
                .kind = componentKindToTargetKind(component.kind),
                .name = component.name,
            },
            .insert_location = .{
                .line = line,
            },
        });
    }

    /// Add @needs_review annotation.
    fn addNeedsReviewAnnotation(self: *Annotator, component: *const CodeComponent, line: u32) !void {
        const reason = component.review_reason orelse
            if (component.confidence < self.review_threshold)
            "low confidence score"
        else
            "flagged for review";

        const priority = reviewPriority(component.confidence);

        try self.annotations.append(self.allocator, .{
            .name = "needs_review",
            .args = &[_]AnnotationArg{
                .{ .name = "reason", .value = .{ .string = reason } },
                .{ .name = "priority", .value = .{ .string = priority } },
            },
            .target = .{
                .kind = componentKindToTargetKind(component.kind),
                .name = component.name,
            },
            .insert_location = .{
                .line = line,
            },
        });
    }

    /// Insert annotations into source code.
    pub fn insertAnnotations(
        self: *Annotator,
        source: []const u8,
        annotations_to_insert: []const Annotation,
    ) ![]u8 {
        if (annotations_to_insert.len == 0) {
            return try self.allocator.dupe(u8, source);
        }

        // Sort annotations by line (descending) to insert from bottom up
        const sorted = try self.allocator.alloc(Annotation, annotations_to_insert.len);
        defer self.allocator.free(sorted);
        @memcpy(sorted, annotations_to_insert);

        std.sort.pdq(Annotation, sorted, {}, struct {
            fn lessThan(_: void, a: Annotation, b: Annotation) bool {
                return a.insert_location.line > b.insert_location.line;
            }
        }.lessThan);

        // Build result
        var result = std.ArrayListUnmanaged(u8){};
        errdefer result.deinit(self.allocator);

        // Split source into lines
        var lines = std.ArrayListUnmanaged([]const u8){};
        defer lines.deinit(self.allocator);

        var iter = std.mem.splitScalar(u8, source, '\n');
        while (iter.next()) |line| {
            try lines.append(self.allocator, line);
        }

        // Track which annotations go where
        var line_annotations = std.AutoHashMap(u32, std.ArrayListUnmanaged(Annotation)).init(self.allocator);
        defer {
            var it = line_annotations.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            line_annotations.deinit();
        }

        for (sorted) |ann| {
            const entry = try line_annotations.getOrPut(ann.insert_location.line);
            if (!entry.found_existing) {
                entry.value_ptr.* = .{};
            }
            try entry.value_ptr.append(self.allocator, ann);
        }

        // Write output
        for (lines.items, 0..) |line, i| {
            const line_num: u32 = @intCast(i + 1);

            // Insert any annotations for this line
            if (line_annotations.get(line_num)) |anns| {
                for (anns.items) |ann| {
                    const ann_str = try self.formatAnnotation(ann);
                    defer self.allocator.free(ann_str);
                    try result.appendSlice(self.allocator, ann_str);
                    try result.append(self.allocator, '\n');
                }
            }

            try result.appendSlice(self.allocator, line);
            if (i < lines.items.len - 1) {
                try result.append(self.allocator, '\n');
            }
        }

        return result.toOwnedSlice(self.allocator);
    }

    /// Format an annotation as a string.
    fn formatAnnotation(self: *Annotator, ann: Annotation) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        // Comment prefix for target language
        const prefix = commentPrefix(self.target);
        try buf.appendSlice(self.allocator, prefix);

        // Annotation start
        try buf.append(self.allocator, '@');
        try buf.appendSlice(self.allocator, ann.name);

        if (ann.args.len > 0) {
            try buf.append(self.allocator, '(');

            for (ann.args, 0..) |arg, i| {
                if (i > 0) try buf.appendSlice(self.allocator, ", ");

                if (arg.name) |name| {
                    try buf.appendSlice(self.allocator, name);
                    try buf.appendSlice(self.allocator, " = ");
                }

                switch (arg.value) {
                    .string => |s| {
                        try buf.append(self.allocator, '"');
                        try buf.appendSlice(self.allocator, s);
                        try buf.append(self.allocator, '"');
                    },
                    .int => |n| {
                        var num_buf: [20]u8 = undefined;
                        const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{n}) catch unreachable;
                        try buf.appendSlice(self.allocator, num_str);
                    },
                    .float => |f| {
                        var num_buf: [32]u8 = undefined;
                        const num_str = std.fmt.bufPrint(&num_buf, "{d:.2}", .{f}) catch unreachable;
                        try buf.appendSlice(self.allocator, num_str);
                    },
                    .bool_val => |b| {
                        try buf.appendSlice(self.allocator, if (b) "true" else "false");
                    },
                }
            }

            try buf.append(self.allocator, ')');
        }

        return buf.toOwnedSlice(self.allocator);
    }

    /// Annotate all components in generated code and return annotated source.
    pub fn annotateGeneratedCode(self: *Annotator, code: *GeneratedCode) ![]u8 {
        var all_annotations = std.ArrayListUnmanaged(Annotation){};
        defer all_annotations.deinit(self.allocator);

        // Collect existing annotations
        for (code.annotations) |ann| {
            try all_annotations.append(self.allocator, ann);
        }

        // Generate annotations for each component
        for (code.components) |*component| {
            const line = self.findComponentLine(code.source, component.start_offset);
            const anns = try self.annotateComponent(component, line);
            for (anns) |ann| {
                try all_annotations.append(self.allocator, ann);
            }
        }

        return self.insertAnnotations(code.source, all_annotations.items);
    }

    /// Find the line number for a given offset.
    fn findComponentLine(_: *Annotator, source: []const u8, offset: usize) u32 {
        var line: u32 = 1;
        for (source[0..@min(offset, source.len)]) |c| {
            if (c == '\n') line += 1;
        }
        return line;
    }
};

/// Get the comment prefix for a target language.
fn commentPrefix(target: TargetLanguage) []const u8 {
    return switch (target) {
        .klar, .kira, .zig, .rust, .typescript => "// ",
        .python => "# ",
    };
}

/// Convert component kind to annotation target kind.
fn componentKindToTargetKind(kind: CodeComponent.Kind) Annotation.Target.Kind {
    return switch (kind) {
        .function, .method => .function,
        .type_impl => .type_def,
        .helper => .function,
        .test_case => .function,
        .constant => .field,
    };
}

/// Get confidence level string from numeric value.
fn confidenceLevel(confidence: f32) []const u8 {
    if (confidence >= 0.9) return "high";
    if (confidence >= 0.7) return "medium";
    if (confidence >= 0.5) return "low";
    return "very_low";
}

/// Get review priority from confidence.
fn reviewPriority(confidence: f32) []const u8 {
    if (confidence < 0.3) return "critical";
    if (confidence < 0.5) return "high";
    if (confidence < 0.7) return "medium";
    return "low";
}

// ============================================================================
// Tests
// ============================================================================

test "Annotator basic annotation" {
    const testing = std.testing;
    var annotator = Annotator.init(testing.allocator, .zig);
    defer annotator.deinit();

    annotator.setModel(.{
        .provider = "anthropic",
        .name = "claude-3-opus",
    });

    const component = CodeComponent{
        .kind = .function,
        .name = "add",
        .source = "fn add(a: i32, b: i32) i32 { return a + b; }",
        .start_offset = 0,
        .end_offset = 42,
        .confidence = 0.85,
    };

    const anns = try annotator.annotateComponent(&component, 1);
    try testing.expect(anns.len >= 2);
}

test "Annotator insert annotations" {
    const testing = std.testing;
    var annotator = Annotator.init(testing.allocator, .zig);
    defer annotator.deinit();

    const source = "pub fn add(a: i32, b: i32) i32 {\n    return a + b;\n}";

    const ann = Annotation{
        .name = "author",
        .args = &.{},
        .target = .{ .kind = .function, .name = "add" },
        .insert_location = .{ .line = 1 },
    };

    const result = try annotator.insertAnnotations(source, &.{ann});
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "// @author") != null);
}

test "confidenceLevel mapping" {
    const testing = std.testing;

    try testing.expectEqualStrings("high", confidenceLevel(0.95));
    try testing.expectEqualStrings("medium", confidenceLevel(0.75));
    try testing.expectEqualStrings("low", confidenceLevel(0.55));
    try testing.expectEqualStrings("very_low", confidenceLevel(0.3));
}
