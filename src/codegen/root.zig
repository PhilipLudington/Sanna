//! Code Generation Module
//!
//! This module provides the interface for AI-powered code generation from
//! Sanna specifications. It includes:
//!
//! - Request/response types for generation API
//! - Target language configuration
//! - Context preparation for AI prompts
//! - Generated code parsing and validation
//! - Automatic annotation insertion
//! - Typed hole generation for partial implementations
//! - Checkpoint serialization for resumable generation
//! - Mock generator for testing
//!
//! ## Usage
//!
//! ```zig
//! const codegen = @import("codegen");
//!
//! // Create a generation request
//! const request = codegen.GenerationRequest.init(
//!     .{ .function = .{ .module_path = &.{}, .name = "my_func" } },
//!     codegen.LanguageOptions.default(.zig),
//!     .{ .provider = "anthropic", .name = "claude-3-opus" },
//! );
//!
//! // For testing, use mock generator
//! var mock = codegen.MockGenerator.init(allocator, .{});
//! defer mock.deinit();
//! const response = try mock.generate(request);
//! ```

const std = @import("std");

// Re-export types
pub const types = @import("types.zig");
pub const GenerationRequest = types.GenerationRequest;
pub const GenerationResponse = types.GenerationResponse;
pub const GeneratedCode = types.GeneratedCode;
pub const TargetLanguage = types.TargetLanguage;
pub const LanguageOptions = types.LanguageOptions;
pub const AIModel = types.AIModel;
pub const ModelParameters = types.ModelParameters;
pub const SpecificationRef = types.SpecificationRef;
pub const GenerationHint = types.GenerationHint;
pub const GenerationContext = types.GenerationContext;
pub const TypedHole = types.TypedHole;
pub const Annotation = types.Annotation;
pub const Checkpoint = types.Checkpoint;
pub const CheckpointId = types.CheckpointId;

// Re-export context builder
pub const context = @import("context.zig");
pub const ContextBuilder = context.ContextBuilder;
pub const SpecFormatter = context.SpecFormatter;

// Re-export parser
pub const parser = @import("parser.zig");
pub const CodeParser = parser.CodeParser;
pub const ParseResult = parser.ParseResult;

// Re-export annotator
pub const annotator = @import("annotator.zig");
pub const Annotator = annotator.Annotator;
pub const AnnotationBuilder = annotator.AnnotationBuilder;
pub const createAuthorAnnotation = annotator.createAuthorAnnotation;
pub const createConfidenceAnnotation = annotator.createConfidenceAnnotation;
pub const createNeedsReviewAnnotation = annotator.createNeedsReviewAnnotation;
pub const createGeneratingMarker = annotator.createGeneratingMarker;
pub const createVerifiedAnnotation = annotator.createVerifiedAnnotation;

// Re-export holes
pub const holes = @import("holes.zig");
pub const HoleGenerator = holes.HoleGenerator;
pub const HoleScanner = holes.HoleScanner;
pub const PartialImplementation = holes.PartialImplementation;
pub const createPartialImplementation = holes.createPartialImplementation;

// Re-export checkpoint
pub const checkpoint = @import("checkpoint.zig");
pub const CheckpointManager = checkpoint.CheckpointManager;
pub const CheckpointBuilder = checkpoint.CheckpointBuilder;
pub const GenerationSession = checkpoint.GenerationSession;
pub const CheckpointMetadata = checkpoint.CheckpointMetadata;

// Re-export mock generator
pub const mock = @import("mock.zig");
pub const MockGenerator = mock.MockGenerator;
pub const GeneratorFactory = mock.GeneratorFactory;

/// Generator interface that both mock and real implementations can satisfy.
pub const Generator = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        generate: *const fn (ptr: *anyopaque, request: GenerationRequest) anyerror!GenerationResponse,
        deinit: *const fn (ptr: *anyopaque) void,
    };

    pub fn generate(self: Generator, request: GenerationRequest) !GenerationResponse {
        return self.vtable.generate(self.ptr, request);
    }

    pub fn deinit(self: Generator) void {
        self.vtable.deinit(self.ptr);
    }

    /// Create a Generator from a MockGenerator.
    pub fn fromMock(m: *MockGenerator) Generator {
        return .{
            .ptr = m,
            .vtable = &.{
                .generate = struct {
                    fn generate(ptr: *anyopaque, request: GenerationRequest) anyerror!GenerationResponse {
                        const gen: *MockGenerator = @ptrCast(@alignCast(ptr));
                        return gen.generate(request);
                    }
                }.generate,
                .deinit = struct {
                    fn deinit(ptr: *anyopaque) void {
                        const gen: *MockGenerator = @ptrCast(@alignCast(ptr));
                        gen.deinit();
                    }
                }.deinit,
            },
        };
    }
};

/// Pipeline for code generation with all stages.
pub const GenerationPipeline = struct {
    allocator: std.mem.Allocator,
    generator: Generator,
    annotator_instance: Annotator,
    parser_instance: CodeParser,
    hole_scanner: HoleScanner,
    checkpoint_manager: CheckpointManager,

    pub fn init(
        allocator: std.mem.Allocator,
        generator: Generator,
        target: TargetLanguage,
        checkpoint_path: []const u8,
    ) GenerationPipeline {
        return .{
            .allocator = allocator,
            .generator = generator,
            .annotator_instance = Annotator.init(allocator, target),
            .parser_instance = CodeParser.init(allocator, target),
            .hole_scanner = HoleScanner.init(allocator, target),
            .checkpoint_manager = CheckpointManager.init(allocator, checkpoint_path),
        };
    }

    pub fn deinit(self: *GenerationPipeline) void {
        self.annotator_instance.deinit();
        self.parser_instance.deinit();
        self.checkpoint_manager.deinit();
    }

    /// Run the full generation pipeline.
    pub fn run(self: *GenerationPipeline, request: GenerationRequest) !PipelineResult {
        // Step 1: Generate code
        var response = try self.generator.generate(request);
        errdefer response.deinit(self.allocator);

        if (response.status == .failed) {
            return PipelineResult{
                .success = false,
                .response = response,
                .annotated_code = null,
                .parse_result = null,
                .detected_holes = &.{},
            };
        }

        const code = response.code orelse return PipelineResult{
            .success = false,
            .response = response,
            .annotated_code = null,
            .parse_result = null,
            .detected_holes = &.{},
        };

        // Step 2: Parse generated code
        const parse_result = try self.parser_instance.parse(code.source);

        // Step 3: Scan for holes
        const detected_holes = try self.hole_scanner.scan(code.source);
        errdefer self.allocator.free(detected_holes);

        // Step 4: Annotate code
        var annotated_code: ?[]u8 = null;
        if (request.model.provider.len > 0) {
            self.annotator_instance.setModel(request.model);
            var mutable_code = GeneratedCode{
                .source = code.source,
                .language = code.language,
                .components = code.components,
                .holes = code.holes,
                .annotations = code.annotations,
                .confidence = code.confidence,
            };
            annotated_code = try self.annotator_instance.annotateGeneratedCode(&mutable_code);
        }

        return PipelineResult{
            .success = parse_result.success,
            .response = response,
            .annotated_code = annotated_code,
            .parse_result = parse_result,
            .detected_holes = detected_holes,
        };
    }

    /// Resume generation from checkpoint.
    pub fn resumeFromCheckpoint(self: *GenerationPipeline, id: CheckpointId, request: GenerationRequest) !PipelineResult {
        const maybe_checkpoint = try self.checkpoint_manager.resumeFromCheckpoint(id);

        if (maybe_checkpoint) |saved_checkpoint| {
            // Modify request to continue from checkpoint
            var continued_request = request;
            continued_request.checkpoint = saved_checkpoint.id;

            return self.run(continued_request);
        }

        // No checkpoint found, start fresh
        return self.run(request);
    }
};

/// Result of the generation pipeline.
pub const PipelineResult = struct {
    success: bool,
    response: GenerationResponse,
    annotated_code: ?[]u8,
    parse_result: ?ParseResult,
    detected_holes: []const TypedHole,

    pub fn deinit(self: *PipelineResult, allocator: std.mem.Allocator) void {
        self.response.deinit(allocator);
        if (self.annotated_code) |code| {
            allocator.free(code);
        }
        if (self.detected_holes.len > 0) {
            allocator.free(self.detected_holes);
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "module imports" {
    // Verify all public types are accessible
    _ = GenerationRequest;
    _ = GenerationResponse;
    _ = TargetLanguage;
    _ = MockGenerator;
    _ = ContextBuilder;
    _ = CodeParser;
    _ = Annotator;
    _ = HoleGenerator;
    _ = CheckpointManager;
}

test "Generator interface with MockGenerator" {
    const testing = std.testing;

    var mock_gen = MockGenerator.init(testing.allocator, .{
        .hole_probability = 0.0,
    });

    const gen = Generator.fromMock(&mock_gen);
    defer gen.deinit();

    const request = GenerationRequest.init(
        .{ .function = .{ .module_path = &.{}, .name = "test" } },
        LanguageOptions.default(.zig),
        .{ .provider = "mock", .name = "test" },
    );

    var response = try gen.generate(request);
    defer response.deinit(testing.allocator);
    try testing.expectEqual(GenerationResponse.Status.success, response.status);
}

test "GenerationPipeline basic" {
    const testing = std.testing;

    var mock_gen = MockGenerator.init(testing.allocator, .{
        .hole_probability = 0.0,
    });

    const gen = Generator.fromMock(&mock_gen);

    var pipeline = GenerationPipeline.init(
        testing.allocator,
        gen,
        .zig,
        "/tmp/test_checkpoints",
    );
    defer pipeline.deinit();

    const request = GenerationRequest.init(
        .{ .function = .{ .module_path = &.{}, .name = "test_fn" } },
        LanguageOptions.default(.zig),
        .{ .provider = "", .name = "" }, // Empty provider skips annotation
    );

    var result = try pipeline.run(request);
    defer result.deinit(testing.allocator);

    try testing.expect(result.success);
    try testing.expect(result.response.code != null);
}
