//! Code Generation Types
//!
//! Defines the data structures for AI-powered code generation from Sanna specifications.
//! This includes request formats, response formats, target language configuration,
//! and generation context.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("../parser/Ast.zig");
const Type = @import("../types/Type.zig");
const Provenance = @import("../provenance/Provenance.zig");

// ============================================================================
// Target Language Configuration
// ============================================================================

/// Target language for code generation.
pub const TargetLanguage = enum {
    /// Klar - A systems programming language with manual memory management
    klar,
    /// Kira - A higher-level language with garbage collection
    kira,
    /// Zig - Low-level systems programming
    zig,
    /// Rust - Memory-safe systems programming
    rust,
    /// TypeScript - JavaScript with types
    typescript,
    /// Python - Dynamic scripting language
    python,

    pub fn displayName(self: TargetLanguage) []const u8 {
        return switch (self) {
            .klar => "Klar",
            .kira => "Kira",
            .zig => "Zig",
            .rust => "Rust",
            .typescript => "TypeScript",
            .python => "Python",
        };
    }

    pub fn fileExtension(self: TargetLanguage) []const u8 {
        return switch (self) {
            .klar => ".klar",
            .kira => ".kira",
            .zig => ".zig",
            .rust => ".rs",
            .typescript => ".ts",
            .python => ".py",
        };
    }

    pub fn supportsTypeAnnotations(self: TargetLanguage) bool {
        return switch (self) {
            .klar, .kira, .zig, .rust, .typescript => true,
            .python => true, // Python 3 type hints
        };
    }

    pub fn supportsContracts(self: TargetLanguage) bool {
        return switch (self) {
            .klar, .kira => true, // Native contract support
            .rust => false, // Would need macros
            .zig => false, // Would need comptime
            .typescript, .python => false,
        };
    }

    pub fn fromString(s: []const u8) ?TargetLanguage {
        const map = std.StaticStringMap(TargetLanguage).initComptime(.{
            .{ "klar", .klar },
            .{ "kira", .kira },
            .{ "zig", .zig },
            .{ "rust", .rust },
            .{ "typescript", .typescript },
            .{ "ts", .typescript },
            .{ "python", .python },
            .{ "py", .python },
        });
        return map.get(s);
    }
};

/// Language-specific generation options.
pub const LanguageOptions = struct {
    /// Target language
    language: TargetLanguage,
    /// Language version/edition (e.g., "2021" for Rust, "ES2022" for TypeScript)
    version: ?[]const u8 = null,
    /// Whether to generate runtime contract checks
    emit_contracts: bool = true,
    /// Whether to generate documentation comments
    emit_docs: bool = true,
    /// Whether to generate type annotations (for languages that support them)
    emit_types: bool = true,
    /// Indentation style
    indent: IndentStyle = .spaces_4,
    /// Line ending style
    line_ending: LineEnding = .lf,
    /// Maximum line length (0 = no limit)
    max_line_length: u32 = 100,

    pub const IndentStyle = enum {
        spaces_2,
        spaces_4,
        tabs,

        pub fn string(self: IndentStyle) []const u8 {
            return switch (self) {
                .spaces_2 => "  ",
                .spaces_4 => "    ",
                .tabs => "\t",
            };
        }
    };

    pub const LineEnding = enum {
        lf,
        crlf,

        pub fn string(self: LineEnding) []const u8 {
            return switch (self) {
                .lf => "\n",
                .crlf => "\r\n",
            };
        }
    };

    pub fn default(language: TargetLanguage) LanguageOptions {
        return .{
            .language = language,
            .emit_contracts = language.supportsContracts(),
        };
    }
};

// ============================================================================
// AI Model Configuration
// ============================================================================

/// AI model identifier for code generation.
pub const AIModel = struct {
    /// Model provider (e.g., "anthropic", "openai", "local")
    provider: []const u8,
    /// Model name/version (e.g., "claude-3-opus", "gpt-4")
    name: []const u8,
    /// Optional specific version or checkpoint
    version: ?[]const u8 = null,

    pub fn format(self: AIModel, allocator: Allocator) ![]const u8 {
        if (self.version) |v| {
            return std.fmt.allocPrint(allocator, "{s}/{s}:{s}", .{ self.provider, self.name, v });
        }
        return std.fmt.allocPrint(allocator, "{s}/{s}", .{ self.provider, self.name });
    }

    pub fn parse(s: []const u8) ?AIModel {
        // Format: provider/model[:version]
        const slash_idx = std.mem.indexOfScalar(u8, s, '/') orelse return null;
        const provider = s[0..slash_idx];
        const rest = s[slash_idx + 1 ..];

        if (std.mem.indexOfScalar(u8, rest, ':')) |colon_idx| {
            return AIModel{
                .provider = provider,
                .name = rest[0..colon_idx],
                .version = rest[colon_idx + 1 ..],
            };
        }
        return AIModel{
            .provider = provider,
            .name = rest,
        };
    }
};

/// Generation parameters for the AI model.
pub const ModelParameters = struct {
    /// Temperature for sampling (0.0 = deterministic, 1.0 = creative)
    temperature: f32 = 0.2,
    /// Maximum tokens to generate
    max_tokens: u32 = 4096,
    /// Top-p nucleus sampling
    top_p: f32 = 0.95,
    /// Stop sequences
    stop_sequences: []const []const u8 = &.{},
    /// Whether to use streaming
    stream: bool = false,

    pub const default = ModelParameters{};

    pub fn forDeterministic() ModelParameters {
        return .{
            .temperature = 0.0,
            .top_p = 1.0,
        };
    }
};

// ============================================================================
// Generation Request
// ============================================================================

/// A request to generate code from a specification.
pub const GenerationRequest = struct {
    /// Unique request identifier
    id: RequestId,
    /// The specification to implement
    spec: SpecificationRef,
    /// Target language configuration
    target: LanguageOptions,
    /// AI model to use
    model: AIModel,
    /// Model parameters
    parameters: ModelParameters = .{},
    /// Additional context for generation
    context: ?GenerationContext = null,
    /// Generation hints from the user
    hints: []const GenerationHint = &.{},
    /// Checkpoint to resume from (for incremental generation)
    checkpoint: ?CheckpointId = null,
    /// Timestamp of request creation
    created_at: i64,

    pub const RequestId = struct {
        bytes: [16]u8,

        pub fn generate() RequestId {
            var bytes: [16]u8 = undefined;
            std.crypto.random.bytes(&bytes);
            return .{ .bytes = bytes };
        }

        pub fn format(self: RequestId) [32]u8 {
            var buf: [32]u8 = undefined;
            _ = std.fmt.bufPrint(&buf, "{}", .{std.fmt.fmtSliceHexLower(&self.bytes)}) catch unreachable;
            return buf;
        }
    };

    pub fn init(
        spec: SpecificationRef,
        target: LanguageOptions,
        model: AIModel,
    ) GenerationRequest {
        return .{
            .id = RequestId.generate(),
            .spec = spec,
            .target = target,
            .model = model,
            .created_at = std.time.timestamp(),
        };
    }
};

/// Reference to a specification to implement.
pub const SpecificationRef = union(enum) {
    /// A single function specification
    function: FunctionSpecRef,
    /// An interface specification
    interface: InterfaceSpecRef,
    /// A type with invariants
    type_def: TypeDefRef,
    /// Multiple related specifications
    module: ModuleRef,

    pub const FunctionSpecRef = struct {
        /// Module path
        module_path: []const []const u8,
        /// Function name
        name: []const u8,
        /// Optional signature for overloads
        signature: ?[]const u8 = null,
    };

    pub const InterfaceSpecRef = struct {
        /// Module path
        module_path: []const []const u8,
        /// Interface name
        name: []const u8,
    };

    pub const TypeDefRef = struct {
        /// Module path
        module_path: []const []const u8,
        /// Type name
        name: []const u8,
    };

    pub const ModuleRef = struct {
        /// Module path
        path: []const []const u8,
        /// Whether to include all declarations
        include_all: bool = true,
        /// Specific declarations to include (if not include_all)
        declarations: []const []const u8 = &.{},
    };
};

/// User-provided hints for generation.
pub const GenerationHint = struct {
    /// Type of hint
    kind: HintKind,
    /// Hint content
    content: []const u8,
    /// Target (e.g., specific function or parameter name)
    target: ?[]const u8 = null,

    pub const HintKind = enum {
        /// Algorithm suggestion
        algorithm,
        /// Data structure suggestion
        data_structure,
        /// Performance requirement
        performance,
        /// Edge case handling
        edge_case,
        /// Example usage
        example,
        /// Implementation constraint
        constraint,
        /// Free-form note
        note,
    };
};

/// Checkpoint identifier for resumable generation.
pub const CheckpointId = struct {
    bytes: [16]u8,

    pub fn generate() CheckpointId {
        var bytes: [16]u8 = undefined;
        std.crypto.random.bytes(&bytes);
        return .{ .bytes = bytes };
    }

    pub fn format(self: CheckpointId) [32]u8 {
        var buf: [32]u8 = undefined;
        const hex_chars = "0123456789abcdef";
        for (self.bytes, 0..) |byte, i| {
            buf[i * 2] = hex_chars[byte >> 4];
            buf[i * 2 + 1] = hex_chars[byte & 0x0f];
        }
        return buf;
    }
};

// ============================================================================
// Generation Context
// ============================================================================

/// Additional context provided to the AI for generation.
pub const GenerationContext = struct {
    /// Related type definitions
    related_types: []const TypeContext = &.{},
    /// Related specifications (dependencies)
    related_specs: []const SpecContext = &.{},
    /// Example implementations for similar specs
    examples: []const ExampleCode = &.{},
    /// Project conventions and patterns
    conventions: ?ConventionGuide = null,
    /// Existing code in the module
    existing_code: ?[]const u8 = null,
    /// Import statements needed
    imports: []const []const u8 = &.{},

    pub const TypeContext = struct {
        /// Type name
        name: []const u8,
        /// Type definition (serialized)
        definition: []const u8,
        /// Invariants (as strings)
        invariants: []const []const u8 = &.{},
    };

    pub const SpecContext = struct {
        /// Specification name
        name: []const u8,
        /// Full specification text
        spec_text: []const u8,
        /// Whether this spec has a verified implementation
        has_implementation: bool = false,
    };

    pub const ExampleCode = struct {
        /// Description of the example
        description: []const u8,
        /// The specification
        spec: []const u8,
        /// The implementation
        implementation: []const u8,
        /// Confidence in this example
        confidence: f32 = 1.0,
    };

    pub const ConventionGuide = struct {
        /// Naming conventions
        naming: ?[]const u8 = null,
        /// Error handling patterns
        error_handling: ?[]const u8 = null,
        /// Memory management patterns
        memory: ?[]const u8 = null,
        /// Testing patterns
        testing: ?[]const u8 = null,
    };
};

// ============================================================================
// Generation Response
// ============================================================================

/// Result of a code generation request.
pub const GenerationResponse = struct {
    /// Request that produced this response
    request_id: GenerationRequest.RequestId,
    /// Generation status
    status: Status,
    /// Generated code (if successful)
    code: ?GeneratedCode = null,
    /// Error information (if failed)
    error_info: ?ErrorInfo = null,
    /// Provenance metadata (null for failed generations)
    provenance: ?Provenance.ProvenanceMetadata = null,
    /// Checkpoint for resuming (if partial)
    checkpoint: ?Checkpoint = null,
    /// Generation metrics
    metrics: Metrics,
    /// Timestamp of completion
    completed_at: i64,

    pub const Status = enum {
        /// Generation completed successfully
        success,
        /// Generation completed with partial results (typed holes)
        partial,
        /// Generation failed
        failed,
        /// Generation was cancelled
        cancelled,
        /// Generation timed out
        timeout,
    };

    pub const ErrorInfo = struct {
        /// Error code
        code: ErrorCode,
        /// Human-readable message
        message: []const u8,
        /// Additional details
        details: ?[]const u8 = null,
        /// Source location if applicable
        location: ?SourceLocation = null,

        pub const ErrorCode = enum {
            /// Invalid specification
            invalid_spec,
            /// Unsupported language feature
            unsupported_feature,
            /// Model error
            model_error,
            /// Rate limit exceeded
            rate_limit,
            /// Context too large
            context_overflow,
            /// Parse error in generated code
            parse_error,
            /// Type error in generated code
            type_error,
            /// Internal error
            internal,
        };

        pub const SourceLocation = struct {
            file: ?[]const u8 = null,
            line: u32,
            column: u32,
        };
    };

    pub const Metrics = struct {
        /// Total generation time in milliseconds
        duration_ms: u64,
        /// Tokens in the prompt
        prompt_tokens: u32,
        /// Tokens generated
        completion_tokens: u32,
        /// Number of retries
        retry_count: u32 = 0,
        /// Cache hit (if using cached generation)
        cache_hit: bool = false,
    };

    pub fn success(
        request_id: GenerationRequest.RequestId,
        code: GeneratedCode,
        provenance: Provenance.ProvenanceMetadata,
        metrics: Metrics,
    ) GenerationResponse {
        return .{
            .request_id = request_id,
            .status = .success,
            .code = code,
            .provenance = provenance,
            .metrics = metrics,
            .completed_at = std.time.timestamp(),
        };
    }

    pub fn partial(
        request_id: GenerationRequest.RequestId,
        code: GeneratedCode,
        checkpoint: Checkpoint,
        provenance: Provenance.ProvenanceMetadata,
        metrics: Metrics,
    ) GenerationResponse {
        return .{
            .request_id = request_id,
            .status = .partial,
            .code = code,
            .checkpoint = checkpoint,
            .provenance = provenance,
            .metrics = metrics,
            .completed_at = std.time.timestamp(),
        };
    }

    pub fn failed(
        request_id: GenerationRequest.RequestId,
        error_info: ErrorInfo,
        metrics: Metrics,
    ) GenerationResponse {
        return .{
            .request_id = request_id,
            .status = .failed,
            .error_info = error_info,
            .metrics = metrics,
            .completed_at = std.time.timestamp(),
        };
    }

    /// Free resources owned by this response.
    pub fn deinit(self: *GenerationResponse, allocator: Allocator) void {
        if (self.code) |*code| {
            code.deinit(allocator);
        }
        if (self.provenance) |*prov| {
            prov.deinit();
        }
    }
};

/// Generated code with metadata.
pub const GeneratedCode = struct {
    /// The generated source code
    source: []const u8,
    /// Target language
    language: TargetLanguage,
    /// Individual components
    components: []const CodeComponent = &.{},
    /// Typed holes (incomplete parts)
    holes: []const TypedHole = &.{},
    /// Annotations to be inserted
    annotations: []const Annotation = &.{},
    /// Confidence in the generated code
    confidence: Provenance.Confidence,
    /// Verification hints for the verifier
    verification_hints: []const []const u8 = &.{},

    pub const CodeComponent = struct {
        /// Component kind
        kind: Kind,
        /// Component name
        name: []const u8,
        /// Source code for this component
        source: []const u8,
        /// Start offset in the full source
        start_offset: usize,
        /// End offset in the full source
        end_offset: usize,
        /// Confidence specific to this component
        confidence: f32,
        /// Whether this component needs review
        needs_review: bool = false,
        /// Review reason if needs_review
        review_reason: ?[]const u8 = null,

        pub const Kind = enum {
            function,
            method,
            type_impl,
            helper,
            test_case,
            constant,
        };
    };

    /// Free resources owned by this generated code.
    pub fn deinit(self: *GeneratedCode, allocator: Allocator) void {
        if (self.source.len > 0) {
            allocator.free(self.source);
        }
        if (self.components.len > 0) {
            allocator.free(self.components);
        }
        if (self.holes.len > 0) {
            allocator.free(self.holes);
        }
    }
};

/// A typed hole representing incomplete code.
pub const TypedHole = struct {
    /// Unique identifier for this hole
    id: u32,
    /// Description of what's needed
    description: []const u8,
    /// Expected type
    expected_type: []const u8,
    /// Location in generated code
    location: Location,
    /// Reason why this couldn't be generated
    reason: Reason,
    /// Suggestions for filling the hole
    suggestions: []const []const u8 = &.{},

    pub const Location = struct {
        line: u32,
        column: u32,
        length: u32,
    };

    pub const Reason = enum {
        /// Specification is ambiguous
        ambiguous_spec,
        /// Missing type information
        missing_type,
        /// Requires external dependency
        external_dependency,
        /// Complex algorithm needed
        complex_algorithm,
        /// Security-sensitive code
        security_sensitive,
        /// Performance-critical section
        performance_critical,
        /// User explicitly marked as hole
        explicit_hole,
    };
};

/// Annotation to be inserted in generated code.
pub const Annotation = struct {
    /// Annotation name (without @)
    name: []const u8,
    /// Annotation arguments
    args: []const AnnotationArg = &.{},
    /// Target of the annotation
    target: Target,
    /// Location where to insert
    insert_location: InsertLocation,

    pub const AnnotationArg = struct {
        name: ?[]const u8 = null,
        value: Value,

        pub const Value = union(enum) {
            string: []const u8,
            int: i64,
            float: f64,
            bool_val: bool,
        };
    };

    pub const Target = struct {
        kind: Kind,
        name: []const u8,

        pub const Kind = enum {
            function,
            type_def,
            field,
            parameter,
            module,
        };
    };

    pub const InsertLocation = struct {
        line: u32,
        column: u32 = 0,
    };
};

/// Checkpoint for resumable generation.
pub const Checkpoint = struct {
    /// Checkpoint identifier
    id: CheckpointId,
    /// Request that created this checkpoint
    request_id: GenerationRequest.RequestId,
    /// Partial code generated so far
    partial_code: []const u8,
    /// Remaining items to generate
    remaining: []const []const u8,
    /// Generation state (opaque to caller)
    state: []const u8,
    /// Timestamp
    created_at: i64,

    pub fn serialize(self: Checkpoint, allocator: Allocator) ![]const u8 {
        var list = std.ArrayListUnmanaged(u8).init(allocator);
        errdefer list.deinit();

        try std.json.stringify(self, .{}, list.writer());
        return list.toOwnedSlice();
    }

    pub fn deserialize(allocator: Allocator, data: []const u8) !Checkpoint {
        return std.json.parseFromSlice(Checkpoint, allocator, data, .{});
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TargetLanguage.fromString" {
    const testing = std.testing;

    try testing.expectEqual(TargetLanguage.klar, TargetLanguage.fromString("klar").?);
    try testing.expectEqual(TargetLanguage.typescript, TargetLanguage.fromString("ts").?);
    try testing.expectEqual(TargetLanguage.python, TargetLanguage.fromString("py").?);
    try testing.expect(TargetLanguage.fromString("unknown") == null);
}

test "AIModel.parse" {
    const testing = std.testing;

    const model1 = AIModel.parse("anthropic/claude-3-opus").?;
    try testing.expectEqualStrings("anthropic", model1.provider);
    try testing.expectEqualStrings("claude-3-opus", model1.name);
    try testing.expect(model1.version == null);

    const model2 = AIModel.parse("openai/gpt-4:turbo").?;
    try testing.expectEqualStrings("openai", model2.provider);
    try testing.expectEqualStrings("gpt-4", model2.name);
    try testing.expectEqualStrings("turbo", model2.version.?);

    try testing.expect(AIModel.parse("invalid") == null);
}

test "GenerationRequest.RequestId.generate" {
    const id1 = GenerationRequest.RequestId.generate();
    const id2 = GenerationRequest.RequestId.generate();

    // Should be different
    const testing = std.testing;
    try testing.expect(!std.mem.eql(u8, &id1.bytes, &id2.bytes));
}

test "LanguageOptions.default" {
    const klar_opts = LanguageOptions.default(.klar);
    const testing = std.testing;

    try testing.expectEqual(TargetLanguage.klar, klar_opts.language);
    try testing.expect(klar_opts.emit_contracts);

    const python_opts = LanguageOptions.default(.python);
    try testing.expect(!python_opts.emit_contracts);
}
