//! Mock Generator
//!
//! Provides a stub/mock generator for testing the code generation pipeline
//! without requiring an actual AI backend.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Provenance = @import("../provenance/Provenance.zig");
const types = @import("types.zig");

const GenerationRequest = types.GenerationRequest;
const GenerationResponse = types.GenerationResponse;
const GeneratedCode = types.GeneratedCode;
const TargetLanguage = types.TargetLanguage;
const SpecificationRef = types.SpecificationRef;
const CodeComponent = types.GeneratedCode.CodeComponent;
const TypedHole = types.TypedHole;

/// Mock generator for testing without AI.
pub const MockGenerator = struct {
    allocator: Allocator,
    config: Config,
    responses: std.StringHashMap(GeneratedCode),
    delay_ms: u64,
    seed: u64,
    stats: Stats,

    pub const Config = struct {
        base_confidence: f32 = 0.75,
        hole_probability: f32 = 0.1,
        max_holes: u32 = 3,
        simulate_failures: bool = false,
        failure_probability: f32 = 0.1,
        simulate_delays: bool = false,
        default_delay_ms: u64 = 100,
    };

    pub const Stats = struct {
        requests_handled: u64 = 0,
        successful_generations: u64 = 0,
        failed_generations: u64 = 0,
        partial_generations: u64 = 0,
        total_code_generated: u64 = 0,
        total_holes_generated: u64 = 0,
    };

    pub fn init(allocator: Allocator, config: Config) MockGenerator {
        return .{
            .allocator = allocator,
            .config = config,
            .responses = std.StringHashMap(GeneratedCode).init(allocator),
            .delay_ms = config.default_delay_ms,
            .seed = 12345,
            .stats = .{},
        };
    }

    pub fn deinit(self: *MockGenerator) void {
        self.responses.deinit();
    }

    pub fn registerResponse(
        self: *MockGenerator,
        spec_name: []const u8,
        code: GeneratedCode,
    ) !void {
        try self.responses.put(spec_name, code);
    }

    pub fn generate(self: *MockGenerator, request: GenerationRequest) !GenerationResponse {
        self.stats.requests_handled += 1;

        // Note: Delay simulation removed (std.time.sleep not available in Zig 0.15)

        if (self.config.simulate_failures and self.shouldFail()) {
            self.stats.failed_generations += 1;
            return GenerationResponse.failed(
                request.id,
                .{
                    .code = .model_error,
                    .message = "Simulated failure for testing",
                },
                .{
                    .duration_ms = self.delay_ms,
                    .prompt_tokens = 0,
                    .completion_tokens = 0,
                },
            );
        }

        const spec_name = self.getSpecName(request.spec);
        if (self.responses.get(spec_name)) |predefined| {
            self.stats.successful_generations += 1;
            self.stats.total_code_generated += predefined.source.len;
            return GenerationResponse.success(
                request.id,
                predefined,
                self.createProvenance(request),
                self.createMetrics(),
            );
        }

        const code = try self.generateMockCode(request);
        self.stats.total_code_generated += code.source.len;

        if (code.holes.len > 0) {
            self.stats.partial_generations += 1;
            self.stats.total_holes_generated += code.holes.len;

            return GenerationResponse.partial(
                request.id,
                code,
                types.Checkpoint{
                    .id = types.CheckpointId.generate(),
                    .request_id = request.id,
                    .partial_code = code.source,
                    .remaining = &.{},
                    .state = &.{},
                    .created_at = std.time.timestamp(),
                },
                self.createProvenance(request),
                self.createMetrics(),
            );
        }

        self.stats.successful_generations += 1;
        return GenerationResponse.success(
            request.id,
            code,
            self.createProvenance(request),
            self.createMetrics(),
        );
    }

    fn generateMockCode(self: *MockGenerator, request: GenerationRequest) !GeneratedCode {
        var code_buf = std.ArrayListUnmanaged(u8){};
        errdefer code_buf.deinit(self.allocator);

        var components = std.ArrayListUnmanaged(CodeComponent){};
        errdefer components.deinit(self.allocator);

        var generated_holes = std.ArrayListUnmanaged(TypedHole){};
        errdefer generated_holes.deinit(self.allocator);

        switch (request.spec) {
            .function => |func_ref| {
                const func_code = try self.generateMockFunction(
                    func_ref.name,
                    request.target.language,
                    &generated_holes,
                );
                defer self.allocator.free(func_code);

                const start_offset = code_buf.items.len;
                try code_buf.appendSlice(self.allocator, func_code);
                const end_offset = code_buf.items.len;

                try components.append(self.allocator, .{
                    .kind = .function,
                    .name = func_ref.name,
                    .source = "", // Source will be derived from offsets
                    .start_offset = start_offset,
                    .end_offset = end_offset,
                    .confidence = self.config.base_confidence,
                });
            },
            .interface => |iface_ref| {
                const iface_code = try self.generateMockInterface(iface_ref.name, request.target.language);
                defer self.allocator.free(iface_code);

                const start_offset = code_buf.items.len;
                try code_buf.appendSlice(self.allocator, iface_code);
                const end_offset = code_buf.items.len;

                try components.append(self.allocator, .{
                    .kind = .type_impl,
                    .name = iface_ref.name,
                    .source = "",
                    .start_offset = start_offset,
                    .end_offset = end_offset,
                    .confidence = self.config.base_confidence,
                });
            },
            .type_def => |type_ref| {
                const type_code = try self.generateMockType(type_ref.name, request.target.language);
                defer self.allocator.free(type_code);

                const start_offset = code_buf.items.len;
                try code_buf.appendSlice(self.allocator, type_code);
                const end_offset = code_buf.items.len;

                try components.append(self.allocator, .{
                    .kind = .type_impl,
                    .name = type_ref.name,
                    .source = "",
                    .start_offset = start_offset,
                    .end_offset = end_offset,
                    .confidence = self.config.base_confidence,
                });
            },
            .module => {
                const mod_code = try self.generateMockModule(request.target.language);
                defer self.allocator.free(mod_code);
                try code_buf.appendSlice(self.allocator, mod_code);
            },
        }

        return GeneratedCode{
            .source = try code_buf.toOwnedSlice(self.allocator),
            .language = request.target.language,
            .components = try components.toOwnedSlice(self.allocator),
            .holes = try generated_holes.toOwnedSlice(self.allocator),
            .annotations = &.{},
            .confidence = Provenance.Confidence.init(self.config.base_confidence),
        };
    }

    fn generateMockFunction(
        self: *MockGenerator,
        name: []const u8,
        lang: TargetLanguage,
        generated_holes: *std.ArrayListUnmanaged(TypedHole),
    ) ![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        const should_add_hole = self.shouldAddHole();

        switch (lang) {
            .zig => {
                try buf.appendSlice(self.allocator, "pub fn ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, "(param: anytype) anytype {\n");
                if (should_add_hole) {
                    try buf.appendSlice(self.allocator, "    @compileError(\"HOLE: implementation needed\");\n");
                    try generated_holes.append(self.allocator, .{
                        .id = 1,
                        .description = "implementation needed",
                        .expected_type = "anytype",
                        .location = .{ .line = 2, .column = 5, .length = 50 },
                        .reason = .explicit_hole,
                    });
                } else {
                    try buf.appendSlice(self.allocator, "    return param;\n");
                }
                try buf.appendSlice(self.allocator, "}\n");
            },
            .rust => {
                try buf.appendSlice(self.allocator, "pub fn ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, "<T>(param: T) -> T {\n");
                if (should_add_hole) {
                    try buf.appendSlice(self.allocator, "    todo!()\n");
                    try generated_holes.append(self.allocator, .{
                        .id = 1,
                        .description = "implementation needed",
                        .expected_type = "T",
                        .location = .{ .line = 2, .column = 5, .length = 7 },
                        .reason = .explicit_hole,
                    });
                } else {
                    try buf.appendSlice(self.allocator, "    param\n");
                }
                try buf.appendSlice(self.allocator, "}\n");
            },
            .typescript => {
                try buf.appendSlice(self.allocator, "export function ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, "<T>(param: T): T {\n");
                if (should_add_hole) {
                    try buf.appendSlice(self.allocator, "    throw new Error('Not implemented');\n");
                    try generated_holes.append(self.allocator, .{
                        .id = 1,
                        .description = "implementation needed",
                        .expected_type = "T",
                        .location = .{ .line = 2, .column = 5, .length = 40 },
                        .reason = .explicit_hole,
                    });
                } else {
                    try buf.appendSlice(self.allocator, "    return param;\n");
                }
                try buf.appendSlice(self.allocator, "}\n");
            },
            .python => {
                try buf.appendSlice(self.allocator, "def ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, "(param):\n");
                if (should_add_hole) {
                    try buf.appendSlice(self.allocator, "    raise NotImplementedError()\n");
                    try generated_holes.append(self.allocator, .{
                        .id = 1,
                        .description = "implementation needed",
                        .expected_type = "Any",
                        .location = .{ .line = 2, .column = 5, .length = 30 },
                        .reason = .explicit_hole,
                    });
                } else {
                    try buf.appendSlice(self.allocator, "    return param\n");
                }
            },
            .klar, .kira => {
                try buf.appendSlice(self.allocator, "fn ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, "[T](param: T) -> T {\n");
                if (should_add_hole) {
                    try buf.appendSlice(self.allocator, "    ???\n");
                    try generated_holes.append(self.allocator, .{
                        .id = 1,
                        .description = "implementation needed",
                        .expected_type = "T",
                        .location = .{ .line = 2, .column = 5, .length = 3 },
                        .reason = .explicit_hole,
                    });
                } else {
                    try buf.appendSlice(self.allocator, "    param\n");
                }
                try buf.appendSlice(self.allocator, "}\n");
            },
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn generateMockInterface(self: *MockGenerator, name: []const u8, lang: TargetLanguage) ![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        switch (lang) {
            .zig => {
                try buf.appendSlice(self.allocator, "pub const ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " = struct {\n    data: []const u8,\n};\n");
            },
            .rust => {
                try buf.appendSlice(self.allocator, "pub struct ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " {\n    data: Vec<u8>,\n}\n");
            },
            .typescript => {
                try buf.appendSlice(self.allocator, "export interface ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " {\n    data: Uint8Array;\n}\n");
            },
            .python => {
                try buf.appendSlice(self.allocator, "class ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, ":\n    def __init__(self, data: bytes):\n        self.data = data\n");
            },
            .klar, .kira => {
                try buf.appendSlice(self.allocator, "type ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " = {\n    data: Bytes\n}\n");
            },
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn generateMockType(self: *MockGenerator, name: []const u8, lang: TargetLanguage) ![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        switch (lang) {
            .zig => {
                try buf.appendSlice(self.allocator, "pub const ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " = struct {\n    value: i32,\n};\n");
            },
            .rust => {
                try buf.appendSlice(self.allocator, "pub struct ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " {\n    pub value: i32,\n}\n");
            },
            .typescript => {
                try buf.appendSlice(self.allocator, "export interface ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " {\n    value: number;\n}\n");
            },
            .python => {
                try buf.appendSlice(self.allocator, "@dataclass\nclass ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, ":\n    value: int\n");
            },
            .klar, .kira => {
                try buf.appendSlice(self.allocator, "type ");
                try buf.appendSlice(self.allocator, name);
                try buf.appendSlice(self.allocator, " = {\n    value: Int\n}\n");
            },
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn generateMockModule(self: *MockGenerator, lang: TargetLanguage) ![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        errdefer buf.deinit(self.allocator);

        switch (lang) {
            .zig => try buf.appendSlice(self.allocator, "//! Mock module\n\npub fn init() void {}\n"),
            .rust => try buf.appendSlice(self.allocator, "//! Mock module\n\npub fn init() {}\n"),
            .typescript => try buf.appendSlice(self.allocator, "// Mock module\n\nexport function init(): void {}\n"),
            .python => try buf.appendSlice(self.allocator, "\"\"\"Mock module.\"\"\"\n\ndef init() -> None:\n    pass\n"),
            .klar, .kira => try buf.appendSlice(self.allocator, "// Mock module\n\npub fn init() -> () {}\n"),
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn getSpecName(_: *MockGenerator, spec: SpecificationRef) []const u8 {
        return switch (spec) {
            .function => |f| f.name,
            .interface => |i| i.name,
            .type_def => |t| t.name,
            .module => "module",
        };
    }

    fn createProvenance(self: *MockGenerator, request: GenerationRequest) Provenance.ProvenanceMetadata {
        _ = request;
        var metadata = Provenance.ProvenanceMetadata.init(self.allocator);
        metadata.confidence = Provenance.Confidence.init(self.config.base_confidence);
        return metadata;
    }

    fn createMetrics(self: *MockGenerator) GenerationResponse.Metrics {
        return .{
            .duration_ms = self.delay_ms,
            .prompt_tokens = 100,
            .completion_tokens = 200,
            .retry_count = 0,
            .cache_hit = false,
        };
    }

    fn shouldAddHole(self: *MockGenerator) bool {
        self.seed = self.seed *% 1103515245 +% 12345;
        const random_val = @as(f32, @floatFromInt(self.seed % 1000)) / 1000.0;
        return random_val < self.config.hole_probability;
    }

    fn shouldFail(self: *MockGenerator) bool {
        self.seed = self.seed *% 1103515245 +% 12345;
        const random_val = @as(f32, @floatFromInt(self.seed % 1000)) / 1000.0;
        return random_val < self.config.failure_probability;
    }

    pub fn getStats(self: *MockGenerator) Stats {
        return self.stats;
    }

    pub fn resetStats(self: *MockGenerator) void {
        self.stats = .{};
    }
};

/// Factory for creating generators.
pub const GeneratorFactory = struct {
    pub fn createMock(allocator: Allocator) MockGenerator {
        return MockGenerator.init(allocator, .{});
    }

    pub fn createMockWithConfig(allocator: Allocator, config: MockGenerator.Config) MockGenerator {
        return MockGenerator.init(allocator, config);
    }

    pub fn createTestingMock(allocator: Allocator) MockGenerator {
        return MockGenerator.init(allocator, .{
            .base_confidence = 0.9,
            .hole_probability = 0.0,
            .simulate_failures = false,
            .simulate_delays = false,
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "MockGenerator basic generation" {
    const testing = std.testing;
    var gen = MockGenerator.init(testing.allocator, .{ .hole_probability = 0.0 });
    defer gen.deinit();

    const request = GenerationRequest.init(
        .{ .function = .{ .module_path = &.{}, .name = "test_func" } },
        types.LanguageOptions.default(.zig),
        .{ .provider = "mock", .name = "test" },
    );

    var response = try gen.generate(request);
    defer response.deinit(testing.allocator);

    try testing.expectEqual(GenerationResponse.Status.success, response.status);
    try testing.expect(response.code != null);
    try testing.expect(std.mem.indexOf(u8, response.code.?.source, "test_func") != null);
}

test "MockGenerator with holes" {
    const testing = std.testing;
    var gen = MockGenerator.init(testing.allocator, .{ .hole_probability = 1.0 });
    defer gen.deinit();

    const request = GenerationRequest.init(
        .{ .function = .{ .module_path = &.{}, .name = "complex_func" } },
        types.LanguageOptions.default(.rust),
        .{ .provider = "mock", .name = "test" },
    );

    var response = try gen.generate(request);
    defer response.deinit(testing.allocator);

    try testing.expectEqual(GenerationResponse.Status.partial, response.status);
    try testing.expect(response.code != null);
    try testing.expect(response.code.?.holes.len > 0);
}

test "MockGenerator statistics" {
    const testing = std.testing;
    var gen = MockGenerator.init(testing.allocator, .{
        .hole_probability = 0.0,
        .simulate_failures = false,
    });
    defer gen.deinit();

    const request = GenerationRequest.init(
        .{ .function = .{ .module_path = &.{}, .name = "func" } },
        types.LanguageOptions.default(.zig),
        .{ .provider = "mock", .name = "test" },
    );

    var r1 = try gen.generate(request);
    defer r1.deinit(testing.allocator);
    var r2 = try gen.generate(request);
    defer r2.deinit(testing.allocator);
    var r3 = try gen.generate(request);
    defer r3.deinit(testing.allocator);

    const stats = gen.getStats();
    try testing.expectEqual(@as(u64, 3), stats.requests_handled);
    try testing.expectEqual(@as(u64, 3), stats.successful_generations);
}

test "GeneratorFactory creates generators" {
    const testing = std.testing;

    var mock = GeneratorFactory.createMock(testing.allocator);
    defer mock.deinit();

    var test_mock = GeneratorFactory.createTestingMock(testing.allocator);
    defer test_mock.deinit();

    try testing.expectEqual(@as(f32, 0.9), test_mock.config.base_confidence);
}
