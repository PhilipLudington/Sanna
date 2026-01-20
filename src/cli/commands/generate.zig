//! sanna generate - Generate code from specifications
//!
//! Generates implementation code from Sanna specifications using
//! AI-powered code generation with provenance tracking.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const root = @import("../../root.zig");

/// Run the generate command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Parse options
    const target_str = args.getFlagValue('t', "target") orelse
        (if (cli.config) |cfg| cfg.generation.target else "klar");

    const model_str = args.getFlagValue('m', "model") orelse
        (if (cli.config) |cfg| cfg.generation.default_model else "anthropic/claude-3-opus");

    const output_path = args.getFlagValue('o', "output");
    const dry_run = args.hasFlag('n', "dry-run");
    const use_mock = args.hasFlag(null, "mock");

    // Get target language
    const target = root.TargetLanguage.fromString(target_str) orelse {
        cli.output.err("Unknown target language: {s}", .{target_str});
        cli.output.print("Supported targets: klar, kira, zig, rust, typescript, python\n", .{});
        return 1;
    };

    // Get specification file
    const spec_file = args.nextPositional() orelse {
        cli.output.err("No specification file provided", .{});
        printHelp(cli);
        return 1;
    };

    // Optional: specific function/type to generate
    const spec_name = args.nextPositional();

    cli.output.info("Generating {s} code from {s}", .{ target.displayName(), spec_file });
    if (spec_name) |name| {
        cli.output.print("  Specification: {s}\n", .{name});
    }
    cli.output.print("  Model: {s}\n", .{model_str});

    if (dry_run) {
        cli.output.warn("Dry run - no code will be generated", .{});
    }

    // Use arena for all parsing allocations - freed after parsing is validated
    var parse_arena = std.heap.ArenaAllocator.init(cli.allocator);
    defer parse_arena.deinit();
    const parse_alloc = parse_arena.allocator();

    // Read and parse specification
    const content = std.fs.cwd().readFileAlloc(parse_alloc, spec_file, 10 * 1024 * 1024) catch |err| {
        cli.output.err("Failed to read {s}: {}", .{ spec_file, err });
        return 1;
    };

    // Parse the specification
    var lexer = root.Lexer.init(parse_alloc, content);

    const tokens = lexer.tokenize() catch |err| {
        cli.output.err("Lexer error: {}", .{err});
        return 1;
    };

    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            cli.output.err("{s}:{d}:{d}: {s}", .{ spec_file, err.location.line, err.location.column, err.message });
        }
        return 1;
    }

    var parser = root.parser.Parser.init(parse_alloc, tokens.items);

    const module = parser.parseModule() catch |err| {
        cli.output.err("Parse error: {}", .{err});
        return 1;
    };
    _ = module;

    if (parser.diagnostics.items.len > 0) {
        for (parser.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                cli.output.err("{s}:{d}:{d}: {s}", .{ spec_file, diag.span.start.line, diag.span.start.column, diag.message });
            }
        }
        return 1;
    }

    // Generate code
    if (dry_run) {
        cli.output.success("Specification parsed successfully (dry run)", .{});
        return 0;
    }

    cli.output.spinner("Generating implementation...", .{});

    var generator: root.MockGenerator = undefined;
    if (use_mock) {
        generator = root.MockGenerator.init(cli.allocator, .{
            .base_confidence = 0.85,
        });
    } else {
        // For real generation, we would use the actual AI model
        // For now, fall back to mock generator with a warning
        cli.output.warn("AI generation not yet implemented, using mock generator", .{});
        generator = root.MockGenerator.init(cli.allocator, .{
            .base_confidence = 0.85,
        });
    }
    defer generator.deinit();

    // Create a generation request
    const model = root.codegen.AIModel.parse(model_str) orelse {
        cli.output.err("Invalid model format: {s}", .{model_str});
        cli.output.print("Expected format: provider/model[:version]\n", .{});
        return 1;
    };

    const language_opts = root.codegen.LanguageOptions.default(target);
    const request = root.GenerationRequest.init(
        .{ .module = .{ .path = &.{}, .include_all = true } },
        language_opts,
        model,
    );

    var response = generator.generate(request) catch |err| {
        cli.output.err("Generation failed: {}", .{err});
        return 1;
    };
    defer response.deinit(cli.allocator);

    cli.output.clearLine();

    if (response.status != .success and response.status != .partial) {
        if (response.error_info) |err_info| {
            cli.output.err("Generation failed: {s}", .{err_info.message});
        } else {
            cli.output.err("Generation failed with status: {s}", .{@tagName(response.status)});
        }
        return 1;
    }

    // Output generated code
    if (response.code) |code| {
        if (output_path) |out_path| {
            // Write to file
            const file = std.fs.cwd().createFile(out_path, .{}) catch |err| {
                cli.output.err("Failed to create {s}: {}", .{ out_path, err });
                return 1;
            };
            defer file.close();

            file.writeAll(code.source) catch |err| {
                cli.output.err("Failed to write to {s}: {}", .{ out_path, err });
                return 1;
            };

            cli.output.success("Generated code written to {s}", .{out_path});
        } else {
            // Print to stdout
            if (cli.output.json_mode) {
                cli.output.writeJson(.{
                    .status = @tagName(response.status),
                    .language = target.displayName(),
                    .confidence = code.confidence.value,
                    .source = code.source,
                    .holes = code.holes.len,
                    .metrics = .{
                        .duration_ms = response.metrics.duration_ms,
                        .prompt_tokens = response.metrics.prompt_tokens,
                        .completion_tokens = response.metrics.completion_tokens,
                    },
                });
            } else {
                cli.output.header("Generated Code ({s})", .{target.displayName()});
                cli.output.print("{s}\n", .{code.source});

                cli.output.print("\n", .{});
                cli.output.success("Generation complete", .{});
                cli.output.print("  Confidence: ", .{});
                cli.output.formatTrustScore(@floatCast(code.confidence.value));
                cli.output.print("\n", .{});
                cli.output.print("  Tokens: {d} prompt, {d} completion\n", .{
                    response.metrics.prompt_tokens,
                    response.metrics.completion_tokens,
                });

                if (code.holes.len > 0) {
                    cli.output.warn("Generated code contains {d} typed hole(s)", .{code.holes.len});
                    for (code.holes) |hole| {
                        cli.output.print("  - {s}: {s}\n", .{ hole.description, hole.expected_type });
                    }
                }
            }
        }
    }

    return 0;
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna generate - Generate code from specifications
        \\
        \\USAGE:
        \\    sanna generate [OPTIONS] <SPEC_FILE> [SPEC_NAME]
        \\
        \\ARGUMENTS:
        \\    SPEC_FILE    Specification file to generate from
        \\    SPEC_NAME    Optional: specific function/type to generate
        \\
        \\OPTIONS:
        \\    -t, --target <LANG>    Target language (default: from config or "klar")
        \\                           Options: klar, kira, zig, rust, typescript, python
        \\    -m, --model <MODEL>    AI model to use (default: from config)
        \\                           Format: provider/model[:version]
        \\    -o, --output <FILE>    Output file (default: stdout)
        \\    -n, --dry-run          Parse only, don't generate code
        \\    --mock                 Use mock generator (for testing)
        \\    --json                 Output as JSON
        \\    -h, --help             Print help information
        \\
        \\EXAMPLES:
        \\    sanna generate specs/auth.sanna
        \\    sanna generate -t zig specs/parser.sanna parse_token
        \\    sanna generate --target rust -o src/auth.rs specs/auth.sanna
        \\    sanna generate --mock specs/example.sanna
        \\
    ;
    cli.output.print("{s}", .{help});
}
