//! sanna review - List items needing review
//!
//! Shows specifications and implementations that need human review
//! based on trust scores, verification status, and AI confidence.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const root = @import("../../root.zig");

/// Run the review command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Parse filter options
    const unproven_only = args.hasFlag('u', "unproven");
    const low_confidence = args.hasFlag('l', "low-confidence");
    const ai_only = args.hasFlag('a', "ai-only");
    const unapproved_only = args.hasFlag(null, "unapproved");
    const limit_str = args.getFlagValue('n', "limit");
    const limit: ?usize = if (limit_str) |l|
        std.fmt.parseInt(usize, l, 10) catch {
            cli.output.err("Invalid limit value: {s}", .{l});
            return 1;
        }
    else
        null;

    // Collect files
    var files = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (files.items) |f| {
            if (std.mem.indexOfScalar(u8, f, '/') != null) {
                cli.allocator.free(f);
            }
        }
        files.deinit(cli.allocator);
    }

    while (args.nextPositional()) |path| {
        files.append(cli.allocator, path) catch {
            cli.output.err("Out of memory", .{});
            return 1;
        };
    }

    if (files.items.len == 0) {
        collectSpecFiles(cli.allocator, cli.cwd, &files) catch |err| {
            cli.output.err("Failed to find spec files: {}", .{err});
            return 1;
        };
    }

    if (files.items.len == 0) {
        cli.output.warn("No specification files found", .{});
        return 0;
    }

    // Collect review items
    var items = std.ArrayListUnmanaged(ReviewItem){};
    defer {
        // Free duplicated name strings
        for (items.items) |item| {
            cli.allocator.free(item.name);
        }
        items.deinit(cli.allocator);
    }

    const review_threshold = if (cli.config) |cfg| cfg.trust.require_review_below else 0.6;

    for (files.items) |file_path| {
        collectReviewItems(cli, file_path, &items, .{
            .unproven_only = unproven_only,
            .low_confidence = low_confidence,
            .ai_only = ai_only,
            .unapproved_only = unapproved_only,
            .review_threshold = review_threshold,
        }) catch |err| {
            cli.output.err("Failed to process {s}: {}", .{ file_path, err });
        };
    }

    // Sort by priority (lowest trust score first)
    std.mem.sort(ReviewItem, items.items, {}, struct {
        fn lessThan(_: void, a: ReviewItem, b: ReviewItem) bool {
            // Priority order: unproven > low confidence > AI-generated > others
            const a_priority = priorityScore(a);
            const b_priority = priorityScore(b);
            if (a_priority != b_priority) return a_priority > b_priority;
            return a.trust_score < b.trust_score;
        }

        fn priorityScore(item: ReviewItem) u8 {
            var score: u8 = 0;
            if (item.verification_status == .unproven or item.verification_status == .failed) score += 4;
            if (item.confidence < 0.5) score += 2;
            if (item.is_ai_generated) score += 1;
            return score;
        }
    }.lessThan);

    // Apply limit
    var display_items = items.items;
    if (limit) |l| {
        if (l < display_items.len) {
            display_items = display_items[0..l];
        }
    }

    // Output results
    if (cli.output.json_mode) {
        outputJsonReview(cli, display_items);
    } else {
        outputTextReview(cli, display_items, items.items.len, limit);
    }

    return if (display_items.len > 0) 1 else 0;
}

const ReviewItem = struct {
    name: []const u8,
    file: []const u8,
    line: u32,
    kind: Kind,
    reason: Reason,
    trust_score: f32,
    confidence: f32,
    verification_status: VerificationStatus,
    is_ai_generated: bool,
    model: ?[]const u8,

    const Kind = enum {
        function_spec,
        type_def,
        interface_spec,
        implementation,
    };

    const Reason = enum {
        low_trust_score,
        unproven,
        low_confidence,
        ai_generated_unapproved,
        security_sensitive,
        recently_modified,
    };

    const VerificationStatus = enum {
        proven,
        unproven,
        failed,
        not_verified,
    };
};

const FilterOptions = struct {
    unproven_only: bool = false,
    low_confidence: bool = false,
    ai_only: bool = false,
    unapproved_only: bool = false,
    review_threshold: f32 = 0.6,
};

fn collectReviewItems(cli: *Cli, file_path: []const u8, items: *std.ArrayListUnmanaged(ReviewItem), opts: FilterOptions) !void {
    // Use arena for all parsing allocations - freed at end of function
    var arena = std.heap.ArenaAllocator.init(cli.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Read and parse file
    const content = try std.fs.cwd().readFileAlloc(alloc, file_path, 10 * 1024 * 1024);

    var lexer = root.Lexer.init(alloc, content);

    const tokens = try lexer.tokenize();

    if (lexer.hasErrors()) return;

    var parser = root.parser.Parser.init(alloc, tokens.items);

    const module = parser.parseModule() catch return;
    if (parser.diagnostics.items.len > 0) return;

    const basename = std.fs.path.basename(file_path);

    for (module.declarations) |decl| {
        const ast_name = getDeclName(decl) orelse continue;

        // Simulate getting metadata (in full implementation, use ProvenanceStore)
        const trust_score: f32 = 0.55; // Simulated
        const confidence: f32 = 0.7;
        const is_ai_generated = false;
        const verification_status = ReviewItem.VerificationStatus.not_verified;

        // Apply filters
        if (opts.unproven_only and verification_status != .unproven and verification_status != .failed) continue;
        if (opts.low_confidence and confidence >= 0.7) continue;
        if (opts.ai_only and !is_ai_generated) continue;
        if (opts.unapproved_only and !is_ai_generated) continue;

        // Check if item needs review
        const needs_review = trust_score < opts.review_threshold or
            verification_status == .unproven or
            verification_status == .failed or
            (is_ai_generated and confidence < 0.8);

        if (!needs_review) continue;

        // Determine reason
        const reason: ReviewItem.Reason = if (verification_status == .unproven or verification_status == .failed)
            .unproven
        else if (confidence < 0.5)
            .low_confidence
        else if (is_ai_generated)
            .ai_generated_unapproved
        else
            .low_trust_score;

        const kind: ReviewItem.Kind = switch (decl.kind) {
            .spec_fn => .function_spec,
            .type_def => .type_def,
            .spec_interface => .interface_spec,
            else => continue,
        };

        // Duplicate name string since arena will be freed
        const name = try cli.allocator.dupe(u8, ast_name);
        errdefer cli.allocator.free(name);

        try items.append(cli.allocator, .{
            .name = name,
            .file = basename, // basename is a slice into file_path which outlives us
            .line = decl.span.start.line,
            .kind = kind,
            .reason = reason,
            .trust_score = trust_score,
            .confidence = confidence,
            .verification_status = verification_status,
            .is_ai_generated = is_ai_generated,
            .model = null,
        });
    }
}

fn outputTextReview(cli: *Cli, items: []const ReviewItem, total: usize, limit: ?usize) void {
    cli.output.header("Items Needing Review", .{});

    if (items.len == 0) {
        cli.output.success("No items need review!", .{});
        return;
    }

    for (items, 0..) |item, i| {
        cli.output.print("\n", .{});
        cli.output.printColored(.bold, "{d}. {s}", .{ i + 1, item.name });
        cli.output.printColored(.dim, " ({s}:{d})\n", .{ item.file, item.line });

        // Show reason
        cli.output.print("   Reason: ", .{});
        switch (item.reason) {
            .unproven => cli.output.printColored(.yellow, "verification unproven\n", .{}),
            .low_confidence => cli.output.printColored(.yellow, "low AI confidence ({d:.0}%)\n", .{item.confidence * 100}),
            .ai_generated_unapproved => cli.output.printColored(.cyan, "AI-generated, needs approval\n", .{}),
            .low_trust_score => cli.output.printColored(.yellow, "trust score below threshold\n", .{}),
            .security_sensitive => cli.output.printColored(.red, "security-sensitive code\n", .{}),
            .recently_modified => cli.output.printColored(.dim, "recently modified\n", .{}),
        }

        // Show details
        cli.output.print("   Trust: ", .{});
        cli.output.formatTrustScore(item.trust_score);
        cli.output.print("  Verification: ", .{});
        switch (item.verification_status) {
            .proven => cli.output.printColored(.green, "proven", .{}),
            .unproven => cli.output.printColored(.yellow, "unproven", .{}),
            .failed => cli.output.printColored(.red, "failed", .{}),
            .not_verified => cli.output.printColored(.dim, "not verified", .{}),
        }
        cli.output.print("\n", .{});

        if (item.is_ai_generated) {
            cli.output.print("   Generated by: ", .{});
            cli.output.printColored(.cyan, "{s}\n", .{item.model orelse "AI"});
        }
    }

    // Summary
    cli.output.print("\n", .{});
    if (limit) |l| {
        if (l < total) {
            cli.output.print("Showing {d} of {d} items (use --limit to see more)\n", .{ items.len, total });
        }
    }
    cli.output.warn("{d} item(s) need review", .{total});

    // Suggested actions
    cli.output.print("\nTo approve an item:\n", .{});
    cli.output.print("  sanna approve <file>::<name> --note \"Reviewed and approved\"\n", .{});
}

fn outputJsonReview(cli: *Cli, items: []const ReviewItem) void {
    _ = cli;
    const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };

    stdout_file.writeAll("{\n  \"items\": [\n") catch {};

    for (items, 0..) |item, i| {
        if (i > 0) stdout_file.writeAll(",\n") catch {};

        var buf: [1024]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf,
            \\    {{
            \\      "name": "{s}",
            \\      "file": "{s}",
            \\      "line": {d},
            \\      "kind": "{s}",
            \\      "reason": "{s}",
            \\      "trust_score": {d:.4},
            \\      "confidence": {d:.4},
            \\      "verification_status": "{s}",
            \\      "is_ai_generated": {s}
            \\    }}
        , .{
            item.name,
            item.file,
            item.line,
            @tagName(item.kind),
            @tagName(item.reason),
            item.trust_score,
            item.confidence,
            @tagName(item.verification_status),
            if (item.is_ai_generated) "true" else "false",
        }) catch continue;
        stdout_file.writeAll(formatted) catch {};
    }

    stdout_file.writeAll("\n  ],\n  \"total\": ") catch {};
    var total_buf: [32]u8 = undefined;
    const total_str = std.fmt.bufPrint(&total_buf, "{d}", .{items.len}) catch "0";
    stdout_file.writeAll(total_str) catch {};
    stdout_file.writeAll("\n}\n") catch {};
}

fn collectSpecFiles(allocator: Allocator, base_path: []const u8, files: *std.ArrayListUnmanaged([]const u8)) !void {
    const specs_path = try std.fs.path.join(allocator, &.{ base_path, "specs" });
    defer allocator.free(specs_path);

    var dir = std.fs.cwd().openDir(specs_path, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".sanna")) {
            const full_path = try std.fs.path.join(allocator, &.{ specs_path, entry.name });
            try files.append(allocator, full_path);
        }
    }
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna review - List items needing review
        \\
        \\USAGE:
        \\    sanna review [OPTIONS] [FILES...]
        \\
        \\ARGUMENTS:
        \\    FILES    Specification files to check (default: specs/*.sanna)
        \\
        \\FILTER OPTIONS:
        \\    -u, --unproven         Only show unproven/failed verifications
        \\    -l, --low-confidence   Only show items with low AI confidence
        \\    -a, --ai-only          Only show AI-generated code
        \\    --unapproved           Only show unapproved AI-generated code
        \\
        \\OUTPUT OPTIONS:
        \\    -n, --limit <N>        Limit output to N items
        \\    --json                 Output as JSON
        \\    -h, --help             Print help information
        \\
        \\REVIEW PRIORITIES (highest to lowest):
        \\    1. Failed/unproven verifications
        \\    2. Low AI confidence (<50%)
        \\    3. AI-generated, unapproved
        \\    4. Trust score below threshold
        \\
        \\EXAMPLES:
        \\    sanna review                     # Show all items needing review
        \\    sanna review --unproven          # Focus on verification issues
        \\    sanna review --ai-only -n 10     # Top 10 AI-generated items
        \\    sanna review --json > queue.json # Export review queue
        \\
    ;
    cli.output.print("{s}", .{help});
}

/// Extract name from a declaration
fn getDeclName(decl: root.parser.Ast.Declaration) ?[]const u8 {
    return switch (decl.kind) {
        .type_def => |td| td.name.name,
        .model_def => |md| md.name.name,
        .spec_fn => |sf| sf.name.name,
        .spec_interface => |si| si.name.name,
        .invariant => null,
        .axiom => |ax| ax.name.name,
        .lemma => |lm| lm.name.name,
    };
}
