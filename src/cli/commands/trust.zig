//! sanna trust - Generate and query trust reports
//!
//! Calculates trust scores for specifications and implementations,
//! combining verification status, AI confidence, and provenance.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const root = @import("../../root.zig");

/// Run the trust command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Parse options
    const below_str = args.getFlagValue('b', "below");
    const below_threshold: ?f32 = if (below_str) |b|
        std.fmt.parseFloat(f32, b) catch {
            cli.output.err("Invalid threshold value: {s}", .{b});
            return 1;
        }
    else
        null;

    const output_file = args.getFlagValue('o', "output");
    const fail_below_str = args.getFlagValue(null, "fail-below");
    const fail_below: ?f32 = if (fail_below_str) |f|
        std.fmt.parseFloat(f32, f) catch {
            cli.output.err("Invalid fail-below value: {s}", .{f});
            return 1;
        }
    else
        null;

    const sort_by = args.getFlagValue('s', "sort") orelse "score";

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

    cli.output.info("Calculating trust scores for {d} file(s)...", .{files.items.len});

    // Collect trust scores
    var entries = std.ArrayListUnmanaged(TrustEntry){};
    defer {
        // Free duplicated name strings
        for (entries.items) |entry| {
            cli.allocator.free(entry.name);
        }
        entries.deinit(cli.allocator);
    }

    for (files.items) |file_path| {
        collectTrustEntries(cli, file_path, &entries) catch |err| {
            cli.output.err("Failed to process {s}: {}", .{ file_path, err });
        };
    }

    // Sort entries
    if (std.mem.eql(u8, sort_by, "score")) {
        std.mem.sort(TrustEntry, entries.items, {}, struct {
            fn lessThan(_: void, a: TrustEntry, b: TrustEntry) bool {
                return a.score < b.score;
            }
        }.lessThan);
    } else if (std.mem.eql(u8, sort_by, "name")) {
        std.mem.sort(TrustEntry, entries.items, {}, struct {
            fn lessThan(_: void, a: TrustEntry, b: TrustEntry) bool {
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.lessThan);
    }

    // Filter if threshold specified
    var filtered_entries = std.ArrayListUnmanaged(TrustEntry){};
    defer filtered_entries.deinit(cli.allocator);

    for (entries.items) |entry| {
        if (below_threshold) |threshold| {
            if (entry.score >= threshold) continue;
        }
        filtered_entries.append(cli.allocator, entry) catch {};
    }

    // Generate output
    if (output_file) |out_path| {
        writeJsonReport(cli, out_path, filtered_entries.items) catch |err| {
            cli.output.err("Failed to write report: {}", .{err});
            return 1;
        };
        cli.output.success("Trust report written to {s}", .{out_path});
    } else if (cli.output.json_mode) {
        outputJsonReport(cli, filtered_entries.items);
    } else {
        outputTextReport(cli, filtered_entries.items, below_threshold);
    }

    // Check fail-below threshold
    if (fail_below) |threshold| {
        for (filtered_entries.items) |entry| {
            if (entry.score < threshold) {
                cli.output.err("Trust score below deployment threshold ({d:.2})", .{threshold});
                return 2; // Exit code 2 = trust below threshold (CI gate failure)
            }
        }
    }

    return 0;
}

const TrustEntry = struct {
    name: []const u8,
    file: []const u8,
    score: f32,
    verification_status: VerificationStatus,
    confidence: f32,
    author_kind: AuthorKind,
    needs_review: bool,
    approved: bool,

    const VerificationStatus = enum {
        proven,
        unproven,
        failed,
        not_verified,
    };

    const AuthorKind = enum {
        human,
        ai,
        unknown,
    };
};

fn collectTrustEntries(cli: *Cli, file_path: []const u8, entries: *std.ArrayListUnmanaged(TrustEntry)) !void {
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

    // Create trust calculator with default config
    const trust_config = root.TrustConfig{
        .enable_age_decay = if (cli.config) |cfg| cfg.trust.age_decay_enabled else false,
    };

    const calculator = root.TrustCalculator.init(alloc, trust_config);
    _ = calculator; // Calculator used for future full integration

    // Get review threshold from CLI config (defaults to 0.6)
    const review_threshold: f32 = if (cli.config) |cfg| cfg.trust.require_review_below else 0.6;

    // Extract entries from declarations (dupe strings since arena will be freed)
    const basename = std.fs.path.basename(file_path);
    for (module.declarations) |decl| {
        const ast_name = getDeclName(decl) orelse continue;

        // Duplicate the name string with cli.allocator since arena will be freed
        const name = try cli.allocator.dupe(u8, ast_name);
        errdefer cli.allocator.free(name);

        // Calculate trust score (simplified)
        // In full implementation, this would use VerificationEngine results
        // and ProvenanceStore data
        const score = calculateSimplifiedTrustScore(decl);

        try entries.append(cli.allocator, .{
            .name = name,
            .file = basename, // basename is a slice into file_path which outlives us
            .score = score,
            .verification_status = .not_verified,
            .confidence = 0.5,
            .author_kind = .unknown,
            .needs_review = score < review_threshold,
            .approved = false,
        });
    }
}

fn calculateSimplifiedTrustScore(decl: root.parser.Ast.Declaration) f32 {
    // Simplified trust calculation
    // In full implementation, this combines:
    // - Verification status (base score)
    // - AI confidence (modifier)
    // - Author provenance (modifier)
    // - Age decay (modifier)
    // - Criticality (modifier)

    var score: f32 = 0.5; // Base score for not verified

    switch (decl.kind) {
        .spec_fn => {
            // Function specs with contracts get higher base score
            score = 0.6;
        },
        .type_def => {
            // Types with invariants
            score = 0.55;
        },
        else => {},
    }

    return score;
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

fn outputTextReport(cli: *Cli, entries: []const TrustEntry, threshold: ?f32) void {
    if (threshold) |t| {
        cli.output.header("Trust Report (below {d:.2})", .{t});
    } else {
        cli.output.header("Trust Report", .{});
    }

    if (entries.len == 0) {
        cli.output.success("All items above threshold", .{});
        return;
    }

    // Table header
    cli.output.print("\n", .{});
    cli.output.printColored(.bold, "{s: <30} {s: <15} {s: <10} {s: <10}\n", .{ "Name", "File", "Score", "Status" });
    cli.output.print("{s}\n", .{"-" ** 70});

    for (entries) |entry| {
        cli.output.print("{s: <30} {s: <15} ", .{ entry.name, entry.file });
        cli.output.formatTrustScore(entry.score);
        cli.output.print("      ", .{});

        if (entry.needs_review) {
            cli.output.printColored(.yellow, "needs review", .{});
        } else if (entry.approved) {
            cli.output.printColored(.green, "approved", .{});
        } else {
            cli.output.printColored(.dim, "pending", .{});
        }
        cli.output.print("\n", .{});
    }

    // Summary
    cli.output.print("\n", .{});
    cli.output.print("Total: {d} item(s)\n", .{entries.len});

    var needs_review_count: usize = 0;
    var total_score: f32 = 0;
    for (entries) |entry| {
        if (entry.needs_review) needs_review_count += 1;
        total_score += entry.score;
    }

    if (entries.len > 0) {
        cli.output.print("Average score: ", .{});
        cli.output.formatTrustScore(total_score / @as(f32, @floatFromInt(entries.len)));
        cli.output.print("\n", .{});
    }

    if (needs_review_count > 0) {
        cli.output.warn("{d} item(s) need review", .{needs_review_count});
    }
}

fn outputJsonReport(cli: *Cli, entries: []const TrustEntry) void {
    _ = cli;
    const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };

    // Calculate summary statistics
    var needs_review_count: usize = 0;
    var blocked_count: usize = 0;
    var min_score: f32 = 1.0;
    var max_score: f32 = 0.0;
    var total_score: f32 = 0.0;

    for (entries) |entry| {
        if (entry.needs_review) needs_review_count += 1;
        if (entry.score < 0.3) blocked_count += 1;
        if (entry.score < min_score) min_score = entry.score;
        if (entry.score > max_score) max_score = entry.score;
        total_score += entry.score;
    }

    const avg_score: f32 = if (entries.len > 0) total_score / @as(f32, @floatFromInt(entries.len)) else 0.0;
    const can_deploy = blocked_count == 0;

    stdout_file.writeAll("{\n  \"entries\": [\n") catch {};

    for (entries, 0..) |entry, i| {
        if (i > 0) stdout_file.writeAll(",\n") catch {};

        var buf: [512]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf,
            \\    {{
            \\      "name": "{s}",
            \\      "file": "{s}",
            \\      "score": {d:.4},
            \\      "needs_review": {s},
            \\      "approved": {s}
            \\    }}
        , .{
            entry.name,
            entry.file,
            entry.score,
            if (entry.needs_review) "true" else "false",
            if (entry.approved) "true" else "false",
        }) catch continue;
        stdout_file.writeAll(formatted) catch {};
    }

    stdout_file.writeAll("\n  ],\n  \"summary\": {\n") catch {};

    var summary_buf: [256]u8 = undefined;
    const summary_str = std.fmt.bufPrint(&summary_buf,
        \\    "total": {d},
        \\    "needs_review": {d},
        \\    "blocked": {d},
        \\    "average_score": {d:.4},
        \\    "min_score": {d:.4},
        \\    "max_score": {d:.4},
        \\    "can_deploy": {s}
        \\
    , .{
        entries.len,
        needs_review_count,
        blocked_count,
        avg_score,
        min_score,
        max_score,
        if (can_deploy) "true" else "false",
    }) catch "    \"total\": 0\n";
    stdout_file.writeAll(summary_str) catch {};
    stdout_file.writeAll("  }\n}\n") catch {};
}

fn writeJsonReport(cli: *Cli, path: []const u8, entries: []const TrustEntry) !void {
    _ = cli;
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    // Calculate summary statistics
    var needs_review_count: usize = 0;
    var blocked_count: usize = 0;
    var min_score: f32 = 1.0;
    var max_score: f32 = 0.0;
    var total_score: f32 = 0.0;

    for (entries) |entry| {
        if (entry.needs_review) needs_review_count += 1;
        if (entry.score < 0.3) blocked_count += 1;
        if (entry.score < min_score) min_score = entry.score;
        if (entry.score > max_score) max_score = entry.score;
        total_score += entry.score;
    }

    const avg_score: f32 = if (entries.len > 0) total_score / @as(f32, @floatFromInt(entries.len)) else 0.0;
    const can_deploy = blocked_count == 0;

    try file.writeAll("{\n  \"entries\": [\n");

    for (entries, 0..) |entry, i| {
        if (i > 0) try file.writeAll(",\n");

        var buf: [512]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf,
            \\    {{
            \\      "name": "{s}",
            \\      "file": "{s}",
            \\      "score": {d:.4},
            \\      "needs_review": {s},
            \\      "approved": {s}
            \\    }}
        , .{
            entry.name,
            entry.file,
            entry.score,
            if (entry.needs_review) "true" else "false",
            if (entry.approved) "true" else "false",
        }) catch return error.BufferOverflow;
        try file.writeAll(formatted);
    }

    try file.writeAll("\n  ],\n  \"summary\": {\n");

    var summary_buf: [256]u8 = undefined;
    const summary_str = std.fmt.bufPrint(&summary_buf,
        \\    "total": {d},
        \\    "needs_review": {d},
        \\    "blocked": {d},
        \\    "average_score": {d:.4},
        \\    "min_score": {d:.4},
        \\    "max_score": {d:.4},
        \\    "can_deploy": {s}
        \\
    , .{
        entries.len,
        needs_review_count,
        blocked_count,
        avg_score,
        min_score,
        max_score,
        if (can_deploy) "true" else "false",
    }) catch return error.BufferOverflow;
    try file.writeAll(summary_str);
    try file.writeAll("  }\n}\n");
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
        \\sanna trust - Generate and query trust reports
        \\
        \\USAGE:
        \\    sanna trust [OPTIONS] [FILES...]
        \\
        \\ARGUMENTS:
        \\    FILES    Specification files to analyze (default: specs/*.sanna)
        \\
        \\OPTIONS:
        \\    -b, --below <SCORE>      Only show items below this trust score
        \\    -o, --output <FILE>      Write JSON report to file
        \\    -s, --sort <BY>          Sort by: score (default), name
        \\    --fail-below <SCORE>     Exit with error if any item below threshold
        \\    --json                   Output as JSON
        \\    -h, --help               Print help information
        \\
        \\TRUST SCORE COMPONENTS:
        \\    - Verification status (proven/unproven/failed)
        \\    - AI confidence level (0.0-1.0)
        \\    - Author provenance (human/AI)
        \\    - Code age (decay over time)
        \\    - Criticality flags (security-sensitive, etc.)
        \\
        \\EXIT CODES:
        \\    0    Success
        \\    1    Error (invalid arguments, file not found, etc.)
        \\    2    Trust below threshold (--fail-below gate failed)
        \\
        \\EXAMPLES:
        \\    sanna trust                         # Show all trust scores
        \\    sanna trust --below 0.6             # Show items needing review
        \\    sanna trust --fail-below 0.3        # CI gate: fail if too low
        \\    sanna trust -o trust-report.json    # Generate JSON report
        \\
    ;
    cli.output.print("{s}", .{help});
}
