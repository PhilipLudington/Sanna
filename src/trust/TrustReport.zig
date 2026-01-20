//! Trust Report Generation
//!
//! This module generates trust reports from verification results and provenance data.
//! Reports include per-function trust scores, review queues, and filtering capabilities.
//!
//! ## Features
//!
//! - Per-function trust score calculation
//! - Review queue sorted by trust score (lowest first)
//! - Filtering by status, threshold, and flags
//! - JSON output format
//! - Trust threshold evaluation (auto-approve, require-review, block-deployment)

const std = @import("std");
const Allocator = std.mem.Allocator;
const TrustScore = @import("TrustScore.zig");
const TrustCalculator = TrustScore.TrustCalculator;
const TrustConfig = TrustScore.TrustConfig;
const CriticalityLevel = TrustScore.CriticalityLevel;
const VerificationResult = @import("../verify/VerificationResult.zig").VerificationResult;
const VerificationReport = @import("../verify/VerificationResult.zig").VerificationReport;
const VerificationStatus = @import("../verify/ProofObligation.zig").VerificationStatus;
const Provenance = @import("../provenance/Provenance.zig");
const ProvenanceMetadata = Provenance.ProvenanceMetadata;
const ProvenanceStore = @import("../provenance/ProvenanceStore.zig").ProvenanceStore;

// ============================================================================
// Trust Thresholds
// ============================================================================

/// Trust thresholds for deployment decisions
pub const TrustThresholds = struct {
    /// Minimum trust for auto-approval (no review needed)
    auto_approve: f64 = 0.95,
    /// Trust below this requires review
    require_review: f64 = 0.85,
    /// Trust below this blocks deployment
    block_deployment: f64 = 0.5,

    /// Get the action to take for a given trust score
    pub fn getAction(self: TrustThresholds, score: f64) ThresholdAction {
        if (score >= self.auto_approve) return .auto_approve;
        if (score >= self.require_review) return .allow;
        if (score >= self.block_deployment) return .require_review;
        return .block;
    }
};

/// Action determined by trust thresholds
pub const ThresholdAction = enum {
    /// Automatically approved, no review needed
    auto_approve,
    /// Allowed, review optional
    allow,
    /// Must be reviewed before deployment
    require_review,
    /// Blocked from deployment
    block,

    pub fn toString(self: ThresholdAction) []const u8 {
        return switch (self) {
            .auto_approve => "auto_approve",
            .allow => "allow",
            .require_review => "require_review",
            .block => "block",
        };
    }
};

// ============================================================================
// Filter Options
// ============================================================================

/// Filter options for trust reports
pub const FilterOptions = struct {
    /// Include only unproven specifications
    unproven_only: bool = false,
    /// Include only failed specifications
    failed_only: bool = false,
    /// Include only specs below this trust threshold
    below_threshold: ?f64 = null,
    /// Include only specs with this status
    status_filter: ?VerificationStatus = null,
    /// Include only AI-generated code
    ai_only: bool = false,
    /// Include only code needing review
    needs_review_only: bool = false,
    /// Exclude trusted/admitted specs
    exclude_trusted: bool = false,
    /// Maximum number of results (0 = unlimited)
    max_results: usize = 0,
};

// ============================================================================
// Function Trust Entry
// ============================================================================

/// Trust information for a single function/specification
pub const FunctionTrustEntry = struct {
    allocator: Allocator,
    /// Fully qualified name
    name: []const u8,
    /// Trust score
    score: TrustScore.TrustScore,
    /// Verification status
    verification_status: VerificationStatus,
    /// Threshold action
    action: ThresholdAction,
    /// Verification time in milliseconds
    verification_time_ms: u64,
    /// Whether result was cached
    from_cache: bool,
    /// Number of failed obligations
    failed_obligations: usize,
    /// Source file (if available)
    source_file: ?[]const u8,
    /// Line number (if available)
    line: ?u32,

    pub fn deinit(self: *FunctionTrustEntry) void {
        self.allocator.free(self.name);
        if (self.source_file) |sf| self.allocator.free(sf);
    }

    /// Compare by trust score (ascending - lowest first for review queue)
    pub fn compareByScore(context: void, a: FunctionTrustEntry, b: FunctionTrustEntry) bool {
        _ = context;
        return a.score.value < b.score.value;
    }

    /// Compare by trust score descending (highest first)
    pub fn compareByScoreDesc(context: void, a: FunctionTrustEntry, b: FunctionTrustEntry) bool {
        _ = context;
        return a.score.value > b.score.value;
    }

    /// Compare by name
    pub fn compareByName(context: void, a: FunctionTrustEntry, b: FunctionTrustEntry) bool {
        _ = context;
        return std.mem.lessThan(u8, a.name, b.name);
    }
};

// ============================================================================
// Trust Report
// ============================================================================

/// A complete trust report
pub const TrustReport = struct {
    allocator: Allocator,
    /// Report name/title
    name: []const u8,
    /// All function entries
    entries: std.ArrayListUnmanaged(FunctionTrustEntry),
    /// Summary statistics
    summary: Summary,
    /// Thresholds used
    thresholds: TrustThresholds,
    /// Generation timestamp
    generated_at: i64,

    /// Summary statistics for the report
    pub const Summary = struct {
        /// Total number of specifications
        total: usize = 0,
        /// Number auto-approved
        auto_approved: usize = 0,
        /// Number allowed
        allowed: usize = 0,
        /// Number requiring review
        require_review: usize = 0,
        /// Number blocked
        blocked: usize = 0,
        /// Average trust score
        average_trust: f64 = 0.0,
        /// Minimum trust score
        min_trust: f64 = 1.0,
        /// Maximum trust score
        max_trust: f64 = 0.0,
        /// Total verification time
        total_time_ms: u64 = 0,
    };

    pub fn init(allocator: Allocator, name: []const u8, thresholds: TrustThresholds) TrustReport {
        return .{
            .allocator = allocator,
            .name = name,
            .entries = .{},
            .summary = .{},
            .thresholds = thresholds,
            .generated_at = std.time.milliTimestamp(),
        };
    }

    pub fn deinit(self: *TrustReport) void {
        for (self.entries.items) |*entry| {
            entry.deinit();
        }
        self.entries.deinit(self.allocator);
    }

    /// Add an entry and update summary
    pub fn addEntry(self: *TrustReport, entry: FunctionTrustEntry) !void {
        try self.entries.append(self.allocator, entry);

        // Update summary
        self.summary.total += 1;
        switch (entry.action) {
            .auto_approve => self.summary.auto_approved += 1,
            .allow => self.summary.allowed += 1,
            .require_review => self.summary.require_review += 1,
            .block => self.summary.blocked += 1,
        }

        self.summary.min_trust = @min(self.summary.min_trust, entry.score.value);
        self.summary.max_trust = @max(self.summary.max_trust, entry.score.value);
        self.summary.total_time_ms += entry.verification_time_ms;

        // Recalculate average
        const count: f64 = @floatFromInt(self.summary.total);
        const old_avg = self.summary.average_trust;
        self.summary.average_trust = old_avg + (entry.score.value - old_avg) / count;
    }

    /// Sort entries by trust score (lowest first - for review queue)
    pub fn sortByScore(self: *TrustReport) void {
        std.mem.sort(FunctionTrustEntry, self.entries.items, {}, FunctionTrustEntry.compareByScore);
    }

    /// Sort entries by trust score descending
    pub fn sortByScoreDesc(self: *TrustReport) void {
        std.mem.sort(FunctionTrustEntry, self.entries.items, {}, FunctionTrustEntry.compareByScoreDesc);
    }

    /// Sort entries by name
    pub fn sortByName(self: *TrustReport) void {
        std.mem.sort(FunctionTrustEntry, self.entries.items, {}, FunctionTrustEntry.compareByName);
    }

    /// Get entries requiring review
    pub fn getReviewQueue(self: *const TrustReport, allocator: Allocator) ![]const FunctionTrustEntry {
        var queue = std.ArrayListUnmanaged(FunctionTrustEntry){};
        errdefer queue.deinit(allocator);

        for (self.entries.items) |entry| {
            if (entry.action == .require_review or entry.action == .block) {
                try queue.append(allocator, entry);
            }
        }

        // Sort by score (lowest first)
        std.mem.sort(FunctionTrustEntry, queue.items, {}, FunctionTrustEntry.compareByScore);

        return queue.toOwnedSlice(allocator);
    }

    /// Check if all entries pass deployment threshold
    pub fn canDeploy(self: *const TrustReport) bool {
        return self.summary.blocked == 0;
    }

    /// Check if all entries are auto-approved
    pub fn allAutoApproved(self: *const TrustReport) bool {
        return self.summary.auto_approved == self.summary.total;
    }
};

// ============================================================================
// Trust Report Generator
// ============================================================================

/// Generator for trust reports
pub const TrustReportGenerator = struct {
    allocator: Allocator,
    calculator: TrustCalculator,
    thresholds: TrustThresholds,
    provenance_store: ?*const ProvenanceStore,
    /// Criticality mapping (spec name -> criticality level)
    criticality_map: std.StringHashMapUnmanaged(CriticalityLevel),

    pub fn init(
        allocator: Allocator,
        config: TrustConfig,
        thresholds: TrustThresholds,
        provenance_store: ?*const ProvenanceStore,
    ) TrustReportGenerator {
        return .{
            .allocator = allocator,
            .calculator = TrustCalculator.init(allocator, config),
            .thresholds = thresholds,
            .provenance_store = provenance_store,
            .criticality_map = .{},
        };
    }

    pub fn deinit(self: *TrustReportGenerator) void {
        self.criticality_map.deinit(self.allocator);
    }

    /// Set criticality level for a specification
    pub fn setCriticality(self: *TrustReportGenerator, spec_name: []const u8, level: CriticalityLevel) !void {
        try self.criticality_map.put(self.allocator, spec_name, level);
    }

    /// Get criticality level for a specification
    pub fn getCriticality(self: *const TrustReportGenerator, spec_name: []const u8) CriticalityLevel {
        return self.criticality_map.get(spec_name) orelse .normal;
    }

    /// Generate a trust report from verification results
    pub fn generate(
        self: *const TrustReportGenerator,
        verification_report: *const VerificationReport,
        filter: FilterOptions,
    ) !TrustReport {
        var report = TrustReport.init(
            self.allocator,
            verification_report.name,
            self.thresholds,
        );
        errdefer report.deinit();

        var count: usize = 0;

        for (verification_report.results.items) |result| {
            // Get provenance if available
            const provenance = self.getProvenance(result.spec_name);

            // Get criticality
            const criticality = self.getCriticality(result.spec_name);

            // Calculate trust score
            const score = self.calculator.calculate(&result, provenance, criticality);

            // Apply filters
            if (!self.passesFilter(&result, provenance, &score, filter)) {
                continue;
            }

            // Check max results
            if (filter.max_results > 0 and count >= filter.max_results) {
                break;
            }

            // Create entry
            const entry = try self.createEntry(&result, &score, criticality);
            try report.addEntry(entry);
            count += 1;
        }

        return report;
    }

    /// Generate a trust report for a single verification result
    pub fn generateForResult(
        self: *const TrustReportGenerator,
        result: *const VerificationResult,
    ) FunctionTrustEntry {
        const provenance = self.getProvenance(result.spec_name);
        const criticality = self.getCriticality(result.spec_name);
        const score = self.calculator.calculate(result, provenance, criticality);

        return self.createEntry(result, &score, criticality) catch unreachable;
    }

    fn getProvenance(self: *const TrustReportGenerator, spec_name: []const u8) ?*const ProvenanceMetadata {
        if (self.provenance_store) |store| {
            return store.lookup(spec_name);
        }
        return null;
    }

    fn passesFilter(
        self: *const TrustReportGenerator,
        result: *const VerificationResult,
        provenance: ?*const ProvenanceMetadata,
        score: *const TrustScore.TrustScore,
        filter: FilterOptions,
    ) bool {
        _ = self;

        // Unproven only filter
        if (filter.unproven_only) {
            if (result.status == .verified or result.status == .trusted or result.status == .admitted) {
                return false;
            }
        }

        // Failed only filter
        if (filter.failed_only and result.status != .failed) {
            return false;
        }

        // Below threshold filter
        if (filter.below_threshold) |threshold| {
            if (score.value >= threshold) {
                return false;
            }
        }

        // Status filter
        if (filter.status_filter) |status| {
            if (result.status != status) {
                return false;
            }
        }

        // AI only filter
        if (filter.ai_only) {
            if (provenance) |p| {
                if (!p.isAiGenerated()) return false;
            } else {
                return false;
            }
        }

        // Needs review filter
        if (filter.needs_review_only) {
            if (provenance) |p| {
                if (!p.needsReview()) return false;
            } else {
                return false;
            }
        }

        // Exclude trusted filter
        if (filter.exclude_trusted) {
            if (result.status == .trusted or result.status == .admitted) {
                return false;
            }
        }

        return true;
    }

    fn createEntry(
        self: *const TrustReportGenerator,
        result: *const VerificationResult,
        score: *const TrustScore.TrustScore,
        criticality: CriticalityLevel,
    ) !FunctionTrustEntry {
        const name = try self.allocator.dupe(u8, result.spec_name);
        errdefer self.allocator.free(name);

        const source_file: ?[]const u8 = null;
        var line: ?u32 = null;

        if (result.span) |span| {
            // Note: Span doesn't track source file name, only line/column
            line = span.start.line;
        }

        const action = self.thresholds.getAction(score.value);
        _ = criticality;

        return FunctionTrustEntry{
            .allocator = self.allocator,
            .name = name,
            .score = score.*,
            .verification_status = result.status,
            .action = action,
            .verification_time_ms = result.time_ms,
            .from_cache = result.from_cache,
            .failed_obligations = result.getFailedCount(),
            .source_file = source_file,
            .line = line,
        };
    }
};

// ============================================================================
// JSON Serialization
// ============================================================================

/// Serialize a trust report to JSON
pub fn toJson(allocator: Allocator, report: *const TrustReport) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    try buf.appendSlice(allocator, "{");

    // Name
    try appendJsonField(&buf, allocator, "name", report.name);
    try buf.appendSlice(allocator, ",");

    // Generated at
    try buf.appendSlice(allocator, "\"generated_at\":");
    const ts_str = try std.fmt.allocPrint(allocator, "{d}", .{report.generated_at});
    defer allocator.free(ts_str);
    try buf.appendSlice(allocator, ts_str);
    try buf.appendSlice(allocator, ",");

    // Summary
    try buf.appendSlice(allocator, "\"summary\":");
    try appendSummaryJson(&buf, allocator, &report.summary);
    try buf.appendSlice(allocator, ",");

    // Thresholds
    try buf.appendSlice(allocator, "\"thresholds\":");
    try appendThresholdsJson(&buf, allocator, &report.thresholds);
    try buf.appendSlice(allocator, ",");

    // Entries
    try buf.appendSlice(allocator, "\"entries\":[");
    for (report.entries.items, 0..) |entry, i| {
        if (i > 0) try buf.appendSlice(allocator, ",");
        try appendEntryJson(&buf, allocator, &entry);
    }
    try buf.appendSlice(allocator, "]");

    try buf.appendSlice(allocator, "}");

    return buf.toOwnedSlice(allocator);
}

fn appendJsonField(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, key: []const u8, value: []const u8) !void {
    try buf.appendSlice(allocator, "\"");
    try buf.appendSlice(allocator, key);
    try buf.appendSlice(allocator, "\":\"");
    // Escape special characters
    for (value) |c| {
        switch (c) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            '\r' => try buf.appendSlice(allocator, "\\r"),
            '\t' => try buf.appendSlice(allocator, "\\t"),
            else => try buf.append(allocator, c),
        }
    }
    try buf.appendSlice(allocator, "\"");
}

fn appendSummaryJson(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, summary: *const TrustReport.Summary) !void {
    const json = try std.fmt.allocPrint(allocator,
        \\{{"total":{d},"auto_approved":{d},"allowed":{d},"require_review":{d},"blocked":{d},"average_trust":{d:.4},"min_trust":{d:.4},"max_trust":{d:.4},"total_time_ms":{d}}}
    , .{
        summary.total,
        summary.auto_approved,
        summary.allowed,
        summary.require_review,
        summary.blocked,
        summary.average_trust,
        summary.min_trust,
        summary.max_trust,
        summary.total_time_ms,
    });
    defer allocator.free(json);
    try buf.appendSlice(allocator, json);
}

fn appendThresholdsJson(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, thresholds: *const TrustThresholds) !void {
    const json = try std.fmt.allocPrint(allocator,
        \\{{"auto_approve":{d:.2},"require_review":{d:.2},"block_deployment":{d:.2}}}
    , .{
        thresholds.auto_approve,
        thresholds.require_review,
        thresholds.block_deployment,
    });
    defer allocator.free(json);
    try buf.appendSlice(allocator, json);
}

fn appendEntryJson(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, entry: *const FunctionTrustEntry) !void {
    try buf.appendSlice(allocator, "{");

    // Name
    try appendJsonField(buf, allocator, "name", entry.name);
    try buf.appendSlice(allocator, ",");

    // Score details
    const score_json = try std.fmt.allocPrint(allocator,
        \\\"trust_score":{d:.4},"trust_level":"{s}","base_trust":{d:.4},"provenance_modifier":{d:.4},"age_modifier":{d:.4},"criticality_factor":{d:.4}
    , .{
        entry.score.value,
        entry.score.level.toString(),
        entry.score.base_trust,
        entry.score.provenance_modifier,
        entry.score.age_modifier,
        entry.score.criticality_factor,
    });
    defer allocator.free(score_json);
    try buf.appendSlice(allocator, score_json);
    try buf.appendSlice(allocator, ",");

    // Verification status
    try appendJsonField(buf, allocator, "verification_status", entry.verification_status.description());
    try buf.appendSlice(allocator, ",");

    // Action
    try appendJsonField(buf, allocator, "action", entry.action.toString());
    try buf.appendSlice(allocator, ",");

    // Time and cache info
    const time_json = try std.fmt.allocPrint(allocator,
        \\\"verification_time_ms":{d},"from_cache":{s},"failed_obligations":{d}
    , .{
        entry.verification_time_ms,
        if (entry.from_cache) "true" else "false",
        entry.failed_obligations,
    });
    defer allocator.free(time_json);
    try buf.appendSlice(allocator, time_json);

    // Source location (if available)
    if (entry.source_file) |sf| {
        try buf.appendSlice(allocator, ",");
        try appendJsonField(buf, allocator, "source_file", sf);
        if (entry.line) |line| {
            const line_json = try std.fmt.allocPrint(allocator, ",\"line\":{d}", .{line});
            defer allocator.free(line_json);
            try buf.appendSlice(allocator, line_json);
        }
    }

    // Factors
    try buf.appendSlice(allocator, ",\"factors\":{");
    if (entry.score.factors.author_kind) |ak| {
        try appendJsonField(buf, allocator, "author_kind", ak.toString());
    } else {
        try buf.appendSlice(allocator, "\"author_kind\":null");
    }
    if (entry.score.factors.ai_confidence) |conf| {
        const conf_str = try std.fmt.allocPrint(allocator, ",\"ai_confidence\":{d:.2}", .{conf});
        defer allocator.free(conf_str);
        try buf.appendSlice(allocator, conf_str);
    }
    const factors_rest = try std.fmt.allocPrint(allocator,
        \\,"is_approved":{s},"needs_review":{s},"approval_count":{d},"criticality":"{s}"
    , .{
        if (entry.score.factors.is_approved) "true" else "false",
        if (entry.score.factors.needs_review) "true" else "false",
        entry.score.factors.approval_count,
        entry.score.factors.criticality.toString(),
    });
    defer allocator.free(factors_rest);
    try buf.appendSlice(allocator, factors_rest);
    try buf.appendSlice(allocator, "}");

    try buf.appendSlice(allocator, "}");
}

/// Format a trust report for human-readable display
pub fn formatReport(allocator: Allocator, report: *const TrustReport) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    errdefer buf.deinit(allocator);

    // Title
    const title = try std.fmt.allocPrint(allocator, "Trust Report: {s}\n", .{report.name});
    defer allocator.free(title);
    try buf.appendSlice(allocator, title);
    try buf.appendSlice(allocator, "=" ** 60 ++ "\n\n");

    // Summary
    try buf.appendSlice(allocator, "Summary:\n");
    const summary_text = try std.fmt.allocPrint(allocator,
        \\  Total specifications: {d}
        \\  Auto-approved: {d}
        \\  Allowed: {d}
        \\  Require review: {d}
        \\  Blocked: {d}
        \\  Average trust: {d:.2}%
        \\  Trust range: {d:.2}% - {d:.2}%
        \\
        \\
    , .{
        report.summary.total,
        report.summary.auto_approved,
        report.summary.allowed,
        report.summary.require_review,
        report.summary.blocked,
        report.summary.average_trust * 100.0,
        report.summary.min_trust * 100.0,
        report.summary.max_trust * 100.0,
    });
    defer allocator.free(summary_text);
    try buf.appendSlice(allocator, summary_text);

    // Entries
    if (report.entries.items.len > 0) {
        try buf.appendSlice(allocator, "Entries:\n");
        for (report.entries.items) |entry| {
            const entry_text = try std.fmt.allocPrint(allocator,
                \\  [{s}] {s} - {d:.1}% ({s})
                \\
            , .{
                entry.action.toString(),
                entry.name,
                entry.score.value * 100.0,
                entry.verification_status.description(),
            });
            defer allocator.free(entry_text);
            try buf.appendSlice(allocator, entry_text);
        }
    }

    // Deployment status
    try buf.appendSlice(allocator, "\n");
    if (report.canDeploy()) {
        try buf.appendSlice(allocator, "Deployment: ALLOWED\n");
    } else {
        const blocked_text = try std.fmt.allocPrint(allocator,
            \\Deployment: BLOCKED ({d} specification(s) below threshold)
            \\
        , .{report.summary.blocked});
        defer allocator.free(blocked_text);
        try buf.appendSlice(allocator, blocked_text);
    }

    return buf.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "TrustThresholds.getAction" {
    const testing = std.testing;

    const thresholds = TrustThresholds{};

    try testing.expectEqual(ThresholdAction.auto_approve, thresholds.getAction(0.96));
    try testing.expectEqual(ThresholdAction.allow, thresholds.getAction(0.90));
    try testing.expectEqual(ThresholdAction.require_review, thresholds.getAction(0.70));
    try testing.expectEqual(ThresholdAction.block, thresholds.getAction(0.30));
}

test "TrustReport basic" {
    const testing = std.testing;

    var report = TrustReport.init(testing.allocator, "test", .{});
    defer report.deinit();

    try testing.expectEqualStrings("test", report.name);
    try testing.expectEqual(@as(usize, 0), report.summary.total);
    try testing.expect(report.canDeploy());
}

test "TrustReport addEntry" {
    const testing = std.testing;

    var report = TrustReport.init(testing.allocator, "test", .{});
    defer report.deinit();

    const name = try testing.allocator.dupe(u8, "test.spec");
    const entry = FunctionTrustEntry{
        .allocator = testing.allocator,
        .name = name,
        .score = TrustScore.TrustScore.init(0.90, 1.0, 1.0, 1.0, .{}),
        .verification_status = .verified,
        .action = .allow,
        .verification_time_ms = 100,
        .from_cache = false,
        .failed_obligations = 0,
        .source_file = null,
        .line = null,
    };

    try report.addEntry(entry);

    try testing.expectEqual(@as(usize, 1), report.summary.total);
    try testing.expectEqual(@as(usize, 1), report.summary.allowed);
}

test "TrustReportGenerator basic" {
    const testing = std.testing;

    var generator = TrustReportGenerator.init(testing.allocator, .{}, .{}, null);
    defer generator.deinit();

    // Create a verification report
    var ver_report = VerificationReport.init(testing.allocator, "test_module");
    defer ver_report.deinit();

    var result = VerificationResult.init("test.spec");
    result.status = .verified;
    try ver_report.addResult(result);

    // Generate trust report
    var trust_report = try generator.generate(&ver_report, .{});
    defer trust_report.deinit();

    try testing.expectEqual(@as(usize, 1), trust_report.summary.total);
}

test "TrustReport JSON serialization" {
    const testing = std.testing;

    var report = TrustReport.init(testing.allocator, "test", .{});
    defer report.deinit();

    const json = try toJson(testing.allocator, &report);
    defer testing.allocator.free(json);

    // Check that JSON contains expected fields
    try testing.expect(std.mem.indexOf(u8, json, "\"name\"") != null);
    try testing.expect(std.mem.indexOf(u8, json, "\"summary\"") != null);
    try testing.expect(std.mem.indexOf(u8, json, "\"entries\"") != null);
}

test "FilterOptions with unproven_only" {
    const testing = std.testing;

    var generator = TrustReportGenerator.init(testing.allocator, .{}, .{}, null);
    defer generator.deinit();

    var ver_report = VerificationReport.init(testing.allocator, "test");
    defer ver_report.deinit();

    // Add verified and pending results
    var verified = VerificationResult.init("verified.spec");
    verified.status = .verified;
    try ver_report.addResult(verified);

    var pending = VerificationResult.init("pending.spec");
    pending.status = .pending;
    try ver_report.addResult(pending);

    // Generate with unproven_only filter
    var filtered = try generator.generate(&ver_report, .{ .unproven_only = true });
    defer filtered.deinit();

    try testing.expectEqual(@as(usize, 1), filtered.summary.total);
}
