//! Trust Score Calculation
//!
//! This module implements the trust score system that combines verification status,
//! provenance information, age, and criticality to produce a composite trust score.
//!
//! ## Trust Score Formula
//!
//! The composite trust score is calculated as:
//!
//!     trust = base_trust * provenance_modifier * age_modifier * criticality_factor
//!
//! Where:
//! - base_trust: Derived from verification status (0.0 - 1.0)
//! - provenance_modifier: Adjusts based on authorship and approval status
//! - age_modifier: Optional decay over time for stale proofs
//! - criticality_factor: Applies stricter requirements for sensitive code
//!
//! ## Usage
//!
//! ```zig
//! var calculator = TrustCalculator.init(allocator, .{});
//! const score = calculator.calculate(verification_result, provenance);
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const VerificationStatus = @import("../verify/ProofObligation.zig").VerificationStatus;
const VerificationResult = @import("../verify/VerificationResult.zig").VerificationResult;
const VerificationReport = @import("../verify/VerificationResult.zig").VerificationReport;
const Provenance = @import("../provenance/Provenance.zig");
const ProvenanceMetadata = Provenance.ProvenanceMetadata;
const AuthorKind = Provenance.AuthorKind;
const Confidence = Provenance.Confidence;

// ============================================================================
// Trust Score
// ============================================================================

/// A trust score with its component factors
pub const TrustScore = struct {
    /// The final composite trust score (0.0 - 1.0)
    value: f64,
    /// Base trust from verification status
    base_trust: f64,
    /// Provenance modifier applied
    provenance_modifier: f64,
    /// Age modifier applied
    age_modifier: f64,
    /// Criticality factor applied
    criticality_factor: f64,
    /// Trust level category
    level: Level,
    /// Factors that contributed to the score
    factors: Factors,

    pub const min_value: f64 = 0.0;
    pub const max_value: f64 = 1.0;

    /// Trust level categories
    pub const Level = enum {
        /// Very high trust (>= 0.95)
        very_high,
        /// High trust (>= 0.85)
        high,
        /// Medium trust (>= 0.7)
        medium,
        /// Low trust (>= 0.5)
        low,
        /// Very low trust (< 0.5)
        very_low,
        /// Untrusted (failed verification or critical issues)
        untrusted,

        pub fn fromScore(score: f64) Level {
            if (score >= 0.95) return .very_high;
            if (score >= 0.85) return .high;
            if (score >= 0.7) return .medium;
            if (score >= 0.5) return .low;
            if (score >= 0.1) return .very_low;
            return .untrusted;
        }

        pub fn toString(self: Level) []const u8 {
            return switch (self) {
                .very_high => "very_high",
                .high => "high",
                .medium => "medium",
                .low => "low",
                .very_low => "very_low",
                .untrusted => "untrusted",
            };
        }
    };

    /// Factors that contributed to the trust score
    pub const Factors = struct {
        /// Verification status used
        verification_status: VerificationStatus = .pending,
        /// Author kind (if known)
        author_kind: ?AuthorKind = null,
        /// AI confidence value (if applicable)
        ai_confidence: ?f64 = null,
        /// Whether code has been approved
        is_approved: bool = false,
        /// Whether code needs review
        needs_review: bool = false,
        /// Number of approvals
        approval_count: usize = 0,
        /// Age in days (if age modifier applied)
        age_days: ?f64 = null,
        /// Criticality level applied
        criticality: CriticalityLevel = .normal,
    };

    /// Create a trust score from components
    pub fn init(
        base_trust: f64,
        provenance_modifier: f64,
        age_modifier: f64,
        criticality_factor: f64,
        factors: Factors,
    ) TrustScore {
        const value = std.math.clamp(
            base_trust * provenance_modifier * age_modifier * criticality_factor,
            min_value,
            max_value,
        );
        return .{
            .value = value,
            .base_trust = base_trust,
            .provenance_modifier = provenance_modifier,
            .age_modifier = age_modifier,
            .criticality_factor = criticality_factor,
            .level = Level.fromScore(value),
            .factors = factors,
        };
    }

    /// Create a score indicating untrusted (failed verification)
    pub fn untrusted(status: VerificationStatus) TrustScore {
        return .{
            .value = 0.0,
            .base_trust = 0.0,
            .provenance_modifier = 1.0,
            .age_modifier = 1.0,
            .criticality_factor = 1.0,
            .level = .untrusted,
            .factors = .{ .verification_status = status },
        };
    }

    /// Check if this score meets a threshold
    pub fn meetsThreshold(self: TrustScore, threshold: f64) bool {
        return self.value >= threshold;
    }
};

// ============================================================================
// Criticality Level
// ============================================================================

/// Criticality level for code that requires extra scrutiny
pub const CriticalityLevel = enum {
    /// Normal code (no special handling)
    normal,
    /// Security-sensitive code
    security,
    /// Money/financial handling code
    financial,
    /// Safety-critical code
    safety,
    /// Maximum criticality (all flags)
    critical,

    pub fn toString(self: CriticalityLevel) []const u8 {
        return switch (self) {
            .normal => "normal",
            .security => "security",
            .financial => "financial",
            .safety => "safety",
            .critical => "critical",
        };
    }
};

// ============================================================================
// Trust Calculator Configuration
// ============================================================================

/// Configuration for trust score calculation
pub const TrustConfig = struct {
    // Base trust values for verification status
    base_trust_verified: f64 = 0.95,
    base_trust_trusted: f64 = 0.85,
    base_trust_admitted: f64 = 0.75,
    base_trust_unknown: f64 = 0.50,
    base_trust_timeout: f64 = 0.30,
    base_trust_pending: f64 = 0.40,
    base_trust_skipped: f64 = 0.30,
    base_trust_failed: f64 = 0.0,

    // Provenance modifiers
    /// Bonus for human-authored code
    human_author_bonus: f64 = 0.05,
    /// Penalty for unknown authorship
    unknown_author_penalty: f64 = 0.15,
    /// Bonus per approval
    approval_bonus: f64 = 0.05,
    /// Maximum bonus from approvals
    max_approval_bonus: f64 = 0.15,
    /// Penalty for needs_review flag
    needs_review_penalty: f64 = 0.10,
    /// Minimum AI confidence multiplier (low confidence reduces score)
    ai_confidence_floor: f64 = 0.5,

    // Age decay configuration
    /// Enable age-based decay
    enable_age_decay: bool = false,
    /// Days until decay starts
    decay_start_days: f64 = 365.0,
    /// Days until minimum trust
    decay_full_days: f64 = 730.0,
    /// Minimum trust after full decay
    decay_floor: f64 = 0.7,

    // Criticality factors (multiplied with base score requirement)
    criticality_normal: f64 = 1.0,
    criticality_security: f64 = 0.95,
    criticality_financial: f64 = 0.95,
    criticality_safety: f64 = 0.90,
    criticality_critical: f64 = 0.85,

    /// Get base trust value for a verification status
    pub fn getBaseTrust(self: TrustConfig, status: VerificationStatus) f64 {
        return switch (status) {
            .verified => self.base_trust_verified,
            .trusted => self.base_trust_trusted,
            .admitted => self.base_trust_admitted,
            .unknown => self.base_trust_unknown,
            .timeout => self.base_trust_timeout,
            .pending => self.base_trust_pending,
            .skipped => self.base_trust_skipped,
            .failed => self.base_trust_failed,
            .in_progress => self.base_trust_pending,
        };
    }

    /// Get criticality factor
    pub fn getCriticalityFactor(self: TrustConfig, level: CriticalityLevel) f64 {
        return switch (level) {
            .normal => self.criticality_normal,
            .security => self.criticality_security,
            .financial => self.criticality_financial,
            .safety => self.criticality_safety,
            .critical => self.criticality_critical,
        };
    }
};

// ============================================================================
// Trust Calculator
// ============================================================================

/// Calculator for computing trust scores
pub const TrustCalculator = struct {
    allocator: Allocator,
    config: TrustConfig,
    /// Current timestamp (milliseconds since epoch) - can be set for testing
    current_time_ms: ?i64 = null,

    pub fn init(allocator: Allocator, config: TrustConfig) TrustCalculator {
        return .{
            .allocator = allocator,
            .config = config,
        };
    }

    /// Calculate trust score from verification result and provenance
    pub fn calculate(
        self: *const TrustCalculator,
        result: *const VerificationResult,
        provenance: ?*const ProvenanceMetadata,
        criticality: CriticalityLevel,
    ) TrustScore {
        // Calculate base trust from verification status
        const base_trust = self.calculateBaseTrust(result.status);

        // If failed, return untrusted immediately
        if (result.status == .failed) {
            return TrustScore.untrusted(result.status);
        }

        // Calculate provenance modifier
        const prov_result = self.calculateProvenanceModifier(provenance);

        // Calculate age modifier
        const age_result = self.calculateAgeModifier(provenance);

        // Get criticality factor
        const criticality_factor = self.config.getCriticalityFactor(criticality);

        // Build factors struct
        var factors = TrustScore.Factors{
            .verification_status = result.status,
            .criticality = criticality,
        };

        if (provenance) |p| {
            if (p.primaryAuthor()) |author| {
                factors.author_kind = author.kind;
            }
            if (p.confidence) |c| {
                factors.ai_confidence = c.value;
            }
            factors.is_approved = p.isApproved();
            factors.needs_review = p.needsReview();
            factors.approval_count = p.approvals.items.len;
        }

        factors.age_days = age_result.age_days;

        return TrustScore.init(
            base_trust,
            prov_result.modifier,
            age_result.modifier,
            criticality_factor,
            factors,
        );
    }

    /// Calculate trust score from verification status only
    pub fn calculateFromStatus(self: *const TrustCalculator, status: VerificationStatus) TrustScore {
        const base_trust = self.calculateBaseTrust(status);

        if (status == .failed) {
            return TrustScore.untrusted(status);
        }

        return TrustScore.init(
            base_trust,
            1.0, // No provenance modifier
            1.0, // No age modifier
            1.0, // Normal criticality
            .{ .verification_status = status },
        );
    }

    /// Calculate base trust from verification status
    fn calculateBaseTrust(self: *const TrustCalculator, status: VerificationStatus) f64 {
        return self.config.getBaseTrust(status);
    }

    /// Result of provenance modifier calculation
    const ProvenanceModifierResult = struct {
        modifier: f64,
    };

    /// Calculate provenance modifier from metadata
    fn calculateProvenanceModifier(
        self: *const TrustCalculator,
        provenance: ?*const ProvenanceMetadata,
    ) ProvenanceModifierResult {
        const p = provenance orelse return .{ .modifier = 1.0 - self.config.unknown_author_penalty };

        var modifier: f64 = 1.0;

        // Author kind adjustments
        if (p.primaryAuthor()) |author| {
            switch (author.kind) {
                .human => modifier += self.config.human_author_bonus,
                .ai => {
                    // AI-generated code uses confidence as modifier
                    if (p.confidence) |c| {
                        // Scale confidence: low confidence reduces trust
                        // confidence 1.0 => modifier 1.0
                        // confidence 0.0 => modifier = ai_confidence_floor
                        const conf_modifier = self.config.ai_confidence_floor +
                            (1.0 - self.config.ai_confidence_floor) * c.value;
                        modifier *= conf_modifier;
                    } else {
                        // AI code without confidence: apply penalty
                        modifier *= 0.8;
                    }
                },
                .mixed => {
                    // Mixed authorship: slight bonus but use confidence if available
                    modifier += self.config.human_author_bonus * 0.5;
                    if (p.confidence) |c| {
                        const conf_modifier = 0.8 + 0.2 * c.value;
                        modifier *= conf_modifier;
                    }
                },
                .unknown => modifier -= self.config.unknown_author_penalty,
            }
        } else {
            // No author information
            modifier -= self.config.unknown_author_penalty;
        }

        // Approval bonus
        const approval_count = p.approvals.items.len;
        if (approval_count > 0) {
            const bonus = @min(
                @as(f64, @floatFromInt(approval_count)) * self.config.approval_bonus,
                self.config.max_approval_bonus,
            );
            modifier += bonus;
        }

        // Needs review penalty
        if (p.needsReview()) {
            modifier -= self.config.needs_review_penalty;
        }

        // Clamp modifier to reasonable range
        return .{ .modifier = std.math.clamp(modifier, 0.1, 1.5) };
    }

    /// Result of age modifier calculation
    const AgeModifierResult = struct {
        modifier: f64,
        age_days: ?f64,
    };

    /// Calculate age modifier from provenance metadata
    fn calculateAgeModifier(
        self: *const TrustCalculator,
        provenance: ?*const ProvenanceMetadata,
    ) AgeModifierResult {
        if (!self.config.enable_age_decay) {
            return .{ .modifier = 1.0, .age_days = null };
        }

        // Get timestamp from the most recent approval or checkpoint
        const timestamp_str = getTimestamp(provenance) orelse {
            return .{ .modifier = 1.0, .age_days = null };
        };

        // Parse ISO 8601 timestamp
        const timestamp_ms = parseIso8601(timestamp_str) orelse {
            return .{ .modifier = 1.0, .age_days = null };
        };

        // Calculate age in days
        const current_ms = self.current_time_ms orelse std.time.milliTimestamp();
        const age_ms = current_ms - timestamp_ms;
        if (age_ms < 0) {
            return .{ .modifier = 1.0, .age_days = 0.0 };
        }

        const age_days: f64 = @as(f64, @floatFromInt(age_ms)) / (24.0 * 60.0 * 60.0 * 1000.0);

        // Calculate decay
        if (age_days < self.config.decay_start_days) {
            return .{ .modifier = 1.0, .age_days = age_days };
        }

        if (age_days >= self.config.decay_full_days) {
            return .{ .modifier = self.config.decay_floor, .age_days = age_days };
        }

        // Linear decay between start and full
        const decay_range = self.config.decay_full_days - self.config.decay_start_days;
        const decay_progress = (age_days - self.config.decay_start_days) / decay_range;
        const modifier = 1.0 - (1.0 - self.config.decay_floor) * decay_progress;

        return .{ .modifier = modifier, .age_days = age_days };
    }
};

/// Get timestamp from provenance (most recent approval or checkpoint)
fn getTimestamp(provenance: ?*const ProvenanceMetadata) ?[]const u8 {
    const p = provenance orelse return null;

    // Check approvals (most recent)
    if (p.approvals.items.len > 0) {
        const last_approval = p.approvals.items[p.approvals.items.len - 1];
        if (last_approval.at) |at| {
            return at;
        }
    }

    // Check checkpoint
    if (p.checkpoint) |cp| {
        if (cp.timestamp) |ts| {
            return ts;
        }
    }

    return null;
}

/// Parse ISO 8601 timestamp to milliseconds since epoch
/// Supports: "2024-01-15T10:30:00Z" format
fn parseIso8601(s: []const u8) ?i64 {
    // Minimal ISO 8601 parser for YYYY-MM-DDTHH:MM:SSZ format
    if (s.len < 20) return null;

    const year = std.fmt.parseInt(i32, s[0..4], 10) catch return null;
    const month = std.fmt.parseInt(u8, s[5..7], 10) catch return null;
    const day = std.fmt.parseInt(u8, s[8..10], 10) catch return null;
    const hour = std.fmt.parseInt(u8, s[11..13], 10) catch return null;
    const minute = std.fmt.parseInt(u8, s[14..16], 10) catch return null;
    const second = std.fmt.parseInt(u8, s[17..19], 10) catch return null;

    // Simple epoch calculation (approximate, doesn't handle leap seconds)
    const epoch_year: i64 = 1970;
    const days_per_year: i64 = 365;
    const ms_per_second: i64 = 1000;
    const ms_per_minute: i64 = 60 * ms_per_second;
    const ms_per_hour: i64 = 60 * ms_per_minute;
    const ms_per_day: i64 = 24 * ms_per_hour;

    // Days from months (approximate)
    const month_days = [_]i64{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

    if (month < 1 or month > 12) return null;
    if (day < 1 or day > 31) return null;
    if (hour > 23 or minute > 59 or second > 59) return null;

    const years_since_epoch = @as(i64, year) - epoch_year;
    const leap_years = @divFloor(years_since_epoch + 1, 4) - @divFloor(years_since_epoch + 69, 100) + @divFloor(years_since_epoch + 369, 400);

    var total_days: i64 = years_since_epoch * days_per_year + leap_years;
    total_days += month_days[month - 1];
    total_days += @as(i64, day) - 1;

    // Leap year adjustment for months after February
    if (month > 2) {
        const is_leap = (@mod(year, 4) == 0 and @mod(year, 100) != 0) or @mod(year, 400) == 0;
        if (is_leap) total_days += 1;
    }

    const total_ms = total_days * ms_per_day +
        @as(i64, hour) * ms_per_hour +
        @as(i64, minute) * ms_per_minute +
        @as(i64, second) * ms_per_second;

    return total_ms;
}

// ============================================================================
// Tests
// ============================================================================

test "TrustScore.Level.fromScore" {
    const testing = std.testing;

    try testing.expectEqual(TrustScore.Level.very_high, TrustScore.Level.fromScore(0.95));
    try testing.expectEqual(TrustScore.Level.very_high, TrustScore.Level.fromScore(1.0));
    try testing.expectEqual(TrustScore.Level.high, TrustScore.Level.fromScore(0.90));
    try testing.expectEqual(TrustScore.Level.high, TrustScore.Level.fromScore(0.85));
    try testing.expectEqual(TrustScore.Level.medium, TrustScore.Level.fromScore(0.75));
    try testing.expectEqual(TrustScore.Level.low, TrustScore.Level.fromScore(0.55));
    try testing.expectEqual(TrustScore.Level.very_low, TrustScore.Level.fromScore(0.3));
    try testing.expectEqual(TrustScore.Level.untrusted, TrustScore.Level.fromScore(0.0));
}

test "TrustCalculator basic calculation" {
    const testing = std.testing;

    const calc = TrustCalculator.init(testing.allocator, .{});

    // Verified status should give high trust
    const verified_score = calc.calculateFromStatus(.verified);
    try testing.expect(verified_score.value >= 0.9);
    try testing.expectEqual(TrustScore.Level.very_high, verified_score.level);

    // Failed status should give zero trust
    const failed_score = calc.calculateFromStatus(.failed);
    try testing.expectEqual(@as(f64, 0.0), failed_score.value);
    try testing.expectEqual(TrustScore.Level.untrusted, failed_score.level);

    // Pending status should give low trust
    const pending_score = calc.calculateFromStatus(.pending);
    try testing.expect(pending_score.value < 0.5);
}

test "TrustCalculator with provenance" {
    const testing = std.testing;

    const calc = TrustCalculator.init(testing.allocator, .{});

    // Create a verification result
    var result = VerificationResult.init("test.spec");
    result.status = .verified;

    // Test without provenance (should have penalty)
    const no_prov_score = calc.calculate(&result, null, .normal);
    try testing.expect(no_prov_score.value < 0.95); // Penalty for unknown authorship

    // Test with human authorship
    var human_prov = ProvenanceMetadata.init(testing.allocator);
    defer human_prov.deinit();
    try human_prov.addAuthor(Provenance.Author.human(null));

    const human_score = calc.calculate(&result, &human_prov, .normal);
    try testing.expect(human_score.value >= no_prov_score.value); // Should be better than unknown
}

test "TrustCalculator with AI confidence" {
    const testing = std.testing;

    const calc = TrustCalculator.init(testing.allocator, .{});

    var result = VerificationResult.init("test.spec");
    result.status = .verified;

    // High confidence AI
    // Note: Use null for model since deinit tries to free it
    var high_conf_prov = ProvenanceMetadata.init(testing.allocator);
    defer high_conf_prov.deinit();
    try high_conf_prov.addAuthor(.{ .kind = .ai, .model = null });
    high_conf_prov.confidence = Confidence.init(0.95);

    const high_conf_score = calc.calculate(&result, &high_conf_prov, .normal);

    // Low confidence AI
    var low_conf_prov = ProvenanceMetadata.init(testing.allocator);
    defer low_conf_prov.deinit();
    try low_conf_prov.addAuthor(.{ .kind = .ai, .model = null });
    low_conf_prov.confidence = Confidence.init(0.3);

    const low_conf_score = calc.calculate(&result, &low_conf_prov, .normal);

    // High confidence should give higher trust
    try testing.expect(high_conf_score.value > low_conf_score.value);
}

test "TrustCalculator criticality" {
    const testing = std.testing;

    const calc = TrustCalculator.init(testing.allocator, .{});

    var result = VerificationResult.init("test.spec");
    result.status = .verified;

    var prov = ProvenanceMetadata.init(testing.allocator);
    defer prov.deinit();
    try prov.addAuthor(Provenance.Author.human(null));

    const normal_score = calc.calculate(&result, &prov, .normal);
    const critical_score = calc.calculate(&result, &prov, .critical);

    // Critical code should have lower effective trust (stricter requirements)
    try testing.expect(critical_score.value <= normal_score.value);
}

test "parseIso8601" {
    const testing = std.testing;

    // Test valid timestamp
    const ts = parseIso8601("2024-01-15T10:30:00Z");
    try testing.expect(ts != null);

    // Test invalid timestamp
    try testing.expect(parseIso8601("invalid") == null);
    try testing.expect(parseIso8601("2024") == null);
}

test "TrustScore.meetsThreshold" {
    const testing = std.testing;

    const score = TrustScore.init(0.80, 1.0, 1.0, 1.0, .{});

    try testing.expect(score.meetsThreshold(0.7));
    try testing.expect(score.meetsThreshold(0.8));
    try testing.expect(!score.meetsThreshold(0.9));
}
