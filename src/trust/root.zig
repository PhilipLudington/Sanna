//! Trust Score System
//!
//! This module provides trust score calculation and reporting for Sanna specifications.
//!
//! The trust system combines multiple factors to produce a composite trust score:
//! - Verification status (proven, failed, timeout, etc.)
//! - Provenance (human vs AI authorship, confidence levels)
//! - Age decay (optional staleness factor)
//! - Criticality (security, financial, safety requirements)
//!
//! ## Quick Start
//!
//! ```zig
//! const trust = @import("trust/root.zig");
//!
//! // Create a calculator with default configuration
//! var calculator = trust.TrustCalculator.init(allocator, .{});
//!
//! // Calculate trust for a verification result
//! const score = calculator.calculate(&result, provenance, .normal);
//!
//! // Check if it meets deployment threshold
//! const thresholds = trust.TrustThresholds{};
//! const action = thresholds.getAction(score.value);
//! ```
//!
//! ## Trust Reports
//!
//! ```zig
//! // Generate a full trust report
//! var generator = trust.TrustReportGenerator.init(allocator, .{}, .{}, &provenance_store);
//! var report = try generator.generate(&verification_report, .{});
//!
//! // Export to JSON
//! const json = try trust.toJson(allocator, &report);
//! ```

const std = @import("std");

// Core trust score types
pub const TrustScore = @import("TrustScore.zig");
pub const TrustCalculator = TrustScore.TrustCalculator;
pub const TrustConfig = TrustScore.TrustConfig;
pub const CriticalityLevel = TrustScore.CriticalityLevel;
pub const Score = TrustScore.TrustScore;
pub const Level = TrustScore.TrustScore.Level;

// Trust report types
pub const TrustReport = @import("TrustReport.zig");
pub const TrustReportGenerator = TrustReport.TrustReportGenerator;
pub const TrustThresholds = TrustReport.TrustThresholds;
pub const ThresholdAction = TrustReport.ThresholdAction;
pub const FilterOptions = TrustReport.FilterOptions;
pub const FunctionTrustEntry = TrustReport.FunctionTrustEntry;
pub const Report = TrustReport.TrustReport;

// JSON serialization
pub const toJson = TrustReport.toJson;
pub const formatReport = TrustReport.formatReport;

test {
    _ = TrustScore;
    _ = TrustReport;
}
