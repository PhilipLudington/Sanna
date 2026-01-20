//! Provenance and Confidence System
//!
//! This module provides tracking of code authorship, AI confidence levels,
//! review status, and generation metadata for AI-assisted software development.
//!
//! ## Overview
//!
//! The provenance system tracks:
//! - **Authorship**: Who wrote the code (human, AI, or mixed)
//! - **Confidence**: AI-reported confidence levels (0.0 to 1.0)
//! - **Review Status**: Whether code needs review and approval records
//! - **Verification**: Link to formal verification status
//! - **Generation**: Checkpoints and markers for resumable AI generation
//!
//! ## Supported Attributes
//!
//! - `@author(kind, identity = "...", model = "...", prompt_hash = "...", reviewed_by = "...")`
//! - `@confidence(0.0 - 1.0)`
//! - `@needs_review(reason = "...", priority = "...")`
//! - `@approved(by = "...", at = "...", note = "...")`
//! - `@verified(status, obligations = "...")`
//! - `@generation_checkpoint(model = "...", timestamp = "...", context_hash = "...")`
//! - `@generating(model = "...", started_at = "...")`
//!
//! ## Usage
//!
//! ```zig
//! const provenance = @import("provenance");
//!
//! // Extract from AST
//! var extractor = provenance.ProvenanceExtractor.init(allocator);
//! defer extractor.deinit();
//!
//! if (try extractor.extractFromDeclaration(decl)) |metadata| {
//!     // Store in registry
//!     try store.register(spec_name, metadata);
//! }
//!
//! // Query
//! if (store.lookup(spec_name)) |prov| {
//!     if (prov.confidence) |c| {
//!         std.debug.print("Confidence: {d:.2}\n", .{c.value});
//!     }
//! }
//! ```

const std = @import("std");

// Core provenance types
pub const Provenance = @import("Provenance.zig");
pub const AuthorKind = Provenance.AuthorKind;
pub const Author = Provenance.Author;
pub const Confidence = Provenance.Confidence;
pub const NeedsReview = Provenance.NeedsReview;
pub const Approval = Provenance.Approval;
pub const VerifiedStatus = Provenance.VerifiedStatus;
pub const VerificationInfo = Provenance.VerificationInfo;
pub const GenerationCheckpoint = Provenance.GenerationCheckpoint;
pub const GeneratingMarker = Provenance.GeneratingMarker;
pub const ProvenanceMetadata = Provenance.ProvenanceMetadata;
pub const toJson = Provenance.toJson;

// Extractor
pub const ProvenanceExtractor = @import("ProvenanceExtractor.zig").ProvenanceExtractor;
pub const ExtractionError = @import("ProvenanceExtractor.zig").ExtractionError;

// Store
pub const ProvenanceStore = @import("ProvenanceStore.zig").ProvenanceStore;

// ============================================================================
// Tests
// ============================================================================

test {
    _ = Provenance;
    _ = @import("ProvenanceExtractor.zig");
    _ = @import("ProvenanceStore.zig");
}

test "basic provenance workflow" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create store
    var store = ProvenanceStore.init(allocator);
    defer store.deinit();

    // Create metadata manually (in real usage, would come from extractor)
    // Note: The extractor allocates all strings, so we need to do the same
    const metadata = try allocator.create(ProvenanceMetadata);
    metadata.* = ProvenanceMetadata.init(allocator);
    const model_name = try allocator.dupe(u8, "opus-4.5");
    try metadata.addAuthor(.{ .kind = .ai, .model = model_name });
    metadata.confidence = Confidence.init(0.85);

    // Register
    try store.register("my_module.my_spec", metadata);

    // Lookup
    const looked_up = store.lookup("my_module.my_spec");
    try testing.expect(looked_up != null);
    try testing.expect(looked_up.?.isAiGenerated());
    try testing.expectApproxEqAbs(@as(f64, 0.85), looked_up.?.confidence.?.value, 0.001);

    // Export
    const json = try store.exportJson(allocator);
    defer allocator.free(json);
    try testing.expect(std.mem.indexOf(u8, json, "opus-4.5") != null);
}
