//! Sanna project configuration (sanna.toml)
//!
//! Parses and represents the sanna.toml configuration file that defines
//! project settings for generation, verification, trust, and provenance.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Config = @This();

allocator: Allocator,

/// Project metadata
project: Project = .{},
/// Code generation settings
generation: Generation = .{},
/// Verification settings
verification: Verification = .{},
/// Trust score settings
trust: Trust = .{},
/// Provenance tracking settings
provenance_config: ProvenanceConfig = .{},

pub const Project = struct {
    name: []const u8 = "sanna-project",
    version: []const u8 = "0.1.0",
    description: ?[]const u8 = null,
    authors: []const []const u8 = &.{},
};

pub const Generation = struct {
    target: []const u8 = "klar",
    default_model: []const u8 = "anthropic/claude-3-opus",
    confidence_threshold: f32 = 0.7,
    emit_contracts: bool = true,
    emit_docs: bool = true,
};

pub const Verification = struct {
    timeout: u32 = 30,
    solver: []const u8 = "z3",
    parallel: bool = true,
    cache_enabled: bool = true,
    cache_dir: []const u8 = ".sanna-cache",
};

pub const Trust = struct {
    require_review_below: f32 = 0.6,
    auto_approve_above: f32 = 0.95,
    block_deployment_below: f32 = 0.3,
    age_decay_enabled: bool = true,
    age_decay_half_life_days: u32 = 90,
};

pub const ProvenanceConfig = struct {
    require_identity: bool = true,
    allowed_models: []const []const u8 = &.{},
    require_approval_for_ai: bool = true,
};

/// Load configuration from sanna.toml in the given directory
pub fn load(allocator: Allocator, dir_path: []const u8) !Config {
    const config_path = try std.fs.path.join(allocator, &.{ dir_path, "sanna.toml" });
    defer allocator.free(config_path);

    const file = try std.fs.cwd().openFile(config_path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    return parse(allocator, content);
}

/// Parse configuration from TOML content
pub fn parse(allocator: Allocator, content: []const u8) !Config {
    var config = Config{
        .allocator = allocator,
    };

    var current_section: Section = .none;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Check for section header
        if (trimmed[0] == '[') {
            current_section = parseSection(trimmed);
            continue;
        }

        // Parse key-value pair
        const eq_idx = std.mem.indexOfScalar(u8, trimmed, '=') orelse continue;
        const key = std.mem.trim(u8, trimmed[0..eq_idx], " \t");
        const value = std.mem.trim(u8, trimmed[eq_idx + 1 ..], " \t");

        try config.setValue(allocator, current_section, key, value);
    }

    return config;
}

const Section = enum {
    none,
    project,
    generation,
    verification,
    trust,
    provenance,
};

fn parseSection(line: []const u8) Section {
    if (line.len < 3) return .none;
    const section_name = std.mem.trim(u8, line[1 .. line.len - 1], " \t[]");

    if (std.mem.eql(u8, section_name, "project")) return .project;
    if (std.mem.eql(u8, section_name, "generation")) return .generation;
    if (std.mem.eql(u8, section_name, "verification")) return .verification;
    if (std.mem.eql(u8, section_name, "trust")) return .trust;
    if (std.mem.eql(u8, section_name, "provenance")) return .provenance;

    return .none;
}

fn setValue(self: *Config, allocator: Allocator, section: Section, key: []const u8, raw_value: []const u8) !void {
    // Remove quotes from string values
    const value = if (raw_value.len >= 2 and raw_value[0] == '"' and raw_value[raw_value.len - 1] == '"')
        raw_value[1 .. raw_value.len - 1]
    else
        raw_value;

    switch (section) {
        .project => {
            if (std.mem.eql(u8, key, "name")) {
                self.project.name = try allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "version")) {
                self.project.version = try allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "description")) {
                self.project.description = try allocator.dupe(u8, value);
            }
        },
        .generation => {
            if (std.mem.eql(u8, key, "target")) {
                self.generation.target = try allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "default_model")) {
                self.generation.default_model = try allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "confidence_threshold")) {
                self.generation.confidence_threshold = try std.fmt.parseFloat(f32, value);
            } else if (std.mem.eql(u8, key, "emit_contracts")) {
                self.generation.emit_contracts = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "emit_docs")) {
                self.generation.emit_docs = std.mem.eql(u8, value, "true");
            }
        },
        .verification => {
            if (std.mem.eql(u8, key, "timeout")) {
                self.verification.timeout = try std.fmt.parseInt(u32, value, 10);
            } else if (std.mem.eql(u8, key, "solver")) {
                self.verification.solver = try allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "parallel")) {
                self.verification.parallel = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "cache_enabled")) {
                self.verification.cache_enabled = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "cache_dir")) {
                self.verification.cache_dir = try allocator.dupe(u8, value);
            }
        },
        .trust => {
            if (std.mem.eql(u8, key, "require_review_below")) {
                self.trust.require_review_below = try std.fmt.parseFloat(f32, value);
            } else if (std.mem.eql(u8, key, "auto_approve_above")) {
                self.trust.auto_approve_above = try std.fmt.parseFloat(f32, value);
            } else if (std.mem.eql(u8, key, "block_deployment_below")) {
                self.trust.block_deployment_below = try std.fmt.parseFloat(f32, value);
            } else if (std.mem.eql(u8, key, "age_decay_enabled")) {
                self.trust.age_decay_enabled = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "age_decay_half_life_days")) {
                self.trust.age_decay_half_life_days = try std.fmt.parseInt(u32, value, 10);
            }
        },
        .provenance => {
            if (std.mem.eql(u8, key, "require_identity")) {
                self.provenance_config.require_identity = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "require_approval_for_ai")) {
                self.provenance_config.require_approval_for_ai = std.mem.eql(u8, value, "true");
            }
            // Note: allowed_models array parsing would require more complex TOML parsing
        },
        .none => {},
    }
}

pub fn deinit(self: *Config) void {
    // Free allocated strings
    if (!std.mem.eql(u8, self.project.name, "sanna-project")) {
        self.allocator.free(self.project.name);
    }
    if (!std.mem.eql(u8, self.project.version, "0.1.0")) {
        self.allocator.free(self.project.version);
    }
    if (self.project.description) |desc| {
        self.allocator.free(desc);
    }
    if (!std.mem.eql(u8, self.generation.target, "klar")) {
        self.allocator.free(self.generation.target);
    }
    if (!std.mem.eql(u8, self.generation.default_model, "anthropic/claude-3-opus")) {
        self.allocator.free(self.generation.default_model);
    }
    if (!std.mem.eql(u8, self.verification.solver, "z3")) {
        self.allocator.free(self.verification.solver);
    }
    if (!std.mem.eql(u8, self.verification.cache_dir, ".sanna-cache")) {
        self.allocator.free(self.verification.cache_dir);
    }
}

/// Generate default sanna.toml content
pub fn generateDefault(project_name: []const u8) []const u8 {
    _ = project_name;
    return
        \\# Sanna Project Configuration
        \\
        \\[project]
        \\name = "myproject"
        \\version = "0.1.0"
        \\
        \\[generation]
        \\target = "klar"              # Target language: klar, kira, zig, rust, typescript, python
        \\default_model = "anthropic/claude-3-opus"
        \\confidence_threshold = 0.7   # Warn below this confidence
        \\emit_contracts = true        # Generate runtime contract checks
        \\emit_docs = true             # Generate documentation comments
        \\
        \\[verification]
        \\timeout = 30                 # Seconds per proof obligation
        \\solver = "z3"                # SMT solver: z3 or cvc5
        \\parallel = true              # Verify obligations in parallel
        \\cache_enabled = true         # Cache verification results
        \\cache_dir = ".sanna-cache"   # Cache directory
        \\
        \\[trust]
        \\require_review_below = 0.6   # Require human review below this score
        \\auto_approve_above = 0.95    # Auto-approve above this (proven + high confidence)
        \\block_deployment_below = 0.3 # Block deployment below this
        \\age_decay_enabled = true     # Enable trust decay over time
        \\age_decay_half_life_days = 90
        \\
        \\[provenance]
        \\require_identity = true      # All code must have an author
        \\require_approval_for_ai = true # AI-generated code needs approval
        \\# allowed_models = ["anthropic/claude-3-opus", "anthropic/claude-3-sonnet"]
        \\
    ;
}

test "Config.parse basic" {
    const toml =
        \\[project]
        \\name = "test-project"
        \\version = "1.0.0"
        \\
        \\[verification]
        \\timeout = 60
        \\parallel = false
    ;

    var config = try Config.parse(std.testing.allocator, toml);
    defer config.deinit();

    try std.testing.expectEqualStrings("test-project", config.project.name);
    try std.testing.expectEqualStrings("1.0.0", config.project.version);
    try std.testing.expectEqual(@as(u32, 60), config.verification.timeout);
    try std.testing.expect(!config.verification.parallel);
}

test "Config.generateDefault" {
    const content = Config.generateDefault("myproject");
    try std.testing.expect(content.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, content, "[project]") != null);
}
