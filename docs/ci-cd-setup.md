# Sanna CI/CD Integration Guide

This guide explains how to integrate Sanna specification verification into your CI/CD pipeline.

## Overview

Sanna provides built-in CI/CD support with:

- **Exit codes** designed for CI gatekeeping
- **Incremental verification** for faster PR checks
- **Trust thresholds** for deployment gates
- **JSON reports** for artifact generation

## Exit Codes

All Sanna commands return meaningful exit codes for CI integration:

### `sanna check`
| Exit Code | Meaning |
|-----------|---------|
| 0 | All specifications pass syntax and type checking |
| 1 | Syntax or type errors found |

### `sanna verify`
| Exit Code | Meaning |
|-----------|---------|
| 0 | All proof obligations verified |
| 1 | Some obligations could not be proven |
| 2 | Verification failures (specification violated) |

### `sanna trust`
| Exit Code | Meaning |
|-----------|---------|
| 0 | All trust scores above threshold |
| 1 | Error (invalid arguments, file not found, etc.) |
| 2 | Trust score below deployment threshold |

## Quick Start

### GitHub Actions

Copy the workflow template from `.github/workflows/sanna-ci.yml` to your repository.

The default workflow provides:
1. **Build**: Compiles the Sanna tool
2. **Check**: Validates specification syntax and types
3. **Verify**: Runs SMT verification (incremental on PRs)
4. **Trust**: Enforces trust threshold for deployment

### Basic CI Commands

```bash
# Check all specifications
sanna check --json > check-report.json

# Verify all specifications
sanna verify --json > verification-report.json

# Verify only changed specs (for PRs)
sanna verify --changed --base main --json > verification-report.json

# Generate trust report and enforce threshold
sanna trust --output trust-report.json --fail-below 0.5
```

## Incremental Verification

For pull requests, use `--changed` to only verify specifications that changed since the base branch:

```bash
# Verify specs changed since main branch
sanna verify --changed --base main

# Verify specs changed since develop branch
sanna verify --changed --base origin/develop
```

This significantly speeds up PR verification by skipping unchanged specifications.

**Requirements:**
- Git repository with full history (use `fetch-depth: 0` in checkout)
- Base branch must exist and be accessible

## Trust Thresholds

The `--fail-below` flag enables deployment gating based on trust scores:

```bash
# Fail CI if any spec has trust < 0.5
sanna trust --fail-below 0.5

# Fail CI if any spec has trust < 0.3 (less strict)
sanna trust --fail-below 0.3

# Generate report and check threshold
sanna trust --output trust-report.json --fail-below 0.5
```

### Trust Score Components

Trust scores (0.0 - 1.0) are calculated from:

- **Verification status**: Proven specs get higher scores
- **AI confidence**: Lower confidence reduces trust
- **Author provenance**: Human-authored code may have higher trust
- **Code age**: Older, stable code may decay in trust
- **Criticality flags**: Security-sensitive code is scrutinized more

### Recommended Thresholds

| Threshold | Use Case |
|-----------|----------|
| 0.5 | Minimum for deployment (blocks critically unverified code) |
| 0.6 | Standard development (requires review for low-confidence code) |
| 0.85 | Strict environments (most code needs review) |
| 0.95 | Critical systems (only fully verified code deploys) |

## Report Formats

### JSON Output

All commands support `--json` for machine-readable output:

```bash
sanna check --json
sanna verify --json
sanna trust --json
```

### Trust Report File

Generate a trust report file for artifacts:

```bash
sanna trust --output trust-report.json
```

The report includes:
```json
{
  "entries": [
    {
      "name": "OrderTotal",
      "file": "order.sanna",
      "score": 0.6000,
      "needs_review": true,
      "approved": false
    }
  ],
  "summary": {
    "total": 5,
    "needs_review": 2,
    "blocked": 0,
    "average_score": 0.7200,
    "min_score": 0.5500,
    "max_score": 0.9500,
    "can_deploy": true
  }
}
```

## Configuration

### sanna.toml

Configure CI-relevant settings in your project's `sanna.toml`:

```toml
[verification]
timeout = 60        # Seconds per obligation
parallel = true     # Enable parallel verification
cache = true        # Cache proof results

[trust]
require_review_below = 0.6
auto_approve_above = 0.95
block_deployment_below = 0.3
age_decay_enabled = false
```

## CI Pipeline Examples

### GitHub Actions (Full)

See `.github/workflows/sanna-ci.yml` for a complete example.

### GitLab CI

```yaml
stages:
  - build
  - check
  - verify
  - trust

build:
  stage: build
  script:
    - zig build -Doptimize=ReleaseSafe
  artifacts:
    paths:
      - zig-out/bin/sanna

check:
  stage: check
  script:
    - ./zig-out/bin/sanna check --json > check-report.json
  artifacts:
    paths:
      - check-report.json

verify:
  stage: verify
  script:
    - apt-get update && apt-get install -y z3
    - |
      if [ "$CI_PIPELINE_SOURCE" = "merge_request_event" ]; then
        ./zig-out/bin/sanna verify --changed --base origin/$CI_MERGE_REQUEST_TARGET_BRANCH_NAME --json > verification-report.json
      else
        ./zig-out/bin/sanna verify --json > verification-report.json
      fi
  artifacts:
    paths:
      - verification-report.json

trust:
  stage: trust
  script:
    - ./zig-out/bin/sanna trust --output trust-report.json --fail-below 0.5
  artifacts:
    paths:
      - trust-report.json
```

### Jenkins Pipeline

```groovy
pipeline {
    agent any

    environment {
        TRUST_THRESHOLD = '0.5'
    }

    stages {
        stage('Build') {
            steps {
                sh 'zig build -Doptimize=ReleaseSafe'
            }
        }

        stage('Check') {
            steps {
                sh './zig-out/bin/sanna check --json > check-report.json'
                archiveArtifacts artifacts: 'check-report.json'
            }
        }

        stage('Verify') {
            steps {
                script {
                    if (env.CHANGE_TARGET) {
                        sh "./zig-out/bin/sanna verify --changed --base origin/${env.CHANGE_TARGET} --json > verification-report.json"
                    } else {
                        sh './zig-out/bin/sanna verify --json > verification-report.json'
                    }
                }
                archiveArtifacts artifacts: 'verification-report.json'
            }
        }

        stage('Trust Gate') {
            steps {
                sh "./zig-out/bin/sanna trust --output trust-report.json --fail-below ${TRUST_THRESHOLD}"
                archiveArtifacts artifacts: 'trust-report.json'
            }
        }
    }
}
```

## Troubleshooting

### "Failed to get changed files"

Ensure:
1. You're in a git repository
2. The base branch exists: `git fetch origin main`
3. Full history is available (not shallow clone)

### "Z3 solver not found"

Install Z3:
```bash
# Ubuntu/Debian
sudo apt-get install z3

# macOS
brew install z3

# Windows
choco install z3
```

### "Trust score below threshold"

Review the trust report to identify low-scoring specifications:
```bash
sanna trust --below 0.6  # Show items needing review
sanna review --unproven  # List unproven specs
```

Consider:
1. Adding more specifications for unverified code
2. Marking reviewed code with `@approved`
3. Improving AI confidence with better prompts
4. Lowering the threshold (not recommended for production)

## Best Practices

1. **Start with lenient thresholds** (0.3-0.5) and tighten over time
2. **Use incremental verification** for PRs to keep CI fast
3. **Review trust reports** regularly to identify problem areas
4. **Configure appropriate timeouts** for your verification complexity
5. **Cache verification results** to speed up repeated runs
6. **Set up required status checks** in your repository settings
