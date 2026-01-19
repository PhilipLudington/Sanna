# Sanna Language Implementation Plan

This plan outlines the implementation of Sanna, a specification language for AI-assisted software development that combines formal specifications, mechanical verification, confidence tracking, and provenance.

## Phase 1: Core Language Infrastructure

Foundation components for parsing and representing Sanna specifications.

- [ ] Define token types for all keywords (spec, type, model, invariant, axiom, requires, ensures, etc.)
- [ ] Define token types for operators (logical, comparison, arithmetic, set/collection, specification-specific)
- [ ] Implement lexer with support for comments, string literals, and number literals
- [ ] Define AST node types for modules, imports, and top-level declarations
- [ ] Define AST node types for type definitions (product types, sum types, generic types)
- [ ] Define AST node types for specifications (function specs, interface specs)
- [ ] Define AST node types for expressions (logical, arithmetic, quantifiers, comprehensions)
- [ ] Implement recursive descent parser for module declarations
- [ ] Implement parser for type definitions with invariants
- [ ] Implement parser for function specifications (requires/ensures/modifies)
- [ ] Implement parser for expressions including old(), result, and quantifiers
- [ ] Implement error recovery and source location tracking
- [ ] Create pretty-printer for AST debugging

## Phase 2: Type System

Type checking with refinement types, invariants, and generics.

- [ ] Implement basic type representation (primitives, named types, generic types)
- [ ] Implement type environment and scope management
- [ ] Implement type inference for expressions
- [ ] Implement subtype checking for refinement types
- [ ] Implement generic type instantiation and unification
- [ ] Implement trait/bound checking (Ord, Eq, Hash constraints)
- [ ] Implement invariant collection and association with types
- [ ] Implement model type handling (specification-only mathematical objects)
- [ ] Implement ghost state tracking (exists only for verification)
- [ ] Implement type checking for function specifications
- [ ] Implement type checking for interface specifications
- [ ] Create type error reporting with source locations

## Phase 3: Specification Semantics

Semantic analysis and interpretation of specifications.

- [ ] Implement module resolution and import handling
- [ ] Implement specification binding (connecting specs to implementations)
- [ ] Implement precondition (requires) semantic analysis
- [ ] Implement postcondition (ensures) semantic analysis with old() and result
- [ ] Implement modifies clause analysis for frame conditions
- [ ] Implement pure function detection and enforcement
- [ ] Implement axiom registration and consistency checking
- [ ] Implement lemma dependency tracking
- [ ] Implement invariant scoping (type-level, interface-level, module-level)
- [ ] Implement partial specification handling (??? holes)
- [ ] Implement decreases clause for termination checking

## Phase 4: Verification Engine

SMT-based verification and proof obligation management.

- [ ] Define proof obligation data structures
- [ ] Implement proof obligation generation from function specs
- [ ] Implement proof obligation generation from type invariants
- [ ] Implement proof obligation generation from axioms and lemmas
- [ ] Create SMT-LIB encoding for Sanna types
- [ ] Create SMT-LIB encoding for Sanna expressions
- [ ] Create SMT-LIB encoding for quantifiers (forall, exists)
- [ ] Integrate Z3 solver (via subprocess or bindings)
- [ ] Implement verification result handling (proven, unproven, timeout, unknown)
- [ ] Implement verification hints (@hint) processing
- [ ] Implement trusted block (@trusted) handling with reason tracking
- [ ] Implement admitted obligations tracking
- [ ] Implement parallel verification of independent obligations
- [ ] Implement verification caching (proofs/.sanna-cache/)
- [ ] Implement incremental verification for partial implementations

## Phase 5: Provenance and Confidence System

Track code authorship and AI confidence levels.

- [ ] Define provenance metadata structures (human, AI, model, identity, etc.)
- [ ] Implement @author attribute parsing and storage
- [ ] Implement @confidence attribute parsing (0.0 - 1.0 scale)
- [ ] Implement @needs_review attribute with reason tracking
- [ ] Implement @approved attribute with by, at, and note fields
- [ ] Implement @verified attribute with status and obligations
- [ ] Implement @generation_checkpoint for incremental AI generation
- [ ] Implement @generating marker for in-progress generation
- [ ] Create provenance database/storage format
- [ ] Implement provenance querying API

## Phase 6: Trust Score System

Calculate and report trust scores combining verification, confidence, and provenance.

- [ ] Implement base trust calculation from verification status
- [ ] Implement provenance modifier calculation
- [ ] Implement age modifier calculation (decay over time)
- [ ] Implement criticality modifier (security-sensitive, money-handling flags)
- [ ] Implement composite trust score formula
- [ ] Generate per-function trust reports
- [ ] Implement review queue generation sorted by trust score
- [ ] Implement filtering (unproven only, below threshold, etc.)
- [ ] Generate trust-report.json output format
- [ ] Implement trust thresholds (auto-approve, require-review, block-deployment)

## Phase 7: Code Generation Interface

Interface for AI-powered implementation generation.

- [ ] Define generation request format (spec + target language + model)
- [ ] Define generation response format (code + confidence + provenance)
- [ ] Implement target language selection (Klar vs Kira)
- [ ] Implement generation context preparation (spec + related types + examples)
- [ ] Implement generated code parsing and AST integration
- [ ] Implement automatic annotation insertion (@author, @confidence, etc.)
- [ ] Implement typed hole generation (???) for partial implementations
- [ ] Implement checkpoint serialization for resumable generation
- [ ] Create stub/mock generator for testing without AI

## Phase 8: CLI Implementation

Command-line interface for all Sanna operations.

- [ ] Implement project structure detection and sanna.toml parsing
- [ ] Implement `sanna init` command (create project scaffold)
- [ ] Implement `sanna check` command (syntax and consistency checking)
- [ ] Implement `sanna generate` command with --target and --model flags
- [ ] Implement `sanna verify` command with --timeout and --obligations flags
- [ ] Implement `sanna trust` command with --below filter
- [ ] Implement `sanna review` command with --unproven and --low-confidence filters
- [ ] Implement `sanna approve` command with --note flag
- [ ] Implement `sanna repl` command (interactive mode)
- [ ] Implement progress reporting and colored output
- [ ] Implement JSON output mode for tooling integration

## Phase 9: Standard Library Specifications

Built-in specifications for common patterns.

- [ ] Implement std.collections.List specification
- [ ] Implement std.collections.Map specification
- [ ] Implement std.collections.Set specification
- [ ] Implement std.result.Result specification with map, and_then, unwrap_or
- [ ] Implement std.option.Option specification
- [ ] Implement std.ordering.Ord specification with axioms
- [ ] Implement std.ordering.Eq specification
- [ ] Implement std.money.Money specification
- [ ] Implement std.time.DateTime and Duration specifications
- [ ] Implement std.crypto specifications (Hash, Salt)
- [ ] Create standard library loading mechanism

## Phase 10: IDE Integration

Language Server Protocol support for IDE features.

- [ ] Implement LSP server scaffold
- [ ] Implement textDocument/didOpen and didChange handlers
- [ ] Implement textDocument/diagnostic for syntax and type errors
- [ ] Implement textDocument/hover for trust scores and verification status
- [ ] Implement textDocument/completion for keywords and identifiers
- [ ] Implement textDocument/definition for go-to-definition
- [ ] Implement custom commands for approve/review actions
- [ ] Implement progress notifications for verification
- [ ] Create VS Code extension package
- [ ] Create Neovim plugin configuration

## Phase 11: CI/CD Integration

Continuous integration support and deployment gates.

- [ ] Create GitHub Actions workflow template
- [ ] Implement exit codes for CI (0 = pass, 1 = verification failed, 2 = trust below threshold)
- [ ] Implement --fail-below flag for trust gate
- [ ] Implement trust report artifact generation
- [ ] Create documentation for CI/CD setup
- [ ] Implement incremental verification for PR checks (only verify changed specs)

## Architecture Notes

### File Structure
```
sanna/
├── src/
│   ├── lexer/          # Tokenization
│   ├── parser/         # Parsing and AST
│   ├── types/          # Type system
│   ├── specs/          # Specification semantics
│   ├── verify/         # SMT integration and proof obligations
│   ├── trust/          # Trust scores and review queue
│   ├── provenance/     # Authorship and confidence tracking
│   ├── codegen/        # AI generation interface
│   ├── cli/            # Command-line interface
│   ├── lsp/            # Language server
│   └── stdlib/         # Standard library specs
├── specs/              # Example specifications
├── tests/              # Test suite
└── docs/               # Documentation
```

### Key Design Decisions
1. **Verification before generation**: Specs must type-check before code generation
2. **SMT-first**: Use Z3 as primary solver, with CVC5 as fallback
3. **Incremental by default**: Cache proof results, verify only changed code
4. **Trust is calculated, not stored**: Recompute trust from current verification + provenance

### Dependencies
- SMT solver: Z3 (required), CVC5 (optional)
- Parser generator: Consider tree-sitter for IDE integration
- Serialization: JSON for reports, TOML for config
