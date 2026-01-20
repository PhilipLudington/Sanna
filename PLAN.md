# Sanna Language Implementation Plan

This plan outlines the implementation of Sanna, a specification language for AI-assisted software development that combines formal specifications, mechanical verification, confidence tracking, and provenance.

## Phase 1: Core Language Infrastructure

Foundation components for parsing and representing Sanna specifications.

- [x] Define token types for all keywords (spec, type, model, invariant, axiom, requires, ensures, etc.)
- [x] Define token types for operators (logical, comparison, arithmetic, set/collection, specification-specific)
- [x] Implement lexer with support for comments, string literals, and number literals
- [x] Define AST node types for modules, imports, and top-level declarations
- [x] Define AST node types for type definitions (product types, sum types, generic types)
- [x] Define AST node types for specifications (function specs, interface specs)
- [x] Define AST node types for expressions (logical, arithmetic, quantifiers, comprehensions)
- [x] Implement recursive descent parser for module declarations
- [x] Implement parser for type definitions with invariants
- [x] Implement parser for function specifications (requires/ensures/modifies)
- [x] Implement parser for expressions including old(), result, and quantifiers
- [x] Implement error recovery and source location tracking
- [x] Create pretty-printer for AST debugging

## Phase 2: Type System

Type checking with refinement types, invariants, and generics.

- [x] Implement basic type representation (primitives, named types, generic types)
- [x] Implement type environment and scope management
- [x] Implement type inference for expressions
- [x] Implement subtype checking for refinement types
- [x] Implement generic type instantiation and unification
- [x] Implement trait/bound checking (Ord, Eq, Hash constraints)
- [x] Implement invariant collection and association with types
- [x] Implement model type handling (specification-only mathematical objects)
- [x] Implement ghost state tracking (exists only for verification)
- [x] Implement type checking for function specifications
- [x] Implement type checking for interface specifications
- [x] Create type error reporting with source locations

## Phase 3: Specification Semantics

Semantic analysis and interpretation of specifications.

- [x] Implement module resolution and import handling
- [x] Implement specification binding (connecting specs to implementations)
- [x] Implement precondition (requires) semantic analysis
- [x] Implement postcondition (ensures) semantic analysis with old() and result
- [x] Implement modifies clause analysis for frame conditions
- [x] Implement pure function detection and enforcement
- [x] Implement axiom registration and consistency checking
- [x] Implement lemma dependency tracking
- [x] Implement invariant scoping (type-level, interface-level, module-level)
- [x] Implement partial specification handling (??? holes)
- [x] Implement decreases clause for termination checking

## Phase 4: Verification Engine

SMT-based verification and proof obligation management.

- [x] Define proof obligation data structures
- [x] Implement proof obligation generation from function specs
- [x] Implement proof obligation generation from type invariants
- [x] Implement proof obligation generation from axioms and lemmas
- [x] Create SMT-LIB encoding for Sanna types
- [x] Create SMT-LIB encoding for Sanna expressions
- [x] Create SMT-LIB encoding for quantifiers (forall, exists)
- [x] Integrate Z3 solver (via subprocess or bindings)
- [x] Implement verification result handling (proven, unproven, timeout, unknown)
- [x] Implement verification hints (@hint) processing
- [x] Implement trusted block (@trusted) handling with reason tracking
- [x] Implement admitted obligations tracking
- [x] Implement parallel verification of independent obligations
- [x] Implement verification caching (proofs/.sanna-cache/)
- [x] Implement incremental verification for partial implementations

## Phase 5: Provenance and Confidence System

Track code authorship and AI confidence levels.

- [x] Define provenance metadata structures (human, AI, model, identity, etc.)
- [x] Implement @author attribute parsing and storage
- [x] Implement @confidence attribute parsing (0.0 - 1.0 scale)
- [x] Implement @needs_review attribute with reason tracking
- [x] Implement @approved attribute with by, at, and note fields
- [x] Implement @verified attribute with status and obligations
- [x] Implement @generation_checkpoint for incremental AI generation
- [x] Implement @generating marker for in-progress generation
- [x] Create provenance database/storage format
- [x] Implement provenance querying API

## Phase 6: Trust Score System

Calculate and report trust scores combining verification, confidence, and provenance.

- [x] Implement base trust calculation from verification status
- [x] Implement provenance modifier calculation
- [x] Implement age modifier calculation (decay over time)
- [x] Implement criticality modifier (security-sensitive, money-handling flags)
- [x] Implement composite trust score formula
- [x] Generate per-function trust reports
- [x] Implement review queue generation sorted by trust score
- [x] Implement filtering (unproven only, below threshold, etc.)
- [x] Generate trust-report.json output format
- [x] Implement trust thresholds (auto-approve, require-review, block-deployment)

## Phase 7: Code Generation Interface

Interface for AI-powered implementation generation.

- [x] Define generation request format (spec + target language + model)
- [x] Define generation response format (code + confidence + provenance)
- [x] Implement target language selection (Klar vs Kira)
- [x] Implement generation context preparation (spec + related types + examples)
- [x] Implement generated code parsing and AST integration
- [x] Implement automatic annotation insertion (@author, @confidence, etc.)
- [x] Implement typed hole generation (???) for partial implementations
- [x] Implement checkpoint serialization for resumable generation
- [x] Create stub/mock generator for testing without AI

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
