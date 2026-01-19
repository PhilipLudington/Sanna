# Sanna Language Specification

> **"Truth by design."**

Sanna is a specification language for AI-assisted software development. It combines formal specifications, mechanical verification, confidence tracking, and provenance into a unified system that targets Klar and Kira for implementation.

---

## Table of Contents

1. [Design Philosophy](#design-philosophy)
2. [Architecture Overview](#architecture-overview)
3. [Lexical Elements](#lexical-elements)
4. [Specification Syntax](#specification-syntax)
5. [Verification System](#verification-system)
6. [Confidence and Provenance](#confidence-and-provenance)
7. [Incremental Generation](#incremental-generation)
8. [Standard Specifications](#standard-specifications)
9. [Tooling Integration](#tooling-integration)
10. [Complete Examples](#complete-examples)

---

## Design Philosophy

### Core Principles

1. **Specifications are source of truth** — implementation is derived artifact
2. **Verify what you can, flag what you can't** — mechanical proof where possible
3. **Explicit uncertainty** — AI reports confidence, humans review intelligently
4. **Full provenance** — know who wrote what and why
5. **Incremental everything** — partial specs, partial implementations, partial proofs
6. **Target agnostic** — generates Klar (imperative) or Kira (functional)

### The Problem Sanna Solves

Traditional development:
```
Human writes code → Human writes tests → Tests pass → Hope it's correct
```

AI-assisted development today:
```
Human describes intent → AI generates code → Human reviews everything → Hope AI understood
```

Sanna development:
```
Human writes spec → AI generates implementation → Verifier proves correctness
                                                → Unproven parts flagged for review
                                                → Confidence guides human attention
```

### Key Insight

**Proof eliminates review burden.** If the verifier proves a function meets its specification, no human review is needed for correctness (only for spec accuracy). Human attention focuses on:

1. Is the specification correct? (what we want)
2. What couldn't be proven? (where AI might be wrong)
3. Low confidence areas (where AI is uncertain)

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                        SANNA SPECIFICATION                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │   Types     │  │  Contracts  │  │ Invariants  │                 │
│  │  & Models   │  │  (pre/post) │  │  & Axioms   │                 │
│  └─────────────┘  └─────────────┘  └─────────────┘                 │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      AI GENERATION ENGINE                           │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  Generates implementation in Klar or Kira                    │   │
│  │  Annotates confidence per block                              │   │
│  │  Records provenance metadata                                 │   │
│  │  Supports incremental/partial generation                     │   │
│  └─────────────────────────────────────────────────────────────┘   │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                       VERIFICATION ENGINE                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │    SMT      │  │  Abstract   │  │   Model     │                 │
│  │   Solver    │  │Interpretation│  │  Checking   │                 │
│  └─────────────┘  └─────────────┘  └─────────────┘                 │
│                              │                                      │
│                              ▼                                      │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  Proof Obligations: proven | unproven | timeout | unknown    │   │
│  └─────────────────────────────────────────────────────────────┘   │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         TRUST REPORT                                │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  Per-function trust score combining:                         │   │
│  │    - Verification status (proven/unproven)                   │   │
│  │    - AI confidence (0.0 - 1.0)                               │   │
│  │    - Provenance (human/AI/reviewed)                          │   │
│  │                                                              │   │
│  │  Human review queue sorted by trust score                    │   │
│  └─────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Lexical Elements

### File Extensions

- `.sanna` — specification files
- `.sanna.kl` — Sanna-annotated Klar implementation
- `.sanna.ki` — Sanna-annotated Kira implementation

### Keywords

```
// Specification
spec     type     model    invariant    axiom
requires ensures  modifies pure         effect
forall   exists   such_that old         result

// Verification
proven   unproven assume   assert       lemma
trusted  unsafe   admits

// Provenance & Confidence
author   confidence  reviewed  generated
needs_review  approved  deprecated

// Structure
import   module   pub   impl   for
where    if       then  else   match
let      in       and   or    not
true     false
```

### Operators

```
// Logical (specification)
=>      <=>     and     or      not
forall  exists

// Comparison
==      !=      <       >       <=      >=

// Arithmetic
+       -       *       /       %

// Set/Collection
in      union   intersect   subset   empty

// Specification-specific
old(x)      // value of x at function entry
result      // return value
@           // attribute prefix
|           // such that (in comprehensions)
..          // range
```

### Literals

```sanna
// Numbers
42              // integer
3.14            // decimal
0xff            // hex

// Strings
"hello"         // string literal

// Boolean
true
false

// Collections
[1, 2, 3]                   // sequence
{1, 2, 3}                   // set
{x: i32 | x > 0 and x < 10} // set comprehension
{"a": 1, "b": 2}            // map literal
```

---

## Specification Syntax

### Module Declaration

```sanna
// payment.sanna
module payment

import std.money.{Money, Currency}
import std.time.{DateTime, Duration}
```

### Type Specifications

Types in Sanna are abstract models. They define *what* data represents, not *how* it's stored.

```sanna
// Abstract type with invariants
type Email = string
  invariant:
    self.contains("@")
    self.length > 0
    self.length < 255

// Refined types
type PositiveInt = i32
  invariant: self > 0

type Percentage = f64
  invariant: self >= 0.0 and self <= 100.0

// Product types (records)
type User = {
    id: UserId,
    email: Email,
    created_at: DateTime,
    balance: Money
}
  invariant:
    self.balance >= Money.zero()
    self.created_at <= DateTime.now()

// Sum types
type PaymentStatus =
    | Pending
    | Completed { completed_at: DateTime, receipt: Receipt }
    | Failed { error: PaymentError, failed_at: DateTime }
    | Refunded { original: Receipt, refund: Receipt }

// Generic types
type NonEmptyList[T] = List[T]
  invariant: self.length > 0

type SortedList[T: Ord] = List[T]
  invariant: forall i, j in 0..self.length:
    i < j => self[i] <= self[j]
```

### Model Types

Models are abstract mathematical objects used in specifications. They don't directly map to runtime types but help express properties.

```sanna
// Mathematical models for specification
model Set[T] {
    empty: Set[T]
    insert(self, elem: T) -> Set[T]
    contains(self, elem: T) -> bool
    union(self, other: Set[T]) -> Set[T]
    size(self) -> Nat

    axiom empty_contains:
        not empty.contains(x)

    axiom insert_contains:
        self.insert(x).contains(y) <=> (x == y or self.contains(y))

    axiom insert_size:
        not self.contains(x) => self.insert(x).size == self.size + 1
}

model Sequence[T] {
    empty: Sequence[T]
    cons(head: T, tail: Sequence[T]) -> Sequence[T]
    append(self, other: Sequence[T]) -> Sequence[T]
    length(self) -> Nat
    nth(self, index: Nat) -> Option[T]

    axiom empty_length:
        empty.length == 0

    axiom cons_length:
        cons(x, xs).length == xs.length + 1

    axiom append_length:
        self.append(other).length == self.length + other.length
}
```

### Function Specifications

```sanna
// Pure function specification
spec fn divide(a: i32, b: i32) -> i32
  requires:
    b != 0
  ensures:
    result * b + (a % b) == a

// Function with effects
spec fn transfer(from: Account, to: Account, amount: Money) -> Result[Receipt, TransferError]
  requires:
    amount > Money.zero()
    from != to
    from.balance >= amount
  ensures:
    match result {
        Ok(receipt) =>
            from.balance == old(from.balance) - amount
            and to.balance == old(to.balance) + amount
            and receipt.amount == amount
        Err(_) =>
            from.balance == old(from.balance)
            and to.balance == old(to.balance)
    }
  modifies:
    from.balance
    to.balance

// Generic function
spec fn sort[T: Ord](items: List[T]) -> List[T]
  ensures:
    result.is_permutation_of(items)
    result.is_sorted()
    result.length == items.length

// Higher-order function
spec fn map[A, B](list: List[A], f: fn(A) -> B) -> List[B]
  ensures:
    result.length == list.length
    forall i in 0..list.length:
      result[i] == f(list[i])
```

### Interface Specifications

```sanna
// Specify an entire interface
spec interface PaymentProcessor {
    type Config
    type Error

    fn initialize(config: Config) -> Result[Self, Error]

    fn charge(self, card: Card, amount: Money) -> Result[Receipt, Error]
      requires:
        amount > Money.zero()
        not card.is_expired()
      ensures:
        Ok(receipt) => receipt.amount == amount

    fn refund(self, receipt: Receipt) -> Result[void, Error]
      requires:
        receipt.age() < Duration.days(90)
        not receipt.already_refunded
      ensures:
        Ok(_) => receipt.status == RefundStatus.Refunded

    // Interface-level invariant
    invariant:
        self.total_charged >= self.total_refunded
}
```

### Invariants and Axioms

```sanna
// Module-level invariants (must always hold)
invariant money_conservation:
    forall accounts: Set[Account]:
        sum(a.balance for a in accounts) == total_money_in_system

// Axioms (assumed true, not proven)
axiom current_time_monotonic:
    forall t1, t2: DateTime:
        t1.before(t2) => not t2.before(t1)

// Lemmas (proven from axioms and other lemmas)
lemma sorted_head_is_min[T: Ord](list: SortedList[T]):
    list.length > 0 => forall x in list: list[0] <= x
```

---

## Verification System

### Proof Obligations

Every specification generates proof obligations. The verifier attempts to discharge them.

```sanna
spec fn binary_search(arr: SortedList[i32], target: i32) -> Option[usize]
  ensures:
    match result {
        Some(i) => arr[i] == target                    // PO1: found_correct
        None => forall i in 0..arr.length: arr[i] != target  // PO2: not_found_correct
    }
```

Generates proof obligations:
- `PO1_found_correct`: If we return `Some(i)`, then `arr[i] == target`
- `PO2_not_found_correct`: If we return `None`, target is not in array

### Verification Statuses

```sanna
// After verification, each obligation has a status:

@verified(proven)           // SMT solver proved it
@verified(unproven)         // Could not prove (may be correct, needs review)
@verified(timeout)          // Solver timed out
@verified(unknown)          // Solver returned unknown
@verified(admitted)         // Explicitly assumed without proof (dangerous)
```

### Trusted Blocks

Sometimes proofs are impractical. Mark code as trusted (requires justification):

```sanna
// In implementation
@trusted(reason = "FFI call to verified C library")
fn fast_sort(arr: &mut [i32]) {
    unsafe { c_quicksort(arr.as_ptr(), arr.len()) }
}

@trusted(reason = "Performance-critical, manually reviewed by @alice on 2024-01-15")
fn matrix_multiply(a: Matrix, b: Matrix) -> Matrix {
    // ... optimized implementation
}
```

### Verification Hints

Help the verifier with hints when automatic proof fails:

```sanna
spec fn factorial(n: Nat) -> Nat
  ensures:
    result >= 1
    n > 0 => result >= n
  decreases: n  // termination hint

// In implementation, add assertions as hints
fn factorial(n: Nat) -> Nat {
    if n == 0 {
        return 1
    }
    let prev = factorial(n - 1)
    @hint(prev >= 1)  // help the verifier
    @hint(n * prev >= n)
    return n * prev
}
```

### Ghost State

Specification-only state for verification (doesn't exist at runtime):

```sanna
spec interface Counter {
    ghost history: Sequence[i32]  // tracks all values, only for spec

    fn increment(self) -> void
      ensures:
        self.value == old(self.value) + 1
        self.history == old(self.history).append(self.value)

    fn get(self) -> i32
      ensures:
        result == self.value
        self.history == old(self.history)  // get doesn't modify history
}
```

---

## Confidence and Provenance

### Provenance Tracking

Every code block tracks its origin:

```sanna
// Provenance attributes
@author(human, identity = "alice@example.com")
@author(ai, model = "opus-4.5", prompt_hash = "a1b2c3...")
@author(ai, model = "opus-4.5", reviewed_by = "bob@example.com")

// Applied to specifications
@author(human, identity = "alice@example.com")
spec fn validate_email(email: string) -> bool
  ensures:
    result == true => email.is_valid_email_format()
```

### Confidence Annotations

AI annotates confidence when generating implementations:

```sanna
// Confidence scale: 0.0 (no confidence) to 1.0 (certain)

@confidence(0.95)  // AI very confident
fn simple_add(a: i32, b: i32) -> i32 {
    return a + b
}

@confidence(0.60)  // AI uncertain
@needs_review(reason = "Complex date arithmetic with timezones")
fn calculate_billing_period(start: DateTime, subscription: Plan) -> DateRange {
    // ...
}

@confidence(0.30)  // AI quite unsure
@needs_review(reason = "Unclear requirements for edge case")
@todo("Confirm behavior when user has multiple active subscriptions")
fn determine_discount(user: User) -> Percentage {
    // ...
}
```

### Trust Score Calculation

The system computes a trust score combining all factors:

```sanna
// Trust score formula (0.0 to 1.0):

trust_score =
    if verified == proven then
        0.9 + (0.1 * provenance_factor)  // Proof dominates
    else
        confidence * provenance_factor * verification_factor

where:
    provenance_factor =
        | human_authored         -> 1.0
        | ai_human_reviewed      -> 0.9
        | ai_unreviewed          -> 0.7

    verification_factor =
        | proven                 -> 1.0   (but we use the if branch above)
        | unproven               -> 0.5
        | timeout                -> 0.4
        | admitted               -> 0.3
```

### Review Queue

Functions sorted by trust score for human review:

```
┌─────────────────────────────────────────────────────────────────────┐
│  REVIEW QUEUE                                        Filter: all    │
├─────────────────────────────────────────────────────────────────────┤
│  ⚠️  determine_discount          trust: 0.21  [unproven, conf:0.30] │
│  ⚠️  calculate_billing_period    trust: 0.42  [unproven, conf:0.60] │
│  ℹ️  process_refund              trust: 0.63  [proven, conf:0.90]   │
│  ✓  validate_email              trust: 0.95  [proven, conf:0.95]   │
│  ✓  simple_add                  trust: 0.98  [proven, conf:0.95]   │
└─────────────────────────────────────────────────────────────────────┘
```

### Approval Workflow

```sanna
// Before review
@author(ai, model = "opus-4.5")
@confidence(0.60)
@verified(unproven, obligations = ["timezone_correctness"])
fn calculate_billing_period(...) { ... }

// After human review
@author(ai, model = "opus-4.5")
@confidence(0.60)
@verified(unproven, obligations = ["timezone_correctness"])
@approved(
    by = "alice@example.com",
    at = "2024-01-15T10:30:00Z",
    note = "Reviewed timezone handling, edge cases acceptable for MVP"
)
fn calculate_billing_period(...) { ... }
```

---

## Incremental Generation

### Partial Specifications

Specs can have holes that are refined later:

```sanna
// Partial spec with holes
spec fn process_order(order: Order) -> Result[Receipt, OrderError]
  requires:
    order.items.length > 0
    ???  // TODO: inventory requirements
  ensures:
    Ok(receipt) => receipt.total == order.calculated_total()
    ???  // TODO: define error conditions
```

### Partial Implementations

Implementations can have typed holes:

```sanna
// Implementation with holes (type-checks, doesn't compile)
fn process_order(order: Order) -> Result[Receipt, OrderError] {
    let validated = validate_order(order)?

    let inventory_result = ???[Result[void, InventoryError]]  // typed hole

    let receipt = create_receipt(validated)
    return Ok(receipt)
}
```

### Incremental Verification

Verify what's complete, track what's pending:

```
┌─────────────────────────────────────────────────────────────────────┐
│  VERIFICATION STATUS: process_order                                 │
├─────────────────────────────────────────────────────────────────────┤
│  Specification:     80% complete (2 holes remaining)                │
│  Implementation:    60% complete (1 hole remaining)                 │
│  Proof obligations:                                                 │
│    ✓ items_not_empty        proven                                  │
│    ✓ receipt_total_correct  proven                                  │
│    ◯ inventory_sufficient   blocked (impl hole)                     │
│    ◯ error_conditions       blocked (spec hole)                     │
└─────────────────────────────────────────────────────────────────────┘
```

### Generation Checkpoints

AI can checkpoint during generation:

```sanna
@generation_checkpoint(
    model = "opus-4.5",
    timestamp = "2024-01-15T10:30:00Z",
    context_hash = "abc123...",
    notes = "Completed happy path, starting error handling"
)
fn process_order(order: Order) -> Result[Receipt, OrderError] {
    // completed section
    let validated = validate_order(order)?
    let receipt = create_receipt(validated)

    @generating  // AI is currently generating from here
    ???
}
```

---

## Standard Specifications

Sanna includes standard specs for common patterns:

### std.collections

```sanna
module std.collections

spec interface List[T] {
    fn new() -> Self
      ensures: result.length == 0

    fn push(self, item: T) -> void
      ensures:
        self.length == old(self.length) + 1
        self[self.length - 1] == item
        forall i in 0..old(self.length): self[i] == old(self[i])

    fn pop(self) -> Option[T]
      ensures:
        old(self.length) == 0 => result == None
        old(self.length) > 0 =>
            result == Some(old(self[self.length - 1]))
            and self.length == old(self.length) - 1

    fn get(self, index: usize) -> Option[T]
      ensures:
        index >= self.length => result == None
        index < self.length => result == Some(self[index])

    pure fn length(self) -> usize

    invariant:
        self.length >= 0
}

spec interface Map[K: Eq + Hash, V] {
    fn new() -> Self
      ensures: result.size == 0

    fn insert(self, key: K, value: V) -> Option[V]
      ensures:
        self.contains_key(key)
        self.get(key) == Some(value)
        old(self.contains_key(key)) => result == old(self.get(key))
        not old(self.contains_key(key)) =>
            result == None and self.size == old(self.size) + 1

    fn get(self, key: K) -> Option[V]
      ensures:
        self.contains_key(key) <=> result.is_some()

    pure fn size(self) -> usize
    pure fn contains_key(self, key: K) -> bool
}
```

### std.result

```sanna
module std.result

type Result[T, E] =
    | Ok(T)
    | Err(E)

spec fn map[T, E, U](self: Result[T, E], f: fn(T) -> U) -> Result[U, E]
  ensures:
    match old(self) {
        Ok(t) => result == Ok(f(t))
        Err(e) => result == Err(e)
    }

spec fn and_then[T, E, U](self: Result[T, E], f: fn(T) -> Result[U, E]) -> Result[U, E]
  ensures:
    match old(self) {
        Ok(t) => result == f(t)
        Err(e) => result == Err(e)
    }

spec fn unwrap_or[T, E](self: Result[T, E], default: T) -> T
  ensures:
    match self {
        Ok(t) => result == t
        Err(_) => result == default
    }
```

### std.ordering

```sanna
module std.ordering

type Ordering = Less | Equal | Greater

spec interface Ord: Eq {
    pure fn compare(self, other: Self) -> Ordering

    axiom reflexive:
        self.compare(self) == Equal

    axiom antisymmetric:
        self.compare(other) == Less <=> other.compare(self) == Greater

    axiom transitive:
        self.compare(b) == Less and b.compare(c) == Less
            => self.compare(c) == Less
}
```

---

## Tooling Integration

### CLI Commands

```bash
# Initialize a Sanna project
sanna init myproject

# Check specifications (syntax and consistency)
sanna check

# Generate implementation from specs
sanna generate --target klar    # or --target kira
sanna generate --model opus-4.5 # specific AI model

# Verify implementation against specs
sanna verify
sanna verify --timeout 60       # per-obligation timeout in seconds
sanna verify --obligations PO1,PO2  # specific obligations

# Show trust report
sanna trust
sanna trust --below 0.8         # only show low-trust items

# Show review queue
sanna review
sanna review --unproven         # only unproven
sanna review --low-confidence 0.7

# Approve reviewed code
sanna approve src/payment.sanna.kl --note "Reviewed by Alice"

# Interactive mode
sanna repl
```

### Project Structure

```
myproject/
├── sanna.toml              # Project configuration
├── specs/
│   ├── payment.sanna       # Specifications
│   ├── user.sanna
│   └── inventory.sanna
├── src/
│   ├── payment.sanna.kl    # Generated Klar implementation
│   ├── user.sanna.kl
│   └── inventory.sanna.kl
├── proofs/
│   └── .sanna-cache/       # Cached proof results
└── reports/
    └── trust-report.json   # Latest trust report
```

### Configuration (sanna.toml)

```toml
[project]
name = "myproject"
version = "1.0.0"

[generation]
target = "klar"              # or "kira"
default_model = "opus-4.5"
confidence_threshold = 0.7   # warn below this

[verification]
timeout = 30                 # seconds per obligation
solver = "z3"                # or "cvc5"
parallel = true

[trust]
require_review_below = 0.6   # block deployment below this
auto_approve_above = 0.95    # auto-approve above this (proven + high confidence)

[provenance]
require_identity = true      # all code must have author
allowed_models = ["opus-4.5", "sonnet-3.5"]
```

### IDE Integration

```
┌─────────────────────────────────────────────────────────────────────┐
│  payment.sanna                                                      │
├─────────────────────────────────────────────────────────────────────┤
│  1  spec fn charge(card: Card, amount: Money) -> Result[Receipt]   │
│  2    requires:                                                     │
│  3      amount > Money.zero()      ✓ verified                       │
│  4      not card.is_expired()      ✓ verified                       │
│  5    ensures:                                                      │
│  6      Ok(r) => r.amount == amount   ⚠️ unproven (click for details)│
│  7                                                                  │
│  8  // Hover shows:                                                 │
│  9  // - Proof obligation status                                    │
│ 10  // - AI confidence: 0.85                                        │
│ 11  // - Author: ai/opus-4.5, unreviewed                           │
│ 12  // - Trust score: 0.60                                          │
│ 13  // - [Approve] [Request Review] [View Proof Attempt]           │
└─────────────────────────────────────────────────────────────────────┘
```

### CI/CD Integration

```yaml
# .github/workflows/sanna.yml
name: Sanna Verification

on: [push, pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Sanna
        run: curl -sSf https://sanna-lang.org/install.sh | sh

      - name: Check specifications
        run: sanna check

      - name: Verify implementation
        run: sanna verify

      - name: Trust gate
        run: sanna trust --require 0.6 --fail-below

      - name: Upload trust report
        uses: actions/upload-artifact@v3
        with:
          name: trust-report
          path: reports/trust-report.json
```

---

## Complete Examples

### Example 1: User Authentication

```sanna
// specs/auth.sanna
module auth

import std.crypto.{Hash, Salt}
import std.time.{DateTime, Duration}

// Types with invariants
type Password = string
  invariant:
    self.length >= 8
    self.has_uppercase()
    self.has_lowercase()
    self.has_digit()

type PasswordHash = {
    hash: Hash,
    salt: Salt,
    algorithm: string,
    created_at: DateTime
}

type SessionToken = string
  invariant:
    self.length == 64
    self.is_hex()

type Session = {
    token: SessionToken,
    user_id: UserId,
    created_at: DateTime,
    expires_at: DateTime
}
  invariant:
    self.expires_at > self.created_at

// Specifications
spec fn hash_password(password: Password) -> PasswordHash
  ensures:
    result.algorithm == "bcrypt"
    result.created_at <= DateTime.now()
    // Cannot reverse hash to password (expressed as: no function exists)
    not exists(fn(PasswordHash) -> Password)

spec fn verify_password(password: string, hash: PasswordHash) -> bool
  ensures:
    result == true <=> password == original_password(hash)
  // Note: original_password is a ghost function for specification only

spec fn create_session(user: User) -> Session
  ensures:
    result.user_id == user.id
    result.expires_at == result.created_at + Duration.hours(24)
    result.token.is_cryptographically_random()

spec fn authenticate(email: Email, password: string) -> Result[Session, AuthError]
  requires:
    email.length > 0
  ensures:
    match result {
        Ok(session) =>
            exists user: User:
                user.email == email
                and verify_password(password, user.password_hash)
                and session.user_id == user.id
        Err(AuthError.InvalidCredentials) =>
            not exists user: User:
                user.email == email
                and verify_password(password, user.password_hash)
        Err(AuthError.UserNotFound) =>
            not exists user: User: user.email == email
    }

// Security invariants
invariant no_plaintext_passwords:
    forall log_entry: LogEntry:
        not log_entry.message.contains_password_pattern()

invariant session_tokens_unique:
    forall s1, s2: Session:
        s1 != s2 => s1.token != s2.token

invariant rate_limiting:
    forall ip: IpAddress, window: Duration:
        count(failed_attempts(ip, window)) > 5
            => is_rate_limited(ip)
```

Generated implementation with annotations:

```klar
// src/auth.sanna.kl
module auth

@author(ai, model = "opus-4.5")
@verified(proven)
@confidence(0.95)
fn hash_password(password: Password) -> PasswordHash {
    let salt: Salt = crypto.generate_salt(16)
    let hash: Hash = crypto.bcrypt(password, salt, cost: 12)
    return PasswordHash {
        hash: hash,
        salt: salt,
        algorithm: "bcrypt",
        created_at: DateTime.now()
    }
}

@author(ai, model = "opus-4.5")
@verified(proven)
@confidence(0.95)
fn verify_password(password: string, stored: PasswordHash) -> bool {
    let computed: Hash = crypto.bcrypt(password, stored.salt, cost: 12)
    return crypto.constant_time_compare(computed, stored.hash)
}

@author(ai, model = "opus-4.5")
@verified(proven)
@confidence(0.90)
fn create_session(user: User) -> Session {
    let token: SessionToken = crypto.random_hex(64)
    let now: DateTime = DateTime.now()
    return Session {
        token: token,
        user_id: user.id,
        created_at: now,
        expires_at: now + Duration.hours(24)
    }
}

@author(ai, model = "opus-4.5")
@verified(partial, unproven = ["rate_limiting_applied"])
@confidence(0.75)
@needs_review(reason = "Rate limiting integration with external service")
fn authenticate(email: Email, password: string) -> Result[Session, AuthError] {
    // Check rate limiting
    let ip: IpAddress = request.ip()
    if rate_limiter.is_blocked(ip) {
        return Err(AuthError.RateLimited)
    }

    // Find user
    let user_result: Option[User] = db.find_user_by_email(email)
    match user_result {
        None => {
            rate_limiter.record_failure(ip)
            return Err(AuthError.UserNotFound)
        }
        Some(user) => {
            // Verify password
            if verify_password(password, user.password_hash) {
                rate_limiter.record_success(ip)
                let session: Session = create_session(user)
                return Ok(session)
            } else {
                rate_limiter.record_failure(ip)
                return Err(AuthError.InvalidCredentials)
            }
        }
    }
}
```

### Example 2: Banking Transfer

```sanna
// specs/banking.sanna
module banking

type AccountId = i64
  invariant: self > 0

type Money = {
    cents: i64,
    currency: Currency
}
  invariant: self.cents >= 0

type Account = {
    id: AccountId,
    owner: UserId,
    balance: Money,
    status: AccountStatus
}
  invariant:
    self.balance.cents >= 0
    self.status == Active => self.balance.currency != Currency.None

type Transfer = {
    id: TransferId,
    from_account: AccountId,
    to_account: AccountId,
    amount: Money,
    status: TransferStatus,
    initiated_at: DateTime,
    completed_at: Option[DateTime]
}
  invariant:
    self.from_account != self.to_account
    self.amount.cents > 0
    self.completed_at.is_some() => self.completed_at.unwrap() >= self.initiated_at

// Ghost state for verification
ghost all_transfers: Set[Transfer]
ghost system_total: Money

spec fn transfer(from: AccountId, to: AccountId, amount: Money) -> Result[Transfer, TransferError]
  requires:
    from != to
    amount.cents > 0
  ensures:
    match result {
        Ok(transfer) =>
            // Accounts updated correctly
            get_account(from).balance == old(get_account(from).balance) - amount
            and get_account(to).balance == old(get_account(to).balance) + amount
            // Transfer recorded
            and transfer in all_transfers
            and transfer.status == TransferStatus.Completed
            // Money conserved
            and system_total == old(system_total)

        Err(TransferError.InsufficientFunds) =>
            old(get_account(from).balance) < amount
            and get_account(from).balance == old(get_account(from).balance)
            and get_account(to).balance == old(get_account(to).balance)

        Err(TransferError.AccountNotFound(id)) =>
            not account_exists(id)
            // No state changes
            and get_account(from).balance == old(get_account(from).balance)
            and get_account(to).balance == old(get_account(to).balance)
    }
  modifies:
    get_account(from).balance
    get_account(to).balance
    all_transfers

// Critical system invariant
invariant money_conservation:
    sum(account.balance for account in all_accounts) == system_total

invariant no_negative_balances:
    forall account in all_accounts:
        account.balance.cents >= 0

// Atomicity requirement (for implementation guidance)
axiom transfer_atomicity:
    // Either both balance changes happen, or neither
    forall t: Transfer:
        (t.status == Completed =>
            balance_changed(t.from_account) and balance_changed(t.to_account))
        and
        (t.status == Failed =>
            not balance_changed(t.from_account) and not balance_changed(t.to_account))
```

### Example 3: Sorted List Insertion (Functional)

```sanna
// specs/sorted_list.sanna
module sorted_list

type SortedList[T: Ord] = List[T]
  invariant:
    forall i, j in 0..self.length:
      i < j => self[i] <= self[j]

spec fn insert[T: Ord](list: SortedList[T], elem: T) -> SortedList[T]
  ensures:
    // Result is sorted (maintained by type invariant)
    // Length increased by 1
    result.length == list.length + 1
    // Element is in result
    elem in result
    // All original elements preserved
    forall x in list: x in result
    // No extra elements
    forall x in result: x in list or x == elem

spec fn remove[T: Ord + Eq](list: SortedList[T], elem: T) -> SortedList[T]
  ensures:
    elem in list =>
        result.length == list.length - 1
        and not (elem in result and count(elem, result) == count(elem, list))
    not (elem in list) =>
        result == list

spec fn merge[T: Ord](a: SortedList[T], b: SortedList[T]) -> SortedList[T]
  ensures:
    result.length == a.length + b.length
    forall x in a: x in result
    forall x in b: x in result
    forall x in result: x in a or x in b
```

Generated Kira implementation:

```kira
// src/sorted_list.sanna.ki
module sorted_list

import std.list.{ List, Cons, Nil }

@author(ai, model = "opus-4.5")
@verified(proven)
@confidence(0.95)
let insert[T: Ord]: fn(SortedList[T], T) -> SortedList[T] =
  fn(list: SortedList[T], elem: T) -> SortedList[T] {
    match list {
        Nil => {
            return Cons(elem, Nil)
        }
        Cons(head, tail) => {
            if elem <= head {
                return Cons(elem, list)
            }
            let rest: SortedList[T] = insert[T](tail, elem)
            return Cons(head, rest)
        }
    }
}

@author(ai, model = "opus-4.5")
@verified(proven)
@confidence(0.90)
let merge[T: Ord]: fn(SortedList[T], SortedList[T]) -> SortedList[T] =
  fn(a: SortedList[T], b: SortedList[T]) -> SortedList[T] {
    match (a, b) {
        (Nil, _) => { return b }
        (_, Nil) => { return a }
        (Cons(ha, ta), Cons(hb, tb)) => {
            if ha <= hb {
                let rest: SortedList[T] = merge[T](ta, b)
                return Cons(ha, rest)
            }
            let rest: SortedList[T] = merge[T](a, tb)
            return Cons(hb, rest)
        }
    }
}
```

---

## Appendix: Trust Score Details

### Full Trust Calculation

```
trust(function) =
    base_trust(verification_status, confidence)
    * provenance_modifier(author, review_status)
    * age_modifier(last_modified, last_verified)
    * criticality_modifier(security_sensitive, money_handling, etc.)

where:
    base_trust(proven, _) = 0.95
    base_trust(unproven, conf) = conf * 0.7
    base_trust(timeout, conf) = conf * 0.5
    base_trust(admitted, _) = 0.3

    provenance_modifier(human, _) = 1.0
    provenance_modifier(ai, reviewed) = 0.95
    provenance_modifier(ai, approved) = 0.90
    provenance_modifier(ai, unreviewed) = 0.80

    age_modifier = decay over time since last verification

    criticality_modifier = reduced trust for security-critical code
```

### Trust Thresholds

| Trust Score | Status | Action |
|-------------|--------|--------|
| 0.95+ | Excellent | Auto-deploy |
| 0.80-0.95 | Good | Standard review |
| 0.60-0.80 | Caution | Detailed review required |
| 0.40-0.60 | Warning | Senior review + testing |
| Below 0.40 | Critical | Block deployment |

---

## Appendix: Relationship to Klar and Kira

Sanna specifications target either Klar or Kira for implementation:

| Aspect | Sanna | Klar | Kira |
|--------|-------|------|------|
| Purpose | Specification | Imperative implementation | Functional implementation |
| Abstraction | What | How (with mutation) | How (pure) |
| Types | Abstract models | Concrete with ownership | Concrete with effects |
| Verification | Proof obligations | Type checking | Type + effect checking |
| Runtime | None | Yes | Yes |

### Target Selection Heuristics

Generate **Klar** when:
- Spec involves mutable state
- Performance-critical with fine control needed
- Interfacing with stateful systems
- Team prefers imperative style

Generate **Kira** when:
- Spec is naturally data-transformation
- Purity enables parallelization
- Formal verification is priority
- Team prefers functional style

### Cross-Compilation

Specs can generate both:

```bash
sanna generate --target klar -o src/impl.kl
sanna generate --target kira -o src/impl.ki
```

Both implementations verified against same spec, useful for:
- Comparing approaches
- Gradual migration
- Performance benchmarking

---

*Document version: 1.0*
*Sanna: Truth by design.*
