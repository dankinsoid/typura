# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Typura is a static type analyzer for Clojure, inspired by OCaml's type inference. Not a new language (unlike TypedClojure) — a static analysis tool that infers types from context with minimal annotations. Aims to replace clj-kondo's weak type analysis with deep inference.

### Goals
- Infer argument and return types for every function by analyzing usage context
- Gradual typing — type narrowing via predicates (`int?`, `string?`, etc.)
- Hook/stub system for typing external functions (core, libraries)
- Custom validation rules via eval'd meta-code
- Tags for enabling/disabling rule sets (dev vs CI)
- Support JVM Clojure and ClojureScript (ClojureDart later)
- Maximal inference — where type can theoretically be determined, determine it; otherwise `:any`

### Non-Goals
- Not a new language or syntax extension
- Not a runtime type checker (purely static)
- Not a replacement for Malli at runtime

## Project Structure

```
typura/
├── core/          # typura.core (.cljc) — type DSL, schemas, runtime utilities
│                  #   importable library for in-code schema declarations
├── analyzer/      # typura.analyzer — static analysis engine (pure library)
├── lsp/           # (planned) typura.lsp — LSP server, thin wrapper over analyzer
├── cli/           # (planned) typura.cli — CLI tool, thin wrapper over analyzer
├── stubs/         # (planned) type stubs for clojure.core and libraries
└── core/sketch.cljc  # experimental API sketches
```

## Architecture

### Analysis Pipeline
1. Parse source via `clojure.tools.analyzer(.jvm)` — macros already expanded
2. Walk expanded AST, building **type context** (environment)
3. Bidirectional type inference: synthesize bottom-up + check top-down
4. Propagate constraints through call graph
5. Collect diagnostics where inferred type conflicts with expected type in context

### Type Context
Immutable, scoped map enriched during AST traversal:
```clojure
{:bindings  {symbol -> type}       ; local bindings (let, fn args)
 :globals   {var -> type}          ; def'd vars, registered stubs
 :protocols {protocol -> methods}  ; known protocol signatures
 :records   {record -> fields}     ; known record field types
 :rules     [rule-fn ...]          ; active custom rules
 :tags      #{:dev :ci ...}}       ; active tag set
```

### Type Representation
Malli schemas as the type language. Malli supports keywords, Java classes, and predicates as schemas.

| Category | Example |
|---|---|
| Primitives | `:int`, `:string`, `:keyword`, `:boolean`, `:nil` |
| Java classes | `java.lang.String`, `java.util.Map` (from reflection, not hardcoded) |
| Predicates | `int?`, `pos?`, `string?` |
| Collections | `[:vector :int]`, `[:set :keyword]`, `[:map-of :keyword :any]` |
| Typed maps | `[:map [:id :string] [:name :string]]` |
| Functions | `[:=> [:cat :int :int] :int]` |
| Unions | `[:or :int :nil]` |
| Capabilities | `IFn`, `ILookup`, `Indexed`, `Seqable`, `Associative`, `Counted` |
| Schema constructors | `(Array :int)` → `[:vector :int]` (generics as functions) |

**Generics** are schema constructors — functions that take schemas and return schemas:
```clojure
(defschema Array [Element] [:vector Element])
(defschema Result [Ok Err] [:or Ok Err])
(Array :int) ;=> [:vector :int]
```

**Subtype checking**: need to verify if one schema is a subset of another (e.g. `:int` ⊂ `:number`). Malli doesn't do this natively — we'll need a custom subsumption layer.

### Inference Strategy
- **Java interop**: tools.analyzer.jvm provides Java types via reflection
- **Core stubs**: manually written type definitions for clojure.core
- **User stubs/hooks**: type definitions for third-party libraries via eval
- **Flow analysis**: type narrowing in conditional branches
- **Constraint propagation**: bidirectional — if `+` expects Number, callers must provide Number
- **Fallback**: unresolvable types become `:any` (no errors for unknowns)

### Hardcoded Special Forms
These are handled directly by the analyzer (not via stubs):
`if`, `let`, `def`, `defn`, `fn`, `do`, `loop`/`recur`, `try`/`catch`, `throw`, `var`, `quote`, `new`, `.method`, `set!`

`atom` — also hardcoded (wraps inner type).

### Hook System
Clojure files evaluated via `eval` that modify the type context:
- Register function signatures for a library
- Add custom lint/validation rules
- Arbitrary context modifications
- Tagged for selective activation (`{:tags #{:dev :ci :strict}}`)

Loaded from `.typura/hooks/` or project config.

### Gradual Typing (Type Guards)
```clojure
(if (int? x)
  ;; x is :int here
  (+ x 1)
  ;; x is (subtract original-type :int) here
  (str x))
```
Type narrowing is **not** a hardcoded predicate table. Instead, guards are part of function
stubs — any predicate (including user-defined) can declare narrowing behavior:
```clojure
;; in core stubs:
{clojure.core/int?    {:sig [:=> [:cat :any] :boolean]  :guard {:arg 0 :narrows :int}}
 clojure.core/string? {:sig [:=> [:cat :any] :boolean]  :guard {:arg 0 :narrows :string}}
 clojure.core/nil?    {:sig [:=> [:cat :any] :boolean]  :guard {:arg 0 :narrows :nil}}}
```
The `if` handler in AST walker checks if test-expression's function has a `:guard`.
If yes — applies narrowing in then-branch, inverse narrowing (type subtraction) in else-branch.
Macros like `if-not`, `when`, `cond` are already expanded to nested `if` by tools.analyzer,
so branch inversion is handled automatically.

Also: truthiness narrowing (removes `:nil`/`:false` from unions in then-branch of any `if`).

> **Note:** Consider an alternative approach — hooks as meta-code that receives the analysis
> context (including "am I in test-position of `if`?") and can modify it directly. This would
> handle `not`-inversion naturally (the `not` hook just flips the guard context) and aligns
> better with the vision of hooks as arbitrary validation code. Guards remain the primary
> approach for simple predicates, but meta-code hooks may be more powerful for complex cases.

### ILookup / Usage-Based Inference
- `(x 5)` → x implements IFn (vector, map, set, keyword)
- `(:key x)` → x is map with key `:key`
- `(nth x 0)` → x implements Indexed
- Ideally derived from protocol/interface definitions, not hardcoded

### Protocols, Records, Multimethods
- `defprotocol` → interface with method signatures
- `defrecord` → typed map with known fields
- `defmethod` → tracked implementations; warn if dispatch value not covered
- Protocol method calls → verify argument satisfies protocol

### Diagnostics (Error Reporting)

The analyzer collects diagnostics during inference — not as a separate pass, but inline
as type conflicts are detected. A **conflict** is when a value's inferred type is incompatible
with the expected type in context. This is distinct from unions (multiple possible types)
which are valid.

**Conflict vs union:**
```clojure
;; NOT a conflict — union type [:or :int :string]
(if condition (+ x 1) (str x))

;; CONFLICT — :int is not a valid first arg to `map` (expects IFn/ISeq)
(if (int? i) (map i coll) ...)

;; CONFLICT — arity mismatch
(+ 1 2 3)  ;; if stub says [:=> [:cat :number :number] :number]
```

**Diagnostic structure:**
```clojure
{:level    :error|:warning|:info
 :code     :type-mismatch|:arity-mismatch|:unreachable-branch|...
 :message  "Expected :string, got :int"
 :loc      {:file "src/foo.clj" :line 12 :col 5 :end-line 12 :end-col 15}
 :expected <type>    ; what context required
 :actual   <type>}   ; what was inferred
```

**Diagnostic codes (initial set):**
| Code | Level | Description |
|---|---|---|
| `:type-mismatch` | error | Argument type doesn't satisfy parameter type |
| `:arity-mismatch` | error | Wrong number of arguments |
| `:unreachable-branch` | warning | Guard narrows to `:nothing`, branch is dead code |
| `:narrowed-misuse` | warning | Value used in way incompatible with narrowed type |

**Collection strategy:** collect-all (don't stop at first error). The walker accumulates
diagnostics in the context (`:diagnostics` key) and returns them with the final result.
The analyzer API returns `{:type <inferred> :diagnostics [...]}`  — consumers (CLI, LSP)
format output from this.

**Source locations:** every diagnostic must carry `:loc` from the AST node. tools.analyzer
provides `:env` with `:line`/`:column` on each node — use these directly.

## Dependencies

- **malli** — type/schema representation
- **clojure.tools.analyzer(.jvm)** — AST parsing (macros pre-expanded)
- **tools.analyzer.jvm `analyze+eval`** — evaluates code during analysis (macros, type defs, hooks)

## Development Commands

```bash
# Start REPL with dev dependencies
clj -M:dev

# Run tests
clj -M:test

# Run analyzer REPL
cd analyzer && clj -M:dev
```

## Development Roadmap

### Phase 0 — Foundation (MVP) ✅
Focus: infrastructure, architecture, tests, fast feedback loop.

- [x] **Subtype checking** — core validation primitive (`subtype? :int :number` → true)
  - [x] Primitive hierarchy (`:int` ⊂ `:number`, etc.)
  - [x] Union subtyping (`:int` ⊂ `[:or :int :string]`)
  - [x] Map structural subtyping (`[:map [:a :int] [:b :string]]` ⊂ `[:map [:a :int]]`)
  - [x] `:any` as top type (everything is subtype of `:any`)
- [x] **Type context** data structure with scoping
- [x] **AST walker** — multimethod on `:op` (`:const`, `:local`, `:let`, `:if`, `:do`, `:invoke`, `:fn`, `:def`, `:static-call`, `:instance-call`)
- [x] **Minimal core stubs** — ~15 functions (`+`, `-`, `*`, `/`, `str`, `count`, `conj`, `assoc`, `get`, `first`, `rest`, `nth`, `inc`, `dec`, `println`)
- [x] **REPL-driven testing** — analyze expression → see inferred types instantly
- [x] **Basic test suite**
- [x] **Result**: can analyze `(defn add [a b] (+ a b))` → `{:args [:number :number] :return :number}`

### Phase 1 — Flow Analysis ✅
- [x] Type narrowing in `if` branches via guard predicates (`int?`, `string?`, etc.)
- [x] Truthiness narrowing (remove nil from unions in then-branch)
- [x] Union type normalization (dedup, subtype simplification)
- [x] Type subtraction in else-branch (subtype-aware: `number?` removes `:int`, `:double`)
- [x] Nested guard narrowing (guards through `:do` wrappers from macro expansion)

### Phase 2 — Diagnostics ✅
- [x] **Diagnostic infrastructure** — `:diagnostics` accumulator in context, `{:type _ :diagnostics _}` return shape
- [x] **`:type-mismatch`** — argument type vs parameter type conflict detection (fixed & variadic params)
- [ ] **`:unreachable-branch`** / **`:narrowed-misuse`** — dead code and post-narrowing misuse warnings (deferred)

### Phase 3 — Clojure Core Abstractions & Interop ✅
- [x] Typed collection literals: `[:vector T]`, `[:set T]`, `[:map [:k T] ...]`, `[:map-of K V]`
- [x] Smart `get`/`nth` — resolves value types from structural maps and vectors
- [x] Keyword invocation `(:key m)` via `:keyword-invoke` handler
- [x] Capability types: `:cap/ifn`, `:cap/ilookup`, `:cap/indexed`, `:cap/seqable`, `:cap/associative`, `:cap/counted`
- [x] Capability satisfaction in `subtype?` for vectors, maps, sets, keywords
- [x] Java interop: `class->type` mapping, improved `:instance-call`/`:new`/`:static-call`
- [x] `RT/get`, `RT/nth` static call dispatch (used by macro-expanded destructuring)
- [ ] **Destructuring analysis** — deferred to Phase 6c (see note below)

> **Destructuring limitation:** Clojure's destructuring macros expand to
> `(let* [map__0 m, map__1 (if (seq? map__0) (apply hash-map ...) map__0), x (RT/get map__1 :x)] x)`.
> The `(if (seq? ...) ...)` guard produces `[:or :any <map-type>]` → `:any`, losing type info.
> Sequential destructuring works (`[a b]` → `RT/nth` on typed vector).
> Map destructuring requires stubs that are **arbitrary type-resolver functions**
> `(fn [arg-types arg-nodes ctx] -> [return-type ctx'])` — not fixed `[:=> ...]` signatures.
> This is the motivating case for Phase 6's hook system: `PersistentArrayMap/createAsIfByAssoc`
> needs to propagate the input map type through to its return type (generics).

### Phase 4 — Protocols, Records, Multimethods
- [ ] Protocol definitions → method signatures in context
- [ ] Protocol/interface satisfaction checking in subtype layer
- [ ] Record definitions → typed map schemas
- [ ] Multimethod dispatch tracking and coverage warnings

### Phase 5 — Project-Level Analysis
Focus: transition from single-expression analysis to whole-project analysis.

- [ ] **Namespace resolution** — parse `ns` forms, resolve `:require`/`:use`/`:import`/`:refer`
- [ ] **Source discovery** — find `.clj`/`.cljs`/`.cljc` files from configured source paths
- [ ] **Dependency graph** — build namespace dependency DAG, detect cycles
- [ ] **Topological analysis** — analyze namespaces in dependency order
- [ ] **Cross-namespace globals** — types inferred in ns A available when analyzing ns B via `:require`
- [ ] **Classpath dependencies** — resolve types from JARs (stubs or reflection)
- [ ] **Incremental analysis** — cache results per-namespace, invalidate on file change + transitive dependents
- [ ] **Error recovery** — partial analysis of broken/incomplete files, graceful degradation (critical for LSP)
- [ ] **Project configuration** — `.typura/config.edn` (source paths, excluded namespaces, stub paths, tags)

### Phase 6 — Unified Functional Stubs ✅
Every stub is a function `(fn [arg-types arg-nodes ctx] -> [ret-type ctx'])`.
Helper `sig` creates these from Malli schemas; complex stubs are hand-written fns.
Same mechanism for external stubs and inline annotations.

- [x] **Unified functional stubs** — stubs are functions, not fixed schemas
  - `sig` helper wraps `[:=> ...]` into a stub fn (with schema in metadata)
  - `sig+guard` attaches flow-narrowing guards as fn metadata
  - Custom resolvers for context-dependent return types (`get`, `nth`)
- [x] **Inline annotations** — `{:typura/sig [:=> ...]}` metadata on `defn`
  - Analyzer reads var metadata, wraps schema with `sig`, stores stub fn in globals
  - Callers get arg checking and return type from the annotation
- [x] **Refactored inference** — `special-invoke`/`special-static-call` eliminated
  - All dispatch goes through stub functions (data-driven, not hardcoded)
  - `typura.check` namespace extracted for shared type-checking utilities

### Phase 6a — Inferred Stubs (parametric polymorphism)
Focus: `defn` without annotation produces a callable stub, enabling polymorphic inference.
Cross-namespace constraint propagation is unlimited — inferred stubs follow call chains across namespaces.

- [ ] **Inferred function stubs** — `defn` without annotation produces a stub, not a static schema
  - `:fn` handler returns a stub fn closed over the fn's AST (params + body)
  - On each call site: binds params to concrete arg-types, infers body, returns result type
  - Enables parametric polymorphism: `(defn id [x] x)` → `(id 42)` returns `:int`, `(id "a")` returns `:string`
  - `:invoke` handler checks `fn?` for local bindings (not just `fn-type?`)
  - `:def` handler skips body analysis when external stub already exists for the var
  - Metadata on stub fn carries the "general" schema for display (LSP hover, diagnostics)

### Phase 6b — Generics & Core Stubs
Focus: expand type vocabulary and coverage. Can proceed after 6a stabilizes stub format.

- [ ] Schema constructors for generics (`(Array :int)` → `[:vector :int]`)
- [ ] Expand core stubs to ~50 most-used functions
- [ ] **Mutable references** — `atom`, `ref`, `volatile!`, `agent` typed as `:any` by default
  - `deref` / `@` returns `:any` unless annotated
  - Explicit annotation: `(atom ^{:typura/type :int} 0)` → `deref` returns `:int`, `reset!` checks value type
  - `swap!` checks that fn return type matches annotated type (when annotated)
  - No automatic inner type tracking — mutation makes static tracking unsound

### Phase 6c — Hook System & Extensibility
Focus: plugin system for external stubs and custom rules. Can proceed after 6a.

- [ ] Helper function library (typura core API for stub authors)
- [ ] **Hook/stub loading via eval** — load `.typura/hooks/*.clj` as regular Clojure files
  - `require`/`load-file` in the analyzer's JVM — full access to classpath (Malli, user deps)
  - `analyze+eval` already evals top-level forms — var values available via `deref`
  - User stubs override built-in stubs (user > built-in priority)
- [ ] Hook API (`register-type!`, `register-rule!`, `ctx/narrow!`, `ctx/get-type`)
- [ ] Tag-based rule filtering

### Phase 6d — Deep Java Interop
Focus: fully leverage tools.analyzer.jvm reflection data. Independent of 6b/6c.

- [ ] `class->type` for unknown classes: return the class itself as type (e.g. `java.util.ArrayList`) instead of `:any`
- [ ] `:instance-call` — when `:validated? true`, check argument types against Java method signature from reflection
- [ ] `:static-call` — same, use reflection data as fallback when no stub exists
- [ ] `subtype?` for Java classes: use `.isAssignableFrom` for class hierarchy

### Phase 7 — Literal Types, Tuples & Advanced Type Features
- [ ] **Literal (value) types** — `[:val x]` represents the type of a specific constant value
  - `val->type` returns `[:val x]` instead of `:int` / `:keyword` / etc.
  - `subtype?`: `[:val 0]` ⊂ `:int` ⊂ `:number`, `[:val :red]` ⊂ `:keyword`
  - Unions of literals = enum: `[:or [:val :red] [:val :green] [:val :blue]]`
  - `constrain` with `[:val 0]` against `:number` → ok (via subtype chain)
  - `simplify-union`: collapse `[:or [:val 0] [:val 1] [:val 2]]` to `:int` if all same base type and count > threshold
- [ ] **Tuple types** — `[:tuple T1 T2 ...]` for fixed-size heterogeneous vectors
  - `subtype?`: `[:tuple :int :string]` ⊂ `[:vector [:or :int :string]]`
  - `nth` on tuple with literal index returns positional type: `(nth [:tuple :int :string] [:val 0])` → `:int`
  - Not inferred automatically from vector literals (vector stays `[:vector [:or ...]]`)
  - Available via annotations and stubs
- [ ] **Recursive types** — via Malli `:ref` (trees, linked lists, nested structures)
- [ ] **Intersection types** — schema merging (`[:and TypeA TypeB]`)
  - Useful for narrowing: guard + existing type = intersection
  - `subtype?`: `[:and A B]` ⊂ `A` and `[:and A B]` ⊂ `B`
- [ ] Update `simplify-union` — if union contains `:any`, collapse to `:any`

### Phase 8 — LSP + CLI
Depends on Phase 5 (project-level analysis).

- [ ] CLI: `clj -M:typura check src/`
- [ ] LSP server for real-time editor feedback
- [ ] **Diagnostic output** — human-readable (with source snippets + underlines), EDN, JSON
- [ ] LSP `textDocument/publishDiagnostics` — map internal diagnostics to LSP protocol
- [ ] **Hover documentation** — enrich `textDocument/hover` with inferred type signatures
  - Show inferred arg/return types for functions (even without annotations)
  - Show narrowed type in context (e.g. after guard, inside branch)
  - Augment existing docstrings with type info, don't replace them
- [ ] **Suppression** — `^:typura/ignore` on forms, `; typura-ignore-next-line`, configurable severity levels
- [ ] **Baseline mode** — snapshot current errors, report only new ones (for gradual adoption)

### Phase 9 — VSCode Extension
Depends on Phase 8 (LSP server).

- [ ] Language client wrapping typura LSP
- [ ] Extension config UI (source paths, severity levels, tags)
- [ ] Marketplace publishing

### Phase 10 — CI & Distribution
- [ ] **CI mode** — `--diff-only` (errors in changed code only), `--baseline` support, exit codes
- [ ] **GitHub Action** — reusable action for PR checks
- [ ] Pre-commit hook support
- [ ] Distribution: Clojars artifact, `clj -Ttools install`, brew formula
- [ ] Dogfooding on real projects (ring, compojure, re-frame) — tune false positives, expand stubs

### Future TODO
- **Emacs package** — lsp-mode/eglot integration, MELPA recipe
- **IntelliJ/Cursive** — via LSP if Cursive supports it
- **ClojureScript Support**
  - Abstract analyzer interface — decouple from tools.analyzer.jvm
  - tools.analyzer.js backend for CLJS
  - Platform-specific type resolution (JVM reflection vs CLJS protocol metadata)
  - Shared core logic, platform-specific stubs
  - Conditional reader tags for platform-aware stubs (`.cljc`)
