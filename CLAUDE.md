# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Typura is a static type analyzer for Clojure, inspired by OCaml's type inference. Not a new language (unlike TypedClojure) — a static analysis tool that infers types from context with minimal annotations. Aims to replace clj-kondo's weak type analysis with deep inference.

### Goals
- Infer argument and return types for every function by analyzing usage context
- Gradual typing — type narrowing via predicates (`int?`, `string?`, etc.)
- Hook/stub system for typing external functions (core, libraries)
- Custom validation rules via SCI-evaluated meta-code
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
5. Report errors where constraints conflict

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
- **User stubs/hooks**: type definitions for third-party libraries via SCI
- **Flow analysis**: type narrowing in conditional branches
- **Constraint propagation**: bidirectional — if `+` expects Number, callers must provide Number
- **Fallback**: unresolvable types become `:any` (no errors for unknowns)

### Hardcoded Special Forms
These are handled directly by the analyzer (not via stubs):
`if`, `let`, `def`, `defn`, `fn`, `do`, `loop`/`recur`, `try`/`catch`, `throw`, `var`, `quote`, `new`, `.method`, `set!`

`atom` — also hardcoded (wraps inner type).

### Hook System
SCI-evaluated Clojure files that modify the type context:
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

## Dependencies

- **malli** — type/schema representation
- **clojure.tools.analyzer(.jvm)** — AST parsing (macros pre-expanded)
- **babashka/sci** — sandboxed execution of hooks and meta-code

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

### Phase 1 — Flow Analysis (in progress)
- [x] Type narrowing in `if` branches via guard predicates (`int?`, `string?`, etc.)
- [x] Truthiness narrowing (remove nil from unions in then-branch)
- [x] Union type normalization (dedup, subtype simplification)
- [ ] Type subtraction in else-branch (subtract guard type from unions)
- [ ] Nested guard narrowing (guards inside `let`, `do`, etc.)

### Phase 2 — Stub System
- [ ] Stub file format (EDN with Malli schemas)
- [ ] Stub loading by namespace (user > built-in priority)
- [ ] Expand core stubs to ~50 most-used functions
- [ ] Schema constructors for generics

### Phase 3 — Hook System
- [ ] SCI-based hook API (`register-type!`, `register-rule!`, `ctx/narrow!`, `ctx/get-type`)
- [ ] Hook loading from `.typura/hooks/`
- [ ] Tag-based rule filtering
- [ ] Built-in core hook (replaces/supplements stubs)

### Phase 4 — Protocols, Records, Multimethods
- [ ] Protocol definitions → method signatures in context
- [ ] Protocol/interface satisfaction checking in subtype layer
- [ ] Record definitions → typed map schemas
- [ ] Multimethod dispatch tracking and coverage warnings

### Phase 5 — Clojure Core Abstractions & Interop
Model Clojure's core interfaces/protocols as capabilities that `subtype?` understands:
- [ ] Core capability types: `ILookup`, `IFn`, `Indexed`, `Seqable`, `Associative`, `Counted`
  - Maps, vectors, sets, keywords satisfy different subsets of these
  - Usage-based inference: `(x 5)` → x is `IFn`, `(:key x)` → x has key `:key`, `(nth x 0)` → x is `Indexed`
- [ ] Java interop types — rely on reflection info from tools.analyzer.jvm (not hardcoded hierarchy)
  - Instance method return types, constructor types
  - Class→type mapping from AST `:tag` metadata
- [ ] Platform-agnostic capability model — same abstractions work on JVM (interfaces) and CLJS (protocols)

### Phase 6 — ClojureScript Support
- [ ] Abstract analyzer interface — decouple from tools.analyzer.jvm
- [ ] tools.analyzer.js backend for CLJS
- [ ] Platform-specific type resolution (JVM reflection vs CLJS protocol metadata)
- [ ] Shared core logic, platform-specific stubs
- [ ] Conditional reader tags for platform-aware stubs (`.cljc`)

### Phase 7 — LSP + CLI
- [ ] CLI: `clj -M:typura check src/`
- [ ] LSP server for real-time editor feedback
- [ ] Output formats: human-readable + EDN/JSON
- [ ] Incremental analysis with file-level caching

### Open Questions
- Recursive types (trees, linked lists) — Malli `:ref` + inference?
- Constants as types (TypeScript-style literal types)?
- How deep to go with cross-namespace constraint propagation?
- Schema merging / intersection types?
- Java generics — how deep to go? `List<String>` vs erased `List`?
