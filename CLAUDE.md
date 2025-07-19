# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Typura is an experimental optional static type system for Clojure built on Malli. The project consists of two main components:

1. **typura.core** (.cljc) - Cross-platform library for type definitions and utilities that can be imported into Clojure/ClojureScript code
2. **typura.analyzer** (.clj) - Static analysis tool for type checking and analysis

The project uses a declarative, data-driven type DSL using standard Clojure data structures. The `sketch.cljc` file contains experimental type definition patterns and examples of the intended API.

## Development Commands

```bash
# Start REPL with dev dependencies
clj -M:dev

# Run tests
clj -M:test

# Build tools (when implemented)
clj -M:build
```

## Architecture Notes

- **Cross-platform support**: Core library uses `.cljc` extension for Clojure/ClojureScript compatibility
- **Malli integration**: Primary dependency for schema validation and type checking
- **Experimental design**: The `sketch.cljc` file shows prototype concepts including:
  - `defschema` macro for type definitions
  - Generic type support with parameterized schemas
  - Type refinement patterns (Maybe, Result, etc.)
  - Schema composition and validation

## Key Design Patterns

The sketch file reveals several important patterns:
- Type definitions as data structures (following Malli conventions)
- Schema objects that implement IFn for validation
- Generic type constructors (e.g., `Array [Element]`)
- Type predicates and refinement (`is?`, `some?`, `nil?`)
- Cross-language type compatibility considerations