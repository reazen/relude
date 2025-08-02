# Relude - Alternative Standard Library for ReasonML

## Project Overview

Relude is a **ReasonML/OCaml** standard library replacement ("prelude") written in ReasonML, targeting compilation to JavaScript via the Melange compiler. It provides typeclass-style abstractions inspired by Haskell, PureScript, Elm, and Scala cats/scalaz ecosystems.

## Tech Stack
- Language: ReasonML/OCaml (targeting JavaScript via Melange)
- Build System: Dune 3.12+
- Package Manager: opam 2.1+
- Testing: Alcotest (native OCaml testing)
- Formatter: refmt (ReasonML formatter)
- Documentation: odoc + docsify
- Core Dependencies: mel-bastet (typeclass library)

## Key Features

- Typeclass-based abstractions using mel-bastet
- Comprehensive utility functions for common data types (list, array, option, result)
- Unique modules: AsyncData, AsyncResult, Decimal, HList, HMap, ListZipper, Validation
- IO monad implementation for effectful functions
- Monad transformers (OptionT, ResultT, StateT)
- Extension functions and infix operators
- JavaScript interop utilities

## Architecture & Philosophy

- **Opinionated**: Leans toward Haskell/PureScript style over idiomatic OCaml
- **Naming**: Uses JavaScript/Scala camelCase/TitleCase conventions instead of snake_case
- **Pipe operator**: Standardized on `|>` instead of `->`
- **No .rei files**: Uses module `include` capability extensively, making interface files difficult to maintain

## Development Environment Setup

### Prerequisites

- OCaml/ReasonML toolchain via opam
- Melange compiler

### Setup

```bash
# Create local opam switch
make create-switch
# OR manually: opam switch create . --deps-only

# Install dependencies
make install

# Install development tools with dev dependencies
make dev-tools
# OR manually: opam install . --with-dev-setup

# Update shell environment
eval $(opam env)
```

## Common Commands

```bash
# Build the project
make build
# OR: opam exec -- dune build @relude @test

# Watch mode for development
make watch
# OR: opam exec -- dune build @relude @test -w

# Clean build artifacts
make clean
# OR: opam exec -- dune clean

# Run tests
make test
yarn test

# Test with coverage
make test-coverage
yarn test --coverage

# Generate documentation
make docs
yarn docs

# Format code (ReasonML uses refmt, but dune handles this)
dune build @fmt --auto-promote
```

## Project Structure

```
src/                       # Core library modules
├── Relude.re              # Main module entry point
├── Relude_*.re            # Core modules (Array, List, Option, etc.)
├── array/                 # Array-specific implementations
├── list/                  # List-specific implementations  
├── option/                # Option-specific implementations
├── extensions/            # Typeclass extensions
├── js/                    # JavaScript interop utilities
└── dune                   # Dune build configuration

test/                      # Test suites using Alcotest  
├── test_*.re              # Test files
├── test_runner.re         # Main test executable
└── dune                   # Test configuration

docs/                      # Documentation source
├── *.md                   # Documentation files
└── index.html             # Docsify entry point

_build/                    # Build artifacts (git-ignored)
node_modules/              # JavaScript dependencies (git-ignored)
```

## Code Style Guidelines

### Relude-Specific Conventions
- **Naming**: camelCase for functions, TitleCase for modules (diverges from OCaml snake_case)
- **Pipe operator**: Use `|>` consistently (not `->`)
- **No .rei files**: Uses module `include` capability extensively
- **Line length**: 80 characters (matches refmt)

### Module Organization
- Keep modules focused on single responsibilities
- Use typeclass-based abstractions via mel-bastet
- Prefer composition over inheritance
- Include extension modules to gain utility functions "for free"

### Functional Programming Style
- Prefer immutable data structures
- Use pattern matching over conditional logic
- Favor function composition and higher-order functions
- Use `Option.t` and `Result.t` for error handling
- Prefer `List.map`, `List.fold_left` over loops

### Error Handling
- Use `Result.t` for operations that can fail
- Use `Option.t` for values that may be absent
- Define custom error types when appropriate
- Leverage typeclass abstractions for consistent error handling

### Documentation Standards
- Use odoc syntax with specific formatting guidelines
- Write examples as if `open Relude.Globals` is present
- Include examples in documentation when helpful
- Document function parameters and return values
- Format with line breaks at column 80

## Testing Guidelines
- Uses Alcotest for testing (native OCaml testing)
- Test files follow naming pattern `test_*.re`  
- Tests are run with `dune runtest` or `make test`
- Write unit tests for all public functions
- Test edge cases and error conditions
- Keep tests simple and focused
- Coverage reports available with `make test-coverage`

### ReasonML Syntax Guidelines
**IMPORTANT**: Always use ReasonML syntax, never OCaml syntax in this codebase.

**ReasonML (CORRECT):**
```reason
let myFunction = (param1, param2) => {
  param1 |> someOperation |> anotherOperation(param2);
};

let testFunction = () =>
  inputValue |> Module.functionName |> Alcotest.check(Alcotest.int, "test description", expectedValue);
```

**OCaml (INCORRECT - DO NOT USE):**
```ocaml
let my_function param1 param2 =
  param1 |> some_operation |> another_operation param2

let test_function () =
  input_value |> Module.function_name |> check int "test description" expected_value
```

### Alcotest Testing Patterns

```reason
let test_function_name = () =>
  input_value |> Module.function_name |> Alcotest.check(expected_type, "test description", expected_value);

let suite = [
  ("test name", `Quick, test_function_name),
];
```

### Common Workflows

#### Adding a New Module

1. Create the module file in `src/Relude_ModuleName.re`
2. Add corresponding test file in `test/test_modulename.re`
3. Export the module in `src/Relude.re`  
4. Add test suite to `test/test_runner.re`
5. Update documentation if needed

#### Implementing Typeclass Instances

1. Check if the typeclass exists in mel-bastet
2. Implement required functions for the typeclass
3. Include extension modules to get utility functions
4. Add comprehensive tests

#### Working with JavaScript Interop

- Use modules in `src/js/` for JavaScript-specific functionality
- Follow existing patterns in `Relude_Js_*.re` files
- Test both ReasonML and JavaScript sides

### Release Process

1. Run `npm run releasebuild` to generate all artifacts
2. Bump version: `npm version major|minor|patch`
3. Push with tags: `git push upstream --follow-tags`
4. Publish: `npm publish`
5. Create GitHub release

### Common Issues

#### Type Compatibility Errors

If you see errors like:
```
This has type array(t) but somewhere wanted Bastet.Array.Foldable.t(string)
```

Add `mel-bastet` to your `bs-dependencies` in `bsconfig.json`.

#### Missing Dependencies

Ensure all required dependencies are properly installed via opam and yarn.

## Dependencies Management
- **mel-bastet**: Core typeclass interfaces and implementations
- **bisect_ppx**: Code coverage (dev)
- **alcotest**: Testing framework (dev)
- **qcheck**: Property-based testing (dev)

### Dune Configuration Patterns

#### Library Stanza (from src/dune)
```dune
(library
 (public_name relude)
 (name relude)
 (libraries mel-bastet))
```

#### Test Stanza (from __tests__/dune)
```dune
(melange.emit
 (target output)
 (libraries relude)
 (modules :standard))
```

### Useful Resources

- [Relude Documentation](https://reazen.github.io/relude)
- [mel-bastet](https://github.com/johnhaley81/mel-bastet) - Typeclass library
- [ReasonML Docs](https://reasonml.github.io/)
- [Melange Docs](https://melange.re/)
- [Reazen GitHub Org](https://github.com/reazen) - Ecosystem libraries

## Performance Considerations
- Use tail recursion for list processing
- Consider lazy evaluation for expensive computations
- Profile before optimizing
- Leverage typeclass abstractions for consistent performance patterns
- Use appropriate data structures (Map, Set, Array vs List)

## Do Not Section
- Do not modify `_build/` or `node_modules/` directories manually
- Do not commit generated files (.js artifacts, coverage reports)
- Do not use imperative loops when functional alternatives exist
- Do not ignore compiler warnings without good reason
- Do not break typeclass patterns when extending functionality
- Do not use `Obj.magic` unless absolutely necessary

## Commit Guidelines
- Run `make build` and `make test` before committing
- Format code with refmt (handled by dune)
- Write clear commit messages explaining the "why" not just the "what"
- Include issue numbers in commit messages when applicable

## IDE Integration
- Use Merlin for ReasonML editor integration
- Install ocaml-lsp-server for Language Server Protocol support
- Configure your editor to show type information on hover
- Enable automatic formatting with refmt

## Contributing

- Follow existing code style and conventions
- Add comprehensive tests for new functionality
- Update documentation with odoc comments
- Use existing typeclass patterns where applicable
- Consider performance implications for core utilities
