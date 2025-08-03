# Installation

The library is published on [npm](https://www.npmjs.com/package/relude). We try to respect semantic versioning, but while Relude is at version `0.x`, there will likely be breaking changes without corresponding major version bumps.  We will try to document migration paths in release notes when possible.

Note: we currently depend on [mel-bastet](https://github.com/johnhaley81/mel-bastet).


1. Add relude to your opam dependencies:

```sh
opam install relude
```

2. Add `relude` to your `dune-project` dependencies:

```lisp
(package
 (name your-project)
 (depends
  relude
  ; ... other dependencies
  ))
```

3. Use in your `dune` files:

```lisp
(library
 (name your_lib)
 (libraries relude))
```
