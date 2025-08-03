# Frequently asked questions

## Does `Relude` work with native ReasonML/OCaml?

`Relude` does **not** currently support compiling with native ReasonML/OCaml (i.e. via esy nor dune). There is [an issue open](https://github.com/reazen/relude/issues/133) to track progress.


## What's the difference between `Relude.Result` and `Belt.Result`, `Relude.Option` and OCaml stdlib `option`, `Relude.Js.Json` and `Js.Json`, etc.?

`Relude` has many modules that augment existing types from `Belt` or the OCaml standard library, which are both available by default within Melange/ReasonML projects.  Sometimes we alias the types from `Belt` or the OCaml stdlib, and sometimes our corresponding modules simply add additional functions for these common types.

`Relude` also has many of its own types, which have their own modules, like `Relude.AsyncResult`, `Relude.ReaderT`, etc.


## Why doesn't `Relude` have interface (.rei) files?

`Relude` relies heavily on the module [include](https://reasonml.github.io/docs/en/module#extending-modules) capability of ReasonML/OCaml.  Many of the typeclasses in `Relude` have extension modules that can be `include`'d into your module to add a variety of useful utility functions, operators, and extension modules to your host module "for free," simply because you've taken the time to create an instance of a given typeclass.  Because there are lots of functions automatically added to many modules, it becomes difficult to maintain good .rei files which expose all of these automatically added functions, while at the same time including documentation and more specialized type annotations.  Until we have a maintainable solution for this challenge, we are avoiding the use of .rei files.
