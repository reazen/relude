# Frequently asked questions

## Does `Relude` work with native ReasonML/OCaml?

`Relude` does **not** currently support compiling with native ReasonML/OCaml (i.e. via esy nor dune). There is [an issue open](https://github.com/reazen/relude/issues/133) to track progress.


## What's the difference between `Relude.Result` and `Belt.Result`, `Relude.Option` and OCaml stdlib `option`, `Relude.Js.Json` and `Js.Json`, etc.?

`Relude` has many modules that augment existing types from `Belt` or the OCaml standard library, which are both available by default within Melange/ReasonML projects.  Sometimes we alias the types from `Belt` or the OCaml stdlib, and sometimes our corresponding modules simply add additional functions for these common types.

`Relude` also has many of its own types, which have their own modules, like `Relude.AsyncResult`, `Relude.ReaderT`, etc.

## I'm getting strange errors about types like `array(string)` not being the same as `Bastet.Array.Foldable.t`

If you are trying to compile and get an error like:

```reasonml
This has type array(t) but somewhere wanted Bastet.Array.Foldable.t(string)`
```

you likely need to add `mel-bastet` to your `bs-dependencies` in your `bsconfig.json` file.  This error occurs because Melange is not able to find the types defined in the `Bastet_js` module, so it can't determine that `array(t)` is the same type as `Bastet.Array.Foldable.t(string)`.

Because we are not using .rei interface files, we are not able to abstract these types away for functions that get included into our implementation modules.  (See .rei topic below).

## Why doesn't `Relude` have interface (.rei) files?

`Relude` relies heavily on the module [include](https://reasonml.github.io/docs/en/module#extending-modules) capability of ReasonML/OCaml.  Many of the typeclasses in `Relude` have extension modules that can be `include`'d into your module to add a variety of useful utility functions, operators, and extension modules to your host module "for free," simply because you've taken the time to create an instance of a given typeclass.  Because there are lots of functions automatically added to many modules, it becomes difficult to maintain good .rei files which expose all of these automatically added functions, while at the same time including documentation and more specialized type annotations.  Until we have a maintainable solution for this challenge, we are avoiding the use of .rei files.
