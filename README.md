# Relude

A prelude for `Bucklescript` and `ReasonML` projects

# Overview

* This goal of `Relude` is to be an alternative, somewhat-opinionated, and FP-inspired prelude for `Bucklescript` and `ReasonML` projects.
* The types and functions in `Relude` are inspired by Haskell, Purescript, Scala, and other FP ecosystems.
* `Relude` is built on top of the amazing [`bs-abstract`](https://github.com/Risto-Stevcev/bs-abstract) library, to leverage its reusable category/abstract algebra interfaces and implementations.
* `Relude` also leverages many functions and types from the `OCaml` `Stdlib` and `Bucklescript` `Belt` library, though sometimes with function arguments switched around.
* For functions, `Relude` prefers `thread-last` semantics, rather than `thread-first`
  * This means that the "most significant" data argument tends to be the last argument, rather than the first, which works nicely with the `|>` operator.
  * `Relude` differs from `Belt` in this regard - `Belt` prefers `thread-first` semantics (i.e. functions that work nicely with the fast pipe `->` operator)
  * When using `Relude`, you will likely tend use `|>` more than `->` for most function chaining, but there are always cases where `->` might make sense too.

# Installation

The library is not published to npm yet, but you can install it via a GitHub reference:

`npm install --save reazen/relude`

# Documentation

* TODO

# Development scripts

| Action                 | Command              |
| ---------------------- | -------------------- |
| Clean                  | `npm run clean`      |
| Build                  | `npm run build`      |
| Clean & build          | `npm run cleanbuild` |
| Test                   | `npm run test`       |
| Clean & test           | `npm run cleantest`  |
| Gen docs               | `npm run docs`       |
| Start compiler watcher | `npm run start`      |

# Editor

If you use `vscode`, Press `Windows + Shift + B` it will build automatically
