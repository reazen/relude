# Relude

[![CircleCI](https://circleci.com/gh/reazen/relude.svg?style=svg)](https://circleci.com/gh/reazen/relude)

A prelude for `Bucklescript` and `ReasonML` projects

# Warning: WIP

This project is being actively developed, and is subject to major changes
until we figure out all the conventions for APIs and project organization.
Also, some of the function implementations are likely not ideal and need
optimization.

If you have trouble installing dependencies with `npm install`, try removing
the `redoc` dependency in the package.json. Removing this will make the `npm
run docs` command non-functional, but this command is not essential.

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

* [API documentation](https://reazen.github.io/relude)

# Development scripts

* Clean: `npm run clean`
* Build: `npm run build`
* Clean & build: `npm run cleanbuild`
* Test: `npm run test`
* Clean & test: `npm run cleantest`
* Gen docs: `npm run docs`
  * Note: this currently uses `jaredly/redoc` but this may change
* Start compiler watcher: `npm run start`

# Editor

If you use `vscode`, Press `Windows + Shift + B` it will build automatically

# NixOS

If you are using NixOS, there is an extremely basic `default.nix` file that you can
use to build the project in a `nix-shell`, to avoid the `bs-platform` binary issues
in `node_modules`.

e.g.

```
> nix-shell
%nix% > npm install
%nix% > npm run start
```