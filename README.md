# Relude

[![CircleCI](https://circleci.com/gh/reazen/relude.svg?style=svg)](https://circleci.com/gh/reazen/relude)

Relude is a standard library replacement ("prelude") written in [ReasonML](https://reasonml.github.io/), targeting compilation to JavaScript via the [BuckleScript](https://bucklescript.github.io/) compiler.

Relude aims to provide a robust collection of utility functions built on top of reusable typeclass-style abstractions. While Relude has its foundiation in category theory, our goal is to make the library as easy-to-use as [Lodash](https://lodash.com/docs) for people who don't know or care about category theory. This is possible due to OCaml's powerful module system and the amazing work that has gone into [bs-abstract](https://github.com/Risto-Stevcev/bs-abstract), on which much of Relude is based.

For example, the following functions all exist in Relude, and are all backed by the same underlying function (`fold` from `Foldable`, when the inner type implements `Monoid`):

```reason
open Relude;

List.String.join(["hello ", "world"]); // "hello world"
Array.Float.sum([| 1.1, 2.2, 3.3 |]); // 6.6
List.fold((module Int.Multiplicative.Monoid), [2, 2, 3]); // 12
```

Relude is heavily inspired by [PureScript's Prelude](https://pursuit.purescript.org/packages/purescript-prelude), as well as other FP ecosystems including Scala, Haskell, and Elm. Many of the underlying functions live in either OCaml's stdlib or Bucklescript's Belt, but functionality is increasingly being replaced with custom implementations.

**Note About Piping**

Unlike Belt, Relude prefers significant-data-last (i.e. "pipe last" or "thread last") semantics. This means you'll tend to use `|>` with Relude, instead of `->`.  In specific situations, this style can lead to slightly worse type inference, but it has the benefit of improved composability (and no dependency on compiler magic).

### Project Status

Relude has a fairly complete test suite, and is being used in production. However, it's still a relatively young project and subject to breaking changes or bugs. At this point, we hope most major changes will have a clear migration path.

It's also possible that some function implementations are not completely optimal (e.g. `List.distinct` is currently O(n<sup>2</sup>) because it depends only on equality of its inner elements, not ordering).

We welcome all contributions! This includes code fixes, help with documentation, or just using the library and providing feedback about anything that is unclear or not working the way you would expect.

### Documentation Status

The documentation for relude is a work in progress, and currently on-hold. We are waiting to see what shakes out as the preferred documentation extraction/rendering tool for the ReasonML/Bucklescript community.

Documentation contributions are welcome, but all documentation may be subject to change in the future.

For the most part, the functions in Relude should be understandable by looking at the type signatures and sometimes the implementations.  Let the types guide you!

### Installation

The library is not published to npm yet, but you can install it via a GitHub reference:

```
> npm install --save reazen/relude
```

We would recommend installing the library at a specfic release or even commit, to avoid running into unexpected breaking changes at this stage of Relude's development.

```
install a specific release:

> npm install --save reazen/relude#v0.3.0

or a specific commit:

> npm install --save reazen/relude#03b46a3

```

### Documentation (subject to change)

* [API documentation](https://reazen.github.io/relude)

### Development scripts

* Clean: `npm run clean`
* Build: `npm run build`
* Clean & build: `npm run cleanbuild`
* Test: `npm run test`
* Clean & test: `npm run cleantest`
* Gen docs: `npm run docs`
  * Note: this currently uses `jaredly/redoc` but this may change. If you have any trouble running `npm install`, you may want to try removing `redoc` as a dependency
* Start compiler watcher: `npm run start`

### NixOS

If you are using NixOS, there is an extremely basic `default.nix` file that you can
use to build the project in a `nix-shell`, to avoid the `bs-platform` binary issues
in `node_modules`.

e.g.

```
> nix-shell
%nix% > npm install
%nix% > npm run start
```
