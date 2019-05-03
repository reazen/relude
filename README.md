# Relude

[![CircleCI branch](https://img.shields.io/circleci/project/github/reazen/relude/master.svg)](https://circleci.com/gh/reazen/relude)
[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude)

Relude is a ReasonML/BuckleScript/OCaml standard library replacement ("prelude") written in [ReasonML](https://reasonml.github.io/), targeting compilation to JavaScript via the [BuckleScript](https://bucklescript.github.io/) compiler.

Relude aims to provide a robust collection of modules, types, and functions built on top of reusable typeclass-style abstractions, with a focus on day-to-day practical use. While Relude has its foundation in category theory and abstract algebra concepts, our goal is to make the library as easy-to-use as a library like [Lodash](https://lodash.com/docs) for people who don't know or care about category theory. This is possible due to OCaml's powerful module system and the amazing work that has gone into [bs-abstract](https://github.com/Risto-Stevcev/bs-abstract), on which much of Relude is based.

For example, the following functions all exist in Relude, and are all backed by the same underlying function (`fold` from `Foldable`, when the inner type implements `Monoid`):

```reason
open Relude.Globals;

List.String.join(["hello ", "world"]); // "hello world"
Array.Float.sum([| 1.1, 2.2, 3.3 |]); // 6.6
List.fold((module Int.Multiplicative.Monoid), [2, 2, 3]); // 12
```

Relude is heavily inspired by [PureScript's Prelude](https://pursuit.purescript.org/packages/purescript-prelude), as well as other FP ecosystems including Scala, Haskell, and Elm. Many of the underlying functions live in either OCaml's stdlib or Bucklescript's Belt, but functionality is increasingly being replaced with custom implementations.

### Note About Piping (`->` vs `|>`)

Unlike Belt, Relude prefers **significant-data-last** (i.e. **pipe last** or **thread last**) semantics. This means you'll tend to use `|>` with Relude functions, instead of `->`.  In specific situations, this style can lead to slightly worse type inference, but it has the benefit of improved composability (and no dependency on compiler magic).

## Installation

The library is published on [npm](https://www.npmjs.com/package/relude). We try to respect semantic versioning, but while Relude is at version 0.x, there will likely be major breaking changes without corresponding major version bumps.
We will try to document migration paths in release notes when possible.

```
> npm install --save relude

or point at a git ref (tag, commit has, etc.)

> npm install --save "github:reazen/relude#someref"
```

## Documentation (possibly out of date and subject to change)

* [API documentation](https://reazen.github.io/relude)

## Project Status

Relude has a fairly complete test suite, and is being used in production. However, it's still a relatively young project and subject to breaking changes and likely has some bugs.

It's also true that many function implementations are not optimal (e.g. `List.distinct` is currently O(n<sup>2</sup>) because it depends only on equality of its inner elements, not ordering).  We hope to improve these types of things over time.

We welcome all contributions!  This includes code fixes, help with documentation, or just using the library and providing feedback about anything that is unclear or not working the way you would expect.

## Documentation Status

The documentation for Relude is a work in progress, and HTML generation is currently on-hold - waiting to see what shakes out as the preferred documentation extraction/rendering tool for the ReasonML/BuckleScript community.

Documentation contributions are welcome, but all documentation may be subject to change in the future.

There are some in-code documentation comments, and the type signatures alone are often sufficient documentation by themselves.

# Developer info

## Development scripts

* Clean: `npm run clean`
* Build: `npm run build`
* Clean & build: `npm run cleanbuild`
* Test: `npm run test`
* Clean & test: `npm run cleantest`
* Gen docs: `npm run docs`
  * Note: this currently uses `jaredly/redoc` but this may change. If you have any trouble running `npm install`, you may want to try removing `redoc` as a dependency
* Start compiler watcher: `npm run start`

## Publishing to npm

We currently do this by hand:

```
# Bump the version, update package.json, and create the git tag
> npm version major|minor|patch

# Push git commit and tags to upstream/origin
> git push upstream
> git push upstream --tags
> git push origin
> git push origin --tags

# Publish the new version to npm
> npm publish

# Go to GitHub and create a new release
```

## NixOS

If you are using NixOS, there is an extremely basic `default.nix` file that you can
use to build the project in a `nix-shell`, to avoid the `bs-platform` binary issues
in `node_modules`.

e.g.

```
> nix-shell
%nix% > npm install
%nix% > npm run start
```
