# Relude

[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude/master.svg)](https://circleci.com/gh/reazen/relude)
[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude) 
[![Coveralls](https://img.shields.io/coveralls/github/reazen/relude.svg)](https://coveralls.io/github/reazen/relude) 
[Documentation](https://reazen.github.io/relude)
[Coverage Report](https://reazen.github.io/relude/coverage/lcov-report/index.html)

Relude is a **ReasonML**/**BuckleScript**/**OCaml** standard library replacement ("prelude") written in [ReasonML](https://reasonml.github.io/), targeting compilation to **JavaScript** via the [BuckleScript](https://bucklescript.github.io/) compiler.

Relude aims to provide a robust collection of modules, types, and functions built on top of reusable typeclass-style abstractions, with a focus on day-to-day practical use.  Relude has its foundation on the math concepts found in most modern pure functional languages, but our goal is to make the library as easy-to-use as a library like [Lodash](https://lodash.com/docs) for those who don't know or care about the underlying abstractions.  This is made possible by OCaml's module system and the foundational work that has gone into [bs-abstract](https://github.com/Risto-Stevcev/bs-abstract), on which much of Relude is based.

As an example, the following functions all exist in Relude, and are all backed by the same underlying function (`fold` from `Foldable`, where the inner type has a `Monoid` instance):

```reason
open Relude.Globals;

List.String.join(["hello ", "world"]); // "hello world"
Array.Float.sum([| 1.1, 2.2, 3.3 |]); // 6.6
List.fold((module Int.Multiplicative.Monoid), [2, 2, 3]); // 12
```

The API of Relude is inspired by [PureScript's Prelude](https://pursuit.purescript.org/packages/purescript-prelude), as well as other FP ecosystems including Scala, Haskell, and Elm.

### Note About Piping (`->` vs `|>`)

Unlike [Belt](https://bucklescript.github.io/bucklescript/api/Belt.html), Relude prefers **significant-data-last** (i.e. **pipe last** or **thread last**) semantics.  This means you'll tend to use the `|>` operator with Relude functions, instead of `->`.  This was a conscious decision in favor of more natural (partial) application and composition of functions without the need for compiler magic, and is also more in line with the conventions of other FP languages.  If you would like to learn more about the tradeoffs between `|>` and `->`, here is a great article on the topic by [Javier Chávarri](https://www.javierchavarri.com/): [Data-first and data-last: a comparison](https://www.javierchavarri.com/data-first-and-data-last-a-comparison/).

### Why do we need Relude if we already have bs-abstract?

[bs-abstract](https://github.com/Risto-Stevcev/bs-abstract) provides many of the typeclasses (module types) and base typeclass instances and function implementations, but we wanted a more "batteries-included" style of prelude, where we could add additional modules and functions that are useful, but don't have an obvious place to live in the more abstract world of bs-abstract.  We also wanted a place where we could write the modules in the style that we wanted, and to have a place to experiment with new ideas.

### Why don't you just use PureScript?

[PureScript](http://www.purescript.org) has nailed a great many things, but ReasonML/BuckleScript/OCaml has recently swooped in and captured the hearts of many in both the JavaScript and FP communities, with its mix of functional programming capabilities, immutability, speed, pragmatism, and interop potential.  (This is not to say that there wasn't a vibrant OCaml community before, but ReasonML has been effective at exposing it to a whole new audience.)  Overall, we wanted to have a way to write code in the style of PureScript, but with the pragmatic capabilities and speed of ReasonML/OCaml.

## Installation

The library is published on [npm](https://www.npmjs.com/package/relude). We try to respect semantic versioning, but while Relude is at version `0.x`, there will likely be breaking changes without corresponding major version bumps.  We will try to document migration paths in release notes when possible.

Note: we currently depend on [bs-abstract](https://github.com/Risto-Stevcev/bs-abstract) as a `peerDependency` (and a `devDependency`).  We are trying this as an experiement to try to avoid duplicate package errors when you have multiple projects that depend on the same underlying library, like `bs-abstract`.

```
# bs-abstract needed because it's a peer dependency in relude
> npm install --save bs-abstract
> npm install --save relude

# or point at a specific git ref (tag, commit hash, etc.)
> npm install --save bs-abstract
> npm install --save "github:reazen/relude#someref"
```

## Documentation

* [Documentation site](https://reazen.github.io/relude)

## Project Status

Relude has a fairly complete test suite, and is being used in production at at least two companies. However, it's still a relatively young project, is subject to breaking changes, and likely has some bugs. Some of the function implementations are not optimized, as we have been more focused on the interfaces rather than the implementations initially. We hope to improve implementations and performance over time.

## Documentation Status

The documentation for Relude is a work in progress.  We have started to document the code with inline documentation comments, but HTML generation for in-code documentation is currently on-hold.  We're waiting to see what shakes out as the preferred documentation extraction/rendering tool for the ReasonML/BuckleScript community.  We are also working on a separate [documentation site](https://reazen.github.io/relude) to act as more of a high-level guide for the modules defined in Relude.

# Developer info

## Contributions

We welcome all contributions: bug fixes, features requests (or implementations), documentation, issues, discussions or feedback.

## Development scripts

* Clean: `npm run clean`
* Build: `npm run build`
* Watch: `npm run watch`
* Test: `npm run test`
* Test coverage: `npm run coverage`
* Docs: `npm run docs` (run the docs server)
* Clean & build: `npm run cleanbuild`
* Clean & watch: `npm run cleanwatch`
* Clean & test: `npm run cleantest`
* Clean & coverage: `npm run cleancoverage`
* Release build: `npm run releasebuild`
    * This does not publish the release, it just makes sure various build artifacts (.js, .rei, etc.) are built or re-generated from scratch
    * This should be used before we go to publish a release to make sure the release is clean

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
