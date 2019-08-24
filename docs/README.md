# Relude

[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude/master.svg)](https://circleci.com/gh/reazen/relude)
[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude)
[![Coveralls](https://img.shields.io/coveralls/github/reazen/relude.svg)](https://coveralls.io/github/reazen/relude)

Relude is a **ReasonML**/**BuckleScript**/**OCaml** standard library replacement ("prelude") written in [ReasonML](https://reasonml.github.io/), targeting compilation to **JavaScript** via the [BuckleScript](https://bucklescript.github.io/) compiler.

Relude aims to provide a robust collection of modules, types, and functions built on top of reusable typeclass-style abstractions, with a focus on day-to-day practical use.  Relude has its foundation on the math concepts found in most modern pure functional languages, but our goal is to make the library as easy-to-use as a library like [Lodash](https://lodash.com/docs) for those who don't know or care about the underlying abstractions.  This is made possible by OCaml's module system and the foundational work that has gone into [bs-abstract](https://github.com/Risto-Stevcev/bs-abstract), on which much of Relude is based.

The API of Relude is inspired by [PureScript's Prelude](https://pursuit.purescript.org/packages/purescript-prelude), as well as other FP ecosystems including Scala, Haskell, and Elm.

## Quick links

- [Overview](Overview.md)
- [Installation](Installation.md)
- [Conventions](Conventions.md)
- [Status](Status.md)
- [Developer info](Developers.md)
- [Test coverage](https://reazen.github.io/relude/coverage/lcov-report/index.html)
- [FAQ](FAQ.md)
- [Typeclasses](typeclasses/Introduction.md)
- [API](api/Introduction.md)

## Ecosystem

The [Reazen](https://github.com/reazen) GitHub organization, and some individual contributors have a variety of libraries that are built with [Relude](https://github.com/reazen/relude):

|GitHub|Build|Package|Description|
|-------|-----|-------|-----------|
|[relude](https://github.com/reazen/relude)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude/master.svg)](https://circleci.com/gh/reazen/relude)|[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude)|ReasonML/Bucklescript prelude/standard library|
|[relude-eon](https://github.com/reazen/relude-eon)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude-eon/master.svg)](https://circleci.com/gh/reazen/relude-eon)|[![npm](https://img.shields.io/npm/v/relude-eon.svg)](https://www.npmjs.com/package/relude-eon)|a native Reason/OCaml date/time library|
|[relude-fetch](https://github.com/reazen/relude-fetch)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude-fetch/master.svg)](https://circleci.com/gh/reazen/relude-fetch)|[![npm](https://img.shields.io/npm/v/relude-fetch.svg)](https://www.npmjs.com/package/relude-fetch)|an interop and utility library for the [fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) API|
|[relude-parse](https://github.com/reazen/relude-parse)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude-parse/master.svg)](https://circleci.com/gh/reazen/relude-parse)|[![npm](https://img.shields.io/npm/v/relude-parse.svg)](https://www.npmjs.com/package/relude-parse)|a monadic string parsing library|
|[relude-reason-react](https://github.com/reazen/relude-reason-react)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude-reason-react/master.svg)](https://circleci.com/gh/reazen/relude-reason-react)|[![npm](https://img.shields.io/npm/v/relude-reason-react.svg)](https://www.npmjs.com/package/relude-reason-react)|utilities for [ReasonReact](https://reasonml.github.io/reason-react) based on `Relude` types and conventions|
|[relude-url](https://github.com/reazen/relude-url)|[![CircleCI](https://img.shields.io/circleci/project/github/reazen/relude-url/master.svg)](https://circleci.com/gh/reazen/relude-url)|[![npm](https://img.shields.io/npm/v/relude-url.svg)](https://www.npmjs.com/package/relude-url)|URL/URI utilities|
|[bs-decode](https://github.com/mlms13/bs-decode)|[![CircleCI](https://img.shields.io/circleci/project/github/mlms13/bs-decode/master.svg)](https://circleci.com/gh/mlms13/bs-decode)|[![npm](https://img.shields.io/npm/v/bs-decode.svg)](https://www.npmjs.com/package/bs-decode)|a JSON decoding library inspired by the [Elm JSON Decode Pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/)|

## Upcoming

Below are some libraries which may be coming soon to the `Relude` ecosystem:

- `relude-color` - a color manipulation library
- `relude-rationaljs-future` - an interop library for [rationaljs/future](https://github.com/RationalJS/future)
- others?