# Relude

[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude/CI/master)](https://github.com/reazen/relude/actions)
[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude)
[![Coveralls](https://img.shields.io/coveralls/github/reazen/relude.svg)](https://coveralls.io/github/reazen/relude)

Relude is a **ReasonML**/**OCaml** standard library
replacement ("prelude") written in [ReasonML](https://reasonml.github.io/),
targeting compilation to **JavaScript** via the
[Melange](https://github.com/melange-re/melange) compiler.

Relude aims to provide a robust collection of modules, types, and functions
built on top of reusable typeclass-style abstractions, with a focus on
day-to-day practical use. Relude has its foundation on the math concepts
found in most modern pure functional languages, but our goal is to make the
library as easy-to-use as a library like [Lodash](https://lodash.com/docs)
for those who don't know or care about the underlying abstractions. This is
made possible by OCaml's module system and the foundational work that has
gone into [bs-bastet](https://github.com/Risto-Stevcev/bs-bastet), on
which much of Relude is based.

The API of Relude is inspired by [PureScript's Prelude](https://pursuit.purescript.org/packages/purescript-prelude), as well as other FP ecosystems including Scala, Haskell, and Elm.

Relude is currently at a 0.x version as the API and library organization
settles, but we feel the library is quite stable and comprehensive, and is
in-use in production at several companies that we know about. The main Relude
authors use the library every day and the development is driven by actual
application needs and use-cases, but all backed by the abstractions and
principles we love from languages like Haskell/PureScript/Scala/etc. We're
not trying to re-create Haskell in OCaml/ReasonML - we just want to have some
of the abstractions available to us in a pragmatic format.

That said, we feel the library is usable by all ReasonML users, not just
experts. There will be some learning involved, but we hope to aid that with
blog posts and more documentation, and answering questions or trying to give
advice (if we feel qualified). The beauty of using these common abstractions
is that we don't have to take the full responsibility for explaining
everything from scratch, as there are countless other resources and FP
experts out there to help too.

The API is comprehensive in terms of the tools you likely need for day-to-day
application development - we have utilities for all the core data types
(`string`, `int`, `float`, `list`, `array`, `option`, `Result`, functions,
tuples etc.), some of our own types (`AsyncResult`, `AsyncData`, `Ior`,
`Validation`, `ListZipper`, `Void`, etc.), a general purpose bi-functor `IO`
effect type, basic monad transformers, and a typeclass-based extension system
to grant all of our types with consistent and principled extensions and
operators.

The ideas and concepts in this library come from the rich history of category
theory, abstract algebra, and typed functional programming libraries like
Haskell, Scala, PureScript, and many others, and we are just humble students.

## Quick links

- [Overview](Overview.md)
- [Installation](Installation.md)
- [Conventions](Conventions.md)
- [Status](Status.md)
- [Developer info](Developers.md)
- [FAQ](FAQ.md)
- [Typeclasses](typeclasses/Introduction.md)
- [API](api/Introduction.md)

## Ecosystem

The [Reazen](https://github.com/reazen) GitHub organization, and some individual contributors have a variety of libraries that are built with [Relude](https://github.com/reazen/relude):

|GitHub|Build|Package|Description|
|-------|-----|-------|-----------|
|[relude](https://github.com/reazen/relude)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude/CI/master)](https://github.com/reazen/relude/actions)|[![npm](https://img.shields.io/npm/v/relude.svg)](https://www.npmjs.com/package/relude)|ReasonML prelude/standard library|
|[relude-csv](https://github.com/reazen/relude-csv)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-csv/CI/master)](https://github.com/reazen/relude-csv/actions)|[![npm](https://img.shields.io/npm/v/relude-csv.svg)](https://www.npmjs.com/package/relude-csv)|Pure functional CSV parser library|
|[relude-eon](https://github.com/reazen/relude-eon)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-eon/CI/master)](https://github.com/reazen/relude-eon/actions)|[![npm](https://img.shields.io/npm/v/relude-eon.svg)](https://www.npmjs.com/package/relude-eon)|a native Reason/OCaml date/time library|
|[relude-fetch](https://github.com/reazen/relude-fetch)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-fetch/CI/master)](https://github.com/reazen/relude-fetch/actions)|[![npm](https://img.shields.io/npm/v/relude-fetch.svg)](https://www.npmjs.com/package/relude-fetch)|an interop and utility library for the [fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) API|
|[relude-parse](https://github.com/reazen/relude-parse)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-parse/CI/master)](https://github.com/reazen/relude-parse/actions)|[![npm](https://img.shields.io/npm/v/relude-parse.svg)](https://www.npmjs.com/package/relude-parse)|a monadic string parsing library|
|[relude-random](https://github.com/reazen/relude-random)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-random/CI/master)](https://github.com/reazen/relude-random/actions)|[![npm](https://img.shields.io/npm/v/relude-random.svg)](https://www.npmjs.com/package/relude-random)|Pure functional pseudo-random value generation|
|[relude-reason-react](https://github.com/reazen/relude-reason-react)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-reason-react/CI/master)](https://github.com/reazen/relude-reason-react/actions)|[![npm](https://img.shields.io/npm/v/relude-reason-react.svg)](https://www.npmjs.com/package/relude-reason-react)|utilities for [ReasonReact](https://reasonml.github.io/reason-react) based on `Relude` types and conventions|
|[relude-url](https://github.com/reazen/relude-url)|[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-url/CI/master)](https://github.com/reazen/relude-url/actions)|[![npm](https://img.shields.io/npm/v/relude-url.svg)](https://www.npmjs.com/package/relude-url)|URL/URI utilities|
|[bs-decode](https://github.com/mlms13/bs-decode)|[![CircleCI](https://img.shields.io/circleci/project/github/mlms13/bs-decode/master.svg)](https://circleci.com/gh/mlms13/bs-decode)|[![npm](https://img.shields.io/npm/v/bs-decode.svg)](https://www.npmjs.com/package/bs-decode)|a JSON decoding library inspired by the [Elm JSON Decode Pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/)|

## Upcoming

Below are some libraries which may be coming soon to the `Relude` ecosystem:

- `relude-color` - a color manipulation library
- `relude-rationaljs-future` - an interop library for [rationaljs/future](https://github.com/RationalJS/future)
- See https://github.com/reazen/relude/issues/24 for more discussion

## Contact

- Open a [GitHub issue](https://github.com/reazen/relude/issues) if you have any feature requests, ideas, or bugs to report
- Find us on the [ReasonML Discord Chat](https://discordapp.com/channels/235176658175262720/235176658175262720)
    - `mulmus#4010`
    - `whitehouse3001#3245`

## Contributing

We are very open to contributions and for community help - just let us know
what you'd like to do, and if you want to discuss the approach before jumping
in.
