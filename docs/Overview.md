# Overview

`Relude` is a standard library for ReasonML inspired by the preludes of Haskell, Purescript, Elm, Scala cats/scalaz, and others.  `Relude` was created from a desire to combine the powerful typeclass-style abstractions found in these ecosystems with the practical utilities we deemed necessary and desirable for everyday application-level work.

`Relude` currently works with ReasonML, so the main focus has been on JavaScript-based platforms, like the browser or Node.js.  We would like to one day support native OCaml/ReasonML native, but we are not there yet.

The core typeclass interfaces used in `Relude` come from the library [bs-bastet](https://github.com/Risto-Stevcev/bs-bastet).

## Opinions

`Relude` is opinionated in certain ways.  We maybe lean more to the Haskell/Purescript side of the fence than idiomatic OCaml, although there are many similarities across these languages.  We acknowledge the differences between OCaml and Haskell, and try our best to not go too overboard with trying to be just like Haskell, Purescript, Scala, etc., but we have seen the power of abstractions like `Applicative`, `Monad`, `Traversable`, and friends, and we want to have that power in ReasonML too.  We also follow more of the JavaScript/Scala-style naming conventions (camelcase/titlecase), rather than the usual snake-case style of OCaml - this is mainly just from our background in languages that follow these conventions, and maybe for a slightly better sense of of familiarity for JavaScript developers trying to get used to a new library and way of doing things.  We've also standardized on the use of the `|>` "pipe" operator, as opposed to the `->` "pipe first" style preferred by `Belt` - this is discussed more in [Conventions](Conventions.md).

## Utility functions

`Relude` provides a wide variety of utility functions for many of the most commonly used data types, like `list('a)`, `array('a)`, `option('a)`, `Belt.Result.t('a, 'e)`, and many more.  The goal is to provide every low-level utility function you'd expect to find in any decent standard library, and to replace the need to use `Belt` or OCaml stdlib APIs.  There's nothing inherently wrong with `Belt` and the OCaml stdlib, but we've found them to be lacking in certain ways, or not implemented in the style in which we like to program.

Also, building our own standard library allows us to build out a set of additional tools and libraries, which are all built consistenly with the same underlying abstractions and conventions.  We're not trying to replace the entire universe of libraries, but just to create an ecosystem of modules that do what we need to do, in the way that we like to do it.

## Utility modules/types

`Relude` provides a set of its own unique modules, types, and functions, which we have found to be quite useful for day-to-day application and library development.  Some of these include `AsyncData`, `AsyncResult`, `Decimal`, `HList`, `HMap`, `ListZipper`, `Validation`, etc.  Many of these types were directly inspired by prior work in other FP ecosystems.

## Typeclass module types (interfaces)

`Relude` uses many of the typeclass interface definitions and implementations provided by `bs-bastet`, while also adding some of it's own typeclasses.  Using the typeclass module types from [BsBastet.Interface](https://github.com/Risto-Stevcev/bastet/blob/master/bastet/src/Interface.re) allows us to gain compatibility with any other libraries which happen to use the same typeclass signatures "for free."  E.g. if something implements a `MONAD` in another library, it should be compatible with any of the utiltiies provided by `Relude`, which use the `MONAD` module type.

## Typeclass modules (instances or implementations)

`Relude` also uses many of the actual typeclass instances (or implementations) provided by `bs-bastet`, while also providing some of our own.

## Extensions and infix operators

Using typeclass-style abstractions allows us to provide a ton of extra functionality "for free" for many modules.  For example, for any module that has an implementation of the module type `BsBastet.Interface.MONAD`, we can `include` a wide variety of extension functions and infix operators for free for that module, all implemented consistently and predictably.  If you've ever found yourself implementing `map2`, `flatten`, `length`, `toArray`, etc. for your types, it's likely that you could instead get these functions for free by implementing the corresponding typeclass.

## IO

`Relude` has an implementation of an `IO` monad, which allows for the suspension and sequencing of effectful functions.  This module serves as the main alternative to the `Js.Promise` and other varous `Future`/`Promise` implementations in the ReasonML/OCaml ecosystem.  A goal of `Relude` is to provide interop support with popular async effect systems, so if you need to interoperate with a particular async effect implementation, let us know, and we can likely provide interop support.

## Monad transformers

`Relude` provides some basic implementations of common monad transformers, like `OptionT`, `ResultT`, `StateT`, etc.  It's not yet clear whether these will prove to be of much use, as you lose a lot of the power of monad transformers when you don't have implicit typeclass resolution and ad-hoc polymorphism.

## Ecosystem

`Relude` is the core standard library, but there are also a growing number of libraries in the `Relude` ecosystem which are all built on the common types, utilities, and abstractions provided by `Relude`.  The ecosystem currently has libraries for date/time support, string parsing, JSON decoding, React utilities, etc.  Many of these can be found in the [Reazen GitHub org](https://github.com/reazen).
