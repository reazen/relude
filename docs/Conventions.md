# Conventions

This page will attempt to outline common usage patterns for using Relude.  You will likely find your own patterns, but these are some ideas that have worked well for us so far.

## Naming conventions

`Relude` uses camel and title case naming conventions for pretty much everything.  This may not be in-line with much of the OCaml ecosystem, but it's the style that we're more familiar with, and it has the side benefit of being more familiar to JavaScript developers, which is one of the core target audiences of ReasonML.

## Utilities

Utilies for common modules will be found in the corresponding top-level module of `Relude`.  E.g. all of the utility functions for dealing with `option('a)` will be found in `Relude.Option`, etc.  If you are looking for a function that operates on a `string`, try `Relude.String`.  In some cases, there are modules which provide functionality for multiple types, e.g. if you are looking for a function that operates on a `list(string)`, try `Relude.List.String`.

## "Pipe" operator (|>)

Unlike [Belt](https://rescript-lang.org/docs/manual/latest/api/belt), Relude prefers **significant-data-last** (i.e. **pipe last** or **thread last**) semantics.  This means you'll tend to use the `|>` operator with Relude functions, instead of `->`.  This was a conscious decision in favor of more natural (partial) application and composition of functions without the need for compiler magic, and is also more in line with the conventions of other FP languages.  If you would like to learn more about the tradeoffs between `|>` and `->`, here is a great article on the topic by [Javier Chávarri](https://www.javierchavarri.com/): [Data-first and data-last: a comparison](https://www.javierchavarri.com/data-first-and-data-last-a-comparison/).

Note that the `|>` operator does not actually thread the value into the last argument of the function - it simply passes the value into the next unfilled argument of the given function.  This allows you to partially apply all but one of the arguments of a function, and then feed the "significant data" into what is typically the last argument of the function.  However, you can use `|>` to fill any argument of any function - it doesn't need to be the last argument; although, the most common usecase is to fill the last.

The main benefits of `|>` is that there is no compiler magic involved (it's an operator that you can define in "userland"), it works in native OCaml, where the `->` "pipe-first" operator doesn't work automatically (it requires a ppx), and it is more in-line with the huge body of prior art from other FP ecosystems.

## Typeclasses

See the [typeclasses](typeclasses/Introduction.md) section of the documentation for more information about typeclasses.

[mel-bastet](https://github.com/johnhaley81/mel-bastet) provides many of the typeclass interfaces/signatures (module types) and typeclass instances (implementations).  `Relude` adds the concept of typeclass extensions, which are module functors that you can use to gain access to functions and modules for free by providing an instance of a typeclass signature.

## Extensions

Once you do a fair amount of pure functional programming, you start to see many patterns emerge.  Some examples include monadic function chains (`bind`/`flatMap`/`>>=`), mapping multiple applicative values and combining the results (`map2`), traversing over a data structure and "flipping the types" (`traverse`/`sequence`), tearing down data structures to produce values of other types (`foldLeft`), etc.

Using typeclasses, it's possible to provide a ton of functionality for free when you can provide an implementation for a fundamental function or two.  E.g. if you can implement `foldLeft` and `foldRight` for a particular data structure, you can derive a ton of functions for free, implemented in terms of `foldLeft` and `foldRight`.  Using the [include](https://reasonml.github.io/docs/en/module#extending-modules) mechanism from ReasonML/OCaml, you can also add these extension functions directly to your module, as if you'd implemented them by hand.  Being able to add all of these functions for free gives you a robust and predictable framework for building types and utilities.

`Relude` provides a set of `Extension` module functors, which can be used with the fundamental typeclass module types.  For example, if you create a data type, and can provide the `FUNCTOR`, `APPLY`, `APPLICATIVE`, `ALT`, `MONAD` and any other instances, you can get a bunch of functions for free, simply by `include`ing the corresponding `Extension` modules from `Relude`.

This pattern of defining typeclasses instances and then including the corresponding `Extension` and `Infix` modules can be seen everywhere in `Relude`.  See the code and tests for examples of how it's done.

## First-class modules

ReasonML/OCaml does not support implicit typeclass resolution like Haskell, Purescript, and Scala.  This is a bummer, because it puts some roadblocks in the way of seamlessly using the typeclass infrastructure that is provided by `Relude`.  For example, in `Haskell`, you can use the `>>=` (`bind`) operator anywhere you want, as long as the compiler can find a `Monad` instance for the type in question somewhere in scope.  In ReasonML/OCaml, there is no ad-hoc polymorphism, so you can't just use operators and functions from typeclasses like you can in Haskell and Purescript.

Instead, ReasonML/OCaml provides the concept of [first-class modules](https://v1.realworldocaml.org/v1/en/html/first-class-modules.html).  This allows you to pass actual OCaml modules into functions, which gives you the same power of abstraction as typeclasses, but without the implicit (automatic) resolution.  "First-class modules" can be a bit brain-bending at first, but it becomes more clear when demonstrated with simpler typeclasses like `SHOW` or `EQ`.

## `*By` functions as an alternative to first-class modules

In the places where `Relude` supports first-class modules (i.e. functions that accept a module that conforms to a certain typeclass signature), we try to also provide a corresponding higher-order function that takes a function argument that serves the same purpose as the first-class module.

For example if there is a function in `Relude.Option` `show`, which requires a first-class module `SHOW` for the type `'a` in the `option('a)`, we also try to also provide a function `showBy`, which takes a simple function `'a => string`, so you can still `show` the `option('a)` without having to create an actual `Show: SHOW` module for type `'a`.  These higher-order functions are typically named with the suffix `By` - e.g. `showBy`, `eqBy`, `compareBy`, etc.

## Infix operators

`Relude` provides infix operators for many of the core typeclasses.  These operators typically follow the conventions of the Haskell and Purescript style ecosystems, e.g. `<$>` for `map`, `>>=` for `bind`, etc.  We try not to use and invent other non-standard infix operators, but to just expose the more commonly-used infix operators that are familiar to people coming from Haskell/Purescript/etc.

The infix operators are typically provided in a separate `Infix` module for a given typeclass.  This allows you to define your own `Infix` module for your type, and pull in just the infix operators you want to expose for your module.  There are many examples of this in `Relude` - see the code for more information.
