# Typeclasses

## Overview

This guide will not attempt to fully explain what typeclasses and typeclass instances are, and how they fit into functional programming, but it will try to explain how typeclasses are implemented in [bs-abstract](https://github.com/Risto-Stevcev) and `Relude`.  Also, the descriptions here are meant to help readers gain intuition, and are not intended to be 100% rigorously correct.  There are plenty of other resources on these topics which can be sought out for further reading.

## What is a typeclass?

If you are coming from the OO world, it can be useful to relate typeclasses to OO interfaces or traits; however, there are some important differences.  A typeclass describes some capability or set of capabilities of a type or set of types along with a set of laws to which all instances (implementations) of the typeclass must adhere.  A `typeclass` is sort of like the interface and laws, and a `typeclass instance` is an implementation of the `typeclass` for a particular type, which must adhere to the laws in order to be considered a valid instance of the typeclass.  A `typeclass` is not something that you extend or implement in the OO sense, it is something you implement as its own standalone module for a particular type.  Instead of passing around "rich" values which implement interfaces like you might do in the OO world, you pass around plain values along with `typeclass instance` modules that can operate on the plain values.  You can also build more advanced typeclasses and instances by composing other component typeclasses and instances.

## How can we implement typeclasses and instances in Reason/OCaml?

Languages like Haskell, PureScript, and Scala have the concept of typeclasses (basically) built-in, so there this no (or far less) friction in implementing them.  Reason/OCaml has the concept of `module type`, which can act as the concept of a typeclass, and the concept of a `module`, which when it implements a `module type`, can act as a `typeclass instance`.  We'll see some examples of `module type`s and `module`s below for defining typeclasses.

## Implicit typeclass resolution

One big downsides of OCaml compared to Haskell/PureScript/Scala is the lack of implicit typeclass resolution, or "[modular implicits](http://www.lpw25.net/papers/ml2014.pdf)" in OCaml.  Haskell and friends have the ability to implicitly (automatically) resolve typeclass instances recursively, which makes it extremely easy to write more abstract code and automatically gain the ability to use it with a wide variety of types, without having to create manually-defined specializations for each type or combination of types.

Also, the lack of modular implicits and the related lack of [ad-hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) are a few of the reasons why Reason/OCaml have to have different operators for operating on `int` and `float` values.  (They don't have to have different operators, but they do by default so they can both exist "in scope" at the same time.)  These same reasons are also why you can't use the conventional map `<$>`, bind/flatMap `>>=`, and other infix operators freely on different types in OCaml code without locally scoping the operators.

One of Relude's design goals is to provide as many of the most commonly used typeclass instances for the types and combinations of types you might encounter in most applications.  However, we do not attempt to hide the underlying machinery of bs-abstract, so you are free to define your own typeclasses and typeclass instances, and use them interchangably with the rest of the Relude modules.

## Verifying typeclass laws

Sometimes the type system alone can't represent the laws of a typeclass, or verify that any given typeclass instance adheres to said laws.  A technique that is used in most FP languages to verify typeclass laws is to use a property-based testing library like [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck).  Property based testing allows you to test that certain properties or laws hold for a given typeclass instance, by applying a wide range of randomized, but focused inputs, and making sure the laws are upheld.

## Scrap your typeclasses

You don't always need a typeclass in order to abstract some piece of functionality.  Another common technique for abstracting a capability is to use a "record of functions" - i.e. a record type that has contains some functions that you must implement in order to satisfy some set of requirements.  For example, the `Show` typeclass could alternatively be implemented as a "record of functions" like so:

```reason
type show('a) = {
  show: 'a => string
};

let logShow: (show('a), 'a) => unit = (show, a) => {
  Js.log(show.show(a));
};

logShow({ show: string_of_int }, 42);

// logs "42"
```

One key indicator as to whether to use a typeclass or a "record of functions" is whether you can think of any laws that apply to your functionality.  If you can't think of any reasonable laws, it might be best to go with the "record of functions" approach, as opposed to requiring a typeclass instance.