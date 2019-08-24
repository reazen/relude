# Conventions

This page will attempt to outline common usage patterns for using Relude.  You will likely find your own patterns, but these are some ideas that have worked well for us so far.

## Typeclasses

[bs-abstract](https://github.com/Risto-Stevcev/bs-abstract) provides many of the typeclasses (module types) and base typeclass instances and function implementations, but we wanted a more "batteries-included" style of prelude, where we could add additional modules and functions that are useful, but don't have an obvious place to live in the more abstract world of bs-abstract.  We also wanted a place where we could write the modules in the style that we wanted, and to have a place to experiment with new ideas.

## "Pipe last" operator (|>)

Unlike [Belt](https://bucklescript.github.io/bucklescript/api/Belt.html), Relude prefers **significant-data-last** (i.e. **pipe last** or **thread last**) semantics.  This means you'll tend to use the `|>` operator with Relude functions, instead of `->`.  This was a conscious decision in favor of more natural (partial) application and composition of functions without the need for compiler magic, and is also more in line with the conventions of other FP languages.  If you would like to learn more about the tradeoffs between `|>` and `->`, here is a great article on the topic by [Javier Chávarri](https://www.javierchavarri.com/): [Data-first and data-last: a comparison](https://www.javierchavarri.com/data-first-and-data-last-a-comparison/).

## Extensions

TODO

## First-class modules

TODO

## *By functions as an alternative to modules as arguments

TODO

## Infix operators

TODO