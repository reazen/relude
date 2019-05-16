# Relude.Extensions.Foldable

## Overview

`Relude.Extensions.Foldable` contains module functors which give you access to a wide variety of functions that you can get "for free" when you have a `Foldable` typeclass instance.  For example, if you have a `Foldable` instance for `array('a)`, you can get functions like `any`, `all`, `length`, `forEach`, and many more for `array('a)`.

If you have additional typeclass instances like a `Monoid`, `Eq`, `Ord`, etc. for the innner type of the `Foldable`, you can get access to additional functions using other module functors like `FoldableMonoidExtensions`, `FoldableEqExtensions`, etc.

## Usage

TODO