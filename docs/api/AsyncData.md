# Relude.AsyncData

## Overview

`Relude.AsyncData` contains an variant type `t('a)` for representing the different states in which a value can be during an asynchronous data load.  Because the type only has a single type parameter `'a`, it cannot represent failures by default, but you can choose to use any type for `'a`, including `option('b)` (for data that may or may not load), `result('b, 'e)` (for data that can fail to load), etc.  If you need to represent an error condition along with the success condition, see [AsyncResult](api/AsyncResult.md).

The variant type has the following constructors: `Init`, `Loading`, `Reloading('a)`, and `Complete('a)`.

This type is similar to the [Elm RemoteData](https://github.com/krisajenkins/remotedata) type, with a few key differences:

1. `AsyncData` does not have a way to represent failures - see [AsyncResult](api/AsyncResult.md)
1. `AsyncData` has a `Reloading('a)` state which can be used to indicate that data is being refreshed/reloaded/saved while retaining a current (or previous) value.  E.g. if you have loaded some data, and need to reload it but want to keep the current value around for display purposes, you might use the `Reloading('a)` state rather than the `Loading` state which does not carry any data.

`AsyncData` has instances of various typeclasses like `Functor`, `Applicative`, `Monad` etc.  which unlock the ability to compose and combine `AsyncData` values using functions like `map2`-`map5`, `flatMap`, `traverse`, etc.  For example, if you have multiple pieces of data that are being loaded asynchronously, you can combine the results of several `AsyncData` values into a single tupled `AsyncData` value using `map2`, `map3`, etc. or any of the other combinators.

## Usage

TODO
