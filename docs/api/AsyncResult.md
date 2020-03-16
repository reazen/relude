# Relude.AsyncResult

## Overview

`AsyncResult` is a module containing a type `t('a, 'e)`, which is basically an alias and specialization of `AsyncData.t(result('a, 'e))`. This variant type can be used to represent the different states in which a data value can exist while being loaded asynchronously, with the possibility of either success (`'a`) or failure (`'e`).

Like [AsyncData](api/AsyncData.md), `AsyncResult` is similar to the [Elm RemoteData](https://github.com/krisajenkins/remotedata), with the following key difference:

- `AsyncResult` has a `Reloading(result('a, 'e))` case which can represent a scenario where you've previously loaded data, and are reloading it while retaining the current/previous value or error.

The `map`, `bimap` `apply`, `flatMap`/`bind`, etc. functions have been specialized to operate on the innermost `'a` and `'e` types, so you rarely need to do nested pattern matches with this type.

## Usage

TODO
