# Identity

## Overview

`Relude.Identity` is a type which is useful for acting as an identity `Functor`/`Monad`/etc. in cases where you need to provide such a thing, but you don't need any extra effects or behavior.

Relude doesn't currently have monad transformers like `OptionT` or `ReaderT`, but if we did, a common use of `Identity` would be to create a plain `Reader` monad from a `ReaderT` transformer by using the `Identity` monad as the `ReaderT` monad type.