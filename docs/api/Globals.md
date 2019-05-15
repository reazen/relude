# Globals

## Overview

`Relude.Globals` is a module which is intended to be used as a conventional global open for Relude.  The core `Relude` module is primarily intended to be used as a namespace module for exposing all the Relude modules via a consistent hierarchy, but is not intended to be used as a global open (for those who wish to use a global open for Relude).

The goal of `Relude.Globals` is to expose the most-commonly used modules, functions, and infix operators from `Relude` in a way which is not likely to negatively conflict with other pervasive modules that might also be in scope.  In some cases, `Globals` intentionally shadows modules like the stdlib `List`, as we feel we have provided a sufficient API, and users are not likely to want to access anything from the stdlib `List` module.


## Usage

```reason
open Relude.Globals;

let x = List.map(a => a + 2, [1, 2, 3]); // Relude.List

let y = IO.pure(42); // Relude.IO

...
```