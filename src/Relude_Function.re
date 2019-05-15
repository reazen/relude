/**
  This module defines functions that let you manipulate
  other functions.
*/

/**
  `identity(x)` returns `x`. This is useful when you need
  to supply a function but don’t want to transform any values.
*/
let identity: 'a => 'a = a => a;

/**
  `id` is a synonym for `identity`.
*/
let id: 'a => 'a = identity;

/**
  `const(x, y)` returns `x`.
  
  ### Example
  ```re
  const(3, "ignore") == 3;
  const("keep", -1) == "keep";
  ```
*/
let const: ('a, 'b) => 'a = (a, _) => a;

/**
  `flip(f, a, b)` has a two-parameter function `f()` as its
  first parameter. It calls `f(b, a)`, thus “flipping“ the
  arguments to `f()`.
  
  ### Example
  ```re
  let formula = (x, y) => {x + 2 * y};
  formula(3, 5) == 13;
  flip(formula, 5, 3) == 13;
  ```
*/
let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

/**
  `compose(f, g, a)` is the equivalent of `f(g(a))`.
  
  ### Example
  ```re
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  compose(square, double, 3) == 36;
  compose(double, square, 3) == 18;
  ```
*/
let compose: ('b => 'c, 'a => 'b, 'a) => 'c = (f, g, a) => f(g(a));

/**
  `flipCompose(f, g, a)` is the equivalent of `g(f(a))`.
  
  ### Example
  ```re
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  flipCompose(square, double, 3) == 18;
  flipCompose(double, square, 3) == 36;
  ```
*/
let flipCompose: ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, a) => g(f(a));

/**
  `andThen` is a synonym for `flipCompose`
  
  You can use this synonym with the “pipe first“ operator:
  
  ```re
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  let addFive = (x) => {x + 5};
  
  let formula = square -> andThen(double) -> andThen(addFive);
  formula(3);
  ```
*/
let andThen: ('a => 'b, 'b => 'c, 'a) => 'c = flipCompose;

/**
  `pure` is a synonym for `const`
*/
let pure: ('a, 'r) => 'a = (a, _) => a;

/**
  `map` is a synonym for `compose` and is the equivalent of `f(g(a))`.
  
  ### Example
  ```re
  let square = (x) => {x * x};
  let double = (x) => {2 * x};
  map(square, double, 3) == 36;
  map(double, square, 3) == 18;
  ```
*/
  
let map: ('a => 'b, 'r => 'a, 'r) => 'b = (aToB, rToA, r) => aToB(rToA(r)); /* Same as compose */

/**
  In `apply(hof, f, a)`, `hof` is a higher-order function that takes one argument
  and returns a new function that also takes one argument.
  
  The result of `apply()` is equivalent to:
  
  ```
  let g = hof(a);
  g(f(a))
  ```
  
  ### Example
  ```re
  // This is the higher-order function
  let showResult = (n) => {
    (x: float) => {"input " ++ string_of_int(n)
      ++ " yields " ++ Js.Float.toString(x)
    }
  };
  
  let cube = (x) => { float_of_int(x * x * x) };
  
  apply(showResult, cube, 5) == "input 5 yields 125";
  ```
*/
let apply: (('r, 'a) => 'b, 'r => 'a, 'r) => 'b =
  (rToAToB, rToA, r) => rToAToB(r, rToA(r));

/**
  In `bind(f, hof, a)`, `hof` is a higher-order function that takes one argument
  and returns a new function that also takes one argument.
  
  The result of `bind()` is equivalent to:
  
  ```
  let g = hof(f(a));
  g(a)
  ```
  
  ### Example
  ```re
  // This is the higher-order function
  let showResult = (x) => {
    (n: int) => {"input " ++ string_of_int(n)
      ++ " yields " ++ Js.Float.toString(x)
    }
  };
  
  let cube = (x) => { float_of_int(x * x * x) };
  
  bind(cube, showResult, 5) == "input 5 yields 125";
  ```
*/
let bind: ('r => 'a, ('a, 'r) => 'b, 'r) => 'b =
  (rToA, arToB, r) => arToB(rToA(r), r);

/**
  In `flatMap(hof, f, a)`, `hof` is a higher-order function that takes one argument
  and returns a new function that also takes one argument. It is the same as
  `bind()`, but with the first two arguments in reverse order.
  
  The result of `flatMap()` is equivalent to:
  
  ```
  let g = hof(f(a));
  g(a)
  ```
  
  ### Example
  ```re
  // This is the higher-order function
  let showResult = (x) => {
    (n: int) => {"input " ++ string_of_int(n)
      ++ " yields " ++ Js.Float.toString(x)
    }
  };
  
  let cube = (x) => { float_of_int(x * x * x) };
  
  flatMap(showResult, cube, 5) == "input 5 yields 125";
  ```
*/
let flatMap: (('a, 'r) => 'b, 'r => 'a, 'r) => 'b = (f, fa) => bind(fa, f);

module Functor = BsAbstract.Function.Functor;

module Apply = BsAbstract.Function.Apply;

/**
  The `Infix` submodule provides two infix operators
  for function composition. To use it, you should
  
  ```re
  open Relude.Function.Infix;
  ```
*/
module Infix = {

  /**
    The `<<` operator returns a function that is the equivalent of
    calling `compose()` with its function arguments.

    `(f << g)(x)` is the equivalent of `f(g(x))`.
    
    ### Example
    ```reason
    let sqrtCompFloor = sqrt << floor;
    sqrtCompFloor(4.5) == 2.0;
    ```
  */
  let (<<) = compose;

  /**
    The `>>` operator returns a function that is the equivalent of
    calling `flipCompose()` with its function arguments.

  `(f >> g)(x)` is the equivalent of `g(f(x))`.
  
    ### Example
    ```reason
    let floorFlipSqrt = floor >> sqrt;
    floorFlipSqrt(4.5) == 2.0;
    ```
  */
  let (>>) = flipCompose;
};
