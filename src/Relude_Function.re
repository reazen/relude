/**
  This module defines functions that let you manipulate
  other functions.
*/

/**
  `identity(x)` returns `x`. This is useful when you need
  to supply a function but don’t want to transform any values.
*/
let identity: 'a. 'a => 'a = a => a;

/**
  `id` is a synonym for `identity`.
*/
let id: 'a. 'a => 'a = identity;

/**
  `const(x, y)` returns `x`.

  ### Example
  ```re
  const(3, "ignore") == 3;
  const("keep", -1) == "keep";
  ```
*/
let const: 'a 'b. ('a, 'b) => 'a = (a, _) => a;

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
let flip: 'a 'b 'c. (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

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
let compose: 'a 'b 'c. ('b => 'c, 'a => 'b, 'a) => 'c = (f, g, a) => f(g(a));

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
let flipCompose: 'a 'b 'c. ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, a) => g(f(a));

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
let andThen: 'a 'b 'c. ('a => 'b, 'b => 'c, 'a) => 'c = flipCompose;

/**
 * Converts a function that takes a tuple 2 as an argument to a normal curried function
 */
let curry2: 'a 'b 'c. ((('a, 'b)) => 'c, 'a, 'b) => 'c =
  (f, a, b) => f((a, b));

/**
 * Converts a function that takes a tuple-3 as an argument to a normal curried function
 */
let curry3: 'a 'b 'c 'd. ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd =
  (f, a, b, c) => f((a, b, c));

/**
 * Converts a function that takes a tuple-4 as an argument to a normal curried function
 */
let curry4: 'a 'b 'c 'd 'e. ((('a, 'b, 'c, 'd)) => 'e, 'a, 'b, 'c, 'd) => 'e =
  (f, a, b, c, d) => f((a, b, c, d));

/**
 * Converts a function that takes a tuple-5 as an argument to a normal curried function
 */
let curry5:
  'a 'b 'c 'd 'e 'f.
  ((('a, 'b, 'c, 'd, 'e)) => 'f, 'a, 'b, 'c, 'd, 'e) => 'f
 =
  (f, a, b, c, d, e) => f((a, b, c, d, e));

/**
 * Converts a normal curried function of two arguments to a function that takes a tuple-2 as an argument
 */
let uncurry2: 'a 'b 'c. (('a, 'b) => 'c, ('a, 'b)) => 'c =
  (f, (a, b)) => f(a, b);

/**
 * Converts a normal curried function of 3 arguments to a function that takes a tuple-3 as an argument
 */
let uncurry3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd =
  (f, (a, b, c)) => f(a, b, c);

/**
 * Converts a normal curried function of 4 arguments to a function that takes a tuple-4 as an argument
 */
let uncurry4: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd) => 'e, ('a, 'b, 'c, 'd)) => 'e =
  (f, (a, b, c, d)) => f(a, b, c, d);

/**
 * Converts a normal curried function of 5 arguments to a function that takes a tuple-5 as an argument
 */
let uncurry5:
  'a 'b 'c 'd 'e 'f.
  (('a, 'b, 'c, 'd, 'e) => 'f, ('a, 'b, 'c, 'd, 'e)) => 'f
 =
  (f, (a, b, c, d, e)) => f(a, b, c, d, e);

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
let map: 'a 'b 'r. ('a => 'b, 'r => 'a, 'r) => 'b = (aToB, rToA, r) => aToB(rToA(r)); /* Same as compose */

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
let apply: 'a 'b 'r. (('r, 'a) => 'b, 'r => 'a, 'r) => 'b =
  (rToAToB, rToA, r) => rToAToB(r, rToA(r));

/**
  `pure` is a synonym for `const`
*/
let pure: 'a 'r. ('a, 'r) => 'a = (a, _) => a;

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
let bind: 'a 'b 'r. ('r => 'a, ('a, 'r) => 'b, 'r) => 'b =
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
let flatMap: 'a 'b 'r. (('a, 'r) => 'b, 'r => 'a, 'r) => 'b = (f, fa) => bind(fa, f);

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

module WithArgument = (R: BsAbstract.Interface.TYPE) => {
  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = R.t => 'a = {
    type t('a) = R.t => 'a;
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Apply: BsAbstract.Interface.APPLY with type t('a) = R.t => 'a = {
    include Functor;
    let apply = apply;
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = R.t => 'a = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Monad: BsAbstract.Interface.MONAD with type t('a) = R.t => 'a = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
    include Infix;
  };
};