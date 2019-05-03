/**
  `foldLazy(defaultFcn, f, opt)` returns `f(v)` when `opt` is
  `Some(v)`. If `opt` is `None`, `foldLazy()` returns `default()`.

  The `default()` function must have no parameters and must return
  a value of the same type that `f()` returns.

  This is a *lazy* function because the default value is not evaluated
  unless `opt` is `None`.

  ### Example
  ```re
  let zero = () => 0.0;
  foldLazy(zero, (x) => {1.0 /. x}, Some(2.0)) == 0.5;
  foldLazy(zero, (x) => {1.0 /. x}, None) == 0.0;
  ```
*/
let foldLazy: (unit => 'b, 'a => 'b, option('a)) => 'b =
  (default, f, opt) =>
    switch (opt) {
    | Some(v) => f(v)
    | None => default()
    };

/**
  `fold(default, f, opt)` returns `f(v)` when `opt` is `Some(v)`.
  If `opt` is `None`, `fold()` returns `default`, which must be of
  the same type that `f()` returns.

  This is not a lazy function, as the `default` value is always
  evaluated, no matter what `opt`’s value is.

  ### Example
  ```re
  fold(0.0, (x) => {1.0 /. x}, Some(2.0)) == 0.5;
  fold(0.0, (x) => {1.0 /. x}, None) == 0.0;
  ```
*/
let fold: ('b, 'a => 'b, option('a)) => 'b =
  (default, f, opt) =>
    switch (opt) {
    | Some(v) => f(v)
    | None => default
    };

/**
  In `forEach(f, opt)`, `f()` is a function with one parameter
  that returns unit. If `opt` is `Some(v)`, `forEach()` calls
  `f(v)`. If `opt` is `None`, `forEach()` returns unit.

  You use `forEach()` to produce side effects on option values.

  ### Example
  ```re
  forEach(Js.log, Some(2)) == (); // prints 2
  forEach(Js.log, None) == (); // does not print anything
  ```
*/
let forEach: 'a. ('a => unit, option('a)) => unit =
  (f, opt) => fold((), f, opt);

/**
  `getOrElseLazy(defaultFcn, opt)` returns `v` when `opt` is
  `Some(v)`. If `opt` is `None`, `getOrElseLazy()` returns `defaultFcn()`.

  The `defaultFcn()` function must have no parameters and must return
  a value of the same type that `f()` returns.

  This is a *lazy* function because the default value is not evaluated
  unless `opt` is `None`.

  ### Example
  ```re
  let zero = () => {0};
  getOrElseLazy(zero, Some(3)) == 3;
  getOrElseLazy(zero, None) == 0;
  ```
*/
let getOrElseLazy: (unit => 'a, option('a)) => 'a =
  default => foldLazy(default, a => a);

/**
  `getOrElse(default, opt)` returns `v` when `opt` is
  `Some(v)`. If `opt` is `None`, `getOrelse()` returns `default`.

  If `opt` is `None`, `getOrElse()` returns `default`, which must be of
  the same type that as `v`.

  This is not a lazy function, as the `default` value is always
  evaluated, no matter what `opt`’s value is.

  ### Example
  ```re
  getOrElse(0, Some(3)) == 3;
  getOrElse(0, None) == 0;
  ```
*/
let getOrElse: ('a, option('a)) => 'a = default => fold(default, a => a);

/**
  `toList(opt)` returns the list `[v]` if `opt` is of the form
  `Some(v)`, or the empty list if `opt` is `None`.

  ### Example
  ```re
  toList(Some(5)) == [5];
  toList(None) == [];
  ```
*/
let toList: option('a) => list('a) = t => fold([], v => [v], t);

/**
  `toArray(opt)` returns the array `[|v|]` if `opt` is of the form
  `Some(v)`, or the empty array if `opt` is `None`.

  ### Example
  ```re
  toArray(Some(5)) == [|5|];
  toArray(None) == [| |];
  ```
*/
let toArray: option('a) => array('a) = t => fold([||], v => [|v|], t);

/**
  `isSome(opt)` returns `true` if `opt` is of the form `Some(v)`;
  `false` otherwise.
*/
let isSome: option('a) => bool = t => fold(false, _ => true, t);

/**
  `isNone(opt)` returns `true` if `opt` is `None`;
  `false` otherwise.
*/
let isNone: option('a) => bool = t => fold(true, _ => false, t);

/**
  `map(f, opt)`, when `opt` is `Some(v)`, returns `Some(f(v))`.
  When `opt` is `None`, it returns `None`.

  ### Example
  ```re
  map((x) => {x * x}, Some(12)) == Some(144);
  map((x) => {x * x}, None) == None;
  ```
*/
let map: ('a => 'b, option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Functor.map(fn, opt);

/**
  `void` discards the optional value and makes it `unit`.

  ### Example
  ```re
  Some(42) |> Option.void;
  ```
 */
let void: 'a. option('a) => option(unit) = opt => opt |> map(_ => ());

/**
  `apply(optFcn, optVal)` returns `Some(f(v))` if `optFcn`
  is `Some(f)` and `optVal` is `Some(v)`. In all other cases,
  `apply()` returns `None`.

  ### Example
  ```re
  let square = (x) => {x * x};
  apply(Some(square), Some(12)) == Some(144);
  apply(Some(square), None) == None;
  apply(None, Some(12)) == None;
  apply(None, None) == None;
  ```
*/
let apply: (option('a => 'b), option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Apply.apply(fn, opt);

/**
  `pure(v)` returns `Some(v)`.

  ### Example
  ```re
  pure("Reason") == Some("Reason");
  ```
*/
let pure: 'a => option('a) = v => BsAbstract.Option.Applicative.pure(v);

/**
  `bind(opt, f)` returns `f(v)` if `opt` is `Some(v)`,
  `None` otherwise. In this case, `f` is a function that
  takes a non-`option` argument and returns an `option` result.

  ### Example
  ```re
  let reciprocalOpt = (x) => { x == 0.0 ? None : Some(1.0 /. x) };
  bind(Some(2.0), reciprocalOpt) == Some(0.5);
  bind(Some(0.0), reciprocalOpt) == None;
  bind(None, reciprocalOpt) == None;
  ```

  `bind()` is the same as `flatMap()`, but with the arguments in
  the reverse order.

*/
let bind: (option('a), 'a => option('b)) => option('b) =
  (opt, fn) => BsAbstract.Option.Monad.flat_map(opt, fn);

/**
  `flatMap(f, opt)` returns `f(v)` if `opt` is `Some(v)`,
  `None` otherwise. In this case, `f` is a function that
  takes a non-`option` argument and returns an `option` result.

  ### Example
  ```re
  let reciprocalOpt = (x) => { x == 0.0 ? None : Some(1.0 /. x) };
  flatMap(reciprocalOpt, Some(2.0)) == Some(0.5);
  flatMap(reciprocalOpt, Some(0.0)) == None;
  flatMap(reciprocalOpt, None) == None;
  ```

  `flatMap()` is the same as `bind()`, but with the arguments in
  the reverse order.
*/
let flatMap: ('a => option('b), option('a)) => option('b) =
  (f, fa) => bind(fa, f);

/**
  `foldLeft(f, init, opt)` takes as its first argument a function `f`
  that has two arguments. The first argument is an “accumulator“ of the same type
  as `init` (the initial value of the accumulator), and the second is of
  the same type as the value wrapped in `opt`.
  `f()` returns a value of the same type as `init`.

  If `opt` is `Some(v)`, `foldLeft()` returns `f(accumulator,v)`.
  If `opt` is `None`, `foldLeft()` returns `accumulator`.

  ### Example
  ```re
  let addLength = (accum, str) => {accum + Js.String.length(str)};
  foldLeft(addLength, 0, Some("Reason")) == 6;
  foldLeft(addLength, 0, None) == 0;
  ```
*/
let foldLeft: (('b, 'a) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_left(fn, default);

/**
  `foldRight(f, init, opt)` takes as its first argument a function `f`
  that has two arguments. The first argument is of the same type as the value
  wrapped in `opt`, and the second is an “accumulator“ of the same type
  as `init` (the initial value of the accumulator).
  `f()` returns a value of the same type as `init`.

  If `opt` is `Some(v)`, `foldLeft()` returns `f(v, accumulator)`.
  If `opt` is `None`, `foldLeft()` returns `accumulator`.

  ### Example
  ```re
  let addLength = (str, accum) => {accum + Js.String.length(str)};
  foldRight(addLength, 0, Some("Reason")) == 6;
  foldRight(addLength, 0, None) == 0;
  ```
*/
let foldRight: (('a, 'b) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_right(fn, default);

/**
  `alt(opt1, opt2)` returns `opt1` if it is of the form `Some(v)`;
  otherwise it returns `opt2`.

  ### Example
  ```re
  alt(Some(3), Some(4)) == Some(3);
  alt(Some(3), None) == Some(3);
  alt(None, Some(4)) == Some(4);
  alt(None, None) == None;
  ```
*/
let alt: (option('a), option('a)) => option('a) =
  (a, b) =>
    switch (a) {
    | Some(_) as v => v
    | None => b
    };

/**
  `empty(opt)` always returns `None`, no matter what `opt` is.
*/
let empty: option('a) = None;

/**
  `filter(f, opt)` works as follows:

  - If `opt` is `Some(v)` and `f(v)` is `true`, the result is `Some(v)`.
  - If `opt` is `Some(v)` and `f(v)` is `false`, the result is `None`.
  - If `opt` is `None`, the result is `None`.

  ### Example
  ```re
  filter((x) => {x mod 2 == 0}, Some(2)) == Some(2);
  filter((x) => {x mod 2 == 0}, Some(3)) == None;
  filter((x) => {x mod 2 == 0}, None) == None;
  ```
*/
let filter: ('a => bool, option('a)) => option('a) =
  fn => foldLeft((default, v) => fn(v) ? pure(v) : default, empty);

/**
  `flatten(optOpt)` takes a value of the form `Some(Some(v))` and
  returns `Some(v)`.  If `optOpt` is `Some(None)`, the result is `None`.

  In other words, `flatten` “unwraps” one level of `Some(...)`.

  ### Example
  ```re
  flatten(Some(Some(1066))) == Some(1066);
  flatten(Some(None)) == None;
  ```
*/
let flatten: option(option('a)) => option('a) = opt => bind(opt, a => a);

/**
  In `eqBy(f, opt1, opt2)`, `f` is a function that compares two arguments
  for equality and returns `true` if they are equal.  If `opt1`
  and `opt2` are `Some(v1)` and `Some(v2)`, then `eqBy()` returns
  `f(v1, v2)`.  If both `opt1` and `opt2` are `None`, then `eqBy()`
  also returns `true`. In all other cases, the result is `false`.

  ### Example
  ```re
  let clockEqual = (a, b) => {a mod 12 == b mod 12};
  eqBy(clockEqual, Some(3), Some(15)) == true;
  eqBy(clockEqual, Some(3), Some(16)) == false;
  eqBy(clockEqual, None, Some(15)) == false;
  eqBy(clockEqual, Some(3), None) == false;
  eqBy(clockEqual, None, None) == true;
  ```
*/
let eqBy: (('a, 'a) => bool, option('a), option('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | (Some(va), Some(vb)) => innerEq(va, vb)
    | (None, None) => true
    | _ => false
    };

/**
  `eq((module M), opt1, opt2)`compares its two arguments
  for equality and returns `true` if they are equal.  If `opt1`
  and `opt2` are `Some(v1)` and `Some(v2)`, then `eq()` returns
  the result of comparing two values of their type for equality
  according to `module M`.
  If both `opt1` and `opt2` are `None`, then `eq()`
  also returns `true`. In all other cases, the result is `false`.

  `module M` must implement:
  - a type `t`
  - a function `eq(t, t)` that evaluates equality and returns a boolean

  ### Example
  ```re
  module ClockEq = {
    type t = int;
    let eq = (a, b) => {a mod 12 == b mod 12};
  };
  eq((module ClockEq), Some(3), Some(15)) == true;
  eq((module ClockEq), Some(3), Some(16)) == false;
  eq((module ClockEq), None, Some(16)) == false;
  eq((module ClockEq), Some(3), None) == false;
  eq((module ClockEq), None, None) == true;
  ```
*/
let eq =
    (
      type t,
      innerEq: (module BsAbstract.Interface.EQ with type t = t),
      a: option(t),
      b: option(t),
    )
    : bool => {
  module OptEq = BsAbstract.Option.Eq((val innerEq));
  OptEq.eq(a, b);
};

module Semigroup = BsAbstract.Option.Semigroup; /* Option Semigroup requires semigroup for inner type */

module Monoid = BsAbstract.Option.Monoid;

module Semigroup_Any: BsAbstract.Interface.SEMIGROUP_ANY = {
  /* Option Semigroup_Any behaves like Alt (no Semigroup required for inner type */
  type t('a) = option('a);
  let append = alt;
};

module Monoid_Any = {
  include Semigroup_Any;
  let empty = None;
};

module Alt = BsAbstract.Option.Alt;

module Plus = BsAbstract.Option.Plus;

module Alternative = BsAbstract.Option.Alternative;

module Functor = BsAbstract.Option.Functor;

module Apply = BsAbstract.Option.Apply;

module Applicative = BsAbstract.Option.Applicative;

module Monad = BsAbstract.Option.Monad;

module Foldable = BsAbstract.Option.Foldable;

module Traversable = BsAbstract.Option.Traversable;

module Eq = BsAbstract.Option.Eq;

module Show = BsAbstract.Option.Show;

module ApplyFunctions = BsAbstract.Functions.Apply(BsAbstract.Option.Apply);

/**
  `map2(f, opt1, opt2)` returns `Some(f(v1, v2))` if `opt1` and `opt2`
  are `Some(v1)` and `Some(v2)`. It returns `None` in all other cases.

  ### Example
  ```re
  let combine = (s, n) => {s ++ " " ++ string_of_int(n)};
  map2(combine, Some("cloud"), Some(9)) == Some("cloud 9");
  map2(combine, Some("cloud"), None) == None;
  map2(combine, None, Some(9)) == None;
  map2(combine, None, None) == None;
  ```
*/
let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c) =
  (fn, a, b) => ApplyFunctions.lift2(fn, a, b);

/**
  `map3(f, opt1, opt2, opt3)` returns `Some(f(v1, v2, v3))` if `opt1`, `opt2`,
  and `opt3` are `Some(v1)`, `Some(v2)`, and `Some(v3)`. It returns `None` in all other cases.

  We are not showing all the possible combinations in the following example.

  ### Example
  ```re
  let combine = (s1, s2, s3) => {s1 ++ s2 ++ s3};
  map3(combine, Some("a"), Some("b"), Some("c")) == Some("abc");
  map3(combine, Some("a"), None, Some("c")) == None;
  map3(combine, Some("a"), None, None) == None;
  map3(combine, None, None, None) == None;
  ```
*/
let map3:
  (('a, 'b, 'c) => 'd, option('a), option('b), option('c)) => option('d) =
  (fn, a, b, c) => ApplyFunctions.lift3(fn, a, b, c);

/**
  `map4(f, opt1, opt2, opt3, opt4)` returns `Some(f(v1, v2, v3, v4))` if `opt1`, `opt2`,
  `opt3` and `opt4` are `Some(v1)`, `Some(v2)`, `Some(v3)`, and `Some(v4)`.
  It returns `None` in all other cases.

  We are not showing all the possible combinations in the following example.

  ### Example
  ```re
  let combine = (s1, s2, s3, s4) => {s1 ++ s2 ++ s3 ++ s4};
  map4(combine, Some("a"), Some("b"), Some("c"), Some("d")) == Some("abcd");
  map4(combine, Some("a"), None, Some("c"), None) == None;
  map4(combine, None, Some("b"), Some("c"), Some("d")) == None;
  ```
*/
let map4:
  (
    ('a, 'b, 'c, 'd) => 'e,
    option('a),
    option('b),
    option('c),
    option('d)
  ) =>
  option('e) =
  (fn, a, b, c, d) => ApplyFunctions.lift4(fn, a, b, c, d);

/**
  `map4(f, opt1, opt2, opt3, opt4)` returns `Some(f(v1, v2, v3, v4, v5))`
  if `opt1`, `opt2`, `opt3`, `opt4`, and `opt5` are `Some(v1)`, `Some(v2)`,
  `Some(v3)`, `Some(v4)`, and `Some(v5)`.
  It returns `None` in all other cases.

  We are not showing all the possible combinations in the following example.

  ### Example
  ```re
  let combine = (s1, s2, s3, s4, s5) => {s1 ++ s2 ++ s3 ++ s4 ++ s5};
  map5(combine, Some("a"), Some("b"), Some("c"), Some("d"), Some("e")) == Some("abcde");
  map5(combine, Some("a"), None, Some("c"), None, Some("e")) == None;
  map5(combine, None, Some("b"), Some("c"), Some("d"), None) == None;
  ```
*/
let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    option('a),
    option('b),
    option('c),
    option('d),
    option('e)
  ) =>
  option('f) =
  (fn, a, b, c, d, e) => ApplyFunctions.lift5(fn, a, b, c, d, e);

/**
  The following submodule defines infix operators that you can
  use as shortcuts for function calls. To use them, you should:

  ```re
  open Relude.Option.Infix;
  ```
*/
module Infix = {
  /**
    `opt |? default` yields `v` if `opt` is `Some(v)`; otherwise
    it yields `default`. (Same as `getOrElse(default, opt)`.)

    ### Example
    ```re
    Some(3) |? 0 == 3;
    None |? 0 == 0;
    ```
  */
  let (|?) = (opt, default) => getOrElse(default, opt);

  /**
    `opt1 <|> opt2` yields `opt1` if it is `Some(v)`, otherwise
    it yields `opt2`. (Same as `alt(opt1, opt2)`.)

    ### Example
    ```re
    Some(2) <|> Some(3) == Some(2);
    Some(2) <|> None == None;
    None <|> Some(3) == Some(3);
    None <|> None == None;
    ```
  */
  let (<|>) = alt;

  /**
    `f <$> opt` yields Some(f(v)) if `opt` is `Some(v)`, otherwise `None`.
    (Same as `map(f, opt)`.)

    ### Example
    ```re
    let square = (x) => {x * x};
    square <$> Some(12) == Some(144);
    square <$> None == None;
    ```
  */
  let (<$>) = map;

  /**
    `optFcn <*> optVal` yields `Some(f(v))` when `optFcn`
    is `Some(f)` and optVal is `Some(v)`. In all other ases,
    `<*>` yields `None`.  (Same as `apply(optFcn, optVal)`.)

     ### Example
    ```re
    let square = (x) => {x * x};
    Some(square) <*> Some(12) == Some(144);
    Some(square) <*> None == None;
    None <*> Some(12) == None;
    None <*> None == None;
    ```
  */
  let (<*>) = apply;

  /**
    `opt >>= f` yields `f(v)` if `opt` is `Some(v)`,
  `None` otherwise. In this case, `f` is a function that
  takes a non-`option` argument and returns an `option` result.
  (Same as `bind(opt, f)`.)

  ### Example
  ```re
  let reciprocalOpt = (x) => { x == 0.0 ? None : Some(1.0 /. x) };
  Some(2.0) >>= reciprocalOpt == Some(0.5);
  Some(0.0) >>= reciprocalOpt == None;
  None >>= reciprocalOpt == None;
  ```
  */
  let (>>=) = bind;
};