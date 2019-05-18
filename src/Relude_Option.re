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
/* Comes in via extensions now
 let forEach: 'a. ('a => unit, option('a)) => unit =
   (f, opt) => fold((), f, opt);
   */

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
// Extensions
//let toList: option('a) => list('a) = t => fold([], v => [v], t);

/**
  `toArray(opt)` returns the array `[|v|]` if `opt` is of the form
  `Some(v)`, or the empty array if `opt` is `None`.

  ### Example
  ```re
  toArray(Some(5)) == [|5|];
  toArray(None) == [| |];
  ```
*/
// Extensions
//let toArray: option('a) => array('a) = t => fold([||], v => [|v|], t);

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
// Extensions
/*
 let map: ('a => 'b, option('a)) => option('b) =
   (fn, opt) => BsAbstract.Option.Functor.map(fn, opt);
   */

/**
  `void` discards the optional value and makes it `unit`.

  ### Example
  ```re
  Some(42) |> Option.void;
  ```
 */
// Extensions
//let void: 'a. option('a) => option(unit) = opt => opt |> map(_ => ());

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
// Extensions
/*
 let apply: (option('a => 'b), option('a)) => option('b) =
   (fn, opt) => BsAbstract.Option.Apply.apply(fn, opt);
   */

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
/*
 let bind: (option('a), 'a => option('b)) => option('b) =
   (opt, fn) => BsAbstract.Option.Monad.flat_map(opt, fn);
   */

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
// Extensions
/*
 let flatMap: ('a => option('b), option('a)) => option('b) =
   (f, fa) => bind(fa, f);
   */

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
// Extensions
/*
 let foldRight: (('a, 'b) => 'b, 'b, option('a)) => 'b =
   (fn, default) => BsAbstract.Option.Foldable.fold_right(fn, default);
   */

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
  `empty` is the empty value (`None`)
*/
let empty: option('a) = None;

/**
  `filter(f, opt)` works as follows:

  - If `opt` is `Some(v)` and `f(v)` is `true`, the result is `Some(v)`.
  - If `opt` is `Some(v)` and `f(v)` is `false`, the result is `None`.
  - If `opt` is `None`, the result is `None`.

  ### Example
  ```re
  let isEven = x => x mod 2 == 0;
  filter(isEven, Some(2)) == Some(2);
  filter(isEven, Some(3)) == None;
  filter(isEven, None) == None;
  ```
*/
let filter: 'a. ('a => bool, option('a)) => option('a) =
  fn => foldLeft((default, v) => fn(v) ? pure(v) : default, empty);

/**
  `filterNot` is the inverse of `filter`, meaning `Some` values are preserved
  if the provided predicate function returns false.

  ### Example
  ```re
  let isEven = x => x mod 2 == 0;
  filterNot(isEven, Some(1)) == Some(1);
  filterNot(isEven, Some(2)) == None;
  ```
*/
let filterNot: 'a. ('a => bool, option('a)) => option('a) =
  f => filter(a => !f(a));

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
// Extensions
//let flatten: option(option('a)) => option('a) = opt => bind(opt, a => a);

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

module WithSemigroup = (S: BsAbstract.Interface.SEMIGROUP) => {
  module Semigroup = BsAbstract.Option.Semigroup(S);
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);

  module Monoid = BsAbstract.Option.Monoid(S);
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
};

module Semigroup_Any: BsAbstract.Interface.SEMIGROUP_ANY = {
  type t('a) = option('a);
  let append = alt;
};

module Monoid_Any = {
  include Semigroup_Any;
  let empty = None;
};

module Alt = BsAbstract.Option.Alt;
include Relude_Extensions_Alt.AltExtensions(Alt);

module Plus = BsAbstract.Option.Plus;
//include Relude_Extensions_Plus.PlusExtensions(Plus);

module Alternative = BsAbstract.Option.Alternative;
//include Relude_Extensions_Alternative.AlternativeExtensions(Alternative);

module Functor = BsAbstract.Option.Functor;
include Relude_Extensions_Functor.FunctorExtensions(Functor);

module Apply = BsAbstract.Option.Apply;
include Relude_Extensions_Apply.ApplyExtensions(Apply);

module Applicative = BsAbstract.Option.Applicative;
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

module Monad = BsAbstract.Option.Monad;
include Relude_Extensions_Monad.MonadExtensions(Monad);

module Foldable = BsAbstract.Option.Foldable;
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

module WithApplicative = (A: BsAbstract.Interface.APPLICATIVE) => {
  module Traversable = BsAbstract.Option.Traversable(A);
  include Relude_Extensions_Traversable.TraversableExtensions(Traversable);
};

module Eq = BsAbstract.Option.Eq;

module Show = BsAbstract.Option.Show;

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Alt.AltInfix(Alt);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);

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
};