/**
  `map(f, opt)`, when `opt` is `Some(v)`, returns `Some(f(v))`.
  When `opt` is `None`, it returns `None`.

  ### Example
  ```re
  map((x) => {x * x}, Some(12)) == Some(144);
  map((x) => {x * x}, None) == None;
  ```
*/
let map: 'a 'b. ('a => 'b, option('a)) => option('b) =
  f =>
    fun
    | Some(x) => Some(f(x))
    | None => None;

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = option('a) = {
  type nonrec t('a) = option('a);
  let map = map;
};

include Relude_Extensions_Functor.FunctorExtensions(Functor);

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
let apply: 'a 'b. (option('a => 'b), option('a)) => option('b) = BsAbstract.Option.Apply.apply;

module Apply: BsAbstract.Interface.APPLY with type t('a) = option('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
  `pure(v)` returns `Some(v)`.

  ### Example
  ```re
  pure("Reason") == Some("Reason");
  ```
*/
let pure: 'a. 'a => option('a) = v => Some(v);

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = option('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

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
let bind: 'a 'b. (option('a), 'a => option('b)) => option('b) =
  (x, f) =>
    switch (x) {
    | Some(v) => f(v)
    | None => None
    };

module Monad: BsAbstract.Interface.MONAD with type t('a) = option('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

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
let foldLeft: 'a 'b. (('b, 'a) => 'b, 'b, option('a)) => 'b =
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
let foldRight: 'a 'b. (('a, 'b) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_right(fn, default);

module Foldable = BsAbstract.Option.Foldable;
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

/**
  `alt(opt1, opt2)` returns `opt1` if it is of the form `Some(v)`;
  otherwise it returns `opt2`.

  Note: the value to check is on the left, and the fallback value is on the right.

  ### Example
  ```re
  alt(Some(3), Some(4)) == Some(3);
  alt(Some(3), None) == Some(3);
  alt(None, Some(4)) == Some(4);
  alt(None, None) == None;
  ```
*/
let alt: 'a. (option('a), option('a)) => option('a) =
  (fa1, fa2) =>
    switch (fa1) {
    | Some(_) => fa1
    | None => fa2
    };

/**
Lazy version of `alt()` (doesn't evaluate the second argument unless needed

Note: the value to check is on the left, and the fallback value function is on the right.
*/
let altLazy: 'a. (option('a), unit => option('a)) => option('a) =
  (fa1, getFA2) =>
    switch (fa1) {
    | Some(_) => fa1
    | None => getFA2()
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
include Relude_Extensions_Plus.PlusExtensions(Plus);

module Alternative = BsAbstract.Option.Alternative;
include Relude_Extensions_Alternative.AlternativeExtensions(Alternative);

module Traversable: BsAbstract.Option.TRAVERSABLE_F = BsAbstract.Option.Traversable;

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
      type a,
      showA: (module BsAbstract.Interface.EQ with type t = a),
      fa1: option(a),
      fa2: option(a),
    )
    : bool => {
  module Eq = BsAbstract.Option.Eq((val showA));
  Eq.eq(fa1, fa2);
};

module Eq = (EqA: BsAbstract.Interface.EQ) => {
  type t = list(EqA.t);
  let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
};

/**
 * Converts the option to a string using the given show function
 */
let showBy: 'a. ('a => string, option('a)) => string =
  showA =>
    fun
    | Some(a) => "Some(" ++ showA(a) ++ ")"
    | None => "None";

/**
 * Converts the option to a string using the given SHOW module
 */
let show =
    (
      type a,
      showA: (module BsAbstract.Interface.SHOW with type t = a),
      fa: option(a),
    )
    : string => {
  module Show = BsAbstract.Option.Show((val showA));
  Show.show(fa);
};

module Show = BsAbstract.Option.Show;

module WithSemigroup = (S: BsAbstract.Interface.SEMIGROUP) => {
  module Semigroup = BsAbstract.Option.Semigroup(S);
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);

  module Monoid = BsAbstract.Option.Monoid(S);
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
};

module WithApplicative = (A: BsAbstract.Interface.APPLICATIVE) => {
  module Traversable = BsAbstract.Option.Traversable(A);
  include Relude_Extensions_Traversable.TraversableExtensions(Traversable);
};
