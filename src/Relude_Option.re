let fold: (unit => 'b, 'a => 'b, option('a)) => 'b =
  (default, f, opt) =>
    switch (opt) {
    | Some(v) => f(v)
    | None => default()
    };

let foldStrict: ('b, 'a => 'b, option('a)) => 'b =
  (default, f, opt) =>
    switch (opt) {
    | Some(v) => f(v)
    | None => default
    };

let getOrElse: (unit => 'a, option('a)) => 'a =
  default => fold(default, a => a);

let getOrElseStrict: ('a, option('a)) => 'a =
  default => foldStrict(default, a => a);

let toList: option('a) => list('a) = t => fold(_ => [], v => [v], t);

let toArray: option('a) => array('a) = t => fold(_ => [||], v => [|v|], t);

let isSome: option('a) => bool = t => fold(_ => false, _ => true, t);

let isNone: option('a) => bool = t => fold(_ => true, _ => false, t);

let map: ('a => 'b, option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Functor.map(fn, opt);

let apply: (option('a => 'b), option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Apply.apply(fn, opt);

let pure: 'a => option('a) = v => BsAbstract.Option.Applicative.pure(v);

let bind: (option('a), 'a => option('b)) => option('b) =
  (opt, fn) => BsAbstract.Option.Monad.flat_map(opt, fn);

let flatMap: ('a => option('b), option('a)) => option('b) =
  (f, fa) => bind(fa, f);

let foldLeft: (('b, 'a) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_left(fn, default);

let foldRight: (('a, 'b) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_right(fn, default);

let alt: (option('a), option('a)) => option('a) =
  (a, b) =>
    switch (a) {
    | Some(_) as v => v
    | None => b
    };

let empty: option('a) = None;

let filter: ('a => bool, option('a)) => option('a) =
  fn => foldLeft((default, v) => fn(v) ? pure(v) : default, empty);

let flatten: option(option('a)) => option('a) = opt => bind(opt, a => a);

let eqBy: (('a, 'a) => bool, option('a), option('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | (Some(va), Some(vb)) => innerEq(va, vb)
    | (None, None) => true
    | _ => false
    };

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

let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c) =
  (fn, a, b) => ApplyFunctions.lift2(fn, a, b);

let map3:
  (('a, 'b, 'c) => 'd, option('a), option('b), option('c)) => option('d) =
  (fn, a, b, c) => ApplyFunctions.lift3(fn, a, b, c);

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

module Infix = {
  let (|?) = (opt, default) => getOrElse(default, opt);
  let (<|>) = alt;
  let (<$>) = map;
  let (<*>) = apply;
  let (>>=) = flatMap;
};
