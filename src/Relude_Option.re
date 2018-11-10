let fold: ('b, 'a => 'b, option('a)) => 'b =
  (default, f, opt) => BsAbstract.Option.maybe(~default, ~f, opt);

let getOrElse: ('a, option('a)) => 'a = default => fold(default, a => a);

let toList: option('a) => list('a) = t => fold([], v => [v], t);
let toArray: option('a) => array('a) = t => fold([||], v => [|v|], t);

let isSome: option('a) => bool = t => fold(false, _ => true, t);
let isNone: option('a) => bool = t => fold(true, _ => false, t);

let map: ('a => 'b, option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Functor.map(fn, opt);

let apply: (option('a => 'b), option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Apply.apply(fn, opt);

let pure: 'a => option('a) = v => BsAbstract.Option.Applicative.pure(v);

let flatMap: ('a => option('b), option('a)) => option('b) =
  (fn, opt) => BsAbstract.Option.Monad.flat_map(opt, fn);

let flipFlatMap: (option('a), 'a => option('b)) => option('b) =
  (opt, fn) => flatMap(fn, opt);

let foldLeft: (('b, 'a) => 'b, 'b, option('a)) => 'b =
  (fn, default) => BsAbstract.Option.Foldable.fold_left(fn, default);

let alt: (option('a), option('a)) => option('a) =
  (a, b) => BsAbstract.Option.Alt.alt(a, b);

let empty: option('a) = BsAbstract.Option.Plus.empty;

let filter: ('a => bool, option('a)) => option('a) =
  fn => foldLeft((default, v) => fn(v) ? pure(v) : default, empty);

let flatten: option(option('a)) => option('a) =
  opt => flatMap(a => a, opt);

module OptApply = BsAbstract.Functions.Apply(BsAbstract.Option.Apply);

let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c) =
  (fn, a, b) => OptApply.lift2(fn, a, b);

let map3:
  (('a, 'b, 'c) => 'd, option('a), option('b), option('c)) => option('d) =
  (fn, a, b, c) => OptApply.lift3(fn, a, b, c);

let map4:
  (
    ('a, 'b, 'c, 'd) => 'e,
    option('a),
    option('b),
    option('c),
    option('d)
  ) =>
  option('e) =
  (fn, a, b, c, d) => OptApply.lift4(fn, a, b, c, d);

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
  (fn, a, b, c, d, e) => OptApply.lift5(fn, a, b, c, d, e);

let eq: (('a, 'a) => bool, option('a), option('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | (Some(va), Some(vb)) => innerEq(va, vb)
    | (None, None) => true
    | _ => false
    };

let eqM =
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

module Infix = {
  let (|?) = (opt, default) => getOrElse(default, opt);
  let (<|>) = alt;
  let (<$>) = map;
  let (<*>) = apply;
  let (>>=) = flipFlatMap;
};
