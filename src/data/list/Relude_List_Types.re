open BsAbstract.Interface;

module Foldable: FOLDABLE with type t('a) = list('a) = BsAbstract.List.Foldable;

module SemigroupAny: SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = (xs, ys) => Belt.List.concat(xs, ys);
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = [];
};

module Functor: FUNCTOR with type t('a) = list('a) = BsAbstract.List.Functor;
module Apply: APPLY with type t('a) = list('a) = BsAbstract.List.Apply;
module Applicative: APPLICATIVE with type t('a) = list('a) = BsAbstract.List.Applicative;
module Monad: MONAD with type t('a) = list('a) = BsAbstract.List.Monad;
module Alt: ALT with type t('a) = list('a) = BsAbstract.List.Alt;
module Plus: PLUS with type t('a) = list('a) = BsAbstract.List.Plus;
module Alternative: ALTERNATIVE with type t('a) = list('a) = BsAbstract.List.Alternative;

module IsoArray: Relude_IsoArray.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = Belt.List.fromArray;
  let toArray = Belt.List.toArray;
};

/**
 * Include all of the functions that come for free from typeclass membership
 */

let concat = SemigroupAny.append;
let empty = MonoidAny.empty;

module MonadFunctions = Relude_Monads.Functions(Monad);

// TODO: it would be great if we could just `include` this and call it good, but
// it brings `module Infix` which doesn't let us override/extend that. :(
// include MonadFunctions;

let map = MonadFunctions.map;
let void = MonadFunctions.void;
let apply = MonadFunctions.apply;
let flap = MonadFunctions.flap;
let map2 = MonadFunctions.lift2;
let map3 = MonadFunctions.lift3;
let map4 = MonadFunctions.lift4;
let map5 = MonadFunctions.lift5;
let pure = MonadFunctions.pure;
let bind = MonadFunctions.bind;
let flatMap = MonadFunctions.flatMap;
let flatten = MonadFunctions.flatten;

include Relude_Foldables.Functions(Foldable);

let fromArray = IsoArray.fromArray;
let toArray = IsoArray.toArray;

/**
 * Include typeclass membership that cares about the inner 'a
 */
module Traversable: BsAbstract.List.TRAVERSABLE_F = BsAbstract.List.Traversable;

let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    foldLeft(
      ((acc, result), curr) => {
        let nextAcc = f(acc, curr);
        (nextAcc, [nextAcc, ...result]);
      },
      (init, []),
      xs,
    )
    |> snd
    |> Belt.List.reverse; // TODO use our own implementation

let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    foldRight(
      (curr, (acc, result)) => {
        let nextAcc = f(curr, acc);
        (nextAcc, [nextAcc, ...result]);
      },
      (init, []),
      xs,
    )
    |> snd;


let rec eqBy: (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) when innerEq(x, y) => eqBy(innerEq, xs, ys)
    | _ => false
    };

module Eq = (EqA: EQ) => {
  type t = list(EqA.t);
  let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
};

let eq = (type a, eqA: (module EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

let showBy: ('a => string, list('a)) => string =
  (innerShow, xs) => {
    // TODO: change to Relude_String_Types when that exists
    let join = intercalate((module BsAbstract.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

module Show = (ShowA: SHOW) => {
  type t = list(ShowA.t);
  let show = xs => showBy(ShowA.show, xs);
};

let show = (type a, showA: (module SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};
