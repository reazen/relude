open BsAbstract.Interface;

module Foldable: FOLDABLE with type t('a) = array('a) = BsAbstract.Array.Foldable;

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = Belt.Array.concat;
};

module MonoidAny: MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = [||];
};

module Functor: FUNCTOR with type t('a) = array('a) = BsAbstract.Array.Functor;
module Apply: APPLY with type t('a) = array('a) = BsAbstract.Array.Apply;
module Applicative: APPLICATIVE with type t('a) = array('a) = BsAbstract.Array.Applicative;
module Monad: MONAD with type t('a) = array('a) = BsAbstract.Array.Monad;
module Alt: ALT with type t('a) = array('a) = BsAbstract.Array.Alt;
module Plus: PLUS with type t('a) = array('a) = BsAbstract.Array.Plus;
module Alternative: ALTERNATIVE with type t('a) = array('a) = BsAbstract.Array.Alternative;
module Invariant: INVARIANT with type t('a) = array('a) = BsAbstract.Array.Invariant;
module MonadZero: MONAD_ZERO with type t('a) = array('a) = BsAbstract.Array.Monad_Zero;
module MonadPlus: MONAD_PLUS with type t('a) = array('a) = BsAbstract.Array.Monad_Plus;
module Extend: EXTEND with type t('a) = array('a) = BsAbstract.Array.Extend;

module IsoList: Relude_IsoList.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = Belt.List.toArray;
  let toList = Belt.List.fromArray;
};

/**
 * Include all of the functions that come for free from typeclass membership
 */

let concat = SemigroupAny.append;
let empty = MonoidAny.empty;

// TODO: include instead of rename function
module MonadFunctions = Relude_Monads.Functions(Monad);

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
// TODO: remove this
let mkString = intercalate((module BsAbstract.String.Monoid));

let fromList = IsoList.fromList;
let toList = IsoList.toList;

module Traversable = BsAbstract.Array.Traversable;

let rec eqBy: 'a. (('a, 'a) => bool, array('a), array('a)) => bool =
  (innerEq, xs, ys) => {
    // TODO: this is awkward because these fns live in Relude_Array_Base
    let head = Belt.Array.get(_, 0);
    let tailOrEmpty = Belt.Array.sliceToEnd(_, 1);
    switch (head(xs), head(ys)) {
    | (None, None) => true
    | (Some(x), Some(y)) when innerEq(x, y) =>
      eqBy(innerEq, tailOrEmpty(xs), tailOrEmpty(ys))
    | _ => false
    };
  };

module Eq = (EqA: EQ) => {
  type t = array(EqA.t);
  let eq = eqBy(EqA.eq);
};

let eq = (type a, eqA: (module EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

let showBy: 'a. ('a => string, array('a)) => string =
  (innerShow, xs) => {
    // TODO
    let join = intercalate((module BsAbstract.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

module Show = (ShowA: SHOW) => {
  type t = array(ShowA.t);
  let show = showBy(ShowA.show);
};

let show = (type a, showA: (module SHOW with type t =a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module Ord = BsAbstract.Array.Ord;
