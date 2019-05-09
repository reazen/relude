/**
Typeclass instances and related functions for Array
 */
module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = array('a) = BsAbstract.Array.Foldable;

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = Belt.Array.concat;
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = [||];
};

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = array('a) = BsAbstract.Array.Functor;

module Apply: BsAbstract.Interface.APPLY with type t('a) = array('a) = BsAbstract.Array.Apply;

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = array('a) = BsAbstract.Array.Applicative;

module Monad: BsAbstract.Interface.MONAD with type t('a) = array('a) = BsAbstract.Array.Monad;

module Alt: BsAbstract.Interface.ALT with type t('a) = array('a) = BsAbstract.Array.Alt;

module Plus: BsAbstract.Interface.PLUS with type t('a) = array('a) = BsAbstract.Array.Plus;

module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = array('a) = BsAbstract.Array.Alternative;

module Invariant: BsAbstract.Interface.INVARIANT with type t('a) = array('a) = BsAbstract.Array.Invariant;

module MonadZero: BsAbstract.Interface.MONAD_ZERO with type t('a) = array('a) = BsAbstract.Array.Monad_Zero;

module MonadPlus: BsAbstract.Interface.MONAD_PLUS with type t('a) = array('a) = BsAbstract.Array.Monad_Plus;

module Extend: BsAbstract.Interface.EXTEND with type t('a) = array('a) = BsAbstract.Array.Extend;

module IsoList: Relude_IsoList.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = Belt.List.toArray;
  let toList = Belt.List.fromArray;
};

let concat = SemigroupAny.append;

let empty = MonoidAny.empty;

module MonadExtensions = Relude_Extensions_Monad.MonadExtensions(Monad);

let map = MonadExtensions.map;
let void = MonadExtensions.void;
let apply = MonadExtensions.apply;
let flap = MonadExtensions.flap;
let map2 = MonadExtensions.lift2;
let map3 = MonadExtensions.lift3;
let map4 = MonadExtensions.lift4;
let map5 = MonadExtensions.lift5;

let pure = MonadExtensions.pure;
let bind = MonadExtensions.bind;
let flatMap = MonadExtensions.flatMap;
let flatten = MonadExtensions.flatten;

module FoldableExtensions =
  Relude_Extensions_Foldable.FoldableExtensions(Foldable);

include FoldableExtensions;

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

module Eq = (EqA: BsAbstract.Interface.EQ) => {
  type t = array(EqA.t);
  let eq = eqBy(EqA.eq);
};

let eq = (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

let showBy: 'a. ('a => string, array('a)) => string =
  (innerShow, xs) => {
    // TODO
    let join = intercalate((module BsAbstract.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

module Show = (ShowA: BsAbstract.Interface.SHOW) => {
  type t = array(ShowA.t);
  let show = showBy(ShowA.show);
};

let show = (type a, showA: (module BsAbstract.Interface.SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module Ord = BsAbstract.Array.Ord;