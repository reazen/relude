module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = Belt.Array.concat;
};
let concat = SemigroupAny.append;

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = [||];
};
let empty = MonoidAny.empty;

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = array('a) = BsAbstract.Array.Functor;
include Relude_Extensions_Functor.FunctorExtensions(Functor);

module Apply: BsAbstract.Interface.APPLY with type t('a) = array('a) = BsAbstract.Array.Apply;
include Relude_Extensions_Apply.ApplyExtensions(Apply);

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = array('a) = BsAbstract.Array.Applicative;
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

module Monad: BsAbstract.Interface.MONAD with type t('a) = array('a) = BsAbstract.Array.Monad;
include Relude_Extensions_Monad.MonadExtensions(Monad);

module Alt: BsAbstract.Interface.ALT with type t('a) = array('a) = BsAbstract.Array.Alt;
include Relude_Extensions_Alt.AltExtensions(Alt);

module Plus: BsAbstract.Interface.PLUS with type t('a) = array('a) = BsAbstract.Array.Plus;

module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = array('a) = BsAbstract.Array.Alternative;

module Invariant: BsAbstract.Interface.INVARIANT with type t('a) = array('a) = BsAbstract.Array.Invariant;

module MonadZero: BsAbstract.Interface.MONAD_ZERO with type t('a) = array('a) = BsAbstract.Array.Monad_Zero;

module MonadPlus: BsAbstract.Interface.MONAD_PLUS with type t('a) = array('a) = BsAbstract.Array.Monad_Plus;

module Extend: BsAbstract.Interface.EXTEND with type t('a) = array('a) = BsAbstract.Array.Extend;

module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = array('a) = BsAbstract.Array.Foldable;
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

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

let eq =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

module Ord = BsAbstract.Array.Ord;

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

let show =
    (type a, showA: (module BsAbstract.Interface.SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module IsoList: Relude_IsoList.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = Belt.List.toArray;
  let toList = Belt.List.fromArray;
};

let fromList = IsoList.fromList;

let toList = IsoList.toList;