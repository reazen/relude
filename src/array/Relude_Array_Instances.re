/**
 * Concatenates two arrays with the left side array first, and the right side last
 */
let concat: 'a. (array('a), array('a)) => array('a) = Belt.Array.concat;

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = concat;
};

/**
 * Returns an empty array.  Warning: arrays are mutable so this value cannot be shared.
 */
let empty: 'a. array('a) = [||];

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = empty;
};

/**
 * Applies a pure function to each value in the array
 */
let map: 'a 'b. ('a => 'b, array('a)) => array('b) = BsAbstract.Array.Functor.map;

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = array('a) = {
  type t('a) = array('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
 * Applies an array of functions to an array of values to produce a new array of values.
 */
let apply: 'a 'b. (array('a => 'b), array('a)) => array('b) = BsAbstract.Array.Apply.apply;

module Apply: BsAbstract.Interface.APPLY with type t('a) = array('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
 * Returns a new array containing the single given item
 */
let pure: 'a. 'a => array('a) = a => [|a|];

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = array('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
 * Maps a monadic function over each element of the array, and flattens (concatenates) the result.
 */
let bind: 'a 'b. (array('a), 'a => array('b)) => array('b) = BsAbstract.Array.Monad.flat_map;

module Monad: BsAbstract.Interface.MONAD with type t('a) = array('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

/**
 * Alt for arrays concatenates the two arrays
 */
let alt: 'a. (array('a), array('a)) => array('a) = BsAbstract.Array.Alt.alt;

module Alt: BsAbstract.Interface.ALT with type t('a) = array('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

module Plus: BsAbstract.Interface.PLUS with type t('a) = array('a) = {
  include MonoidAny;
  let map = map;
  let alt = alt;
};
include Relude_Extensions_Plus.PlusExtensions(Plus);

module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = array('a) = {
  include MonoidAny;
  let map = map;
  let alt = alt;
  let apply = apply;
  let pure = pure;
};
include Relude_Extensions_Alternative.AlternativeExtensions(Alternative);

/**
 * Imap is the invariant map function for arrays.
 */
let imap: 'a 'b. ('a => 'b, 'b => 'a, array('a)) => array('b) = BsAbstract.Array.Invariant.imap;

module Invariant: BsAbstract.Interface.INVARIANT with type t('a) = array('a) = {
  type t('a) = array('a);
  let imap = imap;
};

module MonadZero: BsAbstract.Interface.MONAD_ZERO with type t('a) = array('a) = {
  include Monad;
  let empty = empty;
  let alt = alt;
};

module MonadPlus: BsAbstract.Interface.MONAD_PLUS with type t('a) = array('a) = {
  include Monad;
  let empty = empty;
  let alt = alt;
};

/**
 * Extend is the dual of the monadic bind function.
 */
let extend: 'a 'b. (array('a) => 'b, array('a)) => array('b) = BsAbstract.Array.Extend.extend;

module Extend: BsAbstract.Interface.EXTEND with type t('a) = array('a) = {
  include Functor;
  let extend = extend;
};

/**
 * Folds an array from left to right into an accumulator value
 */
let foldLeft = BsAbstract.Array.Foldable.fold_left;

/**
 * Folds an array from right-to-left into an accumulator value
 */
let foldRight = BsAbstract.Array.Foldable.fold_right;

module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = array('a) = {
  include BsAbstract.Array.Foldable;
  let fold_left = foldLeft;
  let fold_right = foldRight;
};
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

module Traversable = BsAbstract.Array.Traversable;

/**
 * Indicates if two arrays are pair-wise equal, using the given equality function
 */
let rec eqBy: 'a. (('a, 'a) => bool, array('a), array('a)) => bool =
  (innerEq, xs, ys) => {
    let head = Belt.Array.get(_, 0);
    let tailOrEmpty = Belt.Array.sliceToEnd(_, 1);
    switch (head(xs), head(ys)) {
    | (None, None) => true
    | (Some(x), Some(y)) when innerEq(x, y) =>
      eqBy(innerEq, tailOrEmpty(xs), tailOrEmpty(ys))
    | _ => false
    };
  };

/**
 * Indicates if two arrays are pair-wise equal, using the given EQ module
 */
let eq =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

module Eq = (EqA: BsAbstract.Interface.EQ) => {
  type t = array(EqA.t);
  let eq = eqBy(EqA.eq);
};

module Ord = BsAbstract.Array.Ord;

/**
 * Converts an array to a string, using the given show function for converting the array items
 */
let showBy: 'a. ('a => string, array('a)) => string =
  (innerShow, xs) => {
    // TODO
    let join = intercalate((module BsAbstract.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

/**
 * Converts an array to a string, using the given SHOW module for converting the array items
 */
let show =
    (type a, showA: (module BsAbstract.Interface.SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module Show = (ShowA: BsAbstract.Interface.SHOW) => {
  type t = array(ShowA.t);
  let show = showBy(ShowA.show);
};

/**
 * Converts the given list to an array
 */
let fromList: 'a. list('a) => array('a) = Belt.List.toArray;

/**
 * Converts the given array to a list
 */
let toList: 'a. array('a) => list('a) = Belt.List.fromArray;

module IsoList: Relude_Interface.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = fromList;
  let toList = toList;
};