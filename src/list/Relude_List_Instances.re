open BsBastet.Interface;

/**
 * Concatenates two lists with the left-side first and the right-side last.
 */
let concat: 'a. (list('a), list('a)) => list('a) =
  (xs, ys) => Belt.List.concat(xs, ys);

module SemigroupAny: SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = concat;
};
include Relude_Extensions_SemigroupAny.SemigroupAnyExtensions(SemigroupAny);

/**
 * Creates an empty list.
 */
let empty: 'a. list('a) = [];

module MonoidAny: MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = empty;
};
include Relude_Extensions_MonoidAny.MonoidAnyExtensions(MonoidAny);

/**
 * Maps a pure function over a list
 */
let map = BsBastet.List.Functor.map;

module Functor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
 * Applies a list of functions to a list of values.
 */
let apply = BsBastet.List.Apply.apply;

module Apply: APPLY with type t('a) = list('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
 * Lifts a single pure value into a list of one value.
 */
let pure = BsBastet.List.Applicative.pure;

module Applicative: APPLICATIVE with type t('a) = list('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
 * Maps a monadic function over a list of values and flattens the result.
 */
let bind = BsBastet.List.Monad.flat_map;

module Monad: MONAD with type t('a) = list('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

/**
 * alt for lists concatenates the lists.
 */
let alt = BsBastet.List.Alt.alt;

module Alt: ALT with type t('a) = list('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

module Plus: PLUS with type t('a) = list('a) = {
  include MonoidAny;
  let map = map;
  let alt = alt;
};
include Relude_Extensions_Plus.PlusExtensions(Plus);

module Alternative: ALTERNATIVE with type t('a) = list('a) = {
  include Plus;
  let apply = apply;
  let pure = pure;
};
include Relude_Extensions_Alternative.AlternativeExtensions(Alternative);

/**
 * Folds a list from left-to-right into a single value using an accumulator
 */
let foldLeft = BsBastet.List.Foldable.fold_left;

/**
 * Folds a list from right-to-left into a single value using an accumulator
 */
let foldRight = BsBastet.List.Foldable.fold_right;

module Foldable: FOLDABLE with type t('a) = list('a) = {
  include BsBastet.List.Foldable;
  let fold_left = foldLeft;
  let fold_right = foldRight;
};
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

/**
 * Create a list by recursively unfolding with a generator function and seed value
 */
let unfold = BsBastet.List.Unfoldable.unfold;

module Unfoldable: UNFOLDABLE with type t('a) = list('a) = {
  include BsBastet.List.Unfoldable;
  let unfold = unfold;
};
include Relude_Extensions_Unfoldable.UnfoldableExtensions(Unfoldable);


module Traversable: BsBastet.List.TRAVERSABLE_F = BsBastet.List.Traversable;

/**
 * Compares two lists for length and pair-wise equality using the given equality function
 */
let rec eqBy: (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) when innerEq(x, y) => eqBy(innerEq, xs, ys)
    | _ => false
    };

/**
 * Compares two lists for length and pair-wise equality using the given EQ module
 */
let eq = (type a, eqA: (module EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

module Eq = (EqA: EQ) => {
  type t = list(EqA.t);
  let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
};

/**
 * Converts a list to a string using the given show function
 */
let showBy: ('a => string, list('a)) => string =
  (innerShow, xs) => {
    let join = intercalate((module BsBastet.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

/**
 * Converts a list to a string using the given SHOW module
 */
let show = (type a, showA: (module SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module Show = (ShowA: SHOW) => {
  type t = list(ShowA.t);
  let show = xs => showBy(ShowA.show, xs);
};

/**
 * Converts the given array to a list
 */
let fromArray = Belt.List.fromArray;

/**
 * Converts the given list to an array
 */
let toArray = Belt.List.toArray;

module IsoArray: Relude_Interface.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = fromArray;
  let toArray = toArray;
};
