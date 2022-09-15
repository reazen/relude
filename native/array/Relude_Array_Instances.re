open Bastet.Interface;

/**
Concatenates two arrays with the left side array first, and the right side last
*/
let concat: 'a. (array('a), array('a)) => array('a) = Base.Array.append;

module SemigroupAny: SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = concat;
};
include Relude_Extensions_SemigroupAny.SemigroupAnyExtensions(SemigroupAny);

/**
Applies a pure function to each value in the array
*/
let map: 'a 'b. ('a => 'b, array('a)) => array('b) = Bastet.Array.Functor.map;

module Functor: FUNCTOR with type t('a) = array('a) = {
  type t('a) = array('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
Applies an array of functions to an array of values to produce a new array of values.
*/
let apply: 'a 'b. (array('a => 'b), array('a)) => array('b) = Bastet.Array.Apply.apply;

module Apply: APPLY with type t('a) = array('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
Returns a new array containing the single given item
*/
let pure: 'a. 'a => array('a) = a => [|a|];

module Applicative: APPLICATIVE with type t('a) = array('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
Maps a monadic function over each element of the array, and flattens (concatenates) the result.
*/
let bind: 'a 'b. (array('a), 'a => array('b)) => array('b) = Bastet.Array.Monad.flat_map;

module Monad: MONAD with type t('a) = array('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

/**
Alt for arrays concatenates the two arrays
*/
let alt: 'a. (array('a), array('a)) => array('a) = Bastet.Array.Alt.alt;

module Alt: ALT with type t('a) = array('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

/**
Imap is the invariant map function for arrays.
*/
let imap: 'a 'b. ('a => 'b, 'b => 'a, array('a)) => array('b) = Bastet.Array.Invariant.imap;

module Invariant: INVARIANT with type t('a) = array('a) = {
  type t('a) = array('a);
  let imap = imap;
};

/**
Extend is the dual of the monadic bind function.
*/
let extend: 'a 'b. (array('a) => 'b, array('a)) => array('b) = Bastet.Array.Extend.extend;

module Extend: EXTEND with type t('a) = array('a) = {
  include Functor;
  let extend = extend;
};

/**
Folds an array from left to right into an accumulator value
*/
let foldLeft = Bastet.Array.Foldable.fold_left;

/**
Folds an array from right-to-left into an accumulator value
*/
let foldRight = Bastet.Array.Foldable.fold_right;

module Foldable: FOLDABLE with type t('a) = array('a) = {
  include Bastet.Array.Foldable;
  let fold_left = foldLeft;
  let fold_right = foldRight;
};
include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

module Traversable = Bastet.Array.Traversable;

/**
Indicates if two arrays are pair-wise equal, using the given equality function
*/
//let eqBy: 'a. (('a, 'a) => bool, array('a), array('a)) => bool = False
//  (f,xs,ys) => {
//    let calcEq = (a,b) => if (f(a,b)) {Continue True} else {Stop False}
//    Base.Array.fold_until(~init=false,~f=calcEq,zip(xs,ys))
//  }
let rec eqByL: 'a. (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, xs, ys) => {
    let head = fun
        | [] => None
        | [x, ..._] => Some(x)
    let tailOrEmpty = fun
          | [] => []
          | [_, ...xs] => xs
    switch (head(xs), head(ys)) {
    | (None, None) => true
    | (Some(x), Some(y)) when innerEq(x, y) =>
      eqByL(innerEq, tailOrEmpty(xs), tailOrEmpty(ys))
    | _ => false
    };
  };
let eqBy: 'a. (('a, 'a) => bool, array('a), array('a)) => bool =
  (innerEq, xs, ys) => {
    let xls = Base.Array.to_list(xs)
    let yls = Base.Array.to_list(ys)
    eqByL(innerEq,xls,yls)

  };


/**
Indicates if two arrays are pair-wise equal, using the given EQ module
*/
let eq = (type a, eqA: (module EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

module Eq = (EqA: EQ) => {
  type t = array(EqA.t);
  let eq = eqBy(EqA.eq);
};

module Ord = Bastet.Array.Ord;

/**
Converts an array to a string, using the given show function for converting the array items
*/
let showBy: 'a. ('a => string, array('a)) => string =
  (innerShow, xs) => {
    // TODO
    let join = intercalate((module Bastet.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

/**
Converts an array to a string, using the given SHOW module for converting the array items
*/
let show = (type a, showA: (module SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module Show = (ShowA: SHOW) => {
  type t = array(ShowA.t);
  let show = showBy(ShowA.show);
};

/**
Converts the given list to an array
*/
let fromList: 'a. list('a) => array('a) = Base.List.to_array;

/**
Converts the given array to a list
*/
let toList: 'a. array('a) => list('a) = Base.Array.to_list;

module IsoList: Relude_Interface.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = fromList;
  let toList = toList;
};
