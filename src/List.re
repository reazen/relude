/********************************************************************************
 Common modules (typeclass instances) for list('a)

 Many of these are just aliases of bs-abstract modules, but a few are our own.
 ********************************************************************************/

module Eq = BsAbstract.List.Eq;

module Show = BsAbstract.List.Show;

/* This didn't exist in bs-abstract b/c it's the same thing as Alt, but we want to have it. */
module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = Belt.List.concat;
};

/* This didn't exist in bs-abstract b/c it's the same thing as Plus, but we want to have it. */
module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = [];
};

module Alt = BsAbstract.List.Alt;

module Plus = BsAbstract.List.Plus;

module Alternative = BsAbstract.List.Alternative;

module Functor = BsAbstract.List.Functor;

module Apply = BsAbstract.List.Apply;

module Applicative = BsAbstract.List.Applicative;

module Monad = BsAbstract.List.Monad;

module Foldable = BsAbstract.List.Foldable;

module Traversable = BsAbstract.List.Foldable;

module Sequence: Interface.SEQUENCE with type t('a) = list('a) = {
  type t('a) = list('a);
  let length = Belt.List.length;
  let isEmpty = list => length(list) == 0;
  let isNonEmpty = list => length(list) > 0;
  let head = Belt.List.head;
  let tail = Belt.List.tail;
  let tailOrEmpty = list => tail(list)->Belt.Option.getWithDefault([]);
  let toList = Function.identity;
  let toArray = Belt.List.toArray;
};

module Infix = BsAbstract.List.Infix;

/********************************************************************************
 Utility functions for list('a)

 Many of these are aliases for functions defined in the typeclasses above, Belt
 functions, or functions from the std lib, but some are our own.
 ********************************************************************************/

let eq: (list('a), list('a), ('a, 'a) => bool) => bool = Belt.List.eq;

let eqM =
    (
      type t,
      l1: list(t),
      l2: list(t),
      eq: (module BsAbstract.Interface.EQ with type t = t),
    )
    : bool => {
  module ListEq = Eq((val eq));
  ListEq.eq(l1, l2);
};

/* Sequence functions */
let length = Sequence.length;

let isEmpty: list('a) => bool = Sequence.isEmpty;

let isNonEmpty: list('a) => bool = Sequence.isNonEmpty;

let head: list('a) => option('a) = Sequence.head;

let tail: list('a) => option(list('a)) = Sequence.tail;

let tailOrEmpty: list('a) => list('a) = Sequence.tailOrEmpty;

let cons: ('a, list('a)) => list('a) = (h, t) => Belt.List.concat([h], t); /* TODO: there must be a better way */

/* Functor */
let map: ('a => 'b, list('a)) => list('b) = Functor.map;
/* TODO: not sure how to fix this compile error
let flipMap = Function.flip(map);
*/

/* Applicative-related */
let pure: 'a => list('a) = Applicative.pure;
let one: 'a => list('a) = pure;
let single: 'a => list('a) = pure;
let apply: (list('a => 'b), list('a)) => list('b) = Applicative.apply;

/* Monad */
let flatMap: (list('a), 'a => list('b)) => list('b) = Monad.flat_map;
let flatten: list(list('a)) => list('a) =
  lists => flatMap(lists, Function.identity);

/* keep */
/* filter */

/* Foldable */
let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = Foldable.fold_left;
let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = Foldable.fold_right;
