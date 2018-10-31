let empty: list('a) = [];

let pure: 'a => list('a) = a => [a]; /*BsAbstract.List.Applicative.pure;*/

let one: 'a => list('a) = pure;

let single: 'a => list('a) = pure;

let cons: ('a, list('a)) => list('a) = (h, t) => [h, ...t];

let fromArray = Belt.List.fromArray;

let toArray = Belt.List.toArray;

let length = Belt.List.length;

let isEmpty: list('a) => bool = list => length(list) == 0;

let isNotEmpty: list('a) => bool = list => length(list) > 0;

let head: list('a) => option('a) = Belt.List.head;

let tail: list('a) => option(list('a)) = Belt.List.tail;

let tailOrEmpty: list('a) => list('a) =
  list => tail(list)->Belt.Option.getWithDefault(empty);

/* TODO
   let init =

   let last =

   let keep =
   let keepWithIndex =

   let filter = keep
   let filterWithIndex = keepWithIndex

   let find =
   let findWithIndex =

   let partition =

   let zip =
   let zipWithIndex =

   let sort =
   let sortWith =

   let distinct =
   let uniq = distinct
   */

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let flipMap: (list('a), 'a => 'b) => list('b) =
  (l, f) => Function.flip(map, l, f);

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let flatten: list(list('a)) => list('a) =
  lists => flatMap(lists, Function.identity);

let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;

let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

let concat = Belt.List.concat;

/* TODO
   let scanLeft
   let scanRight
   */

let eq: (list('a), list('a), ('a, 'a) => bool) => bool = Belt.List.eq;

let eqM =
    (
      type t,
      l1: list(t),
      l2: list(t),
      eq: (module BsAbstract.Interface.EQ with type t = t),
    )
    : bool => {
  module ListEq = BsAbstract.List.Eq((val eq));
  ListEq.eq(l1, l2);
};

module Eq = BsAbstract.List.Eq;

module Show = BsAbstract.List.Show;

/* bs-abstract didn't have this b/c it's the same as Alt, but we wanted to have it. */
module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = concat;
};

/* bs-abstract didn't have this b/c it's the same as Plus, but we wanted to have it. */
module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = empty;
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
  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
}

module IsoArray: Interface.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = fromArray;
  let toArray = toArray;
};

module Infix = BsAbstract.List.Infix;
