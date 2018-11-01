let length: list('a) => int = Belt.List.length;

let isEmpty: list('a) => bool = list => length(list) == 0;

let isNotEmpty: list('a) => bool = list => length(list) > 0;

let empty: list('a) = [];

let pure: 'a => list('a) = a => [a];

let one: 'a => list('a) = pure;

let concat: (list('a), list('a)) => list('a) = Belt.List.concat;

let cons: ('a, list('a)) => list('a) = (h, t) => [h, ...t];

let prepend: ('a, list('a)) => list('a) = cons;

let append: ('a, list('a)) => list('a) = (a, l) => concat(l, [a]);

let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;

let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    snd(
      foldLeft(
        ((acc, result), curr) => {
          let nextAcc = f(acc, curr);
          (nextAcc, append(nextAcc, result));
        },
        (init, []),
        xs,
      ),
    );

let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    snd(
      foldRight(
        (curr, (acc, result)) => {
          let nextAcc = f(curr, acc);
          (nextAcc, prepend(nextAcc, result));
        },
        (init, []),
        xs,
      ),
    );

let head: list('a) => option('a) = Belt.List.head;

let tail: list('a) => option(list('a)) = Belt.List.tail;

let tailOrEmpty: list('a) => list('a) =
  list => tail(list)->Belt.Option.getWithDefault(empty);

let reverse: list('a) => list('a) = Belt.List.reverse;

/* TODO */
let init: list('a) => option(list('a)) = _l => None;

/* TODO */
let last: list('a) => option('a) = _ => None;

let take: (int, list('a)) => option(list('a)) =
  (i, l) => Belt.List.take(l, i);

/* TODO */
let takeUpTo: (int, list('a)) => list('a) = (_i, l) => l;

let drop: (int, list('a)) => option(list('a)) =
  (i, l) => Belt.List.drop(l, i);

/* TODO */
let dropUpTo: (int, list('a)) => list('a) = (_i, l) => l;

let filter: ('a => bool, list('a)) => list('a) =
  (f, l) => Belt.List.keep(l, f);

let filterWithIndex: (('a, int) => bool, list('a)) => list('a) =
  (f, l) => Belt.List.keepWithIndex(l, f);

let rec find: ('a => bool, list('a)) => option('a) =
  (f, l) =>
    switch (l) {
    | [] => None
    | [h, ...tail] =>
      if (f(h)) {
        Some(h);
      } else {
        find(f, tail);
      }
    };

let findWithIndex: (('a, int) => bool, list('a)) => option('a) =
  (f, l) => {
    let rec _findWithIndex = (f, l, i) =>
      switch (l) {
      | [] => None
      | [h, ...tail] =>
        if (f(h, i)) {
          Some(h);
        } else {
          _findWithIndex(f, tail, i + 1);
        }
      };
    _findWithIndex(f, l, 0);
  };

let partition: ('a => bool, list('a)) => (list('a), list('a)) =
  (f, l) => Belt.List.partition(l, f);

let rec prependToAll: ('a, list('a)) => list('a) = (delim, l) => {
  switch (l) {
    | [] => []
    | [h, ...tail] => [delim, h, ...prependToAll(delim, tail)]
  };
};

let intersperse: ('a, list('a)) => list('a) = (delim, l) => {
  switch (l) {
    | [] => []
    | [h, ...tail] => [h, ...prependToAll(delim, tail)]
  };
};

let zip: (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWithIndex: list('a) => list(('a, int)) =
  l => zip(l, Int.range(0, length(l)));

let sort: (('a, 'a) => int, list('a)) => list('a) = (f, l) => Belt.List.sort(l, f);

/* TODO: */
let distinct: list('a) => list('a) = _ => [];

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let flatten: list(list('a)) => list('a) =
  lists => flatMap(lists, Function.identity);

let fromArray: array('a) => list('a) = Belt.List.fromArray;

let toArray: list('a) => array('a) = Belt.List.toArray;

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

let mkString: (string, list(string)) => string = (delim, strings) => {
  let delimited = intersperse(delim, strings);
  foldLeft((acc, curr) => acc ++ curr, "", delimited);
};

let show: ('a => string, list('a)) => string = (showA, l) => {
  let strings = map(showA, l);
  "[" ++ mkString(", ", strings) ++ "]"
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
};

module IsoArray: Interface.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = fromArray;
  let toArray = toArray;
};

module Infix = BsAbstract.List.Infix;
