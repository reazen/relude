let length: list('a) => int = Belt.List.length;

let isEmpty: list('a) => bool = xs => length(xs) == 0;

let isNotEmpty: list('a) => bool = xs => length(xs) > 0;

let empty: list('a) = [];

let pure: 'a => list('a) = a => [a];

let one: 'a => list('a) = pure;

let concat: (list('a), list('a)) => list('a) = Belt.List.concat;

let cons: ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];

let uncons: list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let prepend: ('a, list('a)) => list('a) = cons;

let append: ('a, list('a)) => list('a) = (x, xs) => concat(xs, [x]);

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
  xs => tail(xs)->Belt.Option.getWithDefault(empty);

let reverse: list('a) => list('a) = Belt.List.reverse;

let rec init: list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] => Some(cons(x, init(xs)->Belt.Option.getWithDefault([])))

let rec last: list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.take(xs, i);

let rec takeUpTo: (int, list('a)) => list('a) = (i, xs) =>
  switch(xs) {
    | [] => []
    | [y, ...ys] => if (i == 0) [] else [y, ...takeUpTo(i - 1, ys)]
  }

let rec takeWhile: ('a => bool, list('a)) => list('a) = (f, xs) =>
  switch(xs) {
    | [] => []
    | [x, ...xs] => if (f(x)) [x, ...takeWhile(f, xs)] else []
  };

let drop: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

/* TODO */
let dropUpTo: (int, list('a)) => list('a) = (_i, xs) => xs;

/* TODO */
let dropWhile: ('a => bool, list('a)) => list('a) = (_f, xs) => xs;

let filter: ('a => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keep(xs, f);

let filterWithIndex: (('a, int) => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keepWithIndex(xs, f);

let rec find: ('a => bool, list('a)) => option('a) =
  (f, xs) =>
    switch (xs) {
    | [] => None
    | [y, ...ys] =>
      if (f(y)) {
        Some(y);
      } else {
        find(f, ys);
      }
    };

let findWithIndex: (('a, int) => bool, list('a)) => option('a) =
  (f, xs) => {
    let rec _findWithIndex = (f, ys, i) =>
      switch (ys) {
      | [] => None
      | [z, ...zs] =>
        if (f(z, i)) {
          Some(z);
        } else {
          _findWithIndex(f, zs, i + 1);
        }
      };
    _findWithIndex(f, xs, 0);
  };

let partition: ('a => bool, list('a)) => (list('a), list('a)) =
  (f, xs) => Belt.List.partition(xs, f);

let rec prependToAll: ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [delim, y, ...prependToAll(delim, ys)]
    };

let intersperse: ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [y, ...prependToAll(delim, ys)]
    };

let replicate: (int, list('a)) => list('a) =
  (i, xs) => foldLeft((acc, _i) => concat(acc, xs), [], Int.range(0, i));

let zip: (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWithIndex: list('a) => list(('a, int)) =
  xs => zip(xs, Int.range(0, length(xs)));

let sort: (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

/* TODO: */
let distinct: (('a, 'a) => bool, list('a)) => list('a) = (_f, _xs) => _xs;

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let flatten: list(list('a)) => list('a) =
  xss => flatMap(xss, Function.identity);

let fromArray: array('a) => list('a) = Belt.List.fromArray;

let toArray: list('a) => array('a) = Belt.List.toArray;

let eq: (list('a), list('a), ('a, 'a) => bool) => bool = Belt.List.eq;

let eqM =
    (
      type t,
      xs: list(t),
      ys: list(t),
      eq: (module BsAbstract.Interface.EQ with type t = t),
    )
    : bool => {
  module ListEq = BsAbstract.List.Eq((val eq));
  ListEq.eq(xs, ys);
};

let mkString: (string, list(string)) => string =
  (delim, xs) => {
    let delimited = intersperse(delim, xs);
    foldLeft((acc, curr) => acc ++ curr, "", delimited);
  };

let show: ('a => string, list('a)) => string =
  (showX, xs) => {
    let strings = map(showX, xs);
    "[" ++ mkString(", ", strings) ++ "]";
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
