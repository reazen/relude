let length: list('a) => int = Belt.List.length;

let isEmpty: list('a) => bool =
  xs =>
    switch (xs) {
    | [] => true
    | _ => false
    };

let isNotEmpty: list('a) => bool = xs => !isEmpty(xs);

let empty: list('a) = [];

let pure: 'a => list('a) = a => [a];

let one: 'a => list('a) = pure;

let repeat: (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);

let makeWithIndex: (int, int => 'a) => list('a) = Belt.List.makeBy;

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

let get: (int, list('a)) => option('a) = (i, xs) => Belt.List.get(xs, i);

let head: list('a) => option('a) = Belt.List.head;

let tail: list('a) => option(list('a)) = Belt.List.tail;

let tailOrEmpty: list('a) => list('a) =
  xs => tail(xs)->Belt.Option.getWithDefault(empty);

let rec init: list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] => Some(cons(x, init(xs)->Belt.Option.getWithDefault([])));

let rec last: list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.take(xs, i);

let rec takeUpTo: (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] =>
      if (i == 0) {
        [];
      } else {
        [y, ...takeUpTo(i - 1, ys)];
      }
    };

let rec takeWhile: ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [] => []
    | [x, ...xs] =>
      if (f(x)) {
        [x, ...takeWhile(f, xs)];
      } else {
        [];
      }
    };

let drop: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

let rec dropUpTo: (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ...ys] =>
      if (i == 0) {
        xs;
      } else {
        dropUpTo(i - 1, ys);
      }
    };

let rec dropWhile: ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] =>
      if (f(y)) {
        dropWhile(f, ys);
      } else {
        xs;
      }
    };

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

let splitAt: (int, list('a)) => option((list('a), list('a))) =
  (i, xs) => Belt.List.splitAt(xs, i);

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

let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

let zipWithIndex: list('a) => list(('a, int)) =
  xs => zip(xs, Int.range(0, length(xs)));

let unzip: list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

let sortWithInt: (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

let sort: (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Ordering.toInt, xs);

let shuffle: list('a) => list('a) = Belt.List.shuffle;

let reverse: list('a) => list('a) = Belt.List.reverse;

let rec contains: (('a, 'a) => bool, 'a, list('a)) => bool =
  (f, x, xs) =>
    switch (xs) {
    | [] => false
    | [y, ...ys] =>
      if (f(x, y)) {
        true;
      } else {
        contains(f, x, ys);
      }
    };

let rec any: ('a => bool, list('a)) => bool =
  (f, xs) =>
    switch (xs) {
    | [] => false
    | [y, ...ys] =>
      if (f(y)) {
        true;
      } else {
        any(f, ys);
      }
    };

let rec all: ('a => bool, list('a)) => bool =
  (f, xs) =>
    switch (xs) {
    | [] => true
    | [y, ...ys] =>
      if (f(y)) {
        all(f, ys);
      } else {
        false;
      }
    };

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */

let distinct: (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    foldLeft(
      /* foldRight would probably be faster with cons, but you lose the original ordering on the list */
      (acc, curr) =>
        if (contains(eq, curr, acc)) {
          acc;
        } else {
          append(curr, acc);
        },
      [],
      xs,
    );

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let mapWithIndex: (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, f);

let forEach: ('a => unit, list('a)) => unit =
  (f, xs) => Belt.List.forEach(xs, f);

let forEachWithIndex: (('a, int) => unit, list('a)) => unit =
  (f, xs) => Belt.List.forEachWithIndex(xs, f);

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let flipFlatMap: ('a => list('b), list('a)) => list('b) =
  (f, xs) => flatMap(xs, f);

let flatten: list(list('a)) => list('a) =
  xss => flatMap(xss, Function.identity);

let fromArray: array('a) => list('a) = Belt.List.fromArray;

let toArray: list('a) => array('a) = Belt.List.toArray;

let rec eq: (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) when innerEq(x, y) => eq(innerEq, xs, ys)
    | _ => false
    };

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

module ListFunctor = BsAbstract.Functions.Functor(Functor);

let void: list('a) => list(unit) = ListFunctor.void;

let flap: (list('a => 'b), 'a) => list('b) = ListFunctor.flap;

module Apply = BsAbstract.List.Apply;

module ListApply = BsAbstract.Functions.Apply(Apply);

/* TODO: not sure if we want to include these... these apply the function to all combinations of values (apply lift semantics), not index-by-index */
let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c) = ListApply.lift2;

let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d) = ListApply.lift3;

let map4:
  (('a, 'b, 'c, 'd) => 'e, list('a), list('b), list('c), list('d)) =>
  list('e) = ListApply.lift4;

let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    list('a),
    list('b),
    list('c),
    list('d),
    list('e)
  ) =>
  list('f) = ListApply.lift5;

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

module Infix = {
  include BsAbstract.List.Infix;
  include ListApply.Infix;
};
