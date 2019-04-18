open BsAbstract.Interface;

/**
 * Relude.List.Base includes list-specific functions that aren't derived from
 * typeclass membership.
 */

// TODO: PureScript has a `FunctorWithIndex` typeclass
let mapWithIndex: 'a 'b. (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, (i, x) => f(x, i));

let cons: 'a. ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];
let prepend: 'a. ('a, list('a)) => list('a) = cons;

let uncons: 'a. list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let append: 'a. ('a, list('a)) => list('a) =
  (x, xs) => Relude_List_Types.SemigroupAny.append(xs, [x]);

let repeat: 'a. (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);
let makeWithIndex: 'a. (int, int => 'a) => list('a) = Belt.List.makeBy;

let reverse: 'a. list('a) => list('a) = Belt.List.reverse;
let shuffle: 'a. list('a) => list('a) = Belt.List.shuffle;

let isEmpty: 'a. list('a) => bool =
  fun
  | [] => true
  | _ => false;

let isNotEmpty: 'a. list('a) => bool = xs => !isEmpty(xs);

let at: 'a. (int, list('a)) => option('a) =
  (i, xs) => Belt.List.get(xs, i);

let head: 'a. list('a) => option('a) =
  fun
  | [] => None
  | [x, ..._] => Some(x);

let tail: 'a. list('a) => option(list('a)) =
  fun
  | [] => None
  | [_, ...xs] => Some(xs);

let tailOrEmpty: 'a. list('a) => list('a) =
  xs => tail(xs) |> Relude_Option.getOrElse([]);

let rec init: 'a. list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] => Some(cons(x, Relude_Option.getOrElse([], init(xs))));

// TODO: just `head << reverse` right?
let rec last: 'a. list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: 'a. (int, list('a)) => option(list('a)) =
  (i, xs) => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | _ when count <= 0 => Some(acc)
      | [] => None
      | [y, ...ys] => go([y, ...acc], count - 1, ys)
      };
    if (i >= 0) {
      go([], i, xs) |> Relude_Option.map(reverse);
    } else {
      None;
    };
  };

let takeUpTo: 'a. (int, list('a)) => list('a) =
  (i, xs) => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | _ when count <= 0 => acc
      | [] => acc
      | [y, ...ys] => go([y, ...acc], count - 1, ys)
      };
    go([], i, xs) |> reverse;
  };

let takeWhile: 'a. ('a => bool, list('a)) => list('a) =
  (f, xs) => {
    let rec go = (acc, rest) =>
      switch (rest) {
      | [] => acc
      | [y, ..._] when !f(y) => acc
      | [y, ...ys] => go([y, ...acc], ys)
      };
    go([], xs) |> reverse;
  };

let drop: 'a. (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

let rec dropUpTo: 'a. (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ..._] when i <= 0 => xs
    | [_, ...ys] => dropUpTo(i - 1, ys)
    };

let rec dropWhile: 'a. ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [y, ...ys] when f(y) => dropWhile(f, ys)
    | _ => xs
    };

let filter: 'a. ('a => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keep(xs, f);

let filterWithIndex: 'a. (('a, int) => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keepWithIndex(xs, f);

/**
 * Map all list values from 'a to option('b), filtering out the `None`s
 */
let mapOption: 'a 'b. ('a => option('b), list('a)) => list('b) =
  (f, xs) =>
    Relude_List_Types.foldLeft(
      (acc, curr) => Relude_Option.fold(acc, v => [v, ...acc], f(curr)),
      [],
      xs,
    )
    |> reverse;

let catOptions: 'a. list(option('a)) => list('a) =
  xs => mapOption(a => a, xs);

let partition: 'a. ('a => bool, list('a)) => (list('a), list('a)) =
  (f, xs) => Belt.List.partition(xs, f);

let splitAt: 'a. (int, list('a)) => option((list('a), list('a))) =
  (i, xs) => Belt.List.splitAt(xs, i);

let rec prependToAll: 'a. ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [delim, y, ...prependToAll(delim, ys)]
    };

let intersperse: 'a. ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [y, ...prependToAll(delim, ys)]
    };

let replicate: 'a. (int, list('a)) => list('a) =
  (i, xs) => {
    let rec go = (count, acc) =>
      count <= 1 ? acc : go(count - 1, Relude_List_Types.concat(xs, acc));
    go(i, xs);
  };

let zip: 'a 'b. (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWith: 'a 'b 'c. (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

// TODO: for free with `FunctorWithIndex`?
let zipWithIndex: 'a. list('a) => list(('a, int)) =
  xs => mapWithIndex((v, i) => (v, i), xs);

let unzip: 'a 'b. list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

let sortWithInt: 'a. (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

let sortBy: 'a. (('a, 'a) => ordering, list('a)) => list('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let sort =
    (type a, ordA: (module ORD with type t = a), xs: list(a)): list(a) => {
  module OrdA = (val ordA);
  sortBy(OrdA.compare, xs);
};

// TODO: this is currently O(n^2), but we could make it slightly faster if we
// we could sort the list first. Or maybe we should just use a Set type
let distinctBy: 'a. (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    Relude_List_Types.foldLeft(
      (ys, x) => Relude_List_Types.containsBy(eq, x, ys) ? ys : [x, ...ys],
      [],
      xs,
    )
    |> reverse;

// TODO: rename to removeFirstBy
let removeBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, v, xs) => {
    let go = ((found, ys), x) =>
      found
        ? (true, [x, ...ys])
        : innerEq(v, x) ? (true, ys) : (false, [x, ...ys]);
    Relude_List_Types.foldLeft(go, (false, []), xs) |> snd |> reverse;
  };

// TODO: rename to removeAllBy for consistency with String.replaceAll?
let removeEachBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, x, xs) =>
    Relude_List_Types.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : [y, ...ys],
      [],
      xs,
    )
    |> reverse;

let distinct = (type a, eqA: (module EQ with type t = a), xs) => {
  module EqA = (val eqA);
  distinctBy(EqA.eq, xs);
};

let remove = (type a, eqA: (module EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeBy(EqA.eq, x, xs);
};

let removeEach = (type a, eqA: (module EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeEachBy(EqA.eq, x, xs);
};
