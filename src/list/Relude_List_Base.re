let cons: 'a. ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];

let prepend: 'a. ('a, list('a)) => list('a) = cons;

let uncons: 'a. list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let append: 'a. ('a, list('a)) => list('a) =
  (x, xs) => Relude_List_Instances.SemigroupAny.append(xs, [x]);

let repeat: 'a. (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);

let makeWithIndex: 'a. (int, int => 'a) => list('a) = Belt.List.makeBy;

let mapWithIndex: 'a 'b. (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, (i, x) => f(x, i));

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

let initOrEmpty: list('a) => list('a) =
  xs =>
    switch (init(xs)) {
    | Some(ys) => ys
    | None => []
    };

let rec last: 'a. list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: 'a. (int, list('a)) => list('a) =
  (i, xs) => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | _ when count <= 0 => acc
      | [] => acc
      | [y, ...ys] => go([y, ...acc], count - 1, ys)
      };
    go([], i, xs) |> reverse;
  };

let takeExactly: 'a. (int, list('a)) => option(list('a)) =
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

let rec drop: 'a. (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ..._] when i <= 0 => xs
    | [_, ...ys] => drop(i - 1, ys)
    };

let dropExactly: 'a. (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

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

let filterNot: 'a. ('a => bool, list('a)) => list('a) =
  f => filter(a => !f(a));

let filterNotWithIndex: 'a. (('a, int) => bool, list('a)) => list('a) =
  f => filterWithIndex((a, i) => !f(a, i));

let mapOption: 'a 'b. ('a => option('b), list('a)) => list('b) =
  (f, xs) =>
    Relude_List_Instances.foldLeft(
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

let prependToAll: 'a. ('a, list('a)) => list('a) =
  (delim, xs) => {
    let rec go = acc =>
      fun
      | [] => acc
      | [y, ...ys] => go([y, delim, ...acc], ys);

    go([], xs) |> reverse;
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
      count <= 1
        ? acc : go(count - 1, Relude_List_Instances.concat(xs, acc));
    if (i <= 0) {
      [];
    } else {
      go(i, xs);
    };
  };

let zip: 'a 'b. (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWith: 'a 'b 'c. (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

let zipWithIndex: 'a. list('a) => list(('a, int)) =
  xs => mapWithIndex((v, i) => (v, i), xs);

let unzip: 'a 'b. list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

let sortWithInt: 'a. (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

let sortBy:
  'a.
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a)
 =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let sort =
    (
      type a,
      ordA: (module BsAbstract.Interface.ORD with type t = a),
      xs: list(a),
    )
    : list(a) => {
  module OrdA = (val ordA);
  sortBy(OrdA.compare, xs);
};

// TODO: this is currently O(n^2), but we could make it slightly faster if we
// we could sort the list first. Or maybe we should just use a Set type
let distinctBy: 'a. (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    Relude_List_Instances.foldLeft(
      (ys, x) =>
        Relude_List_Instances.containsBy(eq, x, ys) ? ys : [x, ...ys],
      [],
      xs,
    )
    |> reverse;

let removeFirstBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, v, xs) => {
    let go = ((found, ys), x) =>
      found
        ? (true, [x, ...ys])
        : innerEq(v, x) ? (true, ys) : (false, [x, ...ys]);
    Relude_List_Instances.foldLeft(go, (false, []), xs) |> snd |> reverse;
  };

let removeEachBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, x, xs) =>
    Relude_List_Instances.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : [y, ...ys],
      [],
      xs,
    )
    |> reverse;

let distinct =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs) => {
  module EqA = (val eqA);
  distinctBy(EqA.eq, xs);
};

let removeFirst =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeFirstBy(EqA.eq, x, xs);
};

let removeEach =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeEachBy(EqA.eq, x, xs);
};

// TODO: scans come from TraversableExtensions
let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    Relude_List_Instances.foldLeft(
      ((acc, result), curr) => {
        let nextAcc = f(acc, curr);
        (nextAcc, [nextAcc, ...result]);
      },
      (init, []),
      xs,
    )
    |> snd
    |> Belt.List.reverse; // TODO use our own implementation

let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    Relude_List_Instances.foldRight(
      (curr, (acc, result)) => {
        let nextAcc = f(curr, acc);
        (nextAcc, [nextAcc, ...result]);
      },
      (init, []),
      xs,
    )
    |> snd;
