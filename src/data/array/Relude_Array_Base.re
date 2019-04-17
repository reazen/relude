open BsAbstract.Interface;

/**
 * Relude.Array.Base includes array-specific functions that aren't derived from
 * typeclass membership.
 */

let mapWithIndex: 'a 'b. (('a, int) => 'b, array('a)) => array('b) =
  (f, xs) => Belt.Array.mapWithIndex(xs, (i, x) => f(x, i));

let cons: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Types.concat([|x|], xs);

let prepend: ('a, array('a)) => array('a) = cons;

let uncons: array('a) => option(('a, array('a))) =
  xs =>
    switch (xs) {
    | [||] => None
    | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
    };

let append: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Types.concat(xs, [|x|]);

let repeat: (int, 'a) => array('a) = (i, x) => Belt.Array.make(i, x);

let makeWithIndex: (int, int => 'a) => array('a) = Belt.Array.makeBy;

let reverse: array('a) => array('a) = Belt.Array.reverse;

let shuffleInPlace: array('a) => array('a) =
  xs => {
    Belt.Array.shuffleInPlace(xs);
    xs;
  };

let shuffle: array('a) => array('a) = Belt.Array.shuffle;

/* Replace O(n) definition from Foldables with O(1) version */
let length: array('a) => int = Belt.Array.length;
let isEmpty: array('a) => bool = arr => length(arr) == 0;
let isNotEmpty: array('a) => bool = arr => length(arr) > 0;

// TODO: this could come from foldables (but would be less efficient)
let at: (int, array('a)) => option('a) = (i, xs) => Belt.Array.get(xs, i);

let setAt: (int, 'a, array('a)) => option(array('a)) =
  (i, x, xs) =>
    if (Belt.Array.set(xs, i, x)) {
      Some(xs);
    } else {
      None;
    };

let head: array('a) => option('a) = arr => Belt.Array.get(arr, 0);

let tail: array('a) => option(array('a)) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else if (l == 1) {
      Some([||]);
    } else {
      let ys = Belt.Array.sliceToEnd(xs, 1);
      length(ys) > 0 ? Some(ys) : None;
    };
  };

let tailOrEmpty: array('a) => array('a) =
  xs =>
    switch (tail(xs)) {
    | Some(ys) => ys
    | None => [||]
    };

let init: array('a) => option(array('a)) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=l - 1));
    };
  };

let last: array('a) => option('a) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      at(l - 1, xs);
    };
  };

let take: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=i));
    };

let takeUpTo: (int, array('a)) => array('a) =
  (i, xs) =>
    if (i >= 0) {
      Belt.Array.slice(xs, ~offset=0, ~len=i);
    } else {
      [||];
    };

let rec takeWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => prepend(x, takeWhile(f, tailOrEmpty(xs)))
    | _ => [||]
    };

let drop: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.sliceToEnd(xs, i));
    };

let dropUpTo: (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    if (i >= 0) {
      Belt.Array.sliceToEnd(
        xs,
        if (i > l) {
          l;
        } else {
          i;
        },
      );
    } else {
      [||];
    };
  };

let rec dropWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => dropWhile(f, tailOrEmpty(xs))
    | _ => xs
    };

let filter: ('a => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keep(xs, f);

let filterWithIndex: (('a, int) => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keepWithIndex(xs, f);

let partition: ('a => bool, array('a)) => (array('a), array('a)) =
  (f, xs) => Belt.Array.partition(xs, f);

let splitAt: (int, array('a)) => option((array('a), array('a))) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some((
        Belt.Array.slice(xs, ~offset=0, ~len=i),
        Belt.Array.sliceToEnd(xs, i),
      ));
    };

let rec prependToAll: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) =>
      Relude_Array_Types.concat(
        [|delim, x|],
        prependToAll(delim, tailOrEmpty(xs)),
      )
    };

let intersperse: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => prepend(x, prependToAll(delim, tailOrEmpty(xs)))
    };

let replicate: (int, array('a)) => array('a) =
  (i, xs) =>
    Relude_Array_Types.foldLeft(
      (acc, _i) => Relude_Array_Types.concat(acc, xs),
      [||],
      Relude_Int.rangeAsArray(0, i),
    );

let zip: (array('a), array('b)) => array(('a, 'b)) = Belt.Array.zip;

let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => Belt.Array.zipBy(xs, ys, f);

let zipWithIndex: array('a) => array(('a, int)) =
  xs => zip(xs, Relude_Int.rangeAsArray(0, length(xs)));

let unzip: array(('a, 'b)) => (array('a), array('b)) = Belt.Array.unzip;

let sortWithInt: (('a, 'a) => int, array('a)) => array('a) =
  (f, xs) => Belt.SortArray.stableSortBy(xs, f);

let sortBy: (('a, 'a) => ordering, array('a)) => array('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let sort =
    (type a, ordA: (module ORD with type t = a), xs: array(a)): array(a) => {
  module OrdA = (val ordA);
  sortBy(OrdA.compare, xs);
};

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */
let distinctBy: (('a, 'a) => bool, array('a)) => array('a) =
  (eq, xs) =>
    Relude_Array_Types.foldLeft(
      /* foldRight would probably be faster with cons, but you lose the original ordering on the list */
      (acc, curr) =>
        Relude_Array_Types.containsBy(eq, curr, acc)
          ? acc : append(curr, acc),
      [||],
      xs,
    );

let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b) =
  (f, init, xs) =>
    snd(
      Relude_Array_Types.foldLeft(
        ((acc, result), curr) => {
          let nextAcc = f(acc, curr);
          (nextAcc, append(nextAcc, result));
        },
        (init, [||]),
        xs,
      ),
    );

let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b) =
  (f, init, xs) =>
    snd(
      Relude_Array_Types.foldRight(
        (curr, (acc, result)) => {
          let nextAcc = f(curr, acc);
          (nextAcc, prepend(nextAcc, result));
        },
        (init, [||]),
        xs,
      ),
    );
