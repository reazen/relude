let cons: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat([|x|], xs);

let prepend: ('a, array('a)) => array('a) = cons;

let uncons: array('a) => option(('a, array('a))) =
  xs =>
    switch (xs) {
    | [||] => None
    | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
    };

let append: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat(xs, [|x|]);

let repeat: (int, 'a) => array('a) = (i, x) => Belt.Array.make(i, x);

let makeWithIndex: (int, int => 'a) => array('a) = Belt.Array.makeBy;

let mapWithIndex: 'a 'b. (('a, int) => 'b, array('a)) => array('b) =
  (f, xs) => Belt.Array.mapWithIndex(xs, (i, x) => f(x, i));

let reverse: array('a) => array('a) = Belt.Array.reverse;

let shuffleInPlace: array('a) => array('a) =
  xs => {
    Belt.Array.shuffleInPlace(xs);
    xs;
  };

let shuffle: array('a) => array('a) = Belt.Array.shuffle;

let length: array('a) => int = Belt.Array.length;

let isEmpty: array('a) => bool = arr => length(arr) == 0;

let isNotEmpty: array('a) => bool = arr => length(arr) > 0;

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

let initOrEmpty: array('a) => array('a) =
  xs =>
    switch (init(xs)) {
    | Some(arr) => arr
    | None => [||]
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

let take: (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let len = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.slice(xs, ~offset=0, ~len);
  };

let takeExactly: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=i));
    };

let rec takeWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => prepend(x, takeWhile(f, tailOrEmpty(xs)))
    | _ => [||]
    };

let drop: (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let start = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.sliceToEnd(xs, start);
  };

let dropExactly: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.sliceToEnd(xs, i));
    };

let rec dropWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => dropWhile(f, tailOrEmpty(xs))
    | _ => xs
    };

let filter: 'a. ('a => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keep(xs, f);

let filterWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keepWithIndex(xs, f);

let filterNot: 'a. ('a => bool, array('a)) => array('a) =
  f => filter(a => !f(a));

let filterNotWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  f => filterWithIndex((a, i) => !f(a, i));

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

let prependToAll: ('a, array('a)) => array('a) =
  (delim, xs) => Relude_Array_Instances.flatMap(v => [|delim, v|], xs);

let intersperse: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => prepend(x, prependToAll(delim, tailOrEmpty(xs)))
    };

let replicate: (int, array('a)) => array('a) =
  (i, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, _i) => Relude_Array_Instances.concat(acc, xs),
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

let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let sort =
    (
      type a,
      ordA: (module BsAbstract.Interface.ORD with type t = a),
      xs: array(a),
    )
    : array(a) => {
  module OrdA = (val ordA);
  sortBy(OrdA.compare, xs);
};

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */
let distinctBy: 'a. (('a, 'a) => bool, array('a)) => array('a) =
  (eq, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, curr) =>
        Relude_Array_Instances.containsBy(eq, curr, acc)
          ? acc : append(curr, acc),
      [||],
      xs,
    );

let removeFirstBy: 'a. (('a, 'a) => bool, 'a, array('a)) => array('a) =
  (innerEq, v, xs) =>
    Relude_Array_Instances.foldLeft(
      ((found, ys), x) =>
        found
          ? (true, append(x, ys))
          : innerEq(v, x) ? (true, ys) : (false, append(x, ys)),
      (false, [||]),
      xs,
    )
    |> snd;

let removeEachBy: 'a. (('a, 'a) => bool, 'a, array('a)) => array('a) =
  (innerEq, x, xs) =>
    Relude_Array_Instances.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : append(y, ys),
      [||],
      xs,
    );

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

let replaceAt: 'a. (int, 'a, array('a)) => array('a) =
  (targetIndex, newX, xs) => {
    xs
    |> mapWithIndex((x, currentIndex) =>
         if (currentIndex == targetIndex) {
           newX;
         } else {
           x;
         }
       );
  };

// TODO: scans should come from a typeclass/extension
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b) =
  (f, init, xs) =>
    snd(
      Relude_Array_Instances.foldLeft(
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
      Relude_Array_Instances.foldRight(
        (curr, (acc, result)) => {
          let nextAcc = f(curr, acc);
          (nextAcc, prepend(nextAcc, result));
        },
        (init, [||]),
        xs,
      ),
    );