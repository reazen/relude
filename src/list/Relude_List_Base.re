/**
 * Prepends the given item to the start of the given list.
 */
let cons: 'a. ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];

/**
 * Prepends the given item to the start of the given list.
 *
 * Alias for `cons`
 */
let prepend: 'a. ('a, list('a)) => list('a) = cons;

/**
 * Splits a list into head and tail parts.  Returns None if the list is empty.
 */
let uncons: 'a. list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

/**
 * Appends an item to the end of the list.
 */
let append: 'a. ('a, list('a)) => list('a) =
  (x, xs) => Relude_List_Instances.SemigroupAny.append(xs, [x]);

/**
 * Creates a list that contains `i` copies of the given item.
 */
let repeat: 'a. (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);

/**
 * Makes a list by mapping a function over a range of ints from 0 to `n - 1`
 */
let makeWithIndex: 'a. (int, int => 'a) => list('a) = Belt.List.makeBy;

/**
 * Maps an indexed function over the values of a list to produce a new list.
 */
let mapWithIndex: 'a 'b. (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, (i, x) => f(x, i));

/**
 * Reverses the given list
 */
let reverse: 'a. list('a) => list('a) = Belt.List.reverse;

/**
 * Shuffles the given list to create a new list
 */
let shuffle: 'a. list('a) => list('a) = Belt.List.shuffle;

/**
 * Indicates if the given list is empty (length = 0)
 */
let isEmpty: 'a. list('a) => bool =
  fun
  | [] => true
  | _ => false;

/**
 * Indicates if the given list is non-empty (length > 0)
 */
let isNotEmpty: 'a. list('a) => bool = xs => !isEmpty(xs);

/**
 * Gets the value at the given index of the list, or None if the index is out of range.
 */
let at: 'a. (int, list('a)) => option('a) =
  (i, xs) => Belt.List.get(xs, i);

/**
 * Gets the first item of the list, or None if the list is empty.
 */
let head: 'a. list('a) => option('a) =
  fun
  | [] => None
  | [x, ..._] => Some(x);

/**
 * Gets all but the first items in the list, or None if the list is empty.
 */
let tail: 'a. list('a) => option(list('a)) =
  fun
  | [] => None
  | [_, ...xs] => Some(xs);

/**
 * Gets all but the first items in the list, or [] if the list is empty.
 */
let tailOrEmpty: 'a. list('a) => list('a) =
  xs => tail(xs) |> Relude_Option.getOrElse([]);

/**
 * Gets all but the last item in the list, or None if the list is empty
 */
let rec init: 'a. list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] => Some(cons(x, Relude_Option.getOrElse([], init(xs))));

/**
 * Gets all but the last item in the list, or [] if the list is empty
 */
let initOrEmpty: list('a) => list('a) =
  xs =>
    switch (init(xs)) {
    | Some(ys) => ys
    | None => []
    };

/**
 * Gets the last item of the list, or None if the list is empty.
 */
let rec last: 'a. list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

/**
 * Creates a new list by taking up to `n` items from the given list.
 */
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

/**
 * Creates a new list by taking exactly `n` items from the given list.  If there
 * are not at least `n` items, the result is None.
 */
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

/**
 * Creates a new list by taking items from the list until an item is reached
 * which does not satisfy the given predicate.
 */
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

/**
 * Creates a new list by dropping up to `n` items from the given list.
 */
let rec drop: 'a. (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ..._] when i <= 0 => xs
    | [_, ...ys] => drop(i - 1, ys)
    };

/**
 * Creates a new list by dropping exactly `n` items from the given list.  If there are
 * fewer than `n` items, None is returned.
 */
let dropExactly: 'a. (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

/**
 * Creates a new list by dropping items from the list until an item is reached which
 * does not satisfy the given predicate.
 */
let rec dropWhile: 'a. ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [y, ...ys] when f(y) => dropWhile(f, ys)
    | _ => xs
    };

/**
 * Creates a new list containing only the items from the given list that satisfy the given predicate.
 */
let filter: 'a. ('a => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keep(xs, f);

/**
 * Creates a new list containing only the items from the given list that satisfy the given indexed predicate.
 */
let filterWithIndex: 'a. (('a, int) => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keepWithIndex(xs, f);

/**
 * Creates a new list containing only the items from the given list that do not satisfy the given predicate.
 */
let filterNot: 'a. ('a => bool, list('a)) => list('a) =
  f => filter(a => !f(a));

/**
 * Creates a new list containing only the items from the given list that do not satisfy the given indexed predicate.
 */
let filterNotWithIndex: 'a. (('a, int) => bool, list('a)) => list('a) =
  f => filterWithIndex((a, i) => !f(a, i));

/**
 * Maps a option-creating function over the list, and keeps the Some values without the Some structure.
 */
let mapOption: 'a 'b. ('a => option('b), list('a)) => list('b) =
  (f, xs) =>
    Relude_List_Instances.foldLeft(
      (acc, curr) => Relude_Option.fold(acc, v => [v, ...acc], f(curr)),
      [],
      xs,
    )
    |> reverse;

/**
 * Keeps all the Some values from the list, and removes the Some structure.
 */
let catOptions: 'a. list(option('a)) => list('a) =
  xs => mapOption(a => a, xs);

/**
 * Partitions the given list into two lists, one (left-side) containing values that satisfy the given predicate and one
 * (right-side) containing values that do not satisfy the predicate.
 */
let partition: 'a. ('a => bool, list('a)) => (list('a), list('a)) =
  (f, xs) => Belt.List.partition(xs, f);

/**
 * Splits a list into two lists at the given index.  None is returned if the index is out of range.
 */
let splitAt: 'a. (int, list('a)) => option((list('a), list('a))) =
  (i, xs) => Belt.List.splitAt(xs, i);

/**
 * Creates a new list by prepending the given value to each item in the list.
 */
let prependToAll: 'a. ('a, list('a)) => list('a) =
  (delim, xs) => {
    let rec go = acc =>
      fun
      | [] => acc
      | [y, ...ys] => go([y, delim, ...acc], ys);

    go([], xs) |> reverse;
  };

/**
 * Creates a new list by inserting the given item in between each item in the list.
 */
let intersperse: 'a. ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [y, ...prependToAll(delim, ys)]
    };

/**
 * Creates a new list by copying the given list `n` times.
 */
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

/**
 * Combines two lists pair-wise into a list of tuple-2
 */
let zip: 'a 'b. (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

/**
 * Combines two lists using a function to combine pair-wise values.
 */
let zipWith: 'a 'b 'c. (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

/**
 * Creates a new list with each item paired with its index in the list.
 */
let zipWithIndex: 'a. list('a) => list(('a, int)) =
  xs => mapWithIndex((v, i) => (v, i), xs);

/**
 * Creates two lists by splitting a list of tuple-2 on the left and right.
 */
let unzip: 'a 'b. list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

/**
 * Sorts a list with the given int-based compare function.
 */
let sortWithInt: 'a. (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

/**
 * Sorts a list with the given ordering-based compare function.
 */
let sortBy:
  'a.
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a)
 =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

/**
 * Sorts a list with the given ORD module
 */
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

/**
 * Creates a new list containing only the distinct items of the given list, using the
 * given equality function
 */
let distinctBy: 'a. (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    Relude_List_Instances.foldLeft(
      (ys, x) =>
        Relude_List_Instances.containsBy(eq, x, ys) ? ys : [x, ...ys],
      [],
      xs,
    )
    |> reverse;

/**
 * Removes the first occurrence of the given item from the list, based on the given equality function
 */
let removeFirstBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, v, xs) => {
    let go = ((found, ys), x) =>
      found
        ? (true, [x, ...ys])
        : innerEq(v, x) ? (true, ys) : (false, [x, ...ys]);
    Relude_List_Instances.foldLeft(go, (false, []), xs) |> snd |> reverse;
  };

/**
 * Removes all occurrences of the given item from the list, based on the given equality function
 */
let removeEachBy: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, x, xs) =>
    Relude_List_Instances.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : [y, ...ys],
      [],
      xs,
    )
    |> reverse;

/**
 * Creates a new list containing only the distinct values of the list, based on the given
 * equality function
 */
let distinct =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs) => {
  module EqA = (val eqA);
  distinctBy(EqA.eq, xs);
};

/**
 * Removes the first occurrence of the given item from the list, based on the given EQ module
 */
let removeFirst =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeFirstBy(EqA.eq, x, xs);
};

/**
 * Removes all occurrences of the given item from the list, based on the given EQ module
 */
let removeEach =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeEachBy(EqA.eq, x, xs);
};

/**
 * Creates a new list that replaces the item at the given index with the given value.
 * If the index is out of range, no replacement is made.
 */
let replaceAt: 'a. (int, 'a, list('a)) => list('a) =
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

/**
 * Similar to foldLeft, but collects the results from each iteration,
 * rather than accumulating a single final result.
 */
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

/**
 * Similar to foldRight, but collects the results from each iteration,
 * rather than accumulating a single final result.
 */
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