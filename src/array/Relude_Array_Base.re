/**
 * Prepends a single item to the start of an array.
 */
let cons: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat([|x|], xs);

/**
 * Prepends a single item to the start of an array.
 * Alias for `cons`
 */
let prepend: ('a, array('a)) => array('a) = cons;

/**
 * Splits an array into head and tail parts, but only if the array is non-empty.
 */
let uncons: array('a) => option(('a, array('a))) =
  xs =>
    switch (xs) {
    | [||] => None
    | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
    };

/**
 * Appends a single item to the end of an array.
 */
let append: ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat(xs, [|x|]);

/**
 * Creates an array by repeating the given item the given number of times.
 */
let repeat: (int, 'a) => array('a) = (i, x) => Belt.Array.make(i, x);

/**
 * Creates an array by mapping a function over a range of values from `0` to `n - 1`.
 */
let makeWithIndex: (int, int => 'a) => array('a) = Belt.Array.makeBy;

/**
 * Maps a pure function over the array which provides the index of the item along with the item itself.
 */
let mapWithIndex: 'a 'b. (('a, int) => 'b, array('a)) => array('b) =
  (f, xs) => Belt.Array.mapWithIndex(xs, (i, x) => f(x, i));

/**
 * Reverses an array
 */
let reverse: array('a) => array('a) = Belt.Array.reverse;

/**
 * Shuffles an array in-place (with mutation)
 */
let shuffleInPlace: array('a) => array('a) =
  xs => {
    Belt.Array.shuffleInPlace(xs);
    xs;
  };

/**
 * Creates a new array which is a shuffled version of the input array.
 */
let shuffle: array('a) => array('a) = Belt.Array.shuffle;

/**
 * Gets the length of the array.
 */
let length: array('a) => int = Belt.Array.length;

/**
 * Indicates if the array is empty (length 0).
 */
let isEmpty: array('a) => bool = arr => length(arr) == 0;

/**
 * Indicates if the array is non-empty (length > 0).
 */
let isNotEmpty: array('a) => bool = arr => length(arr) > 0;

/**
 * Gets an item at the given index, or returns None.
 */
let at: (int, array('a)) => option('a) = (i, xs) => Belt.Array.get(xs, i);

/**
 * Sets a value in-place (mutation) at the given index, but only if the index is within
 * the current range of the array.
 */
let setAt: (int, 'a, array('a)) => option(array('a)) =
  (i, x, xs) =>
    if (Belt.Array.set(xs, i, x)) {
      Some(xs);
    } else {
      None;
    };

/**
 * Gets the first item in the array (if non-empty).
 */
let head: array('a) => option('a) = arr => Belt.Array.get(arr, 0);

/**
 * Gets all but the the first item in the array (if non-empty).
 */
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

/**
 * Gets all but the first item item in the array (if non-empty), or an empty array.
 */
let tailOrEmpty: array('a) => array('a) =
  xs =>
    switch (tail(xs)) {
    | Some(ys) => ys
    | None => [||]
    };

/**
 * Gets all but the last item in the array (if non-empty).
 */
let init: array('a) => option(array('a)) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=l - 1));
    };
  };

/**
 * Gets all but the last item in the array (if non-empty), or the empty array.
 */
let initOrEmpty: array('a) => array('a) =
  xs =>
    switch (init(xs)) {
    | Some(arr) => arr
    | None => [||]
    };

/**
 * Gets the last item in the array (if non-empty);
 */
let last: array('a) => option('a) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      at(l - 1, xs);
    };
  };

/**
 * Gets up to the given count of items from the array.
 *
 * If the count is greater than the length, it gets all of the items.
 */
let take: (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let len = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.slice(xs, ~offset=0, ~len);
  };

/**
 * Gets exactly the given count of items from the array.
 *
 * If the count is greater than the length of the array, None is returned.
 */
let takeExactly: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=i));
    };

/**
 * Gets items from the array until it reaches an item which does not satisfy the given predicate.
 */
let rec takeWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => prepend(x, takeWhile(f, tailOrEmpty(xs)))
    | _ => [||]
    };

/**
 * Removes up to the count items from the array.
 *
 * If the count is greater than the length of the array, an empty array is returned.
 */
let drop: (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let start = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.sliceToEnd(xs, start);
  };

/**
 * Removes exactly the count items from the array.
 *
 * If the count is greater than the length of the array, None is returned.
 */
let dropExactly: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.sliceToEnd(xs, i));
    };

/**
 * Drops items from the array until an item is reached which does not satisfy the given predicate.
 */
let rec dropWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => dropWhile(f, tailOrEmpty(xs))
    | _ => xs
    };

/**
 * Returns a new array of items from the input array which satisfy the given predicate.
 */
let filter: 'a. ('a => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keep(xs, f);

/**
 * Returns a new array of items from the input array which satisfy the given indexed predicate.
 */
let filterWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keepWithIndex(xs, f);

/**
 * Returns a new array of items from the input array which do not satisfy the given predicate.
 */
let filterNot: 'a. ('a => bool, array('a)) => array('a) =
  f => filter(a => !f(a));

/**
 * Returns a new array of items from the input array which do not satisfy the given indexed predicate.
 */
let filterNotWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  f => filterWithIndex((a, i) => !f(a, i));

/**
 * Maps a function over the array and keeps only the values which were Some.
 */
let mapOption: 'a 'b. ('a => option('b), array('a)) => array('b) =
  (f, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, curr) =>
        Relude_Option_Base.fold(
          acc,
          v => Relude_Array_Instances.concat(acc, [|v|]),
          f(curr),
        ),
      [||],
      xs,
    );

/**
 * Collapses an array of options into an array of pure values, keeping only the `Some` values.
 */
let catOption: 'a. array(option('a)) => array('a) =
  xs => mapOption(v => v, xs);

/**
 * Splits an array into two separate arrays - one containing items which satisfy the predicate, and
 * the other array containing the items which do not satisfy the predicate.
 *
 * The items which satisfy the predicate are on the left side of the resulting tuple.
 */
let partition: ('a => bool, array('a)) => (array('a), array('a)) =
  (f, xs) => Belt.Array.partition(xs, f);

/**
 * Splits an array into two parts at the given index.
 *
 * Returns `None` if the index is out of range.
 */
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

/**
 * Creates a new array which contains the given delimiter inserted before every item in
 * the input array.
 */
let prependToAll: ('a, array('a)) => array('a) =
  (delim, xs) => Relude_Array_Instances.flatMap(v => [|delim, v|], xs);

/**
 * Creates a new array which contains the given delimiter inserted between every item in
 * the input array.
 */
let intersperse: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => prepend(x, prependToAll(delim, tailOrEmpty(xs)))
    };

/**
 * Creates a new array by concatenating the given array n times.
 */
let replicate: (int, array('a)) => array('a) =
  (i, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, _i) => Relude_Array_Instances.concat(acc, xs),
      [||],
      Relude_Int.rangeAsArray(0, i),
    );

/**
 * Creates a new array by combining pair-wise values from the two input arrays as a tuple.
 */
let zip: (array('a), array('b)) => array(('a, 'b)) = Belt.Array.zip;

/**
 * Creates a new array by combining pair-wise values from the two input arrays using the given function.
 */
let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => Belt.Array.zipBy(xs, ys, f);

/**
 * Creates a new array of each value paired with its index in a tuple.
 */
let zipWithIndex: array('a) => array(('a, int)) =
  xs => zip(xs, Relude_Int.rangeAsArray(0, length(xs)));

/**
 * Converts an array of tuples into two separate arrays, one array containing the left values,
 * and the other array containing the right values.
 */
let unzip: array(('a, 'b)) => (array('a), array('b)) = Belt.Array.unzip;

/**
 * Sorts an array using an int-based compare function.
 */
let sortWithInt: (('a, 'a) => int, array('a)) => array('a) =
  (f, xs) => Belt.SortArray.stableSortBy(xs, f);

/**
 * Sorts an array using a compare function.
 */
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

/**
 * Sorts an array using an ORD module.
 */
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

/**
 * Creates a new array containing only the distinct values of the array, using the given
 * equality function.
 */
let distinctBy: 'a. (('a, 'a) => bool, array('a)) => array('a) =
  (eq, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, curr) =>
        Relude_Array_Instances.containsBy(eq, curr, acc)
          ? acc : append(curr, acc),
      [||],
      xs,
    );

/**
 * Removes the first occurrence of the given value from the array, using the given equality function.
 */
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

/**
 * Removes all occurrences of the given value from the array, using the given equality function.
 */
let removeEachBy: 'a. (('a, 'a) => bool, 'a, array('a)) => array('a) =
  (innerEq, x, xs) =>
    Relude_Array_Instances.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : append(y, ys),
      [||],
      xs,
    );

/**
 * Creates a new array with only the distinct values from the array, using the given EQ module.
 */
let distinct =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs) => {
  module EqA = (val eqA);
  distinctBy(EqA.eq, xs);
};

/**
 * Removes the first occurrence of the given value from the array, using the given EQ module
 */
let removeFirst =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeFirstBy(EqA.eq, x, xs);
};

/**
 * Removes all occurrences of the given value from the array, using the given EQ module
 */
let removeEach =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeEachBy(EqA.eq, x, xs);
};

/**
 * Creates a new array by replace the value at the given index with the given value.  If the
 * index is out of bounds, no replacement is made.
 */
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

/**
 * Folds an array from left-to-right, collecting the results of the
 * accumulation for each individual iteration.
 */
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

/**
 * Folds an array from right-to-left, collecting the results of the
 * accumulation for each individual iteration.
 */
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
