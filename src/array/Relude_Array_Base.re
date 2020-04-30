open BsBastet.Interface;

/**
[Array.cons] prepends a single item to the start of an array. This is an
immutable operation that returns a new array in O(n) time.

{[
  Array.cons(1, [|2, 3, 4|]) == [|1, 2, 3, 4|];
]}
*/
let cons: 'a. ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat([|x|], xs);

/**
[Array.prepend] is an alias for [Array.cons], which prepends a single item to
the start of an array.
*/
let prepend: 'a. ('a, array('a)) => array('a) = cons;

/**
[Array.uncons] splits an array into head and tail parts (as a tuple), but only
if the array is non-empty.

{[
  Array.uncons([||]) == None;
  Array.uncons([|"a"|]) == Some(("a", [||]));
  Array.uncons([|"a", "b", "c"|]) == Some(("a", [|"b", "c"|]));
]}
*/
let uncons: 'a. array('a) => option(('a, array('a))) =
  xs =>
    switch (xs) {
    | [||] => None
    | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
    };

/**
[Array.append] adds a single item to the end of an array. This is an immutable
operation that returns a new array in O(n) time.

{[
  Array.append("z", [|"x", "y"|]) == [|"x", "y", "z"|];
]}
*/
let append: 'a. ('a, array('a)) => array('a) =
  (x, xs) => Relude_Array_Instances.concat(xs, [|x|]);

/**
[Array.repeat] creates an array by repeating the given item the given number of
times. If the first argument (the size) is negative, an empty array is returned.

Running time: O(n) where [n] is the provided size

{[
  Array.repeat(4, "hello") == [|"hello", "hello", "hello", "hello"|];
  Array.repeat(2, 3) == [|3, 3|];
  Array.repeat(0, true) == [||];
  Array.repeat(-10, ()) == [||];
]}
*/
let repeat: 'a. (int, 'a) => array('a) = (i, x) => Belt.Array.make(i, x);

/**
[Array.makeWithIndex] creates an array of the given size, filled with the result
of repeatedly calling the provided function with the index of each slot in the
array. If the size is negative, an empty array is returned.

Running time: O(n) where [n] is the provided size

{[
  Array.makeWithIndex(4, i => i + 2) == [|2, 3, 4, 5|];
  Array.makeWithIndex(3, fun | 0 => "a" | _ => "b") == [|"a", "b", "b"|];
  Array.makeWithIndex(-1, const(true)) == [||];
]}
*/
let makeWithIndex: 'a. (int, int => 'a) => array('a) = Belt.Array.makeBy;

/**
[Array.mapWithIndex] maps a pure function over the array which accepts both the
current item (as normal `map` does) along with the current array index. The
returned array is a new array of the same size with the value returned by the
provided function in each array slot.

Running time: O(n)

{[
  Array.mapWithIndex((x, idx) => x + idx, [|0, 1, 2|]) == [|0, 2, 4|];
]}
*/
let mapWithIndex: 'a 'b. (('a, int) => 'b, array('a)) => array('b) =
  (f, xs) => Belt.Array.mapWithIndex(xs, (i, x) => f(x, i));

/**
[Array.reverse] returns a copy of the input array in reverse order.

Running time: O(n)

{[
  Array.reverse([|1, 2, 3|]) == [|3, 2, 1|];
]}
*/
let reverse: 'a. array('a) => array('a) = Belt.Array.reverse;

/**
[Array.shuffleInPlace] mutates the provided array by randomizing the positions
of the contained values.

{[
  Array.shuffleInPlace([|1, 2, 3, 4|]) == [|3, 1, 4, 2|];
]}
*/
let shuffleInPlace: 'a. array('a) => array('a) =
  xs => {
    Belt.Array.shuffleInPlace(xs);
    xs;
  };

/**
[Array.shuffle] creates a new array which is a shuffled version of the provided
array. This is similar to [Array.shuffleInPlace] except the input array is left
unchanged.
*/
let shuffle: 'a. array('a) => array('a) = Belt.Array.shuffle;

/**
[Array.length] returns the count of items in the array in O(1) time.

{[
  Array.length([|"a", "b", "c"|]) == 3;
]}
*/
let length: 'a. array('a) => int = Belt.Array.length;

/**
[Array.isEmpty] determines whether the array contains no values (has length 0)
in O(1) time.

{[
  Array.isEmpty([||]) == true;
  Array.isEmpty([|"not", "empty"|]) == false;
]}
*/
let isEmpty: 'a. array('a) => bool = arr => length(arr) == 0;

/**
[Array.isNotEmpty] determines whether the array is non-empty (length > 0) in
O(1) time. This is the inverse of [Array.isEmpty].

{[
  Array.isNotEmpty([|3|]) == true;
  Array.isNotEmpty([||]) == false;
]}
*/
let isNotEmpty: 'a. array('a) => bool = arr => length(arr) > 0;

/**
[Array.at] returns [Some] value at the given index, or returns [None] if the
given index is out of range.

Running time: O(1)

{[
  Array.at(1, [|"a", "b"|]) == Some("b");
  Array.at(2, [|"a", "b"|]) == None;
  Array.at(0, [||]) == None;
]}
*/
let at: 'a. (int, array('a)) => option('a) =
  (i, xs) => Belt.Array.get(xs, i);

/**
[Array.setAt] mutates the provided array by setting the provided value at the
provided index. If the index is in range, the return contains [Some] with the
updated array, otherwise [None] is returned.

This is an in-place mutation that doesn't affect the size of the array.

{[
  Array.setAt(1, "a", [|"0", "1", "2"|]) == Some([|"0", "a", "2"|]);
  Array.setAt(0, "hello", [||]) == None;
]}
*/
let setAt: 'a. (int, 'a, array('a)) => option(array('a)) =
  (i, x, xs) =>
    if (Belt.Array.set(xs, i, x)) {
      Some(xs);
    } else {
      None;
    };

/**
[Array.head] returns the first item in the array (if non-empty) in O(1) time.

{[
  Array.head([||]) == None;
  Array.head([|"a", "b", "c"|]) == Some("a");
]}
*/
let head: 'a. array('a) => option('a) = arr => Belt.Array.get(arr, 0);

/**
[Array.tail] optionally returns a new array containing all but the the first
item of the provided array. If the given array is empty (has no tail), [None] is
returned.

If you would prefer to avoid the [option] and fall back to an empty array
instead, see [Array.tailOrEmpty].

{[
  Array.tail([||]) == None;
  Array.tail([|"hello"|]) == Some([||]);
  Array.tail([|5, 4, 3, 2, 1|]) == Some([|4, 3, 2, 1|]);
]}
*/
let tail: 'a. array('a) => option(array('a)) =
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
[Array.tailOrEmpty] returns a new array containing all but the first item of the
provided array. If the given array is empty, an empty array is returned.

{[
  Array.tailOrEmpty([|1, 2, 3|]) == [|2, 3|];
  Array.tailOrEmpty([|1|]) == [||];
  Array.tailOrEmpty([||]) == [||];
]}
*/
let tailOrEmpty: 'a. array('a) => array('a) =
  xs =>
    switch (tail(xs)) {
    | Some(ys) => ys
    | None => [||]
    };

/**
[Array.init] optionally returns a new array with all elements except the last of
the provided array in O(n) time.

{[
  Array.init([|"a"|]) == Some([||]);
  Array.init([|"a", "b", "c", "d"|]) == Some([|"a", "b", "c"|]);
  Array.init([||]) == None;
]}
*/
let init: 'a. array('a) => option(array('a)) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=l - 1));
    };
  };

/**
[Array.initOrEmpty] returns a new array with all elements except the last of
the provided array in O(n) time. If the given array is empty, and empty array
is returned (unlike [Array.init], which makes this case explicit with [option]).

{[
  Array.initOrEmpty([||]) == [||];
  Array.initOrEmpty([|"a"|]) == [||];
  Array.initOrEmpty([|"a", "b", "c", "d"|]) == [|"a", "b", "c"|];
]}
*/
let initOrEmpty: 'a. array('a) => array('a) =
  xs =>
    switch (init(xs)) {
    | Some(arr) => arr
    | None => [||]
    };

/**
[Array.last] optionally returns the last item in the array.

{[
  Array.last([|"a"|]) == Some("a");
  Array.last([|"a", "b", "c", "d"|]) == Some("d");
  Array.last([||]) == None;
]}
*/
let last: 'a. array('a) => option('a) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      at(l - 1, xs);
    };
  };

/**
[Array.take] returns a new array including the first [n] items of the provided
array, where [n] is the requested count. If the requested count is negative, an
empty array will be returned. If the requested count is greater than the length
of the provided array, a copy of the whole array will be returned.

Running time: O(n)

{[
  Array.take(3, [|"a", "b", "c", "d", "e"|]) == [|"a", "b", "c"|];
  Array.take(2, [|"a"|]) == [|"a"|];
  Array.take(-1, [||]) == [||];
]}
*/
let take: 'a. (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let len = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.slice(xs, ~offset=0, ~len);
  };

/**
[Array.takeExactly] optionally returns a new array with the first [n] items of
the provided array. If the number of requested items is negative or larger than
the size of the provided array, [None] is returned. See [Array.take] if you'd
rather not deal with the [option] (in exchange for not getting a guaranteed
number of items back).

{[
  Array.takeExactly(2, [|"a", "b", "c", "d", "e"|]) == Some([|"a", "b"|]);
  Array.take(2, [|"a"|]) == None;
  Array.take(-1, [||]) == None;
]}
*/
let takeExactly: 'a. (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=i));
    };

/**
[Array.takeWhile] returns a new array, filled with items from the provided array
until an item doesn't pass the provided predicate.

The running time is currently O(n{^ 2}), and the function is not stack safe.

{[
  Array.takeWhile(x => x < 2, [|0, 1, 2, 3, 4, 5|]) == [|0, 1|];
  Array.takeWhile(const(false), [|0, 1, 2, 3, 4, 5|]) == [||];
]}
*/
let rec takeWhile: 'a. ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => prepend(x, takeWhile(f, tailOrEmpty(xs)))
    | _ => [||]
    };

/**
[Array.drop] returns a new array that does not contain the first [n] items of
the provided array. If the provided count to be dropped is less than 0, no items
will be dropped (the returned array will have all of the same values as the
provided array). If the count is greater than the length of the array, an empty
array will be returned.

{[
  Array.drop(2, [|"a", "b", "c", "d"|]) == [|"c", "d"|];
  Array.drop(5, [|"a", "b", "c", "d"|]) == [||];
  Array.drop(-1, [|"a", "b"|]) == [|"a", "b"|];
]}
*/
let drop: 'a. (int, array('a)) => array('a) =
  (i, xs) => {
    let l = length(xs);
    let start = i < 0 ? 0 : l < i ? l : i;
    Belt.Array.sliceToEnd(xs, start);
  };

/**
[Array.dropExactly] removes exactly the count items from the array. If the count
is greater than the length of the array, [None] is returned.

{[
  Array.dropExactly(2, [|"a", "b", "c"|]) == Some([|"c"|]);
  Array.dropExactly(3, [|"a", "b"|]) == None;
]}
*/
let dropExactly: 'a. (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.sliceToEnd(xs, i));
    };

/**
[Array.dropWhile] drops items from the beginning of the array until an item is
reached which does not satisfy the given predicate.

{[
  Array.dropWhile(v => v < 2, [|1, 2, 3, 4, 5|]) == [|3, 4, 5|];
]}
*/
let rec dropWhile: 'a. ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => dropWhile(f, tailOrEmpty(xs))
    | _ => xs
    };

/**
[Array.filter] returns a new array containing only the values from the provided
array which satisfy the given predicate.

{[
  Array.filter(v => v mod 2 == 0, [|1, 2, 3, 4|]) == [|2, 4|];
]}
*/
let filter: 'a. ('a => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keep(xs, f);

/**
[Array.keep] is an alias for [filter].
*/
let keep: 'a. ('a => bool, array('a)) => array('a) = filter;

/**
[Array.filterWithIndex] returns a new array of items from the input array which
satisfy the given indexed predicate.
*/
let filterWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keepWithIndex(xs, f);

/**
[Array.keepWithIndex] is an alias for [filterWithIndex].
*/
let keepWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) = filterWithIndex;

/**
[Array.filterNot] returns a new array of items from the input array which do not
satisfy the given predicate.
*/
let filterNot: 'a. ('a => bool, array('a)) => array('a) =
  f => filter(a => !f(a));

/**
[Array.reject] is an alias for [filterNot].
*/
let reject: 'a. ('a => bool, array('a)) => array('a) = filterNot;

/**
[Array.filterNotWithIndex] returns a new array of items from the input array
which do not satisfy the given indexed predicate.
*/
let filterNotWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) =
  f => filterWithIndex((a, i) => !f(a, i));

/**
[Array.rejectWithIndex] is an alias for [filterNotWithIndex].
 */
let rejectWithIndex: 'a. (('a, int) => bool, array('a)) => array('a) = filterNotWithIndex;

/**
[Array.mapOption] maps a function over the array and keeps only the values which
were Some.

{[
  Array.mapOption(String.toInt, [|"1", "a", "2", "3", "b"|]) == [|1, 2, 3|];
]}
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
[Array.catOption] creates a new array from an array of options, filtering out
all of the `None` values.

Running time: O(n)

{[
  Array.catOption([|None, None, Some("a"), Some("b"), None|]) == [|"a", "B"|];
]}
*/
let catOption: 'a. array(option('a)) => array('a) =
  xs => mapOption(v => v, xs);

/**
[Array.partition] splits an array into two separate arrays - one containing
items which satisfy the predicate, and the other array containing the items
which do not satisfy the predicate.

The items which satisfy the predicate are on the left side of the resulting
tuple.

{[
  Array.partition(a => a mod 2 == 0, [|0, 1, 2, 3|]) == ([|0, 2|], [|1, 3|]);
]}
*/
let partition: 'a. ('a => bool, array('a)) => (array('a), array('a)) =
  (f, xs) => Belt.Array.partition(xs, f);

/**
[Array.splitAt] optionally returns two arrays, with the original array divided
at the given index. If the index is out of range, `None` is returned.

{[
  Array.splitAt(3, [|0, 1, 2, 3, 4|]) == Some([|0, 1, 2|], [|3, 4|]);
  Array.splitAt(2, [|0, 1|]) == Some([|0, 1|], [||]);
  Array.splitAt(-1, [|0, 1|]) == None;
]}
*/
let splitAt: 'a. (int, array('a)) => option((array('a), array('a))) =
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
[Array.prependToAll] creates a new array which contains the given delimiter
inserted before every item in the provided array.

{[
  Array.prependToAll(0, [|0, 1, 2|]) == [|0, 0, 0, 1, 0, 2|];
]}
*/
let prependToAll: 'a. ('a, array('a)) => array('a) =
  (delim, xs) => Relude_Array_Instances.flatMap(v => [|delim, v|], xs);

/**
[Array.intersperse] creates a new array which contains the given delimiter
inserted between every item in the input array. If the provided array is empty,
the returned array will also be empty.

{[
  Array.intersperse("z", [|"a", "b", "c"|]) == [|"a", "z", "b", "z", "c"|];
]}
*/
let intersperse: 'a. ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => prepend(x, prependToAll(delim, tailOrEmpty(xs)))
    };

/**
[Array.replicate] creates a new array by concatenating the given array [n]
times. Negative counts will return an empty array.

{[
  Array.replicate(2, [|"a", "b"|]) == [|"a", "b", "a", "b"|];
  Array.replicate(0, [|"a", "b"|]) == [||];
]}
*/
let replicate: 'a. (int, array('a)) => array('a) =
  (i, xs) =>
    Relude_Array_Instances.foldLeft(
      (acc, _i) => Relude_Array_Instances.concat(acc, xs),
      [||],
      Relude_Int.rangeAsArray(0, i),
    );

/**
[Array.zip] creates a new array by combining pair-wise values from the two input
arrays as a tuple. If the two input arrays have different lengths, the returned
array's length will match the shortest array.
*/
let zip: 'a 'b. (array('a), array('b)) => array(('a, 'b)) = Belt.Array.zip;

/**
[Array.zipWith] creates a new array by combining pair-wise values from the two
input arrays using the given function.
*/
let zipWith: 'a 'b 'c. (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => Belt.Array.zipBy(xs, ys, f);

/**
[Array.zipWithIndex] creates a new array of each value paired with its index in
a tuple.

{[
  Array.zipWithIndex([|"a", "b", "c"|]) == [|("a", 0), ("b", 1), ("c", 2)|];
]}
*/
let zipWithIndex: 'a. array('a) => array(('a, int)) =
  xs => zip(xs, Relude_Int.rangeAsArray(0, length(xs)));

/**
[Array.unzip] converts an array of tuples into two separate arrays, one array
containing the left values, and the other array containing the right values.

{[
  Array.unzip([|("a", 3), ("b", 1)|]) == ([|"a", "b"|], [|3, 1|]);
]}
*/
let unzip: 'a 'b. array(('a, 'b)) => (array('a), array('b)) = Belt.Array.unzip;

/**
[Array.sortWithInt] sorts an array in ascending order using the provided
int-based compare function.
*/
let sortWithInt: 'a. (('a, 'a) => int, array('a)) => array('a) =
  (f, xs) => Belt.SortArray.stableSortBy(xs, f);

/**
[Array.sortBy] sorts an array in ascending order using a compare function.

{[
  Array.sortBy(String.compare, [|"z", "a", "b"|]) == [|"a", "b", "z"|];
]}
*/
let sortBy: 'a. (('a, 'a) => ordering, array('a)) => array('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

/**
[Array.sort] sorts an array in ascending order using an ORD module.

{[
  Array.sort((module String.Ord), [|"z", "a", "b"|]) == [|"a", "b", "z"|];
]}
*/
let sort =
    (type a, ordA: (module ORD with type t = a), xs: array(a)): array(a) => {
  module OrdA = (val ordA);
  sortBy(OrdA.compare, xs);
};

/**
[Array.distinctBy] creates a new array containing only the distinct values of
the array, using the given equality function.

This is an O(n{^2}) operation, and in cases where the inner values can be
compared for ordering, using a [Set] is a much more efficient option.
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
[Array.removeFirstBy] removes the first occurrence of the given value from the
array, using the given equality function.
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
[Array.removeEachBy] removes all occurrences of the given value from the array,
using the given equality function.
*/
let removeEachBy: 'a. (('a, 'a) => bool, 'a, array('a)) => array('a) =
  (innerEq, x, xs) =>
    Relude_Array_Instances.foldLeft(
      (ys, y) => innerEq(x, y) ? ys : append(y, ys),
      [||],
      xs,
    );

/**
[Array.distinct] creates a new array with only the distinct values from the
array, using the given EQ module.

This is an O(n{^2}) operation, and in cases where the inner values can be
compared for ordering, using a [Set] is a much more efficient option.
*/
let distinct = (type a, eqA: (module EQ with type t = a), xs) => {
  module EqA = (val eqA);
  distinctBy(EqA.eq, xs);
};

/**
[Array.removeFirst] creates a copy of the given array with the first occurrence
of the given element removed, using the provided EQ module.

Running time: O(n)

{[
  Array.removeFirst((module Int.Eq), 1, [|1, 2, 1, 3|]) == [|2, 1, 3|];
]}
*/
let removeFirst = (type a, eqA: (module EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeFirstBy(EqA.eq, x, xs);
};

/**
 * Removes all occurrences of the given value from the array, using the given EQ module
 */
let removeEach = (type a, eqA: (module EQ with type t = a), x, xs) => {
  module EqA = (val eqA);
  removeEachBy(EqA.eq, x, xs);
};

/**
[Array.replaceAt] creates a new array by replacing the value at the given index
with the given value. If the index is out of bounds, no replacement is made.

Running time: O(n)

{[
  Array.replaceAt(1, "z", [|"a", "b", "c"|]) == [|"a", "z", "c"|];
]}
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
[Array.scanLeft] folds an array from left-to-right, collecting the results of
the accumulation for each individual iteration.

Running time: O(n)

{[
  Array.scanLeft((+), 0, [|1,2,3|]) == [|1,3,6|];
]}
*/
let scanLeft: 'a 'b. (('b, 'a) => 'b, 'b, array('a)) => array('b) =
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
[Array.scanRight] folds an array from right-to-left, collecting the results of
the accumulation for each individual iteration.

Running time: O(n)

{[
  Array.scanRight((+), 0, [|1,2,3|]) == [|6,5,3|];
]}
*/
let scanRight: 'a 'b. (('a, 'b) => 'b, 'b, array('a)) => array('b) =
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

/**
[Array.insertAt] creates a new array that inserts the given value at the given
index. If the index is out of range, no insertion is made.

{[
  Array.insertAt(1, "z", [|"a", "b", "c"|]) == [|"a", "z", "b", "c"|];
  Array.insertAt(2, 3, [|1, 2|]) == [|1, 2, 3|];
]}
*/
let insertAt: 'a. (int, 'a, array('a)) => array('a) =
  (targetIndex, newX, xs) => {
    switch (splitAt(targetIndex, xs)) {
    | Some((before, after)) =>
      Relude_Array_Instances.concat(
        before,
        Relude_Array_Instances.concat([|newX|], after),
      )
    | None => xs
    };
  };

/**
[Array.updateAt] creates a new array that replaces the value at the given index
with the value returned by the provided function. If the index is out of range,
the original array is returned.

Running time: O(n)

{[
  Array.updateAt(2, x => x * 5, [|1, 2, 3, 4|]) == [|1, 2, 15, 4|];
]}
*/
let updateAt: 'a. (int, 'a => 'a, array('a)) => array('a) =
  (targetIndex, f, xs) => {
    xs |> mapWithIndex((x, index) => index == targetIndex ? f(x) : x);
  };

/**
[Array.swapAt] creates a new array with the elements at the two given indexes
swapped. If either index is out of range, a copy of the original array is
returned.

Running time: O(n)

{[
  Array.swapAt(0, 2, [|"a", "b", "c", "d"|]) == [|"c", "b", "a", "d"|];
]}
*/
let swapAt: 'a. (int, int, array('a)) => array('a) =
  (i, j, xs) => {
    switch (at(i, xs), at(j, xs)) {
    | (Some(a), Some(b)) =>
      xs |> mapWithIndex((x, k) => i == k ? b : j == k ? a : x)
    | _ => xs
    };
  };

/**
[Array.removeAt] creates a new array without the element at the given index. If
the index is out of range, a copy of the original array is returned.

Running time: O(n)

{[
  Array.removeAt(1, [|"a", "b", "c"|]) == [|"a", "c"|];
  Array.removeAt(3, [|"a", "b", "c"|]) == [|"a", "b", "c"|];
]}
*/
let removeAt: 'a. (int, array('a)) => array('a) =
  (targetIndex, xs) => {
    xs |> filterWithIndex((_, i) => i != targetIndex);
  };

/**
[Array.chunk] returns an array of arrays, where each of the inner arrays has
length equal to the provided [size] parameter. If the array can't be split
evenly, the final chunk wil contain fewer elements than the requested size.

If the provided array is empty, the returned array will contain a single empty
array. If the requested size is 0 or negative, the provided array will be
returned, wrapped in an array, but this case doesn't make sense and should be
avoided.

This runs in O(n{^2}) time and is not stack safe.

{[
  Array.chunk(3, [|1, 2, 3, 4, 5|]) == [|[|1, 2, 3|], [|4, 5|]|];
  Array.chunk(3, [||]) == [|[||]|];
  Array.chunk(0, [|1, 2, 3|]) == [|[|1, 2, 3|]|];
]}
*/
let rec chunk: 'a. (int, array('a)) => array(array('a)) =
  (size, xs) =>
    size < 1
      ? [|xs|]
      : xs |> length <= size
          ? [|xs|]
          : xs
            |> drop(size)
            |> chunk(size)
            |> Relude_Array_Instances.concat([|xs |> take(size)|]);
