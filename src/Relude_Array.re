/**
  # Array
  
  Various array operations. The array being processed is always
  the last parameter in these functions.
*/;

/**
  `length(xs)` returns the number of items in `xs`.

  ## Example
  ```re
  length([|"a", "b", "c"|]) == 3;
  length([| |]) == 0;
  let x = length([|100, 101, 102|]);
  Js.log(x);
  ```
*/
let length: array('a) => int = Belt.Array.length;

/**
  `isEmpty(xs) returns `true` if `xs` is the empty array `[| |]`; returns `false` otherwise.
*/
let isEmpty: array('a) => bool = arr => length(arr) == 0;

/**
  `isNotEmpty(xs) returns `true` if `xs` is not the empty array `[| |]`; returns `false` otherwise.
*/
let isNotEmpty: array('a) => bool = arr => length(arr) > 0;

/**
  `empty xs` returns a new, empty array.
*/
let empty: array('a) = [||];

/**
  `pure(item)` returns an array containing the given item.
  
  ## Example
  ```re
  pure("single") == [|"single"|];
  ```
*/
let pure: 'a => array('a) = x => [|x|];

/**
  `one(item)` returns an array containing the given item. (Same as `pure`.)
*/
let one: 'a => array('a) = pure;

/**
  `repeat(n, x)` returns an array containing `n` copies of `x`.
  
  ## Example
  ```re
  repeat(3, "ha") == [|"ha", "ha", "ha"|];
  repeat(0, "nothing") == [| |];
  repeat(-2, "nothing") == [| |];
  ```
*/
let repeat: (int, 'a) => array('a) = (i, x) => Belt.Array.make(i, x);

/**
  `makeWithIndex(n, f)` returns the array `[|f(0), f(1), ... f(n - 1)|]`.
  
  
  ## Example
  ```re
  makeWithIndex(3, (x) => {(x + 4) * (x + 4)}) == [|16, 25, 36|];
  makeWithIndex(0, (x) => {x + 1}) == [| |];
  makeWithIndex(-1, (x) => {x + 1}) == [| |];
  ```
*/
let makeWithIndex: (int, int => 'a) => array('a) = Belt.Array.makeBy;

/**
  `concat(xs, ys)` returns an array with the elements of `xs` followed
  by the elements of `ys`.
  
  ## Example
  ```re
  concat([|"a", "b"|], [|"c", "d"|]) == [|"a", "b", "c", "d"|];
  concat([| |], [|"a", "b"|]) == [|"a", "b"|];
  concat([|"a", "b"|], [| |]) == [|"a", "b"|];
  ```
*/
let concat: (array('a), array('a)) => array('a) = Belt.Array.concat;

/**
  `cons(x, xs)` returns a new array with value `x` at the beginning.
  
  ## Example
  ```re
  cons(99, [|100, 101|]) == [|99, 100, 101|];
  cons(99, [| |]) == [|99|];
  ```
*/
let cons: ('a, array('a)) => array('a) = (x, xs) => concat([|x|], xs);

/**
  When given a non-emtpy array, `uncons(xs)` returns `Some(y, ys)`
  where `y` is the first element in the array and `ys` are the remaining
  elements. If given an empty array, `uncons()` returns `None`.

  ## Example
  ```re
  uncons([|100, 101, 102|]) == Some((100, [|101, 102|]));
  uncons([|100|]) == Some((100, [| |]));
  uncons([| |]) == None;
  ```
*/
let uncons: array('a) => option(('a, array('a))) =
  xs =>
    switch (xs) {
    | [||] => None
    | _ => Some((Belt.Array.getExn(xs, 0), Belt.Array.sliceToEnd(xs, 1)))
    };

/**
  Same as `cons`
*/
let prepend: ('a, array('a)) => array('a) = cons;

/**
  `append(x, xs)` adds the value `x` at the end of array `xs`.
  
  ## Example
  ```re
  append(999, [|100, 101, 102|]) == [|100, 102, 103, 999|];
  append(999, [| |]) == [|999|];
  ```
*/
let append: ('a, array('a)) => array('a) =
  (item, array) => concat(array, [|item|]);

/**
  `foldLeft(f, init, xs)` accumulates a value. Starting with `init`,
  as the initial value of the accumulator, `foldLeft` applies function
  `f` to the accumulator and the first element in the list. The result
  becomes the new value of the accumulator, and `f` is applied to that value
  and the next element in `xs`. This process continues until all elements in
  `xs` are processed. The final value of the accumulator is returned.
  
  ## Example
  ```re
  foldLeft((acc, item) => append(item, acc), [| |], [|1, 2, 3|]) == [|1, 2, 3|];
  foldLeft((acc, item) => acc + item, 2, [| |]) == 2;
  ```
*/
let foldLeft: (('b, 'a) => 'b, 'b, array('a)) => 'b = BsAbstract.Array.Foldable.fold_left;

/**
  `foldRight(f, init, xs)` accumulates a value. Starting with `init`,
  as the initial value of the accumulator, `foldRight` applies function
  `f` to the last element in the list and the accumulator. The result
  becomes the new value of the accumulator, and `f` is applied to that value
  and the preceding element in `xs`. This process continues until all elements in
  `xs` are processed. The final value of the accumulator is returned.
  
  ## Example
  ```re
  foldRight((item, acc) => append(item, acc), [| |], [|1, 2, 3|]) == [|3, 2, 1|];
  foldRight((item, acc) => acc + item, 2, [| |]) == 2;
  ```
*/
let foldRight: (('a, 'b) => 'b, 'b, array('a)) => 'b = BsAbstract.Array.Foldable.fold_right;

/**
  `scanLeft(f, init, xs)` returns an array of values. Starting with `init`,
  as the initial value of an accumulator, `scanLeft` applies function
  `f` to the accumulator and the first element in the list. The result
  is appended to the array of values and becomes the new value of the accumulator
  Then `f` is applied to the new accumulator and the next element in `xs`.
  This process continues until all elements in `xs` are processed.
  `scanLeft` returns the array of all the accumulated values.

  In short, `scanLeft` returns an array of the values that a
  `foldLeft` would have computed, in left to right order.

  ## Example
  ```re
  scanLeft( (acc, item) => {acc - item}, 0, [|1, 2, 3|]) == [|-1, -3, -6|];
  scanLeft( (acc, item) => {acc + item}, 0, [| |]) == [| |];
  ```
*/
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b) =
  (f, init, xs) =>
    snd(
      foldLeft(
        ((acc, result), curr) => {
          let nextAcc = f(acc, curr);
          (nextAcc, append(nextAcc, result));
        },
        (init, [||]),
        xs,
      ),
    );

/**
  `scanRight(f, init, xs)` returns an array of values. Starting with `init`,
  as the initial value of an accumulator, `scanRight` applies function
  `f` to the last element in the list and the accumulator. The result
  is prepended to the array of values and becomes the new value of the accumulator.
  Then `f` is applied to the preceding element in `xs` and the accumulator.
  This process continues until all elements in `xs` are processed.
  `scanRight` returns the array of all the accumulated values.
  
  In short, `scanRight` returns an array of the values that a
  `foldRight` would have computed, in right to left order.
  
  ## Example
  ```re
  scanRight( (item, acc) => {acc - item}, 0, [|1, 2, 3|]) == [|-6, -5, -3|];
  scanRight( (item, acc) => {acc + item}, 0, [| |]) == [| |];
  ```
*/
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b) =
  (f, init, xs) =>
    snd(
      foldRight(
        (curr, (acc, result)) => {
          let nextAcc = f(curr, acc);
          (nextAcc, prepend(nextAcc, result));
        },
        (init, [||]),
        xs,
      ),
    );

/**
  `get(n, xs)` returns the value at the given index position as `Some(value)`
  unless `n` is less than zero or greater than the length of `xs`, in which
  case `get()` returns `None.`
  
  ## Example
  ```reason
  get(0, [|100, 101, 102|]) == Some(100);
  get(2, [|100, 101, 102|]) == Some(102);
  get(-1, [|100, 101, 102|]) == None;
  get(3, [|100, 101, 102|]) == None;
  ```
*/
let get: (int, array('a)) => option('a) = (i, xs) => Belt.Array.get(xs, i);

/**
  `set(n, value, xs)` updates the given array with item `n` set to `value` and
  returns `true` when `n` is greater than or equal to zero and less than
  the length of `xs`. If `n` is not a valid index, the array remains unchanged
  and the function returns `false`.
  
  In the following example, the statements are performed in order.
  
  ## Example
  let arr = [|100, 101, 102|];
  set(1, 999, arr) == true && arr == [|100, 999, 102|];
  set(-1, 888, arr) == false && arr == [|100, 999, 102|];
  set(3, 777, arr) == false && arr == [|100, 999, 102|];
  ```
*/
let set: (int, 'a, array('a)) => bool =
  (i, x, xs) => Belt.Array.set(xs, i, x);

/**
  For non-empty arrays, `head(xs)` returns the first item in the array
  as `Some(value)`. For an empty array, the function returns `None`.
  
  ## Example
  ```reason
  head([|100, 101, 102|]) == Some(100);
  head([| |]) == None;
  ```
*/
let head: array('a) => option('a) = arr => Belt.Array.get(arr, 0);

/**
  For non-empty arrays, `tail(xs)` returns an array consisting of all
  but the first item in `xs` as `Some(ys)`. For an empty array, the
  function returns `None`.
  
  ## Example
  ```reason
  tail([|100, 101, 102|]) == Some([|101, 102|]);
  tail([| |]) == None;
  ```
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
      Belt.Array.length(ys) > 0 ? Some(ys) : None;
    };
  };

/**
  For non-empty arrays, `tailOrEmpty(xs)` returns an array consisting of all
  but the first item in `xs`. For an empty array, the function returns
  an empty array.
  
  ## Example
  ```reason
  tailOrEmpty([|100, 101, 102|]) == [|101, 102|];
  tailOrEmpty([| |]) == [| |];
  ```
*/
let tailOrEmpty: array('a) => array('a) =
  arr => tail(arr)->Belt.Option.getWithDefault(empty);

/**
  For non-empty arrays, `init(xs)` returns an array containing
  all but the last item in `xs` as `Some(ys)`. The function returns
  `None` if given an empty array.
  
  ## Example
  ```reason
  init([|100, 101, 102|]) == Some([|100, 101|]);
  init([| |]) == None;
  ```
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
  For non-empty arrays, `last(xs)` returns the last
  item in `xs` as `Some(value)`. The function returns
  `None` if given an empty array.
  
  ## Example
  ```reason
  last([|100, 101, 102|]) == Some(102);
  last([| |]) == None;
  ```
*/
let last: array('a) => option('a) =
  xs => {
    let l = length(xs);
    if (l == 0) {
      None;
    } else {
      get(l - 1, xs);
    };
  };

/**
  `take(n, xs)` returns an array of the first `n` items in `xs` as
  `Some(ys)`, If `n` is less than or equal to zero, `take()` returns `Some([| |])`.
  If `n` is greater than or equal to the length  of `xs`, `take()` returns `None`.
  
  ## Example
  ```reason
  take(2, [|100, 101, 102, 103|]) == Some([|100, 101|]);
  take(0, [|100, 101, 102|]) == Some([| |]);
  take(-1, [|100, 101, 102|]) == Some([| |]);
  take(4, [|100, 101, 102|]) == None;
  take(1, [| |]) == None;
  ```
*/
let take: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.slice(xs, ~offset=0, ~len=i));
    };

/**
  `takeUptTo(n, xs)` returns an array of the first `n` items in `xs`.
  If `n` is less than or equal to zero, `takeUpTo()` returns the empty array `[| |]`.
  If `n` is greater than or equal to the length  of `xs`, `takeUpTo()` returns the
  entire array `xs`.
  
  ## Example
  ```reason
  takeUpTo(0, [|100, 101, 102|]) == [| |];
  takeUpTo(-1, [|100, 101, 102|]) == [| |];
  takeUpTo(4, [|100, 101, 102|]) == [|100, 101, 102|];
  takeUpTo(1, [| |]) == [| |];
  ```
*/
let takeUpTo: (int, array('a)) => array('a) =
  (i, xs) => {
    if (i >= 0) {
      Belt.Array.slice(xs, ~offset=0, ~len=i);
    } else {
      [| |]
    }
  };

/**
  `takeWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `takeWhile()` returns an array consisting
  of the first elements of `xs` which satisfy the predicate. (This
  could be an empty array.)
  
  ## Example
  ```reason
  let even = (x) => {x mod 2 == 0};
  takeWhile(even, [|2, 6, 5, 3, 8|]) == [|2, 6|];
  takeWhile(even, [|5, 3, 8, 2, 6|]) == [| |];
  takeWhile(even, [| |]) == [| |];
  ```
*/
let rec takeWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => prepend(x, takeWhile(f, tailOrEmpty(xs)))
    | _ => [||]
    };

/**
  `drop(n, xs)` returns an array of all *except* the first `n` items in `xs` as
  `Some(ys)`. 
  If `n` is greater than or equal to the length  of `xs`, `drop()` returns `None`.
  If `n` is less than zero, `drop()` returns the *last* `abs(n)` values of `xs`;
  if `n` is less than or equal to the length of `xs`, `drop()` returns all of `xs`.
  
  ## Example
  ```reason
  drop(2, [|100, 101, 102, 103|]) == Some([|102, 103|]);
  drop(0, [|100, 101, 102|]) == Some([| |]);
  drop(4, [|100, 101, 102|]) == None;
  drop(-1, [|100, 101, 102|]) == Some([|102|]);
  drop(-2, [|100, 101, 102|]) == Some([|101, 102|]);
  drop(-4, [|100, 101, 102|]) == Some([|100, 101, 102|]);
  drop(1, [| |]) == None;
  ```
*/
let drop: (int, array('a)) => option(array('a)) =
  (i, xs) =>
    if (i < 0 || i > length(xs)) {
      None;
    } else {
      Some(Belt.Array.sliceToEnd(xs, i));
    };

/**
  `dropUpTo(n, xs)` returns an array of all *except* the first `n` items in `xs`.
  If `n` is greater than or equal to the length  of `xs`, `dropUpTo()` returns the empty array.
  If `n` is less than zero, `dropUpTo()` returns the *last* `abs(n)` values of `xs`;
  if `n` is less than or equal to the length of `xs`, `dropUpTo()` returns all of `xs`.
  
  ## Example
  ```reason
  dropUpTo(2, [|100, 101, 102, 103|]) == [|102, 103|];
  dropUpTo(0, [|100, 101, 102|]) == [|100, 101, 102|];
  dropUpTo(4, [|100, 101, 102|]) == [| |];
  dropUpTo(-1, [|100, 101, 102|]) == [|102|];
  dropUpTo(-2, [|100, 101, 102|]) == [|101, 102|];
  dropUpTo(-4, [|100, 101, 102|]) == [|100, 101, 102|];
  dropUpTo(1, [| |]) == [| |];
  ```
*/let dropUpTo: (int, array('a)) => array('a) =
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
      [| |]
    }
  };

/**
  `dropWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `dropWhile()` returns an array consisting
  all *except* the first elements of `xs` which satisfy the predicate.
  (This could be an empty array.)
  
  ## Example
  ```reason
  let even = (x) => {x mod 2 == 0};
  dropWhile(even, [|2, 6, 5, 3, 8|]) == [|5, 3, 8|];
  dropWhile(even, [|5, 3, 8|]) == [|5, 3, 8|];
  dropWhile(even, [|2, 4, 6|]) == [| |];
  dropWhile(even, [| |]) == [| |];
  ```
*/
let rec dropWhile: ('a => bool, array('a)) => array('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => dropWhile(f, tailOrEmpty(xs))
    | _ => xs
    };

/**
  `filter(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `filter()` returns an array of all
  the elements of `xs` that satisfy the predicate.
  
  ## Example
  ```reason
  let even = (x) => {x mod 2 == 0};
  filter(even, [|2, 6, 5, 3, 4, 1|]) == [|2, 6, 4|];
  filter(even, [|5, 3, 1|]) == [| |];
  filter(even, [| |]) == [| |];
  ```
*/
let filter: ('a => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keep(xs, f);

/**
  `filterWithIndex(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameters an element of `xs` and
  the element’s index value, returning a boolean value.
  `filterWithIndex()` returns an array of all
  the elements of `xs` that satisfy the predicate.
  
  ## Example
  ```reason
  let bothEven = (x, i) => {x mod 2 == 0 && i mod 2 == 0};
  filterWithIndex(bothEven, [|2, 4, 3, 1, 6, 5|]) == [|2, 6|];
  filterWithIndex(bothEven, [|1, 4, 3, 6, 5, 2|]) == [| |];
  ```
*/
let filterWithIndex: (('a, int) => bool, array('a)) => array('a) =
  (f, xs) => Belt.Array.keepWithIndex(xs, f);

/**
  `find(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs`
  and returns a boolean. `find()` returns the first value in
  `xs` which satisfies the predicate as `Some(x)`. If no
  value in `xs` satisfies the predicate, `find()` returns `None`.
  
  ## Example
  ```reason
  let even = (x) => {x mod 2 == 0};
  find(even, [|1, 3, 5, 6, 2, 4|]) == Some(6);
  find(even, [|1, 3, 5|]) == None;
  find(even, [| |]) == None;
  ```  
*/
let rec find: ('a => bool, array('a)) => option('a) =
  (f, xs) =>
    switch (head(xs)) {
    | Some(x) when f(x) => Some(x)
    | None => None
    | _ => find(f, tailOrEmpty(xs))
    };

/**
  `findWithIndex(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameters an element of `xs`
  and its index, returning a boolean. `find()` returns the first value in
  `xs` which satisfies the predicate as `Some(x)`. If no
  value in `xs` satisfies the predicate, `findWithIndex()` returns `None`.
  
  ## Example
  ```reason
  let bothEven = (x, i) => {(x mod 2 == 0) && (i mod 2 == 0)};
  findWithIndex(bothEven, [|1, 5, 2, 4, 3, 6|]) == Some(2);
  findWithIndex(bothEven, [|1, 2, 3|]) == None;
  findWithIndex(bothEven, [| |]) == None;
  ```  
*/
let findWithIndex: (('a, int) => bool, array('a)) => option('a) =
  (f, xs) => {
    let rec go = (f, ys, i) =>
      switch (head(ys)) {
      | Some(y) when f(y, i) => Some(y)
      | None => None
      | _ => go(f, tailOrEmpty(ys), i + 1)
      };
    go(f, xs, 0);
  };

/**
  `partition(f, xs)` takes as its first parameter a predicate function.
  The predicate takes as its parameter an element of `xs` and returns a boolean.
  
  `partition()` returns a tuple of two arrays: the elements in
  `xs` that satisfy the predicate, and the elements that don’t.
  
  ## Example
  
  ```reason
  let even = (x) => {x mod 2 == 0};
  partition(even, [|6, 1, 3, 2, 4, 5|]) == ([|6, 2, 4|], [|1, 3, 5|]);
  partition(even, [|6, 2, 4|]) == ([|6, 2, 4|], [| |]);
  partition(even, [|1, 3, 5|]) == ([| |], [|1, 3, 5|]);
  partition(even, [| |]) == ([| |], [| |]);
  ```
*/
let partition: ('a => bool, array('a)) => (array('a), array('a)) =
  (f, xs) => Belt.Array.partition(xs, f);

/**
  `splitAt(n, xs)` returns `Some(ys, zs)` where `ys` contains the
  first `n` elements of `xs` and `zs` contains the remaining elements,
  when `n` is greater than or equal to zero and less than the length
  of `xs`.  Otherwise, `splitAt()` returns `None`.
  
  ```reason
  splitAt(2, [|100, 101, 102, 103|]) == Some(([|100, 101|], [|102, 103|]))
  splitAt(0, [|100, 101, 102, 103|]) == Some(([| |], [|100, 101, 102, 103|]))
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

let rec prependToAll: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => concat([|delim, x|], prependToAll(delim, tailOrEmpty(xs)))
    };

let intersperse: ('a, array('a)) => array('a) =
  (delim, xs) =>
    switch (head(xs)) {
    | None => [||]
    | Some(x) => prepend(x, prependToAll(delim, tailOrEmpty(xs)))
    };

let replicate: (int, array('a)) => array('a) =
  (i, xs) =>
    foldLeft((acc, _i) => concat(acc, xs), [||], Relude_Int.rangeAsArray(0, i));

let zip: (array('a), array('b)) => array(('a, 'b)) = Belt.Array.zip;

let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => Belt.Array.zipBy(xs, ys, f);

let zipWithIndex: array('a) => array(('a, int)) =
  xs => zip(xs, Relude_Int.rangeAsArray(0, length(xs)));

let unzip: array(('a, 'b)) => (array('a), array('b)) = Belt.Array.unzip;

let sortWithInt: (('a, 'a) => int, array('a)) => array('a) =
  (f, xs) => Belt.SortArray.stableSortBy(xs, f);

let sort: (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let shuffleInPlace: array('a) => array('a) =
  xs => {
    Belt.Array.shuffleInPlace(xs);
    xs;
  };

let reverse: array('a) => array('a) = Belt.Array.reverse;

let any: ('a => bool, array('a)) => bool =
  (f, xs) => Belt.Array.some(xs, f);

let contains: (('a, 'a) => bool, 'a, array('a)) => bool =
  (f, x, xs) => any(f(x), xs);

let indexOf: (('a, 'a) => bool, 'a, array('a)) => option(int) =
  (f, x, xs) => {
    let rec go = (f, ys, i) =>
      switch (head(ys)) {
      | None => None
      | Some(z) when f(x, z) => Some(i)
      | _ => go(f, tailOrEmpty(ys), i + 1)
      };
    go(f, xs, 0);
  };

let all: ('a => bool, array('a)) => bool =
  (f, xs) => Belt.Array.every(xs, f);

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */

let distinct: (('a, 'a) => bool, array('a)) => array('a) =
  (eq, xs) =>
    foldLeft(
      /* foldRight would probably be faster with cons, but you lose the original ordering on the list */
      (acc, curr) =>
        if (contains(eq, curr, acc)) {
          acc;
        } else {
          append(curr, acc);
        },
      [||],
      xs,
    );

let map: 'a 'b. ('a => 'b, array('a)) => array('b) = BsAbstract.Array.Functor.map;

let mapWithIndex: 'a 'b. (('a, int) => 'b, array('a)) => array('b) =
  (f, xs) => Belt.Array.mapWithIndex(xs, (i, x) => f(x, i));

let forEach: 'a. ('a => unit, array('a)) => unit =
  (f, xs) => Belt.Array.forEach(xs, f);

let forEachWithIndex: 'a. (('a, int) => unit, array('a)) => unit =
  (f, xs) => Belt.Array.forEachWithIndex(xs, (i, x) => f(x, i));

let apply: (array('a => 'b), array('a)) => array('b) = BsAbstract.Array.Apply.apply;

let bind: (array('a), 'a => array('b)) => array('b) = BsAbstract.Array.Monad.flat_map;

let flatMap: ('a => array('b), array('a)) => array('b) = (f, fa) => bind(fa, f);

let flatten: array(array('a)) => array('a) = Belt.Array.concatMany;

let fromList: list('a) => array('a) = Belt.List.toArray;

let toList: array('a) => list('a) = Belt.List.fromArray;

let mkString: (string, array(string)) => string =
  (delim, xs) => {
    let delimited = intersperse(delim, xs);
    foldLeft((acc, curr) => acc ++ curr, "", delimited);
  };

module Eq = BsAbstract.Array.Eq;

let rec eqBy: (('a, 'a) => bool, array('a), array('a)) => bool =
  (innerEq, xs, ys) =>
    switch (head(xs), head(ys)) {
    | (None, None) => true
    | (Some(x), Some(y)) when innerEq(x, y) =>
      eqBy(innerEq, tailOrEmpty(xs), tailOrEmpty(ys))
    | _ => false
    };

let eq =
    (
      type a,
      eqA: (module BsAbstract.Interface.EQ with type t = a),
      xs: array(a),
      ys: array(a),
    )
    : bool => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

module Show = BsAbstract.Array.Show;

let showBy: ('a => string, array('a)) => string =
  (showX, xs) => {
    let strings = map(showX, xs);
    "[" ++ mkString(", ", strings) ++ "]";
  };

let show =
    (
      type a,
      showA: (module BsAbstract.Interface.SHOW with type t = a),
      xs: array(a),
    )
    : string => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = concat;
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = empty;
};

module Functor = BsAbstract.Array.Functor;

module Apply = BsAbstract.Array.Apply;

module Applicative = BsAbstract.Array.Applicative;

module Monad = BsAbstract.Array.Monad;

module Alt = BsAbstract.Array.Alt;

module Plus = BsAbstract.Array.Plus;

module Alternative = BsAbstract.Array.Alternative;

module Foldable = BsAbstract.Array.Foldable;

module Traversable = BsAbstract.Array.Traversable;

module Ord = BsAbstract.Array.Ord;

module Invariant = BsAbstract.Array.Invariant;

module MonadZero = BsAbstract.Array.Monad_Zero;

module MonadPlus = BsAbstract.Array.Monad_Plus;

module Extend = BsAbstract.Array.Extend;

module Infix = BsAbstract.Array.Infix;

module Sequence: Relude_Sequence.SEQUENCE with type t('a) = array('a) = {
  type t('a) = array('a);

  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
  let eqBy = eqBy;
  let showBy = showBy;
  let mkString = mkString;

  module SemigroupAny = SemigroupAny;
  module MonoidAny = MonoidAny;
  module Functor = Functor;
  module Apply = Apply;
  module Applicative = Applicative;
  module Monad = Monad;
  module Foldable = Foldable;
  module Traversable = Traversable;
  module Eq = Eq;
  module Show = Show;
};

module IsoList: Relude_IsoList.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = fromList;
  let toList = toList;
};
