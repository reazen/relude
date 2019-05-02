module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = array('a);
module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a);
module MonoidAny:
  BsAbstract.Interface.MONOID_ANY with type t('a) = array('a);
module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = array('a);
module Apply: BsAbstract.Interface.APPLY with type t('a) = array('a);
module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = array('a);
module Monad: BsAbstract.Interface.MONAD with type t('a) = array('a);
module Alt: BsAbstract.Interface.ALT with type t('a) = array('a);
module Plus: BsAbstract.Interface.PLUS with type t('a) = array('a);
module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = array('a);
module Invariant: BsAbstract.Interface.INVARIANT with type t('a) = array('a);
module MonadZero:
  BsAbstract.Interface.MONAD_ZERO with type t('a) = array('a);
module MonadPlus:
  BsAbstract.Interface.MONAD_PLUS with type t('a) = array('a);
module Extend: BsAbstract.Interface.EXTEND with type t('a) = array('a);
module IsoList: Relude_IsoList.ISO_LIST with type t('a) = array('a);

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
let concat: (array('a), array('a)) => array('a);

/**
  `empty` is a new, empty array.
*/
let empty: array('a);

let map: ('a => 'b, array('a)) => array('b);
let void: array('a) => array(unit);
let apply: (array('a => 'b), array('a)) => array('b);
let flap: (array('a => 'b), 'a) => array('b);
let map2: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let map3:
  (('a, 'b, 'c) => 'd, array('a), array('b), array('c)) => array('d);
let map4:
  (('a, 'b, 'c, 'd) => 'e, array('a), array('b), array('c), array('d)) =>
  array('e);
let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    array('a),
    array('b),
    array('c),
    array('d),
    array('e)
  ) =>
  array('f);

/**
  `pure(item)` returns an array containing the given item.

  ## Example
  ```re
  pure("single") == [|"single"|];
  ```
*/
let pure: 'a => array('a);

/**
  In `bind(xs, f)`, `f` is a function that takes an element
  of the type in `xs` and returns an array. The result of `bind`
  is the concatenation of all the arrays produced by applying
  `f()` to the elements of `xs`.
  
  ### Example
  ```re
  let f = (x) => [| x - 5, x + 5 |];
  bind([|100, 101, 102|], f) == [|95, 105, 96, 106, 97, 107|];
  bind([| |], f) == [| |];
  ```
*/
let bind: (array('a), 'a => array('b)) => array('b);

/**
  In `flatMap(f, xs)`, `f` is a function that takes an element
  of the type in `xs` and returns an array. The result of `bind`
  is the concatenation of all the arrays produced by applying
  `f()` to the elements of `xs`.
  
  ### Example
  ```re
  let f = (x) => [| x - 5, x + 5 |];
  flatMap(f, [|100, 101, 102|]) == [|95, 105, 96, 106, 97, 107|];
  flatMap(f, [| |]) == [| |];
  ```
*/
let flatMap: ('a => array('b), array('a)) => array('b);

/**
  `flatten(xs_of_xs)` takes an array of arrays as its argument
  and returns an array with all the sub-arrays concatenated.
  
  ### Example
  ```re
  flatten([| [|"a", "b"|], [| |], [|"c"|], [|"d", "e", "f"|] |]) ==
    [|"a", "b", "c", "d", "e", "f"|];
  ```
*/
let flatten: array(array('a)) => array('a);

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
let foldLeft: (('b, 'a) => 'b, 'b, array('a)) => 'b;

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
let foldRight: (('a, 'b) => 'b, 'b, array('a)) => 'b;

/**
  In `any(pred, xs)`, `pred` is a function that takes an item of the type in the
  array and returns a boolean value.  The `any()` function returns `true` if
  `pred(x)` returns true for any item `x` in the array, `false` otherwise.
  
  ### Example
  ```re
  any( (x) => {x < 0}, [|100, -101, 102|]) == true;
  any( (x) => {x < 0}, [|100, 101, 102|]) == false;
  ```
*/
let any: ('a => bool, array('a)) => bool;

/**
  In `all(pred, xs)`, `pred` is a function that takes an item of the type in the
  array and returns a boolean value.  The `all()` function returns `true` if
  `pred(x)` returns true for every item `x` in the array, `false` otherwise.
  
  ### Example
  ```re
  all( (x) => {x < 0}, [|-100, -101, -102|]) == true;
  all( (x) => {x < 0}, [|-100, 101, -102|]) == false;
  ```
*/
let all: ('a => bool, array('a)) => bool;

/**
  In `containsBy(f, value, xs)`, the function `f` takes two items
  of the type in the array and returns a predicate function `p()`
  by calling `f(value)`.
  
  `containsBy() returns `true` if any item in `xs` satisfies this
  new predicate function `p()`.
  
  ### Example
  ```re
  let aboveLimit = (limit, x) => { x > limit };
  containsBy(aboveLimit, 50, [|30, 70, 20|]) == true;
  containsBy(aboveLimit, 90, [|30, 70, 20|]) == false;
  ```
*/
let containsBy: (('a, 'a) => bool, 'a, array('a)) => bool;

/**
  In `indexOfBy(f, value, xs)`, the function `f` takes two items
  of the type in the array and returns a predicate function `p()`
  by calling `f(value)`.
  
  `indexOfBy() returns `Some(position)` where `position` is the index
  of the first item in `xs` that satisfies this new predicate function `p()`,
  or `None` if no item in `xs` satisfies the predicate..
  
  ### Example
  ```re
  let aboveLimit = (limit, x) => { x > limit };
  indexOfBy(aboveLimit, 50, [|30, 70, 20, 80|]) == Some(1);
  indexOfBy(aboveLimit, 90, [|30, 70, 20, 80|]) == None;
  ```
*/
let indexOfBy: (('a, 'a) => bool, 'a, array('a)) => option(int);

let minBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => option('a);
let maxBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => option('a);
let countBy: ('a => bool, array('a)) => int;

/**
  `length(xs)` returns the number of items in `xs`.

  ## Example
  ```re
  length([|"a", "b", "c"|]) == 3;
  length([| |]) == 0;
  ```
*/
let length: array('a) => int;

let forEach: ('a => unit, array('a)) => unit;
let forEachWithIndex: (('a, int) => unit, array('a)) => unit;
let find: ('a => bool, array('a)) => option('a);
let findWithIndex: (('a, int) => bool, array('a)) => option('a);

let fold:
  ((module BsAbstract.Interface.MONOID with type t = 'a), array('a)) => 'a;
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a, array('a)) => 'a;
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) => bool;
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  option(int);
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  option('a);
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  option('a);

let fromList: list('a) => array('a);
let toList: array('a) => list('a);

module Traversable:
  (BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE;

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
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b);

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
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b);

let eqBy: (('a, 'a) => bool, array('a), array('a)) => bool;
module Eq: (BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;
let eq:
  (
    (module BsAbstract.Interface.EQ with type t = 'a),
    array('a),
    array('a)
  ) =>
  bool;

let showBy: ('a => string, array('a)) => string;
module Show: (BsAbstract.Interface.SHOW) => BsAbstract.Interface.SHOW;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), array('a)) => string;

module Ord: (BsAbstract.Interface.ORD) => BsAbstract.Interface.ORD;

let mapWithIndex: (('a, int) => 'b, array('a)) => array('b);

/**
  `cons(x, xs)` returns a new array with value `x` at the beginning.

  ## Example
  ```re
  cons(99, [|100, 101|]) == [|99, 100, 101|];
  cons(99, [| |]) == [|99|];
  ```
*/
let cons: ('a, array('a)) => array('a);

/**
  Same as `cons`
*/
let prepend: ('a, array('a)) => array('a);

/**
  When given a non-emtpy array, `uncons(xs)` returns `Some((y, ys))`
  where `y` is the first element in the array and `ys` are the remaining
  elements. If given an empty array, `uncons()` returns `None`.

  ## Example
  ```re
  uncons([|100, 101, 102|]) == Some((100, [|101, 102|]));
  uncons([|100|]) == Some((100, [| |]));
  uncons([| |]) == None;
  ```
*/
let uncons: array('a) => option(('a, array('a)));

/**
  `append(x, xs)` adds the value `x` at the end of array `xs`.

  ## Example
  ```re
  append(999, [|100, 101, 102|]) == [|100, 101, 102, 999|];
  append(999, [| |]) == [|999|];
  ```
*/
let append: ('a, array('a)) => array('a);

/**
  `repeat(n, x)` returns an array containing `n` copies of `x`.

  ## Example
  ```re
  repeat(3, "ha") == [|"ha", "ha", "ha"|];
  repeat(0, "nothing") == [| |];
  repeat(-2, "nothing") == [| |];
  ```
*/
let repeat: (int, 'a) => array('a);

/**
  `makeWithIndex(n, f)` returns the array `[|f(0), f(1), ... f(n - 1)|]`.

  ## Example
  ```re
  makeWithIndex(3, (x) => {(x + 4) * (x + 4)}) == [|16, 25, 36|];
  makeWithIndex(0, (x) => {x + 1}) == [| |];
  makeWithIndex(-1, (x) => {x + 1}) == [| |];
  ```
*/
let makeWithIndex: (int, int => 'a) => array('a);

/**
  `reverse(xs)` returns an array with elements in the reverse order
  from the original array.
  
  ### Example
  ```re
  reverse([|100, 101, 102|]) == [|102, 101, 100|];
  ```
*/
let reverse: array('a) => array('a);

/**
  `shuffleInPlace(xs)` will shuffle the array `xs`, returning the randomly ordered
  array. The original array *will* be changed as well.

  ### Example
  ```re
  let data = [|100, 101, 102, 103, 104|];
  let mixed = shuffleInPlace(data);
  mixed == data;
  ```
*/
let shuffleInPlace: array('a) => array('a);

/**
  `shuffle(xs)` will return a new array with the same elements as
  `xs` but in a random order. The original array is not changed.

  ### Example
  ```re
  let data = [|100, 101, 102, 103, 104|];
  let mixed = shuffle(data);
  mixed != data;
  ```
*/
let shuffle: array('a) => array('a);

/**
  `isEmpty(xs) returns `true` if `xs` is the empty array `[| |]`; returns `false` otherwise.
*/
let isEmpty: array('a) => bool;

/**
  `isNotEmpty(xs) returns `true` if `xs` is not the empty array `[| |]`; returns `false` otherwise.
*/
let isNotEmpty: array('a) => bool;

/**
  `at(n, xs)` returns the value at the given index position as `Some(value)`
  unless `n` is less than zero or greater than the length of `xs`, in which
  case `at()` returns `None.`
  
  ## Example
  ```re
  at(0, [|100, 101, 102|]) == Some(100);
  at(2, [|100, 101, 102|]) == Some(102);
  at(-1, [|100, 101, 102|]) == None;
  at(3, [|100, 101, 102|]) == None;
  ```
*/
let at: (int, array('a)) => option('a);

/**
  `setAt(n, value, xs)` updates the given array with item `n` setAt to `value` and
  returns `true` when `n` is greater than or equal to zero and less than
  the length of `xs`. If `n` is not a valid index, the array remains unchanged
  and the function returns `false`.
  
  In the following example, the statements are performed in order.
  
  ## Example
  let arr = [|100, 101, 102|];
  setAt(1, 999, arr) == true && arr == [|100, 999, 102|];
  setAt(-1, 888, arr) == false && arr == [|100, 999, 102|];
  setAt(3, 777, arr) == false && arr == [|100, 999, 102|];
  ```
*/
let setAt: (int, 'a, array('a)) => option(array('a));

/**
  For non-empty arrays, `head(xs)` returns the first item in the array
  as `Some(value)`. For an empty array, the function returns `None`.
  
  ## Example
  ```re
  head([|100, 101, 102|]) == Some(100);
  head([| |]) == None;
  ```
*/
let head: array('a) => option('a);

/**
  For non-empty arrays, `tail(xs)` returns an array consisting of all
  but the first item in `xs` as `Some(ys)`. For an empty array, the
  function returns `None`.
  
  ## Example
  ```re
  tail([|100, 101, 102|]) == Some([|101, 102|]);
  tail([| |]) == None;
  ```
*/
let tail: array('a) => option(array('a));

/**
  For non-empty arrays, `tailOrEmpty(xs)` returns an array consisting of all
  but the first item in `xs`. For an empty array, the function returns
  an empty array.
  
  ## Example
  ```re
  tailOrEmpty([|100, 101, 102|]) == [|101, 102|];
  tailOrEmpty([| |]) == [| |];
  ```
*/
let tailOrEmpty: array('a) => array('a);

/**
  For non-empty arrays, `init(xs)` returns an array containing
  all but the last item in `xs` as `Some(ys)`. The function returns
  `None` if given an empty array.
  
  ## Example
  ```re
  init([|100, 101, 102|]) == Some([|100, 101|]);
  init([| |]) == None;
  ```
*/
let init: array('a) => option(array('a));

/**
  For non-empty arrays, `last(xs)` returns the last
  item in `xs` as `Some(value)`. The function returns
  `None` if given an empty array.
  
  ## Example
  ```re
  last([|100, 101, 102|]) == Some(102);
  last([| |]) == None;
  ```
*/
let last: array('a) => option('a);

/**
  `take(n, xs)` returns an array of the first `n` items in `xs` as
  `Some(ys)`, `n` is pinned to the range 0..`n` - 1.
  
  ## Example
  ```re
  take(2, [|100, 101, 102, 103|]) == [|100, 101|];
  take(0, [|100, 101, 102|]) == [| |];
  take(-1, [|100, 101, 102|]) == [| |];
  take(4, [|100, 101, 102|]) == [|100, 101, 102|];
  take(1, [| |]) == [| |];
  ```
*/
let take: (int, array('a)) => array('a);

/**
  `takeExactly(n, xs)` returns an array of the first `n` items in `xs` as
  `Some(ys)`, If `n` is less than or equal to zero, `takeExactly()` returns `Some([| |])`.
  If `n` is greater than or equal to the length  of `xs`, `takeExactly()` returns `None`.
  
  ## Example
  ```re
  takeExactly(2, [|100, 101, 102, 103|]) == Some([|100, 101|]);
  takeExactly(0, [|100, 101, 102|]) == Some([| |]);
  takeExactly(-1, [|100, 101, 102|]) == None;
  takeExactly(4, [|100, 101, 102|]) == None;
  takeExactly(1, [| |]) == None;
  ```
*/
let takeExactly: (int, array('a)) => option(array('a));

/**
  `takeWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `takeWhile()` returns an array consisting
  of the first elements of `xs` which satisfy the predicate. (This
  could be an empty array.)
  
  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  takeWhile(even, [|2, 6, 5, 3, 8|]) == [|2, 6|];
  takeWhile(even, [|5, 3, 8, 2, 6|]) == [| |];
  takeWhile(even, [| |]) == [| |];
  ```
*/
let takeWhile: ('a => bool, array('a)) => array('a);

/**
  `drop(n, xs)` returns an array of all *except* the first `n` items in `xs` as
  `Some(ys)`. `n` is pinned to the range 0..`n` - 1.
  
  ## Example
  ```re
  drop(2, [|100, 101, 102, 103|]) == [|102, 103|];
  drop(0, [|100, 101, 102|]) == [|100, 101, 102|];
  drop(4, [|100, 101, 102|]) == [| |];
  drop(-1, [|100, 101, 102|]) == [|100, 101, 102|];
  drop(1, [| |]) == [| |];
  ```
*/
let drop: (int, array('a)) => array('a);

/**
  `dropExactly(n, xs)` returns an array of all *except* the first `n` items
  in `xs` as `Some(ys)`. 
  
  If `n` is less than zero or greater than or equal to the length  of `xs`,
  `dropExactly()` returns `None`.
  
  ## Example
  ```re
  dropExactly(2, [|100, 101, 102, 103|]) == Some([|102, 103|]);
  dropExactly(0, [|100, 101, 102|]) == Some([|100, 101, 102|]);
  dropExactly(4, [|100, 101, 102|]) == None;
  dropExactly(-1, [|100, 101, 102|]) == None;
  dropExactly(1, [| |]) == None;
  ```
*/
let dropExactly: (int, array('a)) => option(array('a));

/**
  `dropWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `dropWhile()` returns an array consisting
  all *except* the first elements of `xs` which satisfy the predicate.
  (This could be an empty array.)
  
  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  dropWhile(even, [|2, 6, 5, 3, 8|]) == [|5, 3, 8|];
  dropWhile(even, [|5, 3, 8|]) == [|5, 3, 8|];
  dropWhile(even, [|2, 4, 6|]) == [| |];
  dropWhile(even, [| |]) == [| |];
  ```
*/
let dropWhile: ('a => bool, array('a)) => array('a);


/**
  `filter(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `filter()` returns an array of all
  the elements of `xs` that satisfy the predicate.
  
  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  filter(even, [|2, 6, 5, 3, 4, 1|]) == [|2, 6, 4|];
  filter(even, [|5, 3, 1|]) == [| |];
  filter(even, [| |]) == [| |];
  ```
*/
let filter: ('a => bool, array('a)) => array('a);

/**
  `filterWithIndex(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameters an element of `xs` and
  the element’s index value, returning a boolean value.
  `filterWithIndex()` returns an array of all
  the elements of `xs` that satisfy the predicate.
  
  ## Example
  ```re
  let bothEven = (x, i) => {x mod 2 == 0 && i mod 2 == 0};
  filterWithIndex(bothEven, [|2, 4, 3, 1, 6, 5|]) == [|2, 6|];
  filterWithIndex(bothEven, [|1, 4, 3, 6, 5, 2|]) == [| |];
  ```
*/
let filterWithIndex: (('a, int) => bool, array('a)) => array('a);

/**
  `partition(f, xs)` takes as its first parameter a predicate function.
  The predicate takes as its parameter an element of `xs` and returns a boolean.
  
  `partition()` returns a tuple of two arrays: the elements in
  `xs` that satisfy the predicate, and the elements that don’t.
  
  ## Example
  
  ```re
  let even = (x) => {x mod 2 == 0};
  partition(even, [|6, 1, 3, 2, 4, 5|]) == ([|6, 2, 4|], [|1, 3, 5|]);
  partition(even, [|6, 2, 4|]) == ([|6, 2, 4|], [| |]);
  partition(even, [|1, 3, 5|]) == ([| |], [|1, 3, 5|]);
  partition(even, [| |]) == ([| |], [| |]);
  ```
*/
let partition: ('a => bool, array('a)) => (array('a), array('a));

/**
  `splitAt(n, xs)` returns `Some(ys, zs)` where `ys` contains the
  first `n` elements of `xs` and `zs` contains the remaining elements,
  when `n` is greater than or equal to zero and less than or equal
  to the length of `xs`.  Otherwise, `splitAt()` returns `None`.
  
  ## Example
  ```re
  splitAt(2, [|100, 101, 102, 103|]) == Some(([|100, 101|], [|102, 103|]));
  splitAt(0, [|100, 101, 102, 103|]) == Some(([| |], [|100, 101, 102, 103|]));
  splitAt(4, [|100, 101, 102, 103|]) == Some(([|100, 101, 102, 103|], [| |]));
  splitAt(-1, [|100, 101, 102, 103|]) == None;
  splitAt(5, [|100, 101, 102, 103|]) == None;
  ```
*/
let splitAt: (int, array('a)) => option((array('a), array('a)));

/**
  `prependToAll(delim, xs)` returns a new array with `delim` inserted
  before every current element of `xs`.
  
  ## Example
  ```re
  prependToAll(999, [|100, 101, 102|]) == [|999, 100, 999, 101, 999, 102|];
  prependToAll(999, [| |]) == [| |];
  ```
*/
let prependToAll: ('a, array('a)) => array('a);

/**
  `intersperse(delim, xs)` returns a new array with `delim` inserted
  between all the current elements of `xs`.
  
  ## Example
  ```re
  intersperse(999, [|100, 101, 102|]) == [|100, 999, 101, 999, 102|];
  intersperse(999, [| |]) == [| |];
  ```
*/
let intersperse: ('a, array('a)) => array('a);

/**
  `replicate(n, xs)` returns an array with `n` repetitions of `xs`,
  one after another. If `n` is less than or equal to zero, returns the
  empty array.
  
  ## Example
  ```re
  replicate(3, [|1, 2|]) == [|1, 2, 1, 2, 1, 2|];
  replicate(0, [|1, 2|]) == [| |];
  replicate(-1, [|1, 2|]) == [| |];
  ```
*/
let replicate: (int, array('a)) => array('a);

/**
  `zip(xs, ys)` returns an array of arrays whose elements are the tuples
  `[| (x[0], y[0]), (x[1], y[1])... |]`. The process of combining
  elements stops when the shorter of the two arrays is finished.
  
  ### Example
  ```re
  zip([|1, 2, 3|], [|4.4, 5.5, 6.6|]) == [|(1, 4.4), (2, 5.5), (3, 6.6)|];
  zip([|1, 2, 3|], [|4.4, 5.5|]) == [|(1, 4.4), (2, 5.5)|];
  zip([|1, 2|], [|3.3, 4.4, 5.5|]) == [| (1, 3.3), (2, 4.4) |];
  ```
*/
let zip: (array('a), array('b)) => array(('a, 'b));

/**
  `zipWith(f, xs, ys)` returns an array that is the result of applying
  `f` to corresponding elements of `xs` and `ys`, stopping when it hits
  the end of the shorter array.
  
  ### Example
  ```re
  zipWith( (x, y) => { 2 * x + y }, [|1, 2, 3|], [|4, 5|]) == [|6, 9|];
  zipWith( (x, y) => { 2 * x + y }, [| |], [|4, 5|]) == [| |];
  ```
*/
let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c);

/**
  `zipWithIndex(xs)` produces an array of two-tuples where
  each tuple contains the item from the array and its index number,
  starting at zero.
  
  ### Example
  ```re
  zipWithIndex([|"a", "b", "c"|]) == [|("a", 0), ("b", 1), ("c", 2)|];
  ```
*/
let zipWithIndex: array('a) => array(('a, int));

/**
  `unzip(xs)` takes an array of pairs and creates a pair of arrays.
  The first array contains all the first items of the pairs, and the second
  array contains all the second items.
  
  ### Example
  ```re
  unzip([|("a", 0), ("b", 1), ("c", 2)|]) == ([|"a", "b", "c"|], [|0, 1, 2|]);
  ```
*/
let unzip: array(('a, 'b)) => (array('a), array('b));

/**
  `sortWithInt(f, xs)` sorts the array `xs`, calling `f` every time it needs to
  compare two array elements `a` and `b`.  If `f(a, b)` is negative, then `a`
  precedes `b` in sorting order. If `f(a, b)` is positive, then `a` follows `b`
  in sorting order. If `f(a, b) is zero, then `a` and `b` are considered equal.
  
  This is a stable sort; equal elements will appear in the output array in the
  same order that they appeared in the input array.
  
  ### Example
  ```re
  let cmpMod12 = (a, b) => {(a mod 12) - (b mod 12)};
  sortWithInt(cmpMod12, [|17, 3, 9, 4, 15, 20|]) == [|3, 15, 4, 17, 20, 9|];
  ```
*/
let sortWithInt: (('a, 'a) => int, array('a)) => array('a);

/**
  `sortBy(f, xs)` sorts the array `xs`, calling `f` every time it needs to
  compare two array elements `a` and `b`.  If `f(a, b)` returns
  ` `less_than `, ` `equal_to `, or ` `greater_than ` depending on the
  relationship between `a` and `b`. (These are values defined in `bs-abstract`.)
  
  This is a stable sort; equal elements will appear in the output array in the
  same order that they appeared in the input array.
  
  ### Example
  ```re
  let cmpMod12 = (a: int, b: int): BsAbstract.Interface.ordering => {
    if (a mod 12 < b mod 12) {
      `less_than;
    } else if (a mod 12 > b mod 12) {
      `greater_than;
    } else {
      `equal_to;
    }
  };
  sortBy(cmpMod12, [|17, 3, 9, 4, 15, 20|]) == [|3, 15, 4, 17, 20, 9|];
  ```
*/
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a);

/**
  `sort((module M), xs)` sorts the array `xs`, using the given module to
  compare items in the array.  The module must:
  
  - Specify a type `t` for the items to be compared
  - Specify a function `eq`, which takes two items of type `t` and returns
    `true` if they are considered equal, `false` otherwise.
  - Specify a function `compare`, which takes two items of type `t` and
    returns ` `less_than `, ` `equal_to `, or ` `greater_than `, depending on the
    relation between the two items.
  
  This is a stable sort; equal elements will appear in the output array in the
  same order that they appeared in the input array.
  
  ### Example
  ```re
  module ClockArithmetic = {
    type t = int;
    let eq = (a, b) => { a mod 12 == b mod 12 };
    let compare = (a, b) => {
      if (a mod 12 < b mod 12) {
        `less_than
      } else if (a mod 12 > b mod 12) {
        `greater_than
      } else {
        `equal_to
      };
    }
  };
  
  sort((module ClockArithmetic), [|17, 3, 9, 4, 15, 20|]) == [|3, 15, 4, 17, 20, 9|];
  ```
*/
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  array('a);

/**
 In `distinctBy(f, xs)`, the function `f` compares two items
 of the type in the array `xs` and returns `true` if the two
 items are considered to be equal, `false` otherwise.
 
 `distinctBy()` returns all the items which have unique
 values with respect to `f()`.
 
  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  distinctBy(eqMod12, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 2, 12, 9|];
  ```
*/
let distinctBy: (('a, 'a) => bool, array('a)) => array('a);

/**
  In `removeFirstBy(f, value, xs)`, the function `f` compares two items
  of the type in the array `xs` and returns `true` if the two
  items are considered to be equal, `false` otherwise.
 
  `removeFirstBy()` returns an array with the first element that
  is considered equal to `value` with respect to `f()` removed. 
  
  If no such elements exist, the result is the same as the original array.
 
  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  removeFirstBy(eqMod12, 14, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 12, 9, 21, 0|];
  removeFirstBy(eqMod12, 15, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 12, 9, 21, 0|];
  ```
*/
let removeFirstBy: (('a, 'a) => bool, 'a, array('a)) => array('a);

/**
  In `removeEachBy(f, value, xs)`, the function `f` compares two items
  of the type in the array `xs` and returns `true` if the two
  items are considered to be equal, `false` otherwise.
 
  `removeEachBy()` returns an array with every element that
  is considered equal to `value` with respect to `f()` removed. 
  
  If no such elements exist, the result is the same as the original array.
 
  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  removeEachBy(eqMod12, 12, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 9, 21|];
  removeEachBy(eqMod12, 15, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 12, 9, 21, 0|];
  ```
*/
let removeEachBy: (('a, 'a) => bool, 'a, array('a)) => array('a);

/*
  `distinct((module M), xs)` returns an array of the unique elements `xs`,
  using the given module to determine which elements are considered to be equal.
  The module must:
  
  - Specify a type `t` for the items to be compared
  - Specify a function `eq`, which takes two items of type `t` and returns
    `true` if they are considered equal, `false` otherwise.
    
  ### Example
  ```re
  module ClockEqual = {
    type t = int;
    let eq = (a, b) => { a mod 12 == b mod 12 };
  };

  distinct((module ClockEqual), [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 2, 12, 9|];
  ```
*/    
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a)) => array('a);

/**
  `removeFirst((module M), value, xs)` returns an array with the first element that
  is considered equal to `value` with respect to `module M` removed.
 
  The module must:
  
  - Specify a type `t` for the items to be compared
  - Specify a function `eq`, which takes two items of type `t` and returns
    `true` if they are considered equal, `false` otherwise.
  
  If no elements are equal to the `value`, the result is the same as the original array.
 
  ### Example
  ```re
  module ClockEq2 = {
    type t = int;
    let eq = (a, b) => { a mod 12 == b mod 12 };
  };
  removeFirst((module ClockEq2), 14, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 12, 9, 21, 0|];
  removeFirst((module ClockEq2), 15, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 12, 9, 21, 0|];
  ```
*/
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);

/**
  `removeEach((module M), value, xs)` returns an array with every element that
  is considered equal to `value` with respect to `module M` removed.
 
  The module must:
  
  - Specify a type `t` for the items to be compared
  - Specify a function `eq`, which takes two items of type `t` and returns
    `true` if they are considered equal, `false` otherwise.
  
  If no elements are equal to the `value`, the result is the same as the original array.
 
  ### Example
  ```re
  module ClockEq3 = {
    type t = int;
    let eq = (a, b) => { a mod 12 == b mod 12 };
  };
  removeEach((module ClockEq3), 12, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 9, 21|];
  removeEach((module ClockEq3), 15, [|16, 4, 2, 12, 9, 21, 0|]) == [|16, 4, 2, 12, 9, 21, 0|];
  ```
*/
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);

/**
  This submodule contains operations which have been optimized
  to work on arrays of strings.
*/
module String: {

  /**
    `String.contains(value, xs)` returns `true` if `value` is one of the
    elements of `xs`, `false` otherwise.
    
    ### Example
    ```re
    String.contains("bee", [|"ant", "bee", "cat"|]) == true;
    String.contains("elk", [|"ant", "bee", "cat"|]) == false;
    ```
  */
  let contains: (string, array(string)) => bool;
  
  /**
    `String.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.
    
    ### Example
    ```re
    String.indexOf("bee", [|"ant", "bee", "cat"|]) == Some(1);
    String.indexOf("elk", [|"ant", "bee", "cat"|]) == None;
    ```
  */
  let indexOf: (string, array(string)) => option(int);
  
  /**
    `String.distinct(xs)` returns an array containing the unique
    elements of `xs` in the same order that they occurred in that array.
    
    ### Example
    ```re
    String.distinct([|"noun", "verb", "noun", "adjective", "verb"|]) == [|"noun", "verb", "adjective"|];
    ```
  */
  let distinct: array(string) => array(string);
  
  /**
    `String.removeFirst(value, xs)` returns an array where the first occurrence
    (if any) of `value` has been removed from `xs`.
    
    ### Example
    ```re
    String.removeFirst("x", [|"a", "x", "b", "x"|]) == [|"a", "b", "x"|];
    String.removeFirst("?", [|"a", "x", "b", "x"|]) == [|"a", "x", "b", "x"|];
    ```
  */
  let removeFirst: (string, array(string)) => array(string);

  /**
    `String.removeEach(value, xs)` returns an array where every occurrence
    of `value` has been removed from `xs`.
    
    ### Example
    ```re
    String.removeEach("x", [|"a", "x", "b", "x"|]) == [|"a", "b"|];
    String.removeEach("?", [|"a", "x", "b", "x"|]) == [|"a", "x", "b", "x"|];
    ```
  */
  let removeEach: (string, array(string)) => array(string);
  
  /**
    `String.eq(xs, ys)` returns `true` if the two arrays are element-for-element
    equal, `false` otherwise.
    
    ### Example
    ```re
    String.eq([|"a", "b", "c"|], [|"a", "b", "c"|]) == true;
    String.eq([|"a", "b", "c"|], [|"a", "b"|]) == false;
    String.eq([|"a", "b", "c"|], [|"a", "b", "d"|]) == false;
    String.eq([|"a", "b", "c"|], [|"A", "B", "C"|]) == false;
    ```
  */
  let eq: (array(string), array(string)) => bool;
  
  /**
    `String.min(xs)` returns the element with the minimum lexicographic
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    String.min([|"z", "A", "a"|]) == Some("A");
    String.min([|"bee", "bed", "bet", "beg"|]) == Some("bed");
    String.min([| |]) == None;
    ```
  */
  let min: array(string) => option(string);

  /**
    `String.max(xs)` returns the element with the maximum lexicographic
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    String.max([|"z", "A", "a"|]) == Some("z");
    String.max([|"bee", "bed", "bet", "beg"|]) == Some("bet");
    String.max([| |]) == None;
    ```
  */
  let max: array(string) => option(string);
  
  /**
    `String.sort(xs)` sorts the elements of `xs` into lexicographic order.
    
    ### Example
    ```re
    String.sort([|"z", "A", "a"|]) == [|"A", "a", "z"|];
    String.sort([|"bee", "bed", "bet", "beg"|]) == [|"bed", "bee", "beg", "bet"|];
    String.sort([| |]) == [| |];
    ```
  */
  let sort: array(string) => array(string);
  
  /**
    `String.fold(xs)` concatenates the elements of `xs` into a single string.
    Same as `String.join().`
    
    ### Example
    ```re
    String.fold([|"a", "b", "c"|]) == "abc";
    String.fold([| |]) == "";
    ```
  */
  let fold: array(string) => string;
  
  /**
    `String.join(xs)` concatenates the elements of `xs` into a single string.
    Same as `String.fold().`
    
    ### Example
    ```re
    String.join([|"a", "b", "c"|]) == "abc";
    String.join([| |]) == "";
    ```
  */
  let join: array(string) => string;
  
  /**
    `String.intercalate(delim, xs)` returns a concatenated string with
    `delim` inserted  between the elements of `xs`. Same as `String.joinWith().`
    
    ### Example
    ```re
    String.intercalate("-", [|"year", "month", "day"|]) == "year-month-day";
    String.intercalate(" and ", [|"bread", "wine", "thou"|]) == "bread and wine and thou";
    ```
  */
  let intercalate: (string, array(string)) => string;
  
  /**
    `String.joinWith(delim, xs)` returns a concatenated string with
    `delim` inserted  between the elements of `xs`. Same as `String.intercalate().`
    
    ### Example
    ```re
    String.joinWith("-", [|"year", "month", "day"|]) == "year-month-day";
    String.joinWith(" and ", [|"bread", "wine", "thou"|]) == "bread and wine and thou";
    ```
  */
  let joinWith: (string, array(string)) => string;
};

/**
  This submodule contains operations which have been optimized
  to work on arrays of integers.
*/
module Int: {

  /**
    `Int.contains(value, xs)` returns `true` if `value` is one of the
    elements of `xs`, `false` otherwise.
    
    ### Example
    ```re
    Int.contains(101, [|100, 101, 102|]) == true;
    Int.contains(104, [|100, 101, 102|]) == false;
    ```
  */
  let contains: (int, array(int)) => bool;
  
  /**
    `Int.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.
    
    ### Example
    ```re
    Int.indexOf(101, [|100, 101, 102|]) == Some(1);
    Int.indexOf(104, [|100, 101, 102|]) == None;
    ```
  */

  let indexOf: (int, array(int)) => option(int);
  
  /**
    `Int.distinct(xs)` returns an array containing the unique
    elements of `xs` in the same order that they occurred in that array.
    
    ### Example
    ```re
    Int.distinct([|87, 99, 87, 65, 99|]) == [|87, 99, 65|];
    ```
  */
  let distinct: array(int) => array(int);

  /**
    `Int.removeFirst(value, xs)` returns an array where the first occurrence
    (if any) of `value` has been removed from `xs`.
    
    ### Example
    ```re
    Int.removeFirst(99, [|100, 99, 101, 99|]) == [|100, 101, 99|];
    Int.removeFirst(88, [|100, 99, 101, 99|]) == [|100, 99, 101, 99|];
    ```
  */
  let removeFirst: (int, array(int)) => array(int);

  /**
    `Int.removeEach(value, xs)` returns an array where every occurrence
    of `value` has been removed from `xs`.
    
    ### Example
    ```re
    Int.removeEach(99, [|100, 99, 101, 99|]) == [|100, 101|];
    Int.removeEach(88, [|100, 99, 101, 99|]) == [|100, 99, 101, 99|];
    ```
  */
  let removeEach: (int, array(int)) => array(int);
  
  /**
    `Int.eq(xs, ys)` returns `true` if the two arrays are element-for-element
    equal, `false` otherwise.
    
    ### Example
    ```re
    Int.eq([|100, 101, 102|], [|100, 101, 102|]) == true;
    Int.eq([|100, 101, 102|], [|100, 101|]) == false;
    Int.eq([|100, 101, 102|], [|100, 101, 99|]) == false;
    Int.eq([|100, 101, 102|], [|-100, -101, -102|]) == false;
    ```
  */
  let eq: (array(int), array(int)) => bool;

  /**
    `Int.min(xs)` returns the element with the minimum
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    Int.min([|77, -99, 88, 66|]) == Some(-99);
    Int.min([| |]) == None;
    ```
  */
  let min: array(int) => option(int);

  /**
    `Int.max(xs)` returns the element with the maximum
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    Int.max([|77, -99, 88, 66|]) == Some(88);
    Int.max([| |]) == None;
    ```
  */
  let max: array(int) => option(int);
  
  /**
    `Int.sort(xs)` sorts the elements of `xs` into ascending order.
    
    ### Example
    ```re
    Int.sort([|77, -99, 88, 66|]) == [|-99, 66, 77, 88|];
    Int.sort([| |]) == [| |];
    ```
  */
  let sort: array(int) => array(int);
  
  /**
    `Int.sum(xs)` returns the sum of the elements in `xs`.
    
    ### Example
    ```re
    Int.sum([|3, 7, 5|]) == 15;
    Int.sum([| |]) == 0;
    ```
  */
  let sum: array(int) => int;
  
  /**
    `Int.product(xs)` returns the product of the elements in `xs`.
    
    ### Example
    ```re
    Int.product([|3, 7, 5|]) == 105;
    Int.product([| |]) == 1;
    ```
  */  
  let product: array(int) => int;
};

/**
  This submodule contains operations which have been optimized
  to work on arrays of integers.
*/
module Float: {

  /**
    `Float.contains(value, xs)` returns `true` if `value` is one of the
    elements of `xs`, `false` otherwise.
    
    ### Example
    ```re
    Float.contains(101.1, [|100.0, 101.1, 102.2|]) == true;
    Float.contains(104.4, [|100.0, 101.1, 102.2|]) == false;
    ```
  */
  let contains: (float, array(float)) => bool;
  
  /**
    `Float.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.
    
    ### Example
    ```re
    Float.indexOf(101.1, [|100.0, 101.1, 102.2|]) == Some(1);
    Float.indexOf(104.4, [|100.0, 101.1, 102.2|]) == None;
    ```
  */

  let indexOf: (float, array(float)) => option(int);
  
  /**
    `Float.distinct(xs)` returns an array containing the unique
    elements of `xs` in the same order that they occurred in that array.
    
    ### Example
    ```re
    Float.distinct([|87.7, 99.9, 87.7, 65.5, 99.9|]) == [|87.7, 99.9, 65.5|];
    ```
  */
  let distinct: array(float) => array(float);

  /**
    `Float.removeFirst(value, xs)` returns an array where the first occurrence
    (if any) of `value` has been removed from `xs`.
    
    ### Example
    ```re
    Float.removeFirst(99.9, [|100.0, 99.9, 101.1, 99.9|]) == [|100.0, 101.1, 99.9|];
    Float.removeFirst(88.8, [|100.0, 99.9, 101.1, 99.9|]) == [|100.0, 99.9, 101.1, 99.9|];
    ```
  */
  let removeFirst: (float, array(float)) => array(float);

  /**
    `Float.removeEach(value, xs)` returns an array where every occurrence
    of `value` has been removed from `xs`.
    
    ### Example
    ```re
    Float.removeEach(99.9, [|100.0, 99.9, 101.1, 99.9|]) == [|100.0, 101.1|];
    Float.removeEach(88.8, [|100.0, 99.9, 101.1, 99.9|]) == [|100.0, 99.9, 101.1, 99.9|];
    ```
  */
  let removeEach: (float, array(float)) => array(float);
  
  /**
    `Float.eq(xs, ys)` returns `true` if the two arrays are element-for-element
    equal, `false` otherwise.
    
    ### Example
    ```re
    Float.eq([|100.0, 101.1, 102.2|], [|100.0, 101.1, 102.2|]) == true;
    Float.eq([|100.0, 101.1, 102.2|], [|100.0, 101.1|]) == false;
    Float.eq([|100.0, 101.1, 102.2|], [|100.0, 101.1, 99.9|]) == false;
    Float.eq([|100.0, 101.1, 102.2|], [|-100.0, -101.1, -102.2|]) == false;
    ```
  */
  let eq: (array(float), array(float)) => bool;

  /**
    `Float.min(xs)` returns the element with the minimum
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    Float.min([|77.7, -99.9, 88.8, 66.6|]) == Some(-99.9);
    Float.min([| |]) == None;
    ```
  */
  let min: array(float) => option(float);

  /**
    `Float.max(xs)` returns the element with the maximum
    value as `Some(value)`; returns `None` if given an empty array.
    
    ### Example
    ```re
    Float.max([|77.7, -99.9, 88.8, 66.6|]) == Some(88.8);
    Float.max([| |]) == None;
    ```
  */
  let max: array(float) => option(float);
  
  /**
    `Float.sort(xs)` sorts the elements of `xs` floato ascending order.
    
    ### Example
    ```re
    Float.sort([|77.7, -99.9, 88.8, 66.6|]) == [|-99.9, 66.6, 77.7, 88.8|];
    Float.sort([| |]) == [| |];
    ```
  */
  let sort: array(float) => array(float);
  
  /**
    `Float.sum(xs)` returns the sum of the elements in `xs`.
    
    ### Example
    ```re
    Float.sum([|3.0, 7.5, 5.5|]) == 16.0;
    Float.sum([| |]) == 0.0;
    ```
  */
  let sum: array(float) => float;
  
  /**
    `Float.product(xs)` returns the product of the elements in `xs`.
    
    ### Example
    ```re
    Float.product([|3.0, 7.5, 5.5|]) == 123.75;
    Float.product([| |]) == 1.0;
    ```
  */  
  let product: array(float) => float;
};

module Option: {
  /**
    `Option.traverse(f, xs)`, `f()` is a function that takes an item of the
    same type as `xs` and returns an `option` value.
    
    If `f(x)` returns `Some(value)` for all elements in `xs`, the result is `Some(xs)`.
    
    If `f(x)` returns `None` for any element in `xs`, the result is `None`.
    
    ### Example
    ```re
    let evenValue = (x) => {(x mod 2 == 0) ? Some(x) : None};
    Option.traverse(evenValue, [|100, 102, 104, 106|]) == Some([|100, 102, 104, 106|]);
    Option.traverse(evenValue, [|100, 101, 102, 103|]) == None;
    Option.traverse(evenValue, [| |]) == Some([| |]);
    ```
  */
  let traverse: ('a => option('a), array('a)) => option(array('a));

  /**
    `Option.sequence(opts)`, takes an array of `option` types as its argument.
    If all the elements of `opts` are `Some(value)`, then the return value is
    `Some(xs)`, where each element in `opts` has been unwrapped from its `Some()`.
    
    If any element of `opts` is `None`, then the return value is `None`.

    
    ### Example
    ```re
    Option.sequence([|Some("a"), Some("b"), Some("c")|]) == Some([|"a", "b", "c"|]);
    Option.sequence([|Some("a"), None, Some("c")|]) == None;
    Option.sequence([| |]) == Some([| |]);
    ```
  */
  let sequence: array(option('a)) => option(array('a));
};

module Result: {
  /**
    `Result.traverse(f, xs)`, `f()` is a function that takes an item of the
    same type as `xs` and returns a `Belt.Result.t` value.
    
    If `f(x)` returns `Belt.Result.Ok(value)` for all elements in `xs`, the result is `Ok(xs)`.
    
    If `f(x)` returns `Belt.Result.Error(err)` for any element in `xs`, the result is the
    first `Error(err)` that was encountered.
    
    ### Example
    ```re
    let evenValue = (x) => {
      if (x mod 2 == 0) {
        Belt.Result.Ok(x)
      } else {
        Belt.Result.Error(string_of_int(x) ++ " is odd")
      }
    };
    Result.traverse(evenValue, [|100, 102, 104, 106|]) == Belt.Result.Ok([|100, 102, 104, 106|]);
    Result.traverse(evenValue, [|100, 101, 102, 103|]) == Belt.Result.Error("101 is odd");
    Result.traverse(evenValue, [| |]) == Belt.Result.Ok([| |]);
    ```
  */
  let traverse:
    ('a => Belt.Result.t('b, 'c), array('a)) =>
    Belt.Result.t(array('b), 'c);

  /**
    `Result.sequence(results)`, takes an array of `Belt.Result.t` types as its argument.
    If all the elements of `results` are `Ok(value)`, then the return value is
    `Ok(xs)`, where each element in `results` has been unwrapped from its `Belt.Result.Ok()`.
    
    If any element of `results` is `Belt.Error(err)`, then the return value is the
    first such value encountered..

    
    ### Example
    ```re
    Result.sequence([|Belt.Result.Ok("a"), Belt.Result.Ok("b"), Belt.Result.Ok("c")|]) ==
      Belt.Result.Ok([|"a", "b", "c"|]);
    Result.sequence([|Belt.Result.Ok("a"), Belt.Result.Error(1),
      Belt.Result.Ok("c"), Belt.Result.Error(2)|]) == Belt.Result.Error(1);
    Result.sequence([| |]) == Belt.Result.Ok([| |]);
    ```
  */
  let sequence:
    array(Belt.Result.t('a, 'c)) => Belt.Result.t(array('a), 'c);
};

/**
  This submodule provides infix operators for common array
  operations. You can use these operators by opening the
  module:
  
  ```re
  open Relude.Array.Infix;
  ```
*/
module Infix: {

  /**
    In `xs >>= f`, `f` is a function that takes an element
    of the type in `xs` and returns an array. The result of `>>=`
    is the concatenation of all the arrays produced by applying
    `f()` to the elements of `xs`. (Same as `bind()`.)
    
    ### Example
    ```re
    let f = (x) => [| x - 5, x + 5 |];
    [|100, 101, 102|] >>= f == [|95, 105, 96, 106, 97, 107|];
    [| |] >>= f == [| |];
  ```
  */
  let (>>=): (array('a), 'a => array('b)) => array('b);
  
  /**
    In `f =<< xs`, `f` is a function that takes an element
    of the type in `xs` and returns an array. The result of `=<<`
    is the concatenation of all the arrays produced by applying
    `f()` to the elements of `xs`. (Same as `flatMap()`.)
    
    ### Example
    ```re
    let f = (x) => [| x - 5, x + 5 |];
    f =<< [|100, 101, 102|] == [|95, 105, 96, 106, 97, 107|];
    f =<< [| |] == [| |];
    ```
  */
  let (=<<): ('a => array('b), array('a)) => array('b);
  
  let (>=>): ('a => array('b), 'b => array('c), 'a) => array('c);
  let (<=<): ('a => array('b), 'c => array('a), 'c) => array('b);
  
  /**
    `<|>` takes two arrays with elements of the same type and concatenates them
    in reverse order.
    
    ### Example
    ```re
    [|"a", "b"|] <|> [| |] <|> [|"c"|] <|> [|"d", "e", "f"|] ==
      [|"d", "e", "f", "c", "a", "b"|];
    ```
  */
  let (<|>): (array('a), array('a)) => array('a);
  
  /**
    `f <$> xs` is a shorthand for `map(f, xs)`; it takes a function and an array
    and returns an array with the function applied to each element of `xs`.
    
    ### Example
    ```re
    let f = (x) => {Js.String.length(x)};
    f <$> [|"ReasonML", "OCaml"|] == [|8, 5|];
    ```
  */
  let (<$>): ('a => 'b, array('a)) => array('b);
  
  /**
    `xs <#> f` is a shorthand for `map(f, xs)`; it takes a function and an array
    and returns an array with the function applied to each element of `xs`.
    
    ### Example
    ```re
    let f = (x) => {Js.String.length(x)};
    [|"ReasonML", "OCaml"|] <#> f == [|8, 5|];
    ```
  */
  let (<#>): (array('a), 'a => 'b) => array('b);
  
  /**
    `fs <*> xs` takes an array of functions and an array of values and creates
    an array whose contents are the result of applying the first function of `fs`
    to all the elements of `xs`, the second function of
    `fs` to all the elements of `xs`, and so on. All the functions in `fs` must
    have the same result type.
    
    ### Example
    ```re
    let square = (x) => {x * x};
    let cube = (x) => {x * x * x};
    [|square, cube|] <*> [|10, 11, 12|] == [|100, 121, 144, 1000, 1331, 1728|];
    ```
  */
  let (<*>): (array('a => 'b), array('a)) => array('b);
};
