/**
  `Relude.List` contains a large number of functions that assist you
  in manipulating arrays.

  Besides lists of primitive types such as strings or integers, `Relude.List`
  lets you manipulate lists of data types that you create. In order to do this,
  you must sometimes provide a `module` that tells `Relude.List` how to do
  operations such as comparison or checking for equality with your data type.

  For the documentation of `Relude.List`, we will create a data type that
  represents a duration of time in minutes and seconds as a two-tuple of integers.
  Some of the examples will manipulate a list of these durations. We will use
  the following type definition:

  ```re
  type duration = (int, int);
  ```

  Here is a module that will be used when `Relude.List` needs to check for equality
  of two `duration` values. We will call this an `EQ` module. An `EQ` module must define:

  - the type under consideration
  - a function `eq()`, which takes two items of that type and returns `true` if
    they are to be considered equal, `false` otherwise

  ```re
  module DurationEqual = {
    type t = duration;
    let eq = (a, b) => {
      fst(a) == fst(b) && snd(a) == snd(b)
    };
  };
  ```
  Here is another `EQ` module that we will use in our examples; it checks if
  two integers are equal with respect to “clock time” (mod 12):

  ```re
  module ClockEqual = {
    type t = int;
    let eq = (a, b) => { a mod 12 == b mod 12 };
  };
  ```

  Here is a module that will be used when we need to compare
  two `duration` values. We will call this an `ORD` module. An `ORD` module must define:

  - the type under consideration
  - an `eq()` function, as in the preceding module
  - a `compare()` function that returns one of ` `less_than `, ` `_greater_than `, or ` `equal_to `,
    depending on the relationship between its two arguments

  ```re
  module DurationOrder = {
    type t = duration;
    let eq = (a, b) => {
      fst(a) == fst(b) && snd(a) == snd(b)
    };
    let compare = ((min1, sec1), (min2, sec2)) => {
      if (min1 < min2) {
        `less_than
      } else if (min1 > min2) {
        `greater_than
      } else {
        if (sec1 < sec2) {
          `less_than
        } else if (sec1 > sec2) {
          `greater_than
        } else {
          `equal_to
        }
      }
    };
  };
  ```

  Here is a module that defines how you append two values of a datatype. We will
  call this a `MONOID` module. “Monoid“ is a mathematical term that just happens to
  apply nicely in this context, so we’ll go with it. Besides, it sounds more impressive
  than “append.” A `MONOID` module must provide:

  - a type specification
  - an `append()` function which takes two items of the type and appends them to one another
  - an `empty` elemnt for which `append(x, empty) == append(empty, x) == x`

  For our example data type, we will define appending as addition of minutes and seconds, with
  appropriate calculations to make sure seconds is never greater than 59.

  ```re
  module DurationAppend = {
    type t = duration;
    let append = ((min1, sec1), (min2, sec2)) => {
      (min1 + min2 + (sec1 + sec2) / 60, (sec1 + sec2) mod 60)
    };
    let empty = (0, 0);
  };
  ```

  The last type of module we will use in the examples is a module
  that defines how to show a data type. We will call this a `SHOW` module.
  A `SHOW` module must specify:

  - the type under consideration
  - a `show()` function that takes a value of the given type and converts it to a string

    ```re
  module DurationShow = {
    type t = duration;
    let show = ((min1, sec1)) => {
      string_of_int(min1) ++ "m " ++ string_of_int(sec1) ++ "s";
    };
  };
  ```
*/

/**
 * List is a member of the following typeclasses, regardless of the inner type
 */
module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = list('a);
module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a);
module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a);
module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = list('a);
module Apply: BsAbstract.Interface.APPLY with type t('a) = list('a);
module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = list('a);
module Monad: BsAbstract.Interface.MONAD with type t('a) = list('a);
module Alt: BsAbstract.Interface.ALT with type t('a) = list('a);
module Plus: BsAbstract.Interface.PLUS with type t('a) = list('a);
module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = list('a);
module IsoArray: Relude_IsoArray.ISO_ARRAY with type t('a) = list('a);

/**
 * The following functions come from List's membership in Semigroup and Monoid
 */

/**
  `concat(xs, ys)` returns a list with the elements of `xs` followed
  by the elements of `ys`.

  ## Example
  ```re
  concat(["a", "b"], ["c", "d"]) == ["a", "b", "c", "d"];
  concat([ ], ["a", "b"]) == ["a", "b"];
  concat(["a", "b"], [ ]) == ["a", "b"];
  ```
*/

let concat: (list('a), list('a)) => list('a);

/**
  `empty` returns a new, empty list.
*/

let empty: list('a);

/**
 * The following functions come for free because List is a member of Functor,
 * Apply, Applicative, and Monad
 */

/**
  `map(f, xs)` creates a new list by applying `f` to
  each element of `xs`.

  ### Example
  ```re
  let f = (x) => {Js.String.length(x)};
  map(f, ["ReasonML", "OCaml"]) == [8, 5];
  ```
*/

let map: ('a => 'b, list('a)) => list('b);

/**
  `void(xs)` returns a list of the same length as `xs`,
  with each element equal to unit `()`.

  ### Example
  ```re
  void([100, 101, 102]) == [(), (), ()];
  ```
*/

let void: list('a) => list(unit);

/**
  `apply(fs, xs)` takes a list of functions and a list of values and creates
  a list whose contents are the result of applying the first function of `fs`
  to all the elements of `xs`, the second function of
  `fs` to all the elements of `xs`, and so on. All the functions in `fs` must
  have the same result type.

  ### Example
  ```re
  let square = (x) => {x * x};
  let cube = (x) => {x * x * x};
  apply([square, cube], [10, 11, 12]) == [100, 121, 144, 1000, 1331, 1728];
  ```
*/

let apply: (list('a => 'b), list('a)) => list('b);

/**
  `flap(fs, x)` creates a list whose contents are the result of applying every
  function in `fs` to `x`.

  ### Example
  ```re
  let square = (x) => {x * x};
  let cube = (x) => {x * x * x};
  flap([square, cube], 10) == [100, 1000];
  ```
*/
let flap: (list('a => 'b), 'a) => list('b);

/**
  `map2(f, xs, ys)` has a function that takes two arguments
  as its first parameter. It returns a list with the results
  of all the `f(x, y)` combinations, iterating through `ys` first,
  then `xs`.

  ### Example
  ```re
  let phrase = (str, n) => { str ++ " " ++ string_of_int(n) };
  map2(phrase, ["cat", "dog"], [1, 2, 3]) ==
    ["cat 1", "cat 2", "cat 3", "dog 1", "dog 2", "dog 3"];
  ```
*/
let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c);

/**
  `map3(f, xs, ys, zs)` has a function that takes three arguments
  as its first parameter. It returns a list with the results
  of all the `f(x, y, z)` combinations, iterating through `zs` first,
  then `ys`, then `xs`.

  ### Example
  ```re
  let together = (x, y, z) => {x ++ y ++ z};
  map3(together, ["a", "b", "c"], ["d", "e"], ["f"]) ==
    ["adf", "aef", "bdf", "bef", "cdf", "cef"];
  ```
*/
let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d);

/**
  `map4(f, xs, ys, zs, ws)` has a function that takes four arguments
  as its first parameter. It returns a list with the results
  of all the `f(x, y, z, w)` combinations, iterating through `ws` first,
  then `zs`, then `ys`, then `xs`.

  ### Example
  ```re
  let together = (x, y, z, w) => {x ++ y ++ z ++ w};
  map4(together, ["a", "b"], ["c", "d"], ["e", "f"], ["g", "h"]) ==
    ["aceg", "aceh", "acfg", "acfh", "adeg", "adeh", "adfg", "adfh",
      "bceg", "bceh", "bcfg", "bcfh", "bdeg", "bdeh", "bdfg", "bdfh"];
  ```
*/
let map4:
  (('a, 'b, 'c, 'd) => 'e, list('a), list('b), list('c), list('d)) =>
  list('e);

/**
  `map5(f, xs, ys, zs, ws, qs)` has a function that takes five arguments
  as its first parameter. It returns a list with the results
  of all the `f(x, y, z, w, q)` combinations, iterating through `qs` first,
  then `ws`, then `zs`, then `ys`, then `xs`.

  ### Example
  ```re
  let together = (x, y, z, w, q) => {x ++ y ++ z ++ w ++ q};
  map5(together, ["a", "b"], ["c"], ["d", "e"], ["f"], ["g", "h"]) ==
    ["acdfg", "acdfh", "acefg", "acefh", "bcdfg", "bcdfh", "bcefg", "bcefh"];
  ```
*/
let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    list('a),
    list('b),
    list('c),
    list('d),
    list('e)
  ) =>
  list('f);

/**
  `pure(item)` returns a list containing the given item.

  ## Example
  ```re
  pure("single") == ["single"];
  ```
*/
let pure: 'a => list('a);

/**
  In `bind(xs, f)`, `f` is a function that takes an element
  of the type in `xs` and returns a list. The result of `bind`
  is the concatenation of all the lists produced by applying
  `f()` to the elements of `xs`.

  ### Example
  ```re
  let f = (x) => [ x - 5, x + 5 ];
  bind([100, 101, 102], f) == [95, 105, 96, 106, 97, 107];
  bind([ ], f) == [ ];
  ```
*/
let bind: (list('a), 'a => list('b)) => list('b);

/**
  In `flatMap(f, xs)`, `f` is a function that takes an element
  of the type in `xs` and returns a list. The result of `bind`
  is the concatenation of all the lists produced by applying
  `f()` to the elements of `xs`.

  ### Example
  ```re
  let f = (x) => [ x - 5, x + 5 ];
  flatMap(f, [100, 101, 102]) == [95, 105, 96, 106, 97, 107];
  flatMap(f, [ ]) == [ ];
  ```
*/
let flatMap: ('a => list('b), list('a)) => list('b);

/**
  `flatten(xs_of_xs)` takes a list of lists as its argument
  and returns a list with all the sub-lists concatenated.

  ### Example
  ```re
  flatten([ ["a", "b"], [ ], ["c"], ["d", "e", "f"] ]) ==
    ["a", "b", "c", "d", "e", "f"];
  ```
*/
let flatten: list(list('a)) => list('a);

/**
 * The following functions come for free because List is a member of Foldable
 */

/**
  `foldLeft(f, init, xs)` accumulates a value. Starting with `init`,
  as the initial value of the accumulator, `foldLeft` applies function
  `f` to the accumulator and the first element in the list `xs`. The result
  becomes the new value of the accumulator, and `f` is applied to that value
  and the next element in `xs`. This process continues until all elements in
  `xs` are processed. The final value of the accumulator is returned.

  ## Example
  ```re
  foldLeft((acc, item) => append(item, acc), [ ], [1, 2, 3]) == [1, 2, 3];
  foldLeft((acc, item) => acc * item, 1, [1, 2, 3]) == 6;
  foldLeft((acc, item) => acc + item, 2, [ ]) == 2;
  ```
*/
let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b;

/**
  `foldRight(f, init, xs)` accumulates a value. Starting with `init`,
  as the initial value of the accumulator, `foldRight` applies function
  `f` to the last element in the array `xs` and the accumulator. The result
  becomes the new value of the accumulator, and `f` is applied to that value
  and the preceding element in `xs`. This process continues until all elements in
  `xs` are processed. The final value of the accumulator is returned.

  ## Example
  ```re
  foldRight((item, acc) => append(item, acc), [ ], [1, 2, 3]) == [3, 2, 1];
  foldRight((item, acc) => acc * item, 1, [1, 2, 3]) == 6;
  foldRight((item, acc) => acc + item, 2, [ ]) == 2;
  ```
*/
let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b;

/**
  In `any(pred, xs)`, `pred` is a function that takes an item of the type in the
  list and returns a boolean value.  The `any()` function returns `true` if
  `pred(x)` returns true for any item `x` in the list, `false` otherwise.

  ### Example
  ```re
  any( (x) => {x < 0}, [100, -101, 102]) == true;
  any( (x) => {x < 0}, [100, 101, 102]) == false;
  ```
*/
let any: ('a => bool, list('a)) => bool;

/**
  In `all(pred, xs)`, `pred` is a function that takes an item of the type in the
  list and returns a boolean value.  The `all()` function returns `true` if
  `pred(x)` returns true for every item `x` in the list, `false` otherwise.

  ### Example
  ```re
  all( (x) => {x < 0}, [-100, -101, -102]) == true;
  all( (x) => {x < 0}, [-100, 101, -102]) == false;
  ```
*/
let all: ('a => bool, list('a)) => bool;

/**
  In `containsBy(f, value, xs)`, the function `f` takes two items
  of the type in the list and returns a predicate function `p()`
  by calling `f(value)`.

  `containsBy() returns `true` if any item in `xs` satisfies this
  new predicate function `p()`.

  ### Example
  ```re
  let aboveLimit = (limit, x) => { x > limit };
  containsBy(aboveLimit, 50, [30, 70, 20]) == true;
  containsBy(aboveLimit, 90, [30, 70, 20]) == false;
  ```
*/
let containsBy: (('a, 'a) => bool, 'a, list('a)) => bool;

/**
  In `indexOfBy(f, value, xs)`, the function `f` takes two items
  of the type in the list and returns a predicate function `p()`
  by calling `f(value)`.

  `indexOfBy() returns `Some(position)` where `position` is the index
  of the first item in `xs` that satisfies this new predicate function `p()`,
  or `None` if no item in `xs` satisfies the predicate.

  ### Example
  ```re
  let aboveLimit = (limit, x) => { x > limit };
  indexOfBy(aboveLimit, 50, [30, 70, 20, 80]) == Some(1);
  indexOfBy(aboveLimit, 90, [30, 70, 20, 80]) == None;
  ```
*/
let indexOfBy: (('a, 'a) => bool, 'a, list('a)) => option(int);

/**
  `minBy(f, xs)` returns the minimum value in list `xs` as
  `Some(val)`. It uses function `f` to compare values in the list.
  Function `f` takes two parameters of the type in the list and
  returns a value of ` `less_than `, ` `equal_to `, or ` `greater_than `,
  depending on the relationship of the values.

  If given an empty array, `minBy()` returns `None`.

  ### Example
  ```re
  let clockCompare = (a, b) => {
    if (a mod 12 < b mod 12) {
      `less_than
    } else if (a mod 12 > b mod 12) {
      `greater_than
    } else {
      `equal_to
    }
  };

  minBy(clockCompare, [5, 3, 17, 14, 9]) == Some(14);
  minBy(clockCompare, [5, 17]) == Some(5);
  minBy(clockCompare, [ ]) == None;
  ```
*/

let minBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => option('a);

/**
  `maxBy(f, xs)` returns the maximum value in list `xs` as
  `Some(val)`. It uses function `f` to compare values in the list.
  Function `f` takes two parameters of the type in the list and
  returns a value of ` `less_than `, ` `equal_to `, or ` `greater_than `,
  depending on the relationship of the values.

  If given an empty list, `maxBy()` returns `None`.

  ### Example
  ```re
  let clockCompare = (a, b) => {
    if (a mod 12 < b mod 12) {
      `less_than
    } else if (a mod 12 > b mod 12) {
      `greater_than
    } else {
      `equal_to
    }
  };

  maxBy(clockCompare, [5, 3, 17, 14, 9]) == Some(9);
  maxBy(clockCompare, [5, 17]) == Some(5);
  maxBy(clockCompare, [ ]) == None;
  ```
*/
let maxBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => option('a);

/**
  `countBy(countFcn, xs)` returns the number of items `x` in list `xs`
  for which `countFcn(x)` returns `true`.

  ### Example
  ```re
  let isOdd = (x) => {x mod 2 == 1};
  countBy(isOdd, [33, 22, 55, 11, 44, 66]) == 3;
  countBy(isOdd, [22, 44, 66]) == 0;
  countBy(isOdd, [ ]) == 0;
  ```
*/
let countBy: ('a => bool, list('a)) => int;

/**
  `length(xs)` returns the number of items in `xs`.

  ## Example
  ```re
  length(["a", "b", "c"]) == 3;
  length([ ]) == 0;
  ```
*/
let length: list('a) => int;

/**
  In `forEach(f, xs)`, `f()` is a function that takes an element
  of `xs` and returns `unit`. `forEach()` applies this function to
  each element of `xs`. You use `forEach()` when you are interested in
  the side effects rather than the result of a function.

  ### Example
  ```re
  forEach(Js.log, ["a", "b", "c"]) == (); // prints a, b, and c
  ```
*/
let forEach: ('a => unit, list('a)) => unit;

/**
  In `forEachWithIndex(f, xs)`, `f()` is a function that takes an element
  of `xs` and an integer and returns `unit`. `forEach()` applies this function to
  each element of `xs`, passing the element and its index number (starting
  at zero). You use `forEachWithIndex()` when you are interested in
  the side effects rather than the result of a function.

  ### Example
  ```re
  let debug = (x, i) => {Js.log(string_of_int(i) ++ " " ++ x)};
  forEachWithIndex(debug, ["a", "b", "c"]) == (); // prints 0 a, 1 b, and 2 c
  ```
*/
let forEachWithIndex: (('a, int) => unit, list('a)) => unit;

/**
  `find(pred, xs)` returns `Some(x)` for the first value in `xs`
  for which the predicate function `pred(x)` returns `true`.
  If no value in the list satisfies `pred()`, `find()` returns `None`.

  ### Example
  ```re
  find((x) => {x mod 2 == 0}, [3, 7, 4, 2, 5]) == Some(4);
  find((x) => {x mod 2 == 0}, [3, 7, 5]) == None;
  ```
*/
let find: ('a => bool, list('a)) => option('a);

/**
  `findWithIndex(pred, xs)` calls `pred()` with two arguments: an element
  of `xs` and its index value (zero-based). If `pred()` returns `true`,
  the element is returned as `Some(x)`.  If no element in the list satisfies
  `pred()`, `findWithIndex()` returns `None`.


  ### Example
  ```re
  let bothEven = (x, i) => {x mod 2 == 0 && i mod 2 == 0};
  findWithIndex(bothEven, [3, 6, 4, 7, 5]) == Some(4);
  findWithIndex(bothEven, [3, 6, 7, 8, 5]) == None;
  findWithIndex(bothEven, [3, 7, 5]) == None;
  ```
*/
let findWithIndex: (('a, int) => bool, list('a)) => option('a);

/**
  `fold((module M), xs)` concatenates the elements of `xs` as specified by
  the given module, which is a `MONOID` module.

  ### Example
  ```re
  fold((module DurationAppend), [(3, 25), (7, 45)]) == (11, 10);
  ```
*/
let foldMonoid:
  ((module BsAbstract.Interface.MONOID with type t = 'a), list('a)) => 'a;

/**
  `intercalate((module M), delim, xs)` concatenates the elements of `xs` as specified by
  the given module, with `delim` between all the elements. The module must be a
  `MONOID` module.

  The following example inserts a “rest period“ of fifteen seconds between each
  duration, for a total of an additional thirty seconds:

  ### Example
  ```re
  intercalate((module DurationAppend), (0, 15), [(1, 0), (2, 0), (3, 0)]) == (6, 30);
  ```
*/
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a, list('a)) => 'a;

/**
  `contains((module M), val, xs)` returns `true` if any element of `xs` equals
  `val`, as determined by the module. It returns `false` if no element equals
  `val`. The module must be an `EQ` module.

  ### Example
  ```re
  contains((module DurationEqual), (3, 20), [(1, 10), (3, 20), (4, 40)]) == true;
  contains((module DurationEqual), (3, 20), [(1, 10), (2, 20), (4, 40)]) == false;
  contains((module DurationEqual), (3, 20), [ ]) == false;
  ```
*/
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) => bool;

/**
  `indexOf((module M), val, xs)` finds the position of `val` in the list `xs`.
  If `val` is at position `index`, the return value is `Some(index)`; if `val`
  is not in the array, the return value is `None`. The module you pass must be
  an `EQ` module.

  ### Example
  ```re
  indexOf((module DurationEqual), (3, 20), [(1, 10), (3, 20), (4, 40)]) == Some(1);
  indexOf((module DurationEqual), (3, 20), [(1, 10), (2, 20), (4, 40)]) == None;
  indexOf((module DurationEqual), (3, 20), [ ]) == None;
  ```
*/
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  option(int);

/**
  `min((module M), xs)` returns `Some(val)` where `val` is the
  minimum value in `xs` as determined by the  module. The module must be an `ORD` module.
  `min()` returns `None` if the list is empty.

  ### Example
  ```re
  min((module DurationOrder), [(2, 20), (1, 10), (4, 40), (3, 30)]) == Some((1, 10));
  min((module DurationOrder), [ ]) == None;
  ```
*/
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);

/**
  `max((module M), xs)` returns `Some(val)` where `val` is the
  maximum value in `xs` as determined by the  module. The module must be an `ORD` module.
  `max()` returns `None` if the list is empty.

  ### Example
  ```re
  max((module DurationOrder), [(2, 20), (1, 10), (4, 40), (3, 30)]) == Some((4, 40));
  max((module DurationOrder), [ ]) == None;
  ```
*/
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);

/**
 * The following functions come from List's membership in IsoArray
 */

/**
  `fromArray(xs)` converts an array to a list.

  ### Example
  ```re
  fromArray([|100, 101, 102|]) == [100, 101, 102];
  ```
*/

let fromArray: array('a) => list('a);

/**
  `toArray(xs)` converts a list to an array.

  ### Example
  ```re
  toArray([100, 101, 102]) == [|100, 101, 102|];
  ```
*/
let toArray: list('a) => array('a);

/**
 * The following functions and modules care about properties of the inner type
 */

module Traversable:
  (BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE;

/**
  `scanLeft(f, init, xs)` returns a list of values. Starting with `init`,
  as the initial value of an accumulator, `scanLeft` applies function
  `f` to the accumulator and the first element in the list. The result
  is appended to the list of values and becomes the new value of the accumulator
  Then `f` is applied to the new accumulator and the next element in `xs`.
  This process continues until all elements in `xs` are processed.
  `scanLeft` returns the array of all the accumulated values.

  In short, `scanLeft` returns a list of the values that a
  `foldLeft` would have computed, in left to right order.

  ## Example
  ```re
  scanLeft( (acc, item) => {acc - item}, 0, [1, 2, 3]) == [-1, -3, -6];
  scanLeft( (acc, item) => {acc + item}, 0, [ ]) == [ ];
  ```
*/
let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b);

/**
  `scanRight(f, init, xs)` returns a list of values. Starting with `init`,
  as the initial value of an accumulator, `scanRight` applies function
  `f` to the last element in the list and the accumulator. The result
  is prepended to the list of values and becomes the new value of the accumulator.
  Then `f` is applied to the preceding element in `xs` and the accumulator.
  This process continues until all elements in `xs` are processed.
  `scanRight` returns the list of all the accumulated values.

  In short, `scanRight` returns a list of the values that a
  `foldRight` would have computed, in right to left order.

  ## Example
  ```re
  scanRight( (item, acc) => {acc - item}, 0, [1, 2, 3]) == [-6, -5, -3];
  scanRight( (item, acc) => {acc + item}, 0, [ ]) == [ ];
  ```
*/
let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b);

/**
  `eqBy(f, xs, ys)` compares each element of `xs` to the corresponding
  element of `ys`, using `f(x, y)` to determine if the elements are equal
  or not. If all elements are equal, `eqBy()` returns `true`; otherwise
  it returns `false`.

  ### Example
  ```re
  let eqMod12 = (a, b) => {a mod 12 == b mod 12};
  eqBy(eqMod12, [2, 4, 3, 5], [14, 16, 15, 17]) == true;
  eqBy(eqMod12, [2, 4, 3, 5], [2, 4, 3]) == false;
  eqBy(eqMod12, [ ], [ ]) == true;
  ```
*/
let eqBy: (('a, 'a) => bool, list('a), list('a)) => bool;

module Eq: (BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;

/**
  `eq((module M), xs, ys)` compares each element of `xs` to the corresponding
  element of `ys`, using an `EQ` module to determine if the elements are equal
  or not. If all elements are equal, `eq()` returns `true`; otherwise
  it returns `false`.

  ### Example
  ```re
  eq((module DurationEqual), [(3, 30), (2, 20), (4, 40)],
    [(3, 30), (2, 20), (4, 40)]) == true;
  eq((module DurationEqual), [(3, 30), (2, 20), (4, 40)],
    [(3, 30), (2, 20)]) == false;
  eq((module DurationEqual), [(3, 30), (2, 20), (4, 40)],
    [(3, 30), (2, 25), (4, 40)]) == false;
  ```
*/
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a), list('a)) =>
  bool;

/**
  `showBy(f, xs)` uses `f(x)` to convert each element
  of `xs` to a string, separates the strings with commas, and
  encloses it all in square brackets to be returned as one string.

  ### Example
  ```re
  let toString = (x) => {string_of_int(x)};
  showBy(toString, [100, 101, 102]) == "[100, 101, 102]";
  ```
*/
let showBy: ('a => string, list('a)) => string;
module Show: (BsAbstract.Interface.SHOW) => BsAbstract.Interface.SHOW;

/**
  `show((module M), xs)` uses the given module, which must be a
  `SHOW` module, to convert each element
  of `xs` to a string, separates the strings with commas, and
  encloses it all in square brackets to be returned as one string.

  ### Example
  ```re
  show((module DurationShow), [(1, 10), (2, 20), (3, 30)]) == "[1m 10s, 2m 20s, 3m 30s]";
  ```
*/
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), list('a)) => string;

/**
 * The following functions are list-specific. They may use
 */

/**
  `mapWithIndex(f, xs)` creates a new list by applying `f(x, n)` to each element of `xs`,
  where `n` is the zero-based index of the list element.

  ### Example
  ```re
  let numbered =(s, index) => {string_of_int(index + 1) ++ ". " ++ s};
  mapWithIndex(numbered, ["ant", "bee", "cat"]) == ["1. ant", "2. bee", "3. cat"];
  ```
*/

let mapWithIndex: (('a, int) => 'b, list('a)) => list('b);

/**
  `cons(x, xs)` returns a new list with value `x` at the beginning.

  ## Example
  ```re
  cons(99, [100, 101]) == [99, 100, 101];
  cons(99, [ ]) == [99];
  ```
*/
let cons: ('a, list('a)) => list('a);

/**
  `prepend` is a synonym for `cons`
*/
let prepend: ('a, list('a)) => list('a);

/**
  When given a non-emtpy list, `uncons(xs)` returns `Some((y, ys))`
  where `y` is the first element in the list and `ys` are the remaining
  elements. If given an empty list, `uncons()` returns `None`.

  ## Example
  ```re
  uncons([100, 101, 102]) == Some((100, [101, 102]));
  uncons([100]) == Some((100, [ ]));
  uncons([ ]) == None;
  ```
*/
let uncons: list('a) => option(('a, list('a)));

/**
  `append(x, xs)` adds the value `x` at the end of list `xs`.

  ## Example
  ```re
  append(999, [100, 101, 102]) == [100, 101, 102, 999];
  append(999, [ ]) == [999];
  ```
*/
let append: ('a, list('a)) => list('a);

/**
  `repeat(n, x)` returns a list containing `n` copies of `x`.

  ## Example
  ```re
  repeat(3, "ha") == ["ha", "ha", "ha"];
  repeat(0, "nothing") == [ ];
  repeat(-2, "nothing") == [ ];
  ```
*/
let repeat: (int, 'a) => list('a);

/**
  `makeWithIndex(n, f)` returns the list `[f(0), f(1), ... f(n - 1)]`.

  ## Example
  ```re
  makeWithIndex(3, (x) => {(x + 4) * (x + 4)}) == [16, 25, 36];
  makeWithIndex(0, (x) => {x + 1}) == [ ];
  makeWithIndex(-1, (x) => {x + 1}) == [ ];
  ```
*/
let makeWithIndex: (int, int => 'a) => list('a);

/**
  `reverse(xs)` returns a list with elements in the reverse order
  from the original list.

  ### Example
  ```re
  reverse([100, 101, 102]) == [102, 101, 100];
  ```
*/
let reverse: list('a) => list('a);

/**
  `shuffle(xs)` will return a new array with the same elements as
  `xs` but in a random order. The original array is not changed.

  ### Example
  ```re
  let data = [100, 101, 102, 103, 104];
  let mixed = shuffle(data);
  mixed != data;
  ```
*/
let shuffle: list('a) => list('a);

/**
  `isEmpty(xs) returns `true` if `xs` is the empty list `[ ]`; returns `false` otherwise.
*/
let isEmpty: list('a) => bool;

/**
  `isNotEmpty(xs) returns `true` if `xs` is not the empty list `[ ]`; returns `false` otherwise.
*/
let isNotEmpty: list('a) => bool;

/**
  `at(n, xs)` returns the value at the given index position as `Some(value)`
  unless `n` is less than zero or greater than the length of `xs`, in which
  case `at()` returns `None.`

  ## Example
  ```re
  at(0, [100, 101, 102]) == Some(100);
  at(2, [100, 101, 102]) == Some(102);
  at(-1, [100, 101, 102]) == None;
  at(3, [100, 101, 102]) == None;
  ```
*/
let at: (int, list('a)) => option('a);

/**
  For non-empty lists, `head(xs)` returns the first item in the list
  as `Some(value)`. For an empty list, the function returns `None`.

  ## Example
  ```re
  head([100, 101, 102]) == Some(100);
  head([ ]) == None;
  ```
*/
let head: list('a) => option('a);

/**
  For non-empty lists, `tail(xs)` returns a list consisting of all
  but the first item in `xs` as `Some(ys)`. For an empty list, the
  function returns `None`.

  ## Example
  ```re
  tail([100, 101, 102]) == Some([101, 102]);
  tail([ ]) == None;
  ```
*/
let tail: list('a) => option(list('a));

/**
  For non-empty lists, `tailOrEmpty(xs)` returns a list consisting of all
  but the first item in `xs`. For an empty list, the function returns
  an empty list.

  ## Example
  ```re
  tailOrEmpty([100, 101, 102]) == [101, 102];
  tailOrEmpty([ ]) == [ ];
  ```
*/
let tailOrEmpty: list('a) => list('a);

/**
  For non-empty lists, `init(xs)` returns a list containing
  all but the last item in `xs` as `Some(ys)`. The function returns
  `None` if given an empty list.

  ## Example
  ```re
  init([100, 101, 102]) == Some([100, 101]);
  init([ ]) == None;
  ```
*/
let init: list('a) => option(list('a));

/**
  For non-empty lists, `last(xs)` returns the last
  item in `xs` as `Some(value)`. The function returns
  `None` if given an empty list.

  ## Example
  ```re
  last([100, 101, 102]) == Some(102);
  last([ ]) == None;
  ```
*/
let last: list('a) => option('a);

/**
  `take(n, xs)` returns a list of the first `n` items in `xs` as
  `Some(ys)`, `n` is pinned to the range 0..`n` - 1.

  ## Example
  ```re
  take(2, [100, 101, 102, 103]) == [100, 101];
  take(0, [100, 101, 102]) == [ ];
  take(-1, [100, 101, 102]) == [ ];
  take(4, [100, 101, 102]) == [100, 101, 102];
  take(1, [ ]) == [ ];
  ```
*/
let take: (int, list('a)) => list('a);

/**
  `takeExactly(n, xs)` returns a list of the first `n` items in `xs` as
  `Some(ys)`, If `n` is less than or equal to zero, `takeExactly()` returns `Some([ ])`.
  If `n` is greater than or equal to the length  of `xs`, `takeExactly()` returns `None`.

  ## Example
  ```re
  takeExactly(2, [100, 101, 102, 103]) == Some([100, 101]);
  takeExactly(0, [100, 101, 102]) == Some([ ]);
  takeExactly(-1, [100, 101, 102]) == None;
  takeExactly(4, [100, 101, 102]) == None;
  takeExactly(1, [ ]) == None;
  ```
*/
let takeExactly: (int, list('a)) => option(list('a));

/**
  `takeWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `takeWhile()` returns a list consisting
  of the first elements of `xs` which satisfy the predicate. (This
  could be an empty list.)

  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  takeWhile(even, [2, 6, 5, 3, 8]) == [2, 6];
  takeWhile(even, [5, 3, 8, 2, 6]) == [ ];
  takeWhile(even, [ ]) == [ ];
  ```
*/
let takeWhile: ('a => bool, list('a)) => list('a);

/**
  `drop(n, xs)` returns a list of all *except* the first `n` items in `xs` as
  `Some(ys)`. `n` is pinned to the range 0..`n` - 1.

  ## Example
  ```re
  drop(2, [100, 101, 102, 103]) == [102, 103];
  drop(0, [100, 101, 102]) == [100, 101, 102];
  drop(4, [100, 101, 102]) == [ ];
  drop(-1, [100, 101, 102]) == [100, 101, 102];
  drop(1, [ ]) == [ ];
  ```
*/
let drop: (int, list('a)) => list('a);

/**
  `dropExactly(n, xs)` returns a list of all *except* the first `n` items
  in `xs` as `Some(ys)`.

  If `n` is less than zero or greater than or equal to the length  of `xs`,
  `dropExactly()` returns `None`.

  ## Example
  ```re
  dropExactly(2, [100, 101, 102, 103]) == Some([102, 103]);
  dropExactly(0, [100, 101, 102]) == Some([100, 101, 102]);
  dropExactly(4, [100, 101, 102]) == None;
  dropExactly(-1, [100, 101, 102]) == None;
  dropExactly(1, [ ]) == None;
  ```
*/
let dropExactly: (int, list('a)) => option(list('a));

/**
  `dropWhile(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `dropWhile()` returns a list consisting
  all *except* the first elements of `xs` which satisfy the predicate.
  (This could be an empty list.)

  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  dropWhile(even, [2, 6, 5, 3, 8]) == [5, 3, 8];
  dropWhile(even, [5, 3, 8]) == [5, 3, 8];
  dropWhile(even, [2, 4, 6]) == [ ];
  dropWhile(even, [ ]) == [ ];
  ```
*/
let dropWhile: ('a => bool, list('a)) => list('a);

/**
  `filter(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameter an element of `xs` and
  returns a boolean value. `filter()` returns a list of all
  the elements of `xs` that satisfy the predicate.

  ## Example
  ```re
  let even = (x) => {x mod 2 == 0};
  filter(even, [2, 6, 5, 3, 4, 1]) == [2, 6, 4];
  filter(even, [5, 3, 1]) == [ ];
  filter(even, [ ]) == [ ];
  ```
*/
let filter: ('a => bool, list('a)) => list('a);

/**
  `filterWithIndex(f, xs)` has as its first parameter a predicate
  function. The predicate takes as its parameters an element of `xs` and
  the element’s index value, returning a boolean value.
  `filterWithIndex()` returns a list of all
  the elements of `xs` that satisfy the predicate.

  ## Example
  ```re
  let bothEven = (x, i) => {x mod 2 == 0 && i mod 2 == 0};
  filterWithIndex(bothEven, [2, 4, 3, 1, 6, 5]) == [2, 6];
  filterWithIndex(bothEven, [1, 4, 3, 6, 5, 2]) == [ ];
  ```
*/
let filterWithIndex: (('a, int) => bool, list('a)) => list('a);

/**
  `filterNot(f, xs)` is the inverse of `filter`. Instead of keeping values for
  which the predicate returns `true`, it instead keeps values that return
  `false`.

  ## Example
  ```re
  let isEven = x => x mod 2 == 0;
  filterNot(isEven, [1, 2, 3, 4]) == [1, 3];
  ```
*/
let filterNot: ('a => bool, list('a)) => list('a);

/**
  `filterNotWithIndex(f, xs)` is the inverse of `filterWithIndex`.

  ## Example
  ```re
  filterNotWithIndex((_, idx) => idx > 2, [0, 0, 0, 0]) == [0, 0, 0]
  ```
*/
let filterNotWithIndex: (('a, int) => bool, list('a)) => list('a);

/**
  `mapOption` allows you to map each value in a list to an `option`, then all
  of the `None` values are filtered out.

  ## Example
  ```re
  mapOption(Relude.String.toInt, ["1", "a", "2", "b"]) == [1, 2];
  ```
*/
let mapOption: ('a => option('b), list('a)) => list('b);

/**
  Given a list of options, `catOptions` filters out all of the `None` values and
  returns a list containing only the inner `Some` values.

  ## Example
  ```re
  catOptions([Some(1), None, None, Some(2), None]) == [1, 2];
  catOptions([None, None, None]) == [];
  ```
*/
let catOptions: list(option('a)) => list('a);

/**
  `partition(f, xs)` takes as its first parameter a predicate function.
  The predicate takes as its parameter an element of `xs` and returns a boolean.

  `partition()` returns a tuple of two lists: the elements in
  `xs` that satisfy the predicate, and the elements that don’t.

  ## Example

  ```re
  let even = (x) => {x mod 2 == 0};
  partition(even, [6, 1, 3, 2, 4, 5]) == ([6, 2, 4], [1, 3, 5]);
  partition(even, [6, 2, 4]) == ([6, 2, 4], [ ]);
  partition(even, [1, 3, 5]) == ([ ], [1, 3, 5]);
  partition(even, [ ]) == ([ ], [ ]);
  ```
*/
let partition: ('a => bool, list('a)) => (list('a), list('a));

/**
  `splitAt(n, xs)` returns `Some(ys, zs)` where `ys` contains the
  first `n` elements of `xs` and `zs` contains the remaining elements,
  when `n` is greater than or equal to zero and less than or equal
  to the length of `xs`.  Otherwise, `splitAt()` returns `None`.

  ## Example
  ```re
  splitAt(2, [100, 101, 102, 103]) == Some(([100, 101], [102, 103]));
  splitAt(0, [100, 101, 102, 103]) == Some(([ ], [100, 101, 102, 103]));
  splitAt(4, [100, 101, 102, 103]) == Some(([100, 101, 102, 103], [ ]));
  splitAt(-1, [100, 101, 102, 103]) == None;
  splitAt(5, [100, 101, 102, 103]) == None;
  ```
*/
let splitAt: (int, list('a)) => option((list('a), list('a)));

/**
  `prependToAll(delim, xs)` returns a new list with `delim` inserted
  before every current element of `xs`.

  ## Example
  ```re
  prependToAll(999, [100, 101, 102]) == [999, 100, 999, 101, 999, 102];
  prependToAll(999, [ ]) == [ ];
  ```
*/
let prependToAll: ('a, list('a)) => list('a);

/**
  `intersperse(delim, xs)` returns a new list with `delim` inserted
  between all the current elements of `xs`.

  ## Example
  ```re
  intersperse(999, [100, 101, 102]) == [100, 999, 101, 999, 102];
  intersperse(999, [ ]) == [ ];
  ```
*/
let intersperse: ('a, list('a)) => list('a);

/**
  `replicate(n, xs)` returns a list with `n` repetitions of `xs`,
  one after another. If `n` is less than or equal to zero, returns the
  empty list.

  ## Example
  ```re
  replicate(3, [1, 2]) == [1, 2, 1, 2, 1, 2];
  replicate(0, [1, 2]) == [ ];
  replicate(-1, [1, 2]) == [ ];
  ```
*/
let replicate: (int, list('a)) => list('a);

/**
  `zip(xs, ys)` returns a list of lists whose elements are the tuples
  `[ (x[0], y[0]), (x[1], y[1])... ]`. The process of combining
  elements stops when the shorter of the two lists is finished.

  ### Example
  ```re
  zip([1, 2, 3], [4.4, 5.5, 6.6]) == [(1, 4.4), (2, 5.5), (3, 6.6)];
  zip([1, 2, 3], [4.4, 5.5]) == [(1, 4.4), (2, 5.5)];
  zip([1, 2], [3.3, 4.4, 5.5]) == [ (1, 3.3), (2, 4.4) ];
  ```
*/
let zip: (list('a), list('b)) => list(('a, 'b));

/**
  `zipWith(f, xs, ys)` returns a list that is the result of applying
  `f` to corresponding elements of `xs` and `ys`, stopping when it hits
  the end of the shorter list.

  ### Example
  ```re
  zipWith( (x, y) => { 2 * x + y }, [1, 2, 3], [4, 5]) == [6, 9];
  zipWith( (x, y) => { 2 * x + y }, [ ], [4, 5]) == [ ];
  ```
*/

let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c);

/**
  `zipWithIndex(xs)` produces a list of two-tuples where
  each tuple contains the item from the list and its index number,
  starting at zero.

  ### Example
  ```re
  zipWithIndex(["a", "b", "c"]) == [("a", 0), ("b", 1), ("c", 2)];
  ```
*/
let zipWithIndex: list('a) => list(('a, int));

/**
  `unzip(xs)` takes a list of pairs and creates a pair of lists.
  The first lists contains all the first items of the pairs, and the second
  list contains all the second items.

  ### Example
  ```re
  unzip([("a", 0), ("b", 1), ("c", 2)]) == (["a", "b", "c"], [0, 1, 2]);
  ```
*/
let unzip: list(('a, 'b)) => (list('a), list('b));

/**
  `sortWithInt(f, xs)` sorts the list `xs`, calling `f` every time it needs to
  compare two list elements `a` and `b`.  If `f(a, b)` is negative, then `a`
  precedes `b` in sorting order. If `f(a, b)` is positive, then `a` follows `b`
  in sorting order. If `f(a, b) is zero, then `a` and `b` are considered equal.

  This is a stable sort; equal elements will appear in the output list in the
  same order that they appeared in the input list.

  ### Example
  ```re
  let cmpMod12 = (a, b) => {(a mod 12) - (b mod 12)};
  sortWithInt(cmpMod12, [17, 3, 9, 4, 15, 20]) == [3, 15, 4, 17, 20, 9];
  ```
*/
let sortWithInt: (('a, 'a) => int, list('a)) => list('a);

/**
  `sortBy(f, xs)` sorts the list `xs`, calling `f` every time it needs to
  compare two list elements `a` and `b`.  If `f(a, b)` returns
  ` `less_than `, ` `equal_to `, or ` `greater_than ` depending on the
  relationship between `a` and `b`. (These are values defined in `bs-abstract`.)

  This is a stable sort; equal elements will appear in the output list in the
  same order that they appeared in the input list.

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
  sortBy(cmpMod12, [17, 3, 9, 4, 15, 20]) == [3, 15, 4, 17, 20, 9];
  ```
*/
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a);

/**
  `sort((module M), xs)` sorts the list `xs`, using an `ORD` module to
  compare items in the list.

  This is a stable sort; equal elements will appear in the output list in the
  same order that they appeared in the input list.

  ### Example
  ```re
  sort((module DurationOrder), [(3, 40), (2, 30), (1, 10), (2,20)]) ==
    [(1, 10), (2, 20), (2, 30), (3, 40)];
  ```
*/
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) => list('a);

/**
 In `distinctBy(f, xs)`, the function `f` compares two items
 of the type in the list `xs` and returns `true` if the two
 items are considered to be equal, `false` otherwise.

 `distinctBy()` returns all the items which have unique
 values with respect to `f()`.

  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  distinctBy(eqMod12, [16, 4, 2, 12, 9, 21, 0]) == [16, 2, 12, 9];
  ```
*/
let distinctBy: (('a, 'a) => bool, list('a)) => list('a);

/**
  In `removeFirstBy(f, value, xs)`, the function `f` compares two items
  of the type in the list `xs` and returns `true` if the two
  items are considered to be equal, `false` otherwise.

  `removeFirstBy()` returns a list with the first element that
  is considered equal to `value` with respect to `f()` removed.

  If no such elements exist, the result is the same as the original list.

  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  removeFirstBy(eqMod12, 14, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 12, 9, 21, 0];
  removeFirstBy(eqMod12, 15, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 12, 9, 21, 0];
  ```
*/
let removeFirstBy: (('a, 'a) => bool, 'a, list('a)) => list('a);

/**
  In `removeEachBy(f, value, xs)`, the function `f` compares two items
  of the type in the list `xs` and returns `true` if the two
  items are considered to be equal, `false` otherwise.

  `removeEachBy()` returns a list with every element that
  is considered equal to `value` with respect to `f()` removed.

  If no such elements exist, the result is the same as the original list.

  ### Example
  ```re
  let eqMod12 = (x, y) => {x mod 12 == y mod 12};
  removeEachBy(eqMod12, 12, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 9, 21];
  removeEachBy(eqMod12, 15, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 12, 9, 21, 0];
  ```
*/
let removeEachBy: (('a, 'a) => bool, 'a, list('a)) => list('a);

/*
   `distinct((module M), xs)` returns a list of the unique elements `xs`,
   using the given module, which must be an `EQ` module.

   ### Example
   ```re
   distinct((module ClockEqual), [16, 4, 2, 12, 9, 21, 0]) == [16, 2, 12, 9];
   ```
 */
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a)) => list('a);

/**
  `removeFirst((module M), value, xs)` returns a list with the first element that
  is considered equal to `value` with respect to `module M` removed. The module
  must be an `EQ` module.

 If no elements are equal to the `value`, the result is the same as the original list.

  ### Example
  ```re
  removeFirst((module ClockEqual), 14, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 12, 9, 21, 0];
  removeFirst((module ClockEqual), 15, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 12, 9, 21, 0];
  ```
*/
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);

/**
  `removeEach((module M), value, xs)` returns a list with every element that
  is considered equal to `value` with respect to `module M` removed. The module
  must be an `EQ` module.

  If no elements are equal to the `value`, the result is the same as the original list.

  ### Example
  ```re
  removeEach((module ClockEqual), 12, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 9, 21];
  removeEach((module ClockEqual), 15, [16, 4, 2, 12, 9, 21, 0]) == [16, 4, 2, 12, 9, 21, 0];
  ```
*/
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);

/**
  This submodule contains operations which have been optimized
  to work on lists of strings.
*/

module String: {
  /**
    `String.contains(value, xs)` returns `true` if `value` is one of the
    elements of `xs`, `false` otherwise.

    ### Example
    ```re
    String.contains("bee", ["ant", "bee", "cat"]) == true;
    String.contains("elk", ["ant", "bee", "cat"]) == false;
    ```
  */
  let contains: (string, list(string)) => bool;

  /**
    `String.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.

    ### Example
    ```re
    String.indexOf("bee", ["ant", "bee", "cat"]) == Some(1);
    String.indexOf("elk", ["ant", "bee", "cat"]) == None;
    ```
  */
  let indexOf: (string, list(string)) => option(int);

  /**
    `String.distinct(xs)` returns a list containing the unique
    elements of `xs` in the same order that they occurred in that list.

    ### Example
    ```re
    String.distinct(["noun", "verb", "noun", "adjective", "verb"]) == ["noun", "verb", "adjective"];
    ```
  */
  let distinct: list(string) => list(string);

  /**
    `String.removeFirst(value, xs)` returns a list where the first occurrence
    (if any) of `value` has been removed from `xs`.

    ### Example
    ```re
    String.removeFirst("x", ["a", "x", "b", "x"]) == ["a", "b", "x"];
    String.removeFirst("?", ["a", "x", "b", "x"]) == ["a", "x", "b", "x"];
    ```
  */
  let removeFirst: (string, list(string)) => list(string);

  /**
    `String.removeEach(value, xs)` returns a list where every occurrence
    of `value` has been removed from `xs`.

    ### Example
    ```re
    String.removeEach("x", ["a", "x", "b", "x"]) == ["a", "b"];
    String.removeEach("?", ["a", "x", "b", "x"]) == ["a", "x", "b", "x"];
    ```
  */
  let removeEach: (string, list(string)) => list(string);

  /**
    `String.eq(xs, ys)` returns `true` if the two lists are element-for-element
    equal, `false` otherwise.

    ### Example
    ```re
    String.eq(["a", "b", "c"], ["a", "b", "c"]) == true;
    String.eq(["a", "b", "c"], ["a", "b"]) == false;
    String.eq(["a", "b", "c"], ["a", "b", "d"]) == false;
    String.eq(["a", "b", "c"], ["A", "B", "C"]) == false;
    ```
  */
  let eq: (list(string), list(string)) => bool;

  /**
    `String.min(xs)` returns the element with the minimum lexicographic
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    String.min(["z", "A", "a"]) == Some("A");
    String.min(["bee", "bed", "bet", "beg"]) == Some("bed");
    String.min([ ]) == None;
    ```
  */
  let min: list(string) => option(string);

  /**
    `String.max(xs)` returns the element with the maximum lexicographic
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    String.max(["z", "A", "a"]) == Some("z");
    String.max(["bee", "bed", "bet", "beg"]) == Some("bet");
    String.max([ ]) == None;
    ```
  */
  let max: list(string) => option(string);

  /**
    `String.sort(xs)` sorts the elements of `xs` into lexicographic order.

    ### Example
    ```re
    String.sort(["z", "A", "a"]) == ["A", "a", "z"];
    String.sort(["bee", "bed", "bet", "beg"]) == ["bed", "bee", "beg", "bet"];
    String.sort([ ]) == [ ];
    ```
  */
  let sort: list(string) => list(string);

  /**
    `String.fold(xs)` concatenates the elements of `xs` into a single string.
    Same as `String.join().`

    ### Example
    ```re
    String.fold(["a", "b", "c"]) == "abc";
    String.fold([ ]) == "";
    ```
  */
  let fold: list(string) => string;

  /**
    `String.join(xs)` concatenates the elements of `xs` into a single string.
    Same as `String.fold().`

    ### Example
    ```re
    String.join(["a", "b", "c"]) == "abc";
    String.join([ ]) == "";
    ```
  */
  let join: list(string) => string;

  /**
    `String.intercalate(delim, xs)` returns a concatenated string with
    `delim` inserted  between the elements of `xs`. Same as `String.joinWith().`

    ### Example
    ```re
    String.intercalate("-", ["year", "month", "day"]) == "year-month-day";
    String.intercalate(" and ", ["bread", "wine", "thou"]) == "bread and wine and thou";
    ```
  */
  let intercalate: (string, list(string)) => string;

  /**
    `String.joinWith(delim, xs)` returns a concatenated string with
    `delim` inserted  between the elements of `xs`. Same as `String.intercalate().`

    ### Example
    ```re
    String.joinWith("-", ["year", "month", "day"]) == "year-month-day";
    String.joinWith(" and ", ["bread", "wine", "thou"]) == "bread and wine and thou";
    ```
  */
  let joinWith: (string, list(string)) => string;
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
    Int.contains(101, [100, 101, 102]) == true;
    Int.contains(104, [100, 101, 102]) == false;
    ```
  */
  let contains: (int, list(int)) => bool;

  /**
    `Int.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.

    ### Example
    ```re
    Int.indexOf(101, [100, 101, 102]) == Some(1);
    Int.indexOf(104, [100, 101, 102]) == None;
    ```
  */
  let indexOf: (int, list(int)) => option(int);

  /**
    `Int.distinct(xs)` returns a list containing the unique
    elements of `xs` in the same order that they occurred in that list.

    ### Example
    ```re
    Int.distinct([87, 99, 87, 65, 99]) == [87, 99, 65];
    ```
  */
  let distinct: list(int) => list(int);

  /**
    `Int.removeFirst(value, xs)` returns a list where the first occurrence
    (if any) of `value` has been removed from `xs`.

    ### Example
    ```re
    Int.removeFirst(99, [100, 99, 101, 99]) == [100, 101, 99];
    Int.removeFirst(88, [100, 99, 101, 99]) == [100, 99, 101, 99];
    ```
  */
  let removeFirst: (int, list(int)) => list(int);

  /**
    `Int.removeEach(value, xs)` returns a list where every occurrence
    of `value` has been removed from `xs`.

    ### Example
    ```re
    Int.removeEach(99, [100, 99, 101, 99]) == [100, 101];
    Int.removeEach(88, [100, 99, 101, 99]) == [100, 99, 101, 99];
    ```
  */
  let removeEach: (int, list(int)) => list(int);

  /**
    `Int.eq(xs, ys)` returns `true` if the two lists are element-for-element
    equal, `false` otherwise.

    ### Example
    ```re
    Int.eq([100, 101, 102], [100, 101, 102]) == true;
    Int.eq([100, 101, 102], [100, 101]) == false;
    Int.eq([100, 101, 102], [100, 101, 99]) == false;
    Int.eq([100, 101, 102], [-100, -101, -102]) == false;
    ```
  */
  let eq: (list(int), list(int)) => bool;

  /**
    `Int.min(xs)` returns the element with the minimum
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    Int.min([77, -99, 88, 66]) == Some(-99);
    Int.min([ ]) == None;
    ```
  */
  let min: list(int) => option(int);

  /**
    `Int.max(xs)` returns the element with the maximum
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    Int.max([77, -99, 88, 66]) == Some(88);
    Int.max([ ]) == None;
    ```
  */
  let max: list(int) => option(int);

  /**
    `Int.sort(xs)` sorts the elements of `xs` into ascending order.

    ### Example
    ```re
    Int.sort([77, -99, 88, 66]) == [-99, 66, 77, 88];
    Int.sort([ ]) == [ ];
    ```
  */
  let sort: list(int) => list(int);

  /**
    `Int.sum(xs)` returns the sum of the elements in `xs`.

    ### Example
    ```re
    Int.sum([3, 7, 5]) == 15;
    Int.sum([ ]) == 0;
    ```
  */
  let sum: list(int) => int;

  /**
    `Int.product(xs)` returns the product of the elements in `xs`.

    ### Example
    ```re
    Int.product([3, 7, 5]) == 105;
    Int.product([ ]) == 1;
    ```
  */
  let product: list(int) => int;
};

/**
  This submodule contains operations which have been optimized
  to work on arrays of floats.
*/
module Float: {
  /**
    `Float.contains(value, xs)` returns `true` if `value` is one of the
    elements of `xs`, `false` otherwise.

    ### Example
    ```re
    Float.contains(101.1, [100.0, 101.1, 102.2]) == true;
    Float.contains(104.4, [100.0, 101.1, 102.2]) == false;
    ```
  */
  let contains: (float, list(float)) => bool;

  /**
    `Float.indexOf(value, xs)` returns `Some(index)` where `index`
    is the zero-based position where `value` occurs within `xs`. If
    `value` is not in `xs`, the function returns `None`.

    ### Example
    ```re
    Float.indexOf(101.1, [100.0, 101.1, 102.2]) == Some(1);
    Float.indexOf(104.4, [100.0, 101.1, 102.2]) == None;
    ```
  */
  let indexOf: (float, list(float)) => option(int);

  /**
    `Float.distinct(xs)` returns a list containing the unique
    elements of `xs` in the same order that they occurred in that list.

    ### Example
    ```re
    Float.distinct([87.7, 99.9, 87.7, 65.5, 99.9]) == [87.7, 99.9, 65.5];
    ```
  */
  let distinct: list(float) => list(float);

  /**
    `Float.removeFirst(value, xs)` returns a list where the first occurrence
    (if any) of `value` has been removed from `xs`.

    ### Example
    ```re
    Float.removeFirst(99.9, [100.0, 99.9, 101.1, 99.9]) == [100.0, 101.1, 99.9];
    Float.removeFirst(88.8, [100.0, 99.9, 101.1, 99.9]) == [100.0, 99.9, 101.1, 99.9];
    ```
  */
  let removeFirst: (float, list(float)) => list(float);

  /**
    `Float.removeEach(value, xs)` returns a list where every occurrence
    of `value` has been removed from `xs`.

    ### Example
    ```re
    Float.removeEach(99.9, [100.0, 99.9, 101.1, 99.9]) == [100.0, 101.1];
    Float.removeEach(88.8, [100.0, 99.9, 101.1, 99.9]) == [100.0, 99.9, 101.1, 99.9];
    ```
  */
  let removeEach: (float, list(float)) => list(float);

  /**
    `Float.eq(xs, ys)` returns `true` if the two lists are element-for-element
    equal, `false` otherwise.

    ### Example
    ```re
    Float.eq([100.0, 101.1, 102.2], [100.0, 101.1, 102.2]) == true;
    Float.eq([100.0, 101.1, 102.2], [100.0, 101.1]) == false;
    Float.eq([100.0, 101.1, 102.2], [100.0, 101.1, 99.9]) == false;
    Float.eq([100.0, 101.1, 102.2], [-100.0, -101.1, -102.2]) == false;
    ```
  */
  let eq: (list(float), list(float)) => bool;

  /**
    `Float.min(xs)` returns the element with the minimum
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    Float.min([77.7, -99.9, 88.8, 66.6]) == Some(-99.9);
    Float.min([ ]) == None;
    ```
  */
  let min: list(float) => option(float);

  /**
    `Float.max(xs)` returns the element with the maximum
    value as `Some(value)`; returns `None` if given an empty list.

    ### Example
    ```re
    Float.max([77.7, -99.9, 88.8, 66.6]) == Some(88.8);
    Float.max([ ]) == None;
    ```
  */
  let max: list(float) => option(float);

  /**
    `Float.sort(xs)` sorts the elements of `xs` floato ascending order.

    ### Example
    ```re
    Float.sort([77.7, -99.9, 88.8, 66.6]) == [-99.9, 66.6, 77.7, 88.8];
    Float.sort([ ]) == [ ];
    ```
  */
  let sort: list(float) => list(float);

  /**
    `Float.sum(xs)` returns the sum of the elements in `xs`.

    ### Example
    ```re
    Float.sum([3.0, 7.5, 5.5]) == 16.0;
    Float.sum([ ]) == 0.0;
    ```
  */
  let sum: list(float) => float;

  /**
    `Float.product(xs)` returns the product of the elements in `xs`.

    ### Example
    ```re
    Float.product([3.0, 7.5, 5.5]) == 123.75;
    Float.product([ ]) == 1.0;
    ```
  */
  let product: list(float) => float;
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
    Option.traverse(evenValue, [100, 102, 104, 106]) == Some([100, 102, 104, 106]);
    Option.traverse(evenValue, [100, 101, 102, 103]) == None;
    Option.traverse(evenValue, [ ]) == Some([ ]);
    ```
  */
  let traverse: ('a => option('a), list('a)) => option(list('a));

  /**
    `Option.sequence(opts)`, takes a list of `option` types as its argument.
    If all the elements of `opts` are `Some(value)`, then the return value is
    `Some(xs)`, where each element in `opts` has been unwrapped from its `Some()`.

    If any element of `opts` is `None`, then the return value is `None`.


    ### Example
    ```re
    Option.sequence([Some("a"), Some("b"), Some("c")]) == Some(["a", "b", "c"]);
    Option.sequence([Some("a"), None, Some("c")]) == None;
    Option.sequence([ ]) == Some([ ]);
    ```
  */
  let sequence: list(option('a)) => option(list('a));
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
    Result.traverse(evenValue, [100, 102, 104, 106]) == Belt.Result.Ok([100, 102, 104, 106]);
    Result.traverse(evenValue, [100, 101, 102, 103]) == Belt.Result.Error("101 is odd");
    Result.traverse(evenValue, [ ]) == Belt.Result.Ok([ ]);
    ```
  */
  let traverse:
    ('a => Belt.Result.t('b, 'c), list('a)) => Belt.Result.t(list('b), 'c);

  /**
    `Result.sequence(results)`, takes a list of `Belt.Result.t` types as its argument.
    If all the elements of `results` are `Ok(value)`, then the return value is
    `Ok(xs)`, where each element in `results` has been unwrapped from its `Belt.Result.Ok()`.

    If any element of `results` is `Belt.Error(err)`, then the return value is the
    first such value encountered.


    ### Example
    ```re
    Result.sequence([Belt.Result.Ok("a"), Belt.Result.Ok("b"), Belt.Result.Ok("c")]) ==
      Belt.Result.Ok(["a", "b", "c"]);
    Result.sequence([Belt.Result.Ok("a"), Belt.Result.Error(1),
      Belt.Result.Ok("c"), Belt.Result.Error(2)]) == Belt.Result.Error(1);
    Result.sequence([ ]) == Belt.Result.Ok([ ]);
    ```
  */
  let sequence: list(Belt.Result.t('a, 'c)) => Belt.Result.t(list('a), 'c);
};

module Validation: {
  module Traversable:
    (
      Errors: BsAbstract.Interface.SEMIGROUP_ANY,
      Error: BsAbstract.Interface.TYPE,
    ) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.WithErrors(Errors)(Error).Applicative.t('a);
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  module TraversableWithErrorsAsList:
    (Error: BsAbstract.Interface.TYPE) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.WithErrors(Relude_List_Instances.SemigroupAny)(Error).Applicative.t(
          'a,
        );
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  module TraversableWithErrorsAsListOfStrings: {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
    let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
    module Fold_Map:
      (M: BsAbstract.Interface.MONOID) =>
       {let fold_map: ('a => M.t, t('a)) => M.t;};
    module Fold_Map_Any:
      (M: BsAbstract.Interface.MONOID_ANY) =>
       {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
    module Fold_Map_Plus:
      (P: BsAbstract.Interface.PLUS) =>
       {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
    type applicative_t('a) =
      Relude_Validation.t('a, Relude_List_Instances.SemigroupAny.t(string));
    let traverse: ('a => applicative_t('b), t('a)) => applicative_t(t('b));
    let sequence: t(applicative_t('a)) => applicative_t(t('a));
  };

  module TraversableWithErrorsAsNonEmptyList:
    (Error: BsAbstract.Interface.TYPE) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.WithErrors(Relude_NonEmpty.List.SemigroupAny)(Error).Applicative.t(
          'a,
        );
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  let traverse:
    ('a => Belt.Result.t('b, 'e), list('a)) =>
    Relude_Validation.t(list('b), Relude_NonEmpty.List.t('e));
};

/**
  This submodule provides infix operators for common array
  operations. You can use these operators by opening the
  module:

  ```re
  open Relude.List.Infix;
  ```
*/
module Infix: {
  /**
    In `xs >>= f`, `f` is a function that takes an element
    of the type in `xs` and returns a list. The result of `>>=`
    is the concatenation of all the lists produced by applying
    `f()` to the elements of `xs`. (Same as `bind()`.)

    ### Example
    ```re
    let f = (x) => [ x - 5, x + 5 ];
    [100, 101, 102] >>= f == [95, 105, 96, 106, 97, 107];
    [ ] >>= f == [ ];
  ```
  */
  let (>>=): (list('a), 'a => list('b)) => list('b);

  /**
    In `f =<< xs`, `f` is a function that takes an element
    of the type in `xs` and returns a list. The result of `=<<`
    is the concatenation of all the lists produced by applying
    `f()` to the elements of `xs`. (Same as `flatMap()`.)

    ### Example
    ```re
    let f = (x) => [ x - 5, x + 5 ];
    f =<< [100, 101, 102] == [95, 105, 96, 106, 97, 107];
    f =<< [ ] == [ ];
    ```
  */
  let (=<<): ('a => list('b), list('a)) => list('b);

  /**
    `f >=> g` takes two functions, each of which
    takes a single item and returns a list. The `>=>` operator
    returns a new function that takes a single item of the type
    required for `f()` and returns a list of the type produced
    by `g()`.

    ### Example:
    ```re
    let f = (s) => {[Js.String.length(s) - 1, Js.String.length(s) + 1]};
    let g = (n) => {[float_of_int(n) -. 0.5, float_of_int(n) +. 0.5]};
    let h = (f >=> g);
    h("ReasonML") == [ 6.5, 7.5, 8.5, 9.5 ];
    ```
  */
  let (>=>): ('a => list('b), 'b => list('c), 'a) => list('c);

  /**
    `f <=< g` takes two functions, each of which
    takes a single item and returns a list. The `<=<` operator
    returns a new function that takes a single item of the type
    required for `g()` and returns a list of the type produced
    by `f()`.

    ### Example:
    ```re
    let f = (n) => {[float_of_int(n) -. 0.5, float_of_int(n) +. 0.5]};
    let g = (s) => {[Js.String.length(s) - 1, Js.String.length(s) + 1]};
    let h = (f <=< g);
    h("ReasonML") == [ 6.5, 7.5, 8.5, 9.5 ];
    ```
  */

  let (<=<): ('a => list('b), 'c => list('a), 'c) => list('b);

  /**
    `<|>` takes two lists with elements of the same type and concatenates them
    in order.

    ### Example
    ```re
    ["a", "b"] <|> [ ] <|> ["c"] <|> ["d", "e", "f"] ==
      ["a", "b", "c", "d", "e", "f"];
    ```
  */
  let (<|>): (list('a), list('a)) => list('a);

  /**
    `f <$> xs` is a shorthand for `map(f, xs)`; it takes a function and a list
    and returns a list with the function applied to each element of `xs`.

    ### Example
    ```re
    let f = (x) => {Js.String.length(x)};
    f <$> ["ReasonML", "OCaml"] == [8, 5];
    ```
  */
  let (<$>): ('a => 'b, list('a)) => list('b);

  /**
    `xs <#> f` is a shorthand for `map(f, xs)`; it takes a function and a list
    and returns a list with the function applied to each element of `xs`.

    ### Example
    ```re
    let f = (x) => {Js.String.length(x)};
    ["ReasonML", "OCaml"] <#> f == [8, 5];
    ```
  */
  let (<#>): (list('a), 'a => 'b) => list('b);

  /**
    `fs <*> xs` takes a list of functions and a list of values and creates
    a list whose contents are the result of applying the first function of `fs`
    to all the elements of `xs`, the second function of
    `fs` to all the elements of `xs`, and so on. All the functions in `fs` must
    have the same result type.

    ### Example
    ```re
    let square = (x) => {x * x};
    let cube = (x) => {x * x * x};
    [square, cube] <*> [10, 11, 12] == [100, 121, 144, 1000, 1331, 1728];
    ```
  */
  let (<*>): (list('a => 'b), list('a)) => list('b);
  let ( <* ): (list('a), list('b)) => list('a);
  let ( *> ): (list('a), list('b)) => list('b);
};