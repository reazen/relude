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
  is the concatenation of all the list produced by applying
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
  foldLeft((acc, item) => acc + item, 2, [ ]) == 2;
  ```
*/
let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b;

/**
  `foldRight(f, init, xs)` accumulates a value. Starting with `init`,
  as the initial value of the accumulator, `foldRight` applies function
  `f` to the last element in the list `xs` and the accumulator. The result
  becomes the new value of the accumulator, and `f` is applied to that value
  and the preceding element in `xs`. This process continues until all elements in
  `xs` are processed. The final value of the accumulator is returned.

  ## Example
  ```re
  foldRight((item, acc) => append(item, acc), [ ], [1, 2, 3]) == [3, 2, 1];
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
  
  If given an empty list, `minBy()` returns `None`.
  
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
  
  If given an empty array, `maxBy()` returns `None`.
  
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
  the given module. The module you provide must define the following:
  
  - a type specification
  - an `append()` function which takes two items of the type and appends them
    to one another
  - an “empty” element
  
  Appending the empty element and an item must be commutative;
  `append(x, empty) == append(empty, x)`
  
  ### Example
  ```re
  module CapString = {
    type t = string;
    let append(a, b) = Js.String.toUpperCase(a) ++ Js.String.toUpperCase(b);
    let empty = "";
  };
  
  fold((module CapString), ["it ", "works!"]) == "IT WORKS!";
  ```
*/
let fold:
  ((module BsAbstract.Interface.MONOID with type t = 'a), list('a)) => 'a;

/**
  `intercalate((module M), delim, xs)` concatenates the elements of `xs` as specified by
  the given module, with `delim` between all the elements. The module you provide
  must define:
  
  - a type `t`
  - an `append()` function which takes two items of the type and appends them
    to one another
  - an “empty” element
  
  Appending the empty element and an item must be commutative;
  `append(x, empty) == append(empty, x)`
  
  ### Example
  ```re
  module LowerString = {
    type t = string;
    let append(a, b) = Js.String.toLowerCase(a) ++ Js.String.toLowerCase(b);
    let empty = "";
  };
  
  intercalate((module LowerString), "--", ["2019", "MAY", "5"]) == "2019--may--5";
  ```
*/
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a, list('a)) => 'a;

/**
  `contains((module M), val, xs)` returns `true` if any element of `xs` equals
  `val`, as determined by the module. It returns `false` if no element equals
  `val`.  The module you provide must define:
  
  - a type `t`
  - a function `eq()` which takes two items of type `t` and returns `true` if
    they are to be considered equal, `false` otherwise.
    
  ### Example
  ```re
  module Pair = {
    type t = (string, int);
    let eq = ((pair1First, pair1Second), (pair2First, pair2Second)) => {
      pair1First == pair2First && pair1Second == pair2Second
    };
  };
  
  contains((module Pair), ("c", 5), [("a", 1), ("b", 3), ("c", 5), ("d", 7)]) == true;
  contains((module Pair), ("c", 5), [("c", 1), ("b", 5)]) == false;
  contains((module Pair), ("c", 5), [ ]) == false;
  ```
*/
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) => bool;
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  option(int);
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);

/**
 * The following functions come from List's membership in IsoArray
 */
let fromArray: array('a) => list('a);
let toArray: list('a) => array('a);

/**
 * The following functions and modules care about properties of the inner type
 */

module Traversable:
  (BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE;

let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b);
let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b);

let eqBy: (('a, 'a) => bool, list('a), list('a)) => bool;
module Eq: (BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a), list('a)) =>
  bool;

let showBy: ('a => string, list('a)) => string;
module Show: (BsAbstract.Interface.SHOW) => BsAbstract.Interface.SHOW;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), list('a)) => string;

/**
 * The following functions are list-specific. They may use
 */
let mapWithIndex: (('a, int) => 'b, list('a)) => list('b);
let cons: ('a, list('a)) => list('a);
let prepend: ('a, list('a)) => list('a);
let uncons: list('a) => option(('a, list('a)));
let append: ('a, list('a)) => list('a);
let repeat: (int, 'a) => list('a);
let makeWithIndex: (int, int => 'a) => list('a);
let reverse: list('a) => list('a);
let shuffle: list('a) => list('a);
let isEmpty: list('a) => bool;
let isNotEmpty: list('a) => bool;
let at: (int, list('a)) => option('a);
let head: list('a) => option('a);
let tail: list('a) => option(list('a));
let tailOrEmpty: list('a) => list('a);
let init: list('a) => option(list('a));
let last: list('a) => option('a);
let take: (int, list('a)) => list('a);
let takeExactly: (int, list('a)) => option(list('a));
let takeWhile: ('a => bool, list('a)) => list('a);
let drop: (int, list('a)) => list('a);
let dropExactly: (int, list('a)) => option(list('a));
let dropWhile: ('a => bool, list('a)) => list('a);
let filter: ('a => bool, list('a)) => list('a);
let filterWithIndex: (('a, int) => bool, list('a)) => list('a);
let mapOption: ('a => option('b), list('a)) => list('b);
let catOptions: list(option('a)) => list('a);
let partition: ('a => bool, list('a)) => (list('a), list('a));
let splitAt: (int, list('a)) => option((list('a), list('a)));
let prependToAll: ('a, list('a)) => list('a);
let intersperse: ('a, list('a)) => list('a);
let replicate: (int, list('a)) => list('a);
let zip: (list('a), list('b)) => list(('a, 'b));
let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c);
let zipWithIndex: list('a) => list(('a, int));
let unzip: list(('a, 'b)) => (list('a), list('b));
let sortWithInt: (('a, 'a) => int, list('a)) => list('a);
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) => list('a);
let distinctBy: (('a, 'a) => bool, list('a)) => list('a);
let removeFirstBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let removeEachBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a)) => list('a);
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);

module String: {
  let contains: (string, list(string)) => bool;
  let indexOf: (string, list(string)) => option(int);
  let distinct: list(string) => list(string);
  let removeFirst: (string, list(string)) => list(string);
  let removeEach: (string, list(string)) => list(string);
  let eq: (list(string), list(string)) => bool;
  let min: list(string) => option(string);
  let max: list(string) => option(string);
  let sort: list(string) => list(string);
  let fold: list(string) => string;
  let join: list(string) => string;
  let intercalate: (string, list(string)) => string;
  let joinWith: (string, list(string)) => string;
};

module Int: {
  let contains: (int, list(int)) => bool;
  let indexOf: (int, list(int)) => option(int);
  let distinct: list(int) => list(int);
  let removeFirst: (int, list(int)) => list(int);
  let removeEach: (int, list(int)) => list(int);
  let eq: (list(int), list(int)) => bool;
  let min: list(int) => option(int);
  let max: list(int) => option(int);
  let sort: list(int) => list(int);
  let sum: list(int) => int;
  let product: list(int) => int;
};

module Float: {
  let contains: (float, list(float)) => bool;
  let indexOf: (float, list(float)) => option(int);
  let distinct: list(float) => list(float);
  let removeFirst: (float, list(float)) => list(float);
  let removeEach: (float, list(float)) => list(float);
  let eq: (list(float), list(float)) => bool;
  let min: list(float) => option(float);
  let max: list(float) => option(float);
  let sort: list(float) => list(float);
  let sum: list(float) => float;
  let product: list(float) => float;
};

module Option: {
  let traverse: ('a => option('a), list('a)) => option(list('a));
  let sequence: list(option('a)) => option(list('a));
};

module Result: {
  let traverse:
    ('a => Belt.Result.t('b, 'c), list('a)) => Belt.Result.t(list('b), 'c);

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
        Relude_Validation.Applicative(Errors)(Error).t('a);
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
        Relude_Validation.Applicative(Relude_List_Types.SemigroupAny)(Error).t(
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
      Relude_Validation.t('a, Relude_List_Types.SemigroupAny.t(string));
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
        Relude_Validation.Applicative(Relude_NonEmpty.List.SemigroupAny)(Error).t(
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

module Infix: {
  let (>>=): (list('a), 'a => list('b)) => list('b);
  let (=<<): ('a => list('b), list('a)) => list('b);
  let (>=>): ('a => list('b), 'b => list('c), 'a) => list('c);
  let (<=<): ('a => list('b), 'c => list('a), 'c) => list('b);
  let (<>): (list('a), list('a)) => list('a);
  let (<$>): ('a => 'b, list('a)) => list('b);
  let (<#>): (list('a), 'a => 'b) => list('b);
  let (<*>): (list('a => 'b), list('a)) => list('b);
  let ( <* ): (list('a), list('b)) => list('a);
  let ( *> ): (list('a), list('b)) => list('b);
};
