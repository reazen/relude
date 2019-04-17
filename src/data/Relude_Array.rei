/**
  `length(xs)` returns the number of items in `xs`.

  ## Example
  ```re
  length([|"a", "b", "c"|]) == 3;
  length([| |]) == 0;
  ```
*/
let length: array('a) => int;

/**
  `isEmpty(xs) returns `true` if `xs` is the empty array `[| |]`; returns `false` otherwise.
*/
let isEmpty: array('a) => bool;

/**
  `isNotEmpty(xs) returns `true` if `xs` is not the empty array `[| |]`; returns `false` otherwise.
*/
let isNotEmpty: array('a) => bool;

/**
  `empty xs` is a new, empty array.
*/
let empty: array('a);

/**
  `pure(item)` returns an array containing the given item.

  ## Example
  ```re
  pure("single") == [|"single"|];
  ```
*/
let pure: 'a => array('a);

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
  `cons(x, xs)` returns a new array with value `x` at the beginning.

  ## Example
  ```re
  cons(99, [|100, 101|]) == [|99, 100, 101|];
  cons(99, [| |]) == [|99|];
  ```
*/
let cons: ('a, array('a)) => array('a);

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
  Same as `cons`
*/
let prepend: ('a, array('a)) => array('a);

/**
  `append(x, xs)` adds the value `x` at the end of array `xs`.

  ## Example
  ```re
  append(999, [|100, 101, 102|]) == [|100, 102, 103, 999|];
  append(999, [| |]) == [|999|];
  ```
*/
let append: ('a, array('a)) => array('a);

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
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b);
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b);
let at: (int, array('a)) => option('a);
let setAt: (int, 'a, array('a)) => option(array('a));
let head: array('a) => option('a);
let tail: array('a) => option(array('a));
let tailOrEmpty: array('a) => array('a);
let init: array('a) => option(array('a));
let last: array('a) => option('a);
let take: (int, array('a)) => option(array('a));
let takeUpTo: (int, array('a)) => array('a);
let takeWhile: ('a => bool, array('a)) => array('a);
let drop: (int, array('a)) => option(array('a));
let dropUpTo: (int, array('a)) => array('a);
let dropWhile: ('a => bool, array('a)) => array('a);
let filter: ('a => bool, array('a)) => array('a);
let filterWithIndex: (('a, int) => bool, array('a)) => array('a);
let find: ('a => bool, array('a)) => option('a);
let findWithIndex: (('a, int) => bool, array('a)) => option('a);
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) => bool;
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  option(int);
let partition: ('a => bool, array('a)) => (array('a), array('a));
let splitAt: (int, array('a)) => option((array('a), array('a)));
let prependToAll: ('a, array('a)) => array('a);
let intersperse: ('a, array('a)) => array('a);
let replicate: (int, array('a)) => array('a);
let zip: (array('a), array('b)) => array(('a, 'b));
let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let zipWithIndex: array('a) => array(('a, int));
let unzip: array(('a, 'b)) => (array('a), array('b));
let sortWithInt: (('a, 'a) => int, array('a)) => array('a);
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  array('a);
let shuffleInPlace: array('a) => array('a);
let shuffle: array('a) => array('a);
let reverse: array('a) => array('a);
let any: ('a => bool, array('a)) => bool;
let containsBy: (('a, 'a) => bool, 'a, array('a)) => bool;
let indexOfBy: (('a, 'a) => bool, 'a, array('a)) => option(int);
let all: ('a => bool, array('a)) => bool;
let distinctBy: (('a, 'a) => bool, array('a)) => array('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a)) => array('a);
let map: ('a => 'b, array('a)) => array('b);
let mapWithIndex: (('a, int) => 'b, array('a)) => array('b);
let forEach: ('a => unit, array('a)) => unit;
let forEachWithIndex: (('a, int) => unit, array('a)) => unit;
let apply: (array('a => 'b), array('a)) => array('b);
let bind: (array('a), 'a => array('b)) => array('b);
let flatMap: ('a => array('b), array('a)) => array('b);
let flatten: array(array('a)) => array('a);
let fromList: list('a) => array('a);
let toList: array('a) => list('a);
let mkString: (string, array(string)) => string;
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
module SemigroupAny: {
  type t('a) = array('a);
  let append: (t('a), t('a)) => t('a);
};
module MonoidAny: {
  type t('a) = array('a);
  let append: (t('a), t('a)) => t('a);
  let empty: t('a);
};
module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = array('a);
module Apply: BsAbstract.Interface.APPLY with type t('a) = array('a);
module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = array('a);
module Monad: BsAbstract.Interface.MONAD with type t('a) = array('a);
module Alt: BsAbstract.Interface.ALT with type t('a) = array('a);
module Plus: BsAbstract.Interface.PLUS with type t('a) = array('a);
module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = array('a);
module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = array('a);
module Traversable: BsAbstract.Array.TRAVERSABLE_F;
module Ord: (BsAbstract.Interface.ORD) => BsAbstract.Interface.ORD;
module Invariant: BsAbstract.Interface.INVARIANT with type t('a) = array('a);
module MonadZero:
  BsAbstract.Interface.MONAD_ZERO with type t('a) = array('a);
module MonadPlus:
  BsAbstract.Interface.MONAD_PLUS with type t('a) = array('a);
module Extend: BsAbstract.Interface.EXTEND with type t('a) = array('a);
module Infix: {
  let (>>=): (array('a), 'a => array('b)) => array('b);
  let (=<<): ('a => array('b), array('a)) => array('b);
  let (>=>): ('a => array('b), 'b => array('c), 'a) => array('c);
  let (<=<): ('a => array('b), 'c => array('a), 'c) => array('b);
  let (<|>): (array('a), array('a)) => array('a);
  let (<$>): ('a => 'b, array('a)) => array('b);
  let (<#>): (array('a), 'a => 'b) => array('b);
  let (<*>): (array('a => 'b), array('a)) => array('b);
};

module IsoList: {
  type t('a) = array('a);
  let fromList: list('a) => t('a);
  let toList: t('a) => list('a);
};

module String: {
  let eq: (array(string), array(string)) => bool;
  let contains: (string, array(string)) => bool;
  let indexOf: (string, array(string)) => option(int);
  let distinct: array(string) => array(string);
  let remove: (string, array(string)) => array(string);
  let removeEach: (string, array(string)) => array(string);
  let fold: array(string) => string;
  let join: array(string) => string;
  let intercalate: (string, array(string)) => string;
  let joinWith: (string, array(string)) => string;
  let sort: array(string) => array(string);
};

module Int: {
  let eq: (array(int), array(int)) => bool;
  let contains: (int, array(int)) => bool;
  let indexOf: (int, array(int)) => option(int);
  let distinct: array(int) => array(int);
  let remove: (int, array(int)) => array(int);
  let removeEach: (int, array(int)) => array(int);
  let sort: array(int) => array(int);
  let sum: array(int) => int;
  let product: array(int) => int;
};

module Float: {
  let eq: (array(float), array(float)) => bool;
  let contains: (float, array(float)) => bool;
  let indexOf: (float, array(float)) => option(int);
  let distinct: array(float) => array(float);
  let remove: (float, array(float)) => array(float);
  let removeEach: (float, array(float)) => array(float);
  let sort: array(float) => array(float);
  let sum: array(float) => float;
  let product: array(float) => float;
};

module Option: {
  let traverse: ('a => option('a), array('a)) => option(array('a));
  let sequence: array(option('a)) => option(array('a));
};

module Result: {
  let traverse:
    ('a => Belt.Result.t('b, 'c), array('a)) => Belt.Result.t(array('b), 'c);

  let sequence: array(Belt.Result.t('a, 'c)) => Belt.Result.t(array('a), 'c);
};
