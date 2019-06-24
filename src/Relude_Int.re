/**
  `min(a, b)` returns the lesser of `a` and `b`.
*/
let min: (int, int) => int =
  (a, b) =>
    if (a < b) {
      a;
    } else {
      b;
    };

/**
  `min(a, b)` returns the greater of `a` and `b`.
*/
let max: (int, int) => int =
  (a, b) =>
    if (a > b) {
      a;
    } else {
      b;
    };

/**
  `rangeAsList(n, m)` returns a list of integers
  `[n, n + 1, .. m - 1]`.

  ### Example
  ```re
  rangeAsList(10, 15) == [10, 11, 12, 13, 14];
  rangeAsList(10, 10) == [];
  rangeAsList(15, 10) == [];
  ```
*/
let rec rangeAsList = (start: int, end_: int): list(int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...rangeAsList(start + 1, end_)];
  };

/**
  `rangeAsArray(n, m)` returns an array of integers
  `[|n, n + 1, .. m - 1|]`.

  ### Example
  ```re
  rangeAsArray(10, 15) == [|10, 11, 12, 13, 14|];
  rangeAsArray(10, 10) == [||];
  rangeAsArray(15, 10) == [||];
  ```
*/
let rec rangeAsArray: (int, int) => array(int) =
  (start, end_) =>
    if (start >= end_) {
      [||];
    } else {
      Belt.Array.concat([|start|], rangeAsArray(start + 1, end_));
    };

/**
  `eq(a, b)` returns `true` if the arguments are equal, `false` otherwise.
*/
let eq: (int, int) => bool = BsAbstract.Int.Eq.eq;

module Eq: BsAbstract.Interface.EQ with type t = int = {
  type t = int;
  let eq = eq;
};

/**
  `compare(a, b)` returns ` `less_than ` if `a` is less than `b`,
  ` `equal_to ` if `a` equals `b`, and ` `greater_than ` if `a`
  is greater than `b`. The result is of type `BsAbstract.Interface.ordering`.

  ### Example
  ```re
  compare(3, 5) == `less_than;
  compare(3, 3) == `equal_to;
  compare(5, 3) == `greater_than;
  ```
*/
let compare: (int, int) => BsAbstract.Interface.ordering = BsAbstract.Int.Ord.compare;

module Ord: BsAbstract.Interface.ORD with type t = int = {
  include Eq;
  let compare = compare;
};

module Map = Relude_Map.MakeFromOrderable(Ord);
module Set = Relude_Set.MakeFromOrderable(Ord);

/**
  `compareAsInt(a, b)` returns -1 if `a` is less than `b`,
  0 if `a` equals `b`, and 1 if `a`
  is greater than `b`.

  ### Example
  ```re
  compareAsInt(3, 5) == -1;
  compareAsInt(3, 3) == 0;
  compareAsInt(5, 3) == 1;
  ```
*/
let compareAsInt: (int, int) => int =
  (a, b) => compare(a, b) |> Relude_Ordering.toInt;

/**
  `toString(n)` returns the string representation of `n`. Note
  that this may not be in the same form as you wrote it in the
  source code.

  ### Example
  ```re
  toString(57) == "57";
  toString(0x1a) == "26";
  ```
*/
let show: int => string = string_of_int;
let toString = show;

module Show: BsAbstract.Interface.SHOW with type t = int = {
  type t = int;
  let show = show;
};

/**
  `fromString(s)` returns `Some(n)` if the given string represents
  a valid integer, `None` otherwise.

  ### Example
  ```re
  fromString("57") == Some(57);
  fromString("0x1a") == Some(26);
  fromString("57.3") == None;
  fromString("3dozen") == None;
  ```
*/
let fromString: string => option(int) =
  v =>
    try (Some(int_of_string(v))) {
    | _ => None
    };

module Additive = BsAbstract.Int.Additive;

module Multiplicative = BsAbstract.Int.Multiplicative;

module Subtractive = BsAbstract.Int.Subtractive;

module Divisive = BsAbstract.Int.Divisive;

module Infix = BsAbstract.Int.Infix;
