/**
 * `toFloat` returns the floating-point representation of the int
 */
let toFloat = float_of_int;

/**
 * `fromFloat` turns a float into an int by dropping the fractional part.
 *
 * Floats that can't be represented as an int (e.g. `infinity`, `nan`) will
 * return 0.
 */
let fromFloat = int_of_float;

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
include Relude_Extensions_Eq.EqExtensions(Eq);

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
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Semiring: BsAbstract.Interface.SEMIRING with type t = int = {
  type t = int;
  let zero = 0;
  let one = 1;
  let add = (a, b) => a + b;
  let multiply = (a, b) => a * b;
};
include Relude_Extensions_Semiring.SemiringExtensions(Semiring);

module Ring: BsAbstract.Interface.RING with type t = int = {
  include Semiring;
  let subtract = (a, b) => a - b;
};
include Relude_Extensions_Ring.RingExtensions(Ring);
include OrdRingExtensions(Ring);

/**
 * Map module with an int key
 */
module Map = Relude_Map.WithOrd(Ord);

/**
 * Set module for ints
 */
module Set = Relude_Set.WithOrd(Ord);

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

/**
 * Alias for `show`
 */
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

module Additive = {
  include BsAbstract.Int.Additive;
};

module Multiplicative = {
  include BsAbstract.Int.Multiplicative;
};

module Subtractive = {
  include BsAbstract.Int.Subtractive;
};

module Divisive = {
  include BsAbstract.Int.Divisive;
};

module Infix = {
  include BsAbstract.Int.Infix;
};