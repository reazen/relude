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
 * The integer value 0
 */
let zero = 0;

/**
 * The integer value 1
 */
let one = 1;

/**
 * Finds the sum of two integers
 */
let add: (int, int) => int = (+);

/**
 * Finds the difference of two integers
 */
let subtract: (int, int) => int = (-);

/**
 * Finds the product of two integers
 */
let multiply: (int, int) => int = ( * );

/**
 * Finds the quotient of an integer division
 */
let divide: (int, int) => int = (/);

/**
 * Finds the remainder of an integer division
 */
let modulo: (int, int) => int = (a, b) => a mod b;

/**
 * Finds the quotient and remainder of an integer division
 */
let divideWithModulo: (int, int) => (int, int) =
  (a, b) => (divide(a, b), modulo(a, b));

/**
 * Converts two int values to floats, then performs a float division
 */
let divideAsFloat: (int, int) => float =
  (a, b) => float_of_int(a) /. float_of_int(b);

/**
 * The top bound (max value) of 32 bit int
 */
let top = Js.Int.max;

/**
 * The bottom bound (min value) of 32 bit int
 */
let bottom = Js.Int.min;

/**
 * Degree finds the smaller of the absolute value of the given in, or the max int value
 */
let degree: int => int = a => Js.Math.min_int(Js.Math.abs_int(a), top);

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
      Relude_Array_Instances.concat(
        [|start|],
        rangeAsArray(start + 1, end_),
      );
    };

/**
  `eq(a, b)` returns `true` if the arguments are equal, `false` otherwise.
*/
let eq: (int, int) => bool = BsBastet.Int.Eq.eq;

module Eq: BsBastet.Interface.EQ with type t = int = {
  type t = int;
  let eq = eq;
};
include Relude_Extensions_Eq.EqExtensions(Eq);

/**
  `compare(a, b)` returns ` `less_than ` if `a` is less than `b`,
  ` `equal_to ` if `a` equals `b`, and ` `greater_than ` if `a`
  is greater than `b`. The result is of type `BsBastet.Interface.ordering`.

  ### Example
  ```re
  compare(3, 5) == `less_than;
  compare(3, 3) == `equal_to;
  compare(5, 3) == `greater_than;
  ```
*/
let compare: (int, int) => BsBastet.Interface.ordering = BsBastet.Int.Ord.compare;

module Ord: BsBastet.Interface.ORD with type t = int = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Bounded: BsBastet.Interface.BOUNDED with type t = int = {
  include Ord;
  let top = top;
  let bottom = bottom;
};
include Relude_Extensions_Bounded.BoundedExtensions(Bounded);

module Enum: Relude_Interface.ENUM with type t = int = {
  include Ord;
  let pred = i =>
    if (i > bottom) {
      Some(i - 1);
    } else {
      None;
    };
  let succ = i =>
    if (i < top) {
      Some(i + 1);
    } else {
      None;
    };
};
include Relude_Extensions_Enum.EnumExtensions(Enum);

// Not a BoundedEnum b/c cardinality would be larger than an signed int can represent

module Semiring: BsBastet.Interface.SEMIRING with type t = int = {
  type t = int;
  let zero = zero;
  let one = one;
  let add = add;
  let multiply = multiply;
};
include Relude_Extensions_Semiring.SemiringExtensions(Semiring);

module Ring: BsBastet.Interface.RING with type t = int = {
  include Semiring;
  let subtract = subtract;
};
include Relude_Extensions_Ring.RingExtensions(Ring);
include OrdRingExtensions(Ring);

module EuclideanRing: BsBastet.Interface.EUCLIDEAN_RING with type t = int = {
  include Ring;
  let divide = divide;
  let modulo = modulo;
  let degree = degree;
};

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

module Show: BsBastet.Interface.SHOW with type t = int = {
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
    try(Some(int_of_string(v))) {
    | _ => None
    };

module Additive = {
  include BsBastet.Int.Additive;
};

module Multiplicative = {
  include BsBastet.Int.Multiplicative;
};

module Subtractive = {
  include BsBastet.Int.Subtractive;
};

module Divisive = {
  include BsBastet.Int.Divisive;
};

module Infix = {
  include BsBastet.Int.Infix;
  include Relude_Extensions_Eq.EqInfix(Eq);
  include Relude_Extensions_Ord.OrdInfix(Ord);
};
