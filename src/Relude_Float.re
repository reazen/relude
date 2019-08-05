let eq: (float, float) => bool = (a, b) => a == b;

module Eq: BsAbstract.Interface.EQ with type t = float = {
  type t = float;
  let eq = eq;
};

let compare: (float, float) => BsAbstract.Interface.ordering = BsAbstract.Float.Ord.compare;

module Ord: BsAbstract.Interface.ORD with type t = float = {
  include Eq;
  let compare = compare;
};

module Semiring: BsAbstract.Interface.SEMIRING with type t = float = {
  type t = float;
  let zero = 0.0;
  let one = 1.0;
  let add = (a, b) => a +. b;
  let multiply = (a, b) => a *. b;
};

module Ring: BsAbstract.Interface.RING with type t = float = {
  include Semiring;
  let subtract = (a, b) => a -. b;
};

include Relude_Extensions_Ord.Make(Ord);
include Relude_Extensions_Ord.MakeWithRing(Ord, Ring);

/**
  `approximatelyEqual(~tolerance=t, x, y)` returns `true` if `x` and `y`
  are within `t` of one another, `false` otherwise. The `~tolerance`
  must be non-negative.

  ### Example
  ```re
  approximatelyEqual(~tolerance=0.01, 1.500, 1.495) == true;
  approximatelyEqual(~tolerance=0.01, 1.500, 1.49) == false;
  ```
*/
let approximatelyEqual: (~tolerance: float, float, float) => bool =
  (~tolerance, x, y) => Js.Math.abs_float(x -. y) <= tolerance;

/**
  `toInt` converts a float to an int by dropping the fractional part.

  Floats that can't be represented as an int (e.g. `infinity`, `nan`) will
  return 0.
 */
let toInt = int_of_float;

/**
  `fromInt` converts an int (e.g. 1) to its floating-point representation (1.0)
 */
let fromInt = float_of_int;

/**
  Return only the decimal portion as a positive floating point number, with the
  whole-number portion set to 0.

  Note that the returned value is subject to floating point rounding errors and
  probably won't be exactly equal to the fractional part of the original number.

  ### Example
  ```re
  fractionalPart(3.141592) ~= 0.141592;
  fractionalPart(-12.3456) ~= 0.3456;
  ```
 */
let fractionalPart = v => {
  let whole = fromInt(toInt(v));
  abs(v >= 0.0 ? v -. whole : v +. abs(whole));
};

/**
  Round a floating point number to the nearest lower whole number.
 */
let floor = floor;

/**
  Round a floating point number to the nearest lower integer.
 */
let floorAsInt = v => toInt(floor(v));

/**
  Round a floating point number to the nearest higher whole number.
 */
let ceil = ceil;

/**
  Round a floating point number to the nearest higher integer.
 */
let ceilAsInt = v => toInt(ceil(v));

/**
  Round a floating point number to the nearest whole number.
 */
let round = v => fractionalPart(v) >= 0.5 ? ceil(v) : floor(v);

/**
  Round a floating point number to the nearest integer
 */
let roundAsInt = v => toInt(round(v));

/**
  `toPrecision` drops decimals so that the given float has no more than the
  requested number of decimals.

  ### Example
  ```re
  toPrecision(~decimals=2, 3.141592) == 3.14;
  toPrecision(~decimals=4, -4.99999999999) == -4.9999;
  ```
 */

let toPrecision = (~decimals, num) => {
  let pow = 10.0 ** fromInt(decimals);
  let multiplied = num >= 0.0 ? floor(pow *. num) : ceil(pow *. num);
  multiplied /. pow;
};

/**
  `toString(x)` returns the string representation of `x`. Note
  that this may not be in the same form as you wrote it in the
  source code.

  ### Example
  ```re
  toString(3.49) == "3.49";
  toString(7.0000) == "7";
  toString(1.0e+03) == "1000";
  ```
*/
let show: float => string = Js.Float.toString;
let toString = show;

module Show: BsAbstract.Interface.SHOW with type t = float = {
  type t = float;
  let show = show;
};

/**
  `fromString(s)` returns `Some(x)` if the given string represents
  a valid floating point number, `None` otherwise.

  ### Example
  ```re
  fromString("1000.0") == Some(1000.0);
  fromString("-5.25E2") == Some(-525.0);
  fromString("3.4.5") == None;
  ```
*/
let fromString: string => option(float) =
  v =>
    try (Some(float_of_string(v))) {
    | _ => None
    };

module Additive = BsAbstract.Float.Additive;

module Multiplicative = BsAbstract.Float.Multiplicative;

module Subtractive = BsAbstract.Float.Subtractive;

module Divisive = BsAbstract.Float.Divisive;

module Infix = BsAbstract.Float.Infix;
