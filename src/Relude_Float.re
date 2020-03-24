/**
 * Indicates if two floats are exactly equal
 */
let eq: (float, float) => bool = (a, b) => a == b;

module Eq: BsBastet.Interface.EQ with type t = float = {
  type t = float;
  let eq = eq;
};

/**
 * 0.0 value
 */
let zero: float = 0.0;

/**
 * 1.0 value
 */
let one: float = 1.0;

/**
 * Value for "not a number", used for mathematical operations that do not result
 * in a real number, such as `0.0 /. 0.0` and `sqrt(-2)`.
 */
let nan: float = nan;

/**
 * Positive infinity
 */
let infinity: float = infinity;

/**
 * Negative infinity
 */
let negativeInfinity: float = neg_infinity;

/**
 * Finds the sum of two floats
 */
let add: (float, float) => float = (+.);

/**
 * Finds the difference of two floats
 */
let subtract: (float, float) => float = (-.);

/**
 * Finds the product of two floats
 */
let multiply: (float, float) => float = ( *. );

/**
 * Finds the quotient of two floats
 */
let divide: (float, float) => float = (/.);

/**
 * Raise the first number to the power of the second number.
 *
 * ```re
 * pow(2.0, 4.0) == 16.0;
 * pow(3.0, 2.0) == 9.0;
 * pow(-2.0, (0.333333)) |> isNaN;
 * ```
 */
let pow = (a, b) => a ** b;

/**
 * Find the square root of the given float. The square root of negative numbers
 * is `nan`.
 */
let sqrt = sqrt;

/**
 * The maximum float value.
 */
let top: float = max_float;

/**
 * The minimum float value. Note that when using Bucklescript, this value is
 * hard-coded and is not necessarily equal to `Number.MIN_VALUE` in JS.
 */
let bottom: float = min_float;

/**
 * NaN values are never equal to other numeric values, including other NaN
 * values. This means that the intuitive `myValue == nan` is not sufficient for
 * determining whether a value is NaN.
 *
 * ```re
 * isNaN(3.14) == false;
 * isNaN(infinity) == false;
 * isNaN(nan) == true;
 * ```
 */
let isNaN: float => bool = x => x != x;

/**
 * Compates two floats
 */
let compare: (float, float) => BsBastet.Interface.ordering = BsBastet.Float.Ord.compare;

module Ord: BsBastet.Interface.ORD with type t = float = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Semiring: BsBastet.Interface.SEMIRING with type t = float = {
  type t = float;
  let zero = zero;
  let one = one;
  let add = add;
  let multiply = multiply;
};
include Relude_Extensions_Semiring.SemiringExtensions(Semiring);

module Ring: BsBastet.Interface.RING with type t = float = {
  include Semiring;
  let subtract = (a, b) => a -. b;
};
include Relude_Extensions_Ring.RingExtensions(Ring);
include OrdRingExtensions(Ring);

module EuclideanRing: BsBastet.Interface.EUCLIDEAN_RING with type t = float = {
  include Ring;
  let divide = divide;
  let modulo = (_, _) => 0.0;
  let degree = _ => 1;
};

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

/**
 * Alias for `show`
 */
let toString = show;

module Show: BsBastet.Interface.SHOW with type t = float = {
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
    try(Some(float_of_string(v))) {
    | _ => None
    };

module Additive = {
  include BsBastet.Float.Additive;
};

module Multiplicative = {
  include BsBastet.Float.Multiplicative;
};

module Subtractive = {
  include BsBastet.Float.Subtractive;
};

module Divisive = {
  include BsBastet.Float.Divisive;
};

module Infix = {
  include BsBastet.Float.Infix;
};
