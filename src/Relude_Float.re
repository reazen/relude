open BsBastet.Interface;
/**
[Relude.Float] contains typeclass instances and utility functions for the
[float] type.
*/

/**
[Float.eq] indicates whether the two provided floats are exactly equal.
*/
let eq: (float, float) => bool = (a, b) => a == b;

module Eq: EQ with type t = float = {
  type t = float;
  let eq = eq;
};

/**
[Float.zero] is the zero constant used by [Semiring]. For floats, it is equal to
[0.0].
*/
let zero: float = 0.0;

/**
[Float.one] is the one constant used by [Semiring]. For floats, it is equal to
[1.0].
*/
let one: float = 1.0;

/**
[Float.nan] is the floating point value representing "not a number." This may be
used for mathematical operations that do not result in a real number, such as
[0.0 /. 0.0] and [sqrt(-2)].

Note that two [Float.nan] values are not equal to each other (and in fact,
[Float.nan] is not equal to itself). To check for [nan], use [Float.isNaN].
*/
let nan: float = nan;

/**
Positive infinity
*/
let infinity: float = infinity;

/**
Negative infinity
*/
let negativeInfinity: float = neg_infinity;

/**
[Float.add] finds the sum of two floats.
*/
let add: (float, float) => float = (+.);

/**
[Float.subtract] finds the difference between two floats.
*/
let subtract: (float, float) => float = (-.);

/**
[Float.multiply] finds the product of two floats.
*/
let multiply: (float, float) => float = ( *. );

/**
[Float.divide] finds the quotient of two floats.
*/
let divide: (float, float) => float = (/.);

/**
[Float.pow] raises the first number to the power of the second number.

{[
  Float.pow(2.0, 4.0) == 16.0;
  Float.pow(3.0, 2.0) == 9.0;
  Float.(pow(-2.0, (0.333333)) |> isNaN) == true;
]}
*/
let pow = (a, b) => a ** b;

/**
[Float.sqrt] determines the square root of the given float. The square root of
negative numbers is [nan].
*/
let sqrt = sqrt;

/**
[Float.top] is the constant representing the maximum float value.
*/
let top: float = max_float;

/**
[Float.bottom] is the minimum float value. Note that when using Bucklescript,
this value is hard-coded and is not necessarily equal to [Number.MIN_VALUE] in
JS.
*/
let bottom: float = min_float;

/**
[Float.isNaN] determines whether the provided floating point number is [nan].
NaN values are never equal to other numeric values, including other NaN values.
This means that the intuitive [myValue == nan] is not sufficient for determining
whether a value is NaN.

{[
  Float.isNaN(3.14) == false;
  Float.isNaN(infinity) == false;
  Float.isNaN(nan) == true;
]}
*/
let isNaN: float => bool = x => x != x;

/**
Compates two floats
*/
let compare: (float, float) => ordering = BsBastet.Float.Ord.compare;

module Ord: ORD with type t = float = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Semiring: SEMIRING with type t = float = {
  type t = float;
  let zero = zero;
  let one = one;
  let add = add;
  let multiply = multiply;
};
include Relude_Extensions_Semiring.SemiringExtensions(Semiring);

module Ring: RING with type t = float = {
  include Semiring;
  let subtract = (a, b) => a -. b;
};
include Relude_Extensions_Ring.RingExtensions(Ring);
include OrdRingExtensions(Ring);

module EuclideanRing: EUCLIDEAN_RING with type t = float = {
  include Ring;
  let divide = divide;
  let modulo = (_, _) => 0.0;
  let degree = _ => 1;
};

/**
[Float.approximatelyEqual] returns [true] if the two provided floats are within
[~tolerance] of one another.

If a negative [~tolerance] is provided, the returned value will always be
[false].

{[
  Float.approximatelyEqual(~tolerance=0.01, 1.500, 1.495) == true;
  Float.approximatelyEqual(~tolerance=0.01, 1.500, 1.49) == false;
]}
*/
let approximatelyEqual: (~tolerance: float, float, float) => bool =
  (~tolerance, x, y) => Js.Math.abs_float(x -. y) <= tolerance;

/**
[Float.toInt] converts a float to an int by dropping the fractional part.
Removing the portion after the decimal is distinct from {!val:round} or
{!val:floor}.

Floats that can't be represented as an integer (e.g. [infinity], [nan]) will
return [0]. Because the range of ints is smaller (32-bit) than floats, this
operation can lead to int overflows.

{[
  Float.toInt(3.1415) == 3;
  Float.toInt(-2.999) == -2;
  Float.toInt(Float.infinity) == 0;
  Float.toInt(12345678901.0) == -539222987;
]}
*/
let toInt = int_of_float;

/**
[Float.fromInt] converts an int (e.g. [1]) to its floating-point representation
([1.0]).
*/
let fromInt = float_of_int;

/**
[Float.fractionalPart] returns only the decimal portion as a positive floating
point number, with the whole-number portion set to 0.

Note that the returned value is subject to floating point rounding errors and
probably won't be exactly equal to the fractional part of the original number.

{[
  Float.fractionalPart(3.141592) ~= 0.141592;
  Float.fractionalPart(-12.3456) ~= 0.3456;
]}
*/
let fractionalPart = v => {
  let whole = fromInt(toInt(v));
  abs(v >= 0.0 ? v -. whole : v +. abs(whole));
};

/**
[Float.floor] rounds a floating point number to the nearest lower whole number.
*/
let floor = floor;

/**
[Float.floorAsInt] rounds a floating point number to the nearest lower integer.
*/
let floorAsInt = v => toInt(floor(v));

/**
[Float.ceil] rounds a floating point number to the nearest higher whole number.
*/
let ceil = ceil;

/**
[Float.ceilAsInt] rounds a floating point number to the nearest higher integer.
*/
let ceilAsInt = v => toInt(ceil(v));

/**
[Float.round] rounds a floating point number to the nearest whole number.
*/
let round = v => fractionalPart(v) >= 0.5 ? ceil(v) : floor(v);

/**
[Float.roundAsInt] rounds a floating point number to the nearest integer.
 */
let roundAsInt = v => toInt(round(v));

/**
[Float.toPrecision] drops decimals so that the given float has no more than the
requested number of decimals.

{[
  Float.toPrecision(~decimals=2, 3.141592) == 3.14;
  Float.toPrecision(~decimals=4, -4.99999999999) == -4.9999;
]}
*/
let toPrecision = (~decimals, num) => {
  let pow = 10.0 ** fromInt(decimals);
  let multiplied = num >= 0.0 ? floor(pow *. num) : ceil(pow *. num);
  multiplied /. pow;
};

/**
[Float.show] returns the string representation of [x].

Note that because there are multiple ways to represent floats in source code,
the output returned by [show] may not match the literal float syntax provided to
the function.

{[
  Float.show(3.49) == "3.49";
  Float.show(7.0000) == "7";
  Float.show(1.0e+03) == "1000";
]}
*/
let show: float => string = Js.Float.toString;

/**
[Float.toString] is an alias for {!val:show}.
*/
let toString = show;

module Show: SHOW with type t = float = {
  type t = float;
  let show = show;
};

/**
[Float.fromString] attempts to parse the provided string, returning [Some] float
if the input string is a valid floating point number or [None] otherwise.

{[
  Float.fromString("1000.0") == Some(1000.0);
  Float.fromString("-5.25E2") == Some(-525.0);
  Float.fromString("3.4.5") == None;
]}
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
