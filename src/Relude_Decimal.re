/**
[Relude.Decimal] contains a type [t] which represents arbitrary precision
numeric values, backed by an [int] mantissa and an [int] exponent. This can be
useful for representing currency values without loss in precision or floating
point errors.

Because the mantissa and exponent are backed by ints, there is a fairly
restrictive range of values that can be represented.

{[
  let a = Decimal(12345, -2); // 123.45
  let b = Decimal(6789, 3); // 6789000
]}
*/

/**
The type of the base of our Decimal
*/
type mantissa = int;

/**
The type of the exponent of our Decimal
*/
type exponent = int;

/**
Represents an arbitrary precision number, backed by an integer mantissa, and
integer exponent.
*/
type t =
  | Decimal(mantissa, exponent);

/**
Represents a preference for rounding a [Decimal] value, when applicable.
*/
type rounding =
  | Truncate
  | RoundUp
  | RoundDown
  | RoundUpOrDown;

/**
[Decimal.make] constructs a [Decimal] from a [mantissa] and [exponent].
*/
let make: (int, int) => t =
  (mantissa, exponent) => Decimal(mantissa, exponent);

/**
[Decimal.fromInt] constructs a [Decimal] from an [int].
*/
let fromInt: int => t = intValue => Decimal(intValue, 0);

/**
[Decimal.fromString] attempts to parse a [Decimal] from a [string]
*/
/*
 let fromString: string => option(t) = _ => None; // TODO
 */

/**
[Decimal.show] renders the [Decimal] value to a [string], as if the [Decimal]
was represented as a [float].

{[
  Decimal(12345, -2) |> Decimal.toString // "123.45"
  Decimal(12345, 3) |> Decimal.toString // "12345000"
]}
*/
let show: t => string =
  (Decimal(mantissa, exponent)) =>
    if (exponent == 0) {
      string_of_int(mantissa);
    } else if (exponent > 0) {
      if (mantissa == 0) {
        "0";
      } else {
        let zeroes = Relude_String.repeat(exponent, "0");
        string_of_int(mantissa) ++ zeroes;
      };
    } else /* mantissa < 0 */ {
      let (whole, fractional) =
        Relude_String.splitAt(exponent, string_of_int(mantissa));
      whole ++ "." ++ fractional;
    };

/**
[Decimal.round] rounds a [Decimal] using the given rounding strategy.
*/
/*
 let round: (rounding, t) => t = (_rounding, decimal) => decimal; // TODO
 */

/**
[Decimal.tenToThePowerOfPositive] computes the value of [10^exponent].

The return value of this function is undefined for exponent values < 0.

{[
  Decimal.tenToThePowerOf(3) == 1000;
]}
*/
let tenToThePowerOfPositive: int => int =
  exponent =>
    Relude_Int.rangeAsArray(1, exponent + 1)
    |> Relude_Array.foldLeft((acc, _) => 10 * acc, 1);

/**
[Decimal.normalize] normalizes the exponent to the minimal exponent for two
given [Decimal] values.

{[
  let a = Decimal(12345, -2); // 123.45
  let b = Decimal(12345, 3); // 12345000
  let res = Decimal.normalize(a, b);

  res == (Decimal(12345, -2), Decimal(1234500000, -2), -2)
]}
*/
let normalize: (t, t) => (t, t, int) =
  (Decimal(mantissaA, exponentA), Decimal(mantissaB, exponentB)) => {
    let exponentMin = Relude_Int.min(exponentA, exponentB);
    let newMantissaA =
      mantissaA * tenToThePowerOfPositive(exponentA - exponentMin);
    let newMantissaB =
      mantissaB * tenToThePowerOfPositive(exponentB - exponentMin);
    (
      Decimal(newMantissaA, exponentMin),
      Decimal(newMantissaB, exponentMin),
      exponentMin,
    );
  };

/**
[Decimal.add] adds two [Decimal] values with no attempt at avoiding overflow.

Note: the arguments are in order of [lhs], [rhs].
*/
let add: (t, t) => t =
  (lhs, rhs) => {
    let (Decimal(mantissaLHS, _), Decimal(mantissaRHS, _), exponent) =
      normalize(lhs, rhs);
    Decimal(mantissaLHS + mantissaRHS, exponent);
  };

/**
[Decimal.(+..)] is the infix operator for [add].
*/
let (+..) = add;

/**
[Decimal.subtract] subtracts two [Decimal] values with no attempt at avoiding
overflow.

Note: the arguments are in order of [lhs], [rhs].
*/
let subtract: (t, t) => t =
  (lhs, rhs) => {
    let (Decimal(mantissaLHS, _), Decimal(mantissaRHS, _), exponent) =
      normalize(lhs, rhs);
    Decimal(mantissaLHS - mantissaRHS, exponent);
  };

/**
[Decimal.(-..)] is the infix operator for [subtract].
*/
let (-..) = subtract;

/**
[Decimal.multiply] multiplies two [Decimal] values with no attempt at avoiding
overflow.

Note: the arguments are in order of [lhs], [rhs]
*/;
/*
 let multiply: (t, t) => t =
   (lhs, _rhs) => {
     // TODO
     lhs;
   };
   */

/**
Infix operator for [multiply]
*/;
//let ( *.. ) = multiply;

/**
Divides two [Decimal] values using the given [rounding] preference.

Note: the arguments are in order of [lhs], [rhs]
*/;
/*
 let divide: (t, t, rounding) => t =
   (lhs, _rhs, _rounding) => {
     // TODO
     lhs;
   };
   */

/**
Infix operator for [divide]
*/ /*let (/..) = divide*/;
