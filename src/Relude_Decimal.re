type mantissa = int;
type exponent = int;

/**
Represents an arbitrary precision number, backed by an integer mantissa, and integer exponent.
*/
type t =
  | Decimal(mantissa, exponent);

type rounding =
  | Truncate
  | RoundUp
  | RoundDown;

/**
Constructs a `Decimal` from a `mantissa` and `exponent`
*/
let make: (int, int) => t =
  (mantissa, exponent) => Decimal(mantissa, exponent);

/**
Makes a `Decimal` from an `int`
*/
let fromInt: int => t = intValue => Decimal(intValue, 0);

/**
Attempts to parse a `Decimal` from a `string`
*/
let fromString: string => option(t) = _ => None; // TODO

/**
Renders the `Decimal` value to a string.

e.g.
```
Decimal(12345, -2) |> Decimal.toString // "123.45"
Decimal(12345, 3) |> Decimal.toString // "12345000"
```
*/
let toString: t => string =
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
Computes the value of `10^exponent`.

This return value of this function is undefined for exponent values < 0

E.g. `tenToThePowerOf(3) == 1000`
*/
let tenToThePowerOf: int => int =
  exponent =>
    Relude_Int.rangeAsArray(1, exponent + 1)
    |> Relude_Array.foldLeft((acc, _) => 10 * acc, 1);

/**
Normalizes the exponent to the minimal exponent for two given `Decimal` values.

E.g.
```
let a = Decimal(12345, -2);      // 123.45
let b = Decimal(12345, 3);  // 12345000
let res = Decimal.normalize(a, b);
> res == (Decimal(12345, -2), Decimal(1234500000, -2), -2)
```
*/
let normalize: (t, t) => (t, t, int) =
  (Decimal(mantissaA, exponentA), Decimal(mantissaB, exponentB)) => {
    let exponentMin = Relude_Int.min(exponentA, exponentB);
    let newMantissaA = mantissaA * tenToThePowerOf(exponentA - exponentMin);
    let newMantissaB = mantissaB * tenToThePowerOf(exponentB - exponentMin);
    (
      Decimal(newMantissaA, exponentMin),
      Decimal(newMantissaB, exponentMin),
      exponentMin,
    );
  };

/**
Adds two `Decimal` values by first normalizing the exponent.
*/
let add: (t, t) => t =
  (lhs, rhs) => {
    let (Decimal(mantissaRHS, _), Decimal(mantissaLHS, _), exponent) =
      normalize(rhs, lhs);
    Decimal(mantissaLHS + mantissaRHS, exponent);
  };

/**
Subtracts two `Decimal` values by first normalizing the exponent.
*/
let subtract: (t, t) => t =
  (lhs, rhs) => {
    let (Decimal(mantissaLHS, _), Decimal(mantissaRHS, _), exponent) =
      normalize(lhs, rhs);
    Decimal(mantissaLHS - mantissaRHS, exponent);
  };

let multiply: (t, t) => t =
  (lhs, _rhs) => {
    // TODO
    lhs;
  };

let divide: (t, t, rounding) => t =
  (lhs, _rhs, _rounding) => {
    // TODO
    lhs;
  };