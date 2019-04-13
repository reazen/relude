let approximatelyEqual: (~tolerance: float, float, float) => bool =
  (~tolerance, x, y) => Js.Math.abs_float(x -. y) <= tolerance;

let toString: float => string = Js.Float.toString;

let fromString: string => option(float) =
  v =>
    try (Some(float_of_string(v))) {
    | _ => None
    };

module Additive = BsAbstract.Float.Additive;

module Multiplicative = BsAbstract.Float.Multiplicative;

module Subtractive = BsAbstract.Float.Subtractive;

module Divisive = BsAbstract.Float.Divisive;

module Eq = BsAbstract.Float.Eq;

module Ord = BsAbstract.Float.Ord;

module Show = BsAbstract.Float.Show;

module Infix = BsAbstract.Float.Infix;
