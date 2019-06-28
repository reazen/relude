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
