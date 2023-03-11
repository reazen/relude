open Bastet.Interface;

[@ocaml.text
  {|
[Relude.Int] contains functions and typeclass instances for the [int] type.
|}
]

/**
[toFloat] returns the floating-point representation of the int
*/
let toFloat = float_of_int;

/**
[fromFloat] turns a float into an int by dropping the fractional part.

Floats that can't be represented as an int (e.g. [infinity], [nan]) will
return 0.
*/
let fromFloat = int_of_float;

/**
The integer value 0
*/
let zero = 0;

/**
The integer value 1
*/
let one = 1;

/**
Finds the sum of two integers
*/
let add: (int, int) => int = (+);

/**
Finds the difference of two integers
*/
let subtract: (int, int) => int = (-);

/**
Finds the product of two integers
*/
let multiply: (int, int) => int = ( * );

/**
Finds the quotient of an integer division
*/
let divide: (int, int) => int = (/);

/**
Finds the remainder of an integer division
*/
let modulo: (int, int) => int = (a, b) => a mod b;

/**
Finds the quotient and remainder of an integer division
*/
let divideWithModulo: (int, int) => (int, int) =
  (a, b) => (divide(a, b), modulo(a, b));

/**
Converts two int values to floats, then performs a float division
*/
let divideAsFloat: (int, int) => float =
  (a, b) => float_of_int(a) /. float_of_int(b);

/**
The top bound (max value) of 32 bit int
*/
let top = Base.Int.max_value;

/**
The bottom bound (min value) of 32 bit int
*/
let bottom = Base.Int.min_value;

/**
Degree finds the smaller of the absolute value of the given in, or the max int value
*/
let degree: int => int = a => Base.Int.min(Base.Int.abs(a), top);

/**
[rangeAsList(n, m)] returns a list of integers
[[n, n + 1, .. m - 1]].

{[
  rangeAsList(10, 15) == [10, 11, 12, 13, 14];
  rangeAsList(10, 10) == [];
  rangeAsList(15, 10) == [];
]}
*/
let rec rangeAsList = (start: int, end_: int): list(int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...rangeAsList(start + 1, end_)];
  };

/**
[rangeAsArray(n, m)] returns an array of integers
[[|n, n + 1, .. m - 1|]].

{[
  rangeAsArray(10, 15) == [|10, 11, 12, 13, 14|];
  rangeAsArray(10, 10) == [||];
  rangeAsArray(15, 10) == [||];
]}
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
[eq(a, b)] returns [true] if the arguments are equal, [false] otherwise.
*/
let eq: (int, int) => bool = Bastet.Int.Eq.eq;

module Eq: EQ with type t = int = {
  type t = int;
  let eq = eq;
};
include Relude_Extensions_Eq.EqExtensions(Eq);

/**
[Int.compare] returns [`less_than] if the first int is less than the second,
[`greater_than] if the first is larger than the second, and [`equal_to] if the
two ints are the same.

{[
  Int.compare(3, 5) == `less_than;
  Int.compare(3, 3) == `equal_to;
  Int.compare(5, 3) == `greater_than;
]}
*/
let compare: (int, int) => ordering = Bastet.Int.Ord.compare;

module Ord: ORD with type t = int = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Bounded: BOUNDED with type t = int = {
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

module Semiring: SEMIRING with type t = int = {
  type t = int;
  let zero = zero;
  let one = one;
  let add = add;
  let multiply = multiply;
};
include Relude_Extensions_Semiring.SemiringExtensions(Semiring);

module Ring: RING with type t = int = {
  include Semiring;
  let subtract = subtract;
};
include Relude_Extensions_Ring.RingExtensions(Ring);
include OrdRingExtensions(Ring);

module EuclideanRing: EUCLIDEAN_RING with type t = int = {
  include Ring;
  let divide = divide;
  let modulo = modulo;
  let degree = degree;
};

/**
Map module with an int key
*/
module Map = Relude_Map.WithOrd(Ord);

/**
Set module for ints
*/
module Set = Relude_Set.WithOrd(Ord);

/**
[Int.show] returns the string representation of the provided int.

Note that because there are multiple ways to represent integer constants in
source code, this string representation may not match the exact input to the
function.

{[
  Int.show(57) == "57";
  Int.show(0x1a) == "26";
]}
*/
let show: int => string = string_of_int;

/**
[Int.toString] is an alias for {!val:show}.
*/
let toString = show;

module Show: SHOW with type t = int = {
  type t = int;
  let show = show;
};

/**
[Int.fromString] attempts to parse the given string as an integer. If the given
string is a valid integer, [Some] is returned, otherwise [None].

Note that unlike [parseInt] in JavaScript, this won't return "the good part" up
until it fails to parse. If any part of the provided string is not a valid
[int], the whole thing fails to parse and returns [None].

{[
  Int.fromString("57") == Some(57);
  Int.fromString("0x1a") == Some(26);
  Int.fromString("57.3") == None;
  Int.fromString("3dozen") == None;
]}
*/
let fromString: string => option(int) =
  v =>
    try(Some(int_of_string(v))) {
    | _ => None
    };

module Additive = {
  include Bastet.Int.Additive;
};

module Multiplicative = {
  include Bastet.Int.Multiplicative;
};

module Subtractive = {
  include Bastet.Int.Subtractive;
};

module Divisive = {
  include Bastet.Int.Divisive;
};

module Infix = {
  include Bastet.Int.Infix;
  include Relude_Extensions_Eq.EqInfix(Eq);
  include Relude_Extensions_Ord.OrdInfix(Ord);
};
