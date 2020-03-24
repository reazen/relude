/**
 * Contains functions and typeclass instances for the `ordering` type - the enum
 * with inhabitants [ | `less_than, | `equal_to, | `greater_than ]
 *
 * Functions that deal with comparing values don't belong here - these go in Relude_Ord/OrdExtensions.
 * This module is just for working with values of type `ordering`.
 */
type t = BsBastet.Interface.ordering;

/**
 * Converts an int to a type-safe ordering value
 */
let fromInt: int => t =
  i =>
    if (i < 0) {
      `less_than;
    } else if (i == 0) {
      `equal_to;
    } else {
      `greater_than;
    };

/**
 * Converts a type-safe ordering value to an int
 */
let toInt: t => int =
  fun
  | `less_than => (-1)
  | `equal_to => 0
  | `greater_than => 1;

/**
 * Reverses the value of the ordering (`greater_than becomes `less_than and vice versa)
 */
let reverse: t => t =
  fun
  | `less_than => `greater_than
  | `equal_to => `equal_to
  | `greater_than => `less_than;

/**
 * Compares two orderings for equality
 */
let eq: (t, t) => bool =
  (o1, o2) =>
    switch (o1, o2) {
    | (`less_than, `less_than) => true
    | (`equal_to, `equal_to) => true
    | (`greater_than, `greater_than) => true
    | (`less_than, _) => false
    | (`equal_to, _) => false
    | (`greater_than, _) => false
    };

module Eq: BsBastet.Interface.EQ with type t = t = {
  type nonrec t = t;
  let eq = eq;
};
include Relude_Extensions_Eq.EqExtensions(Eq);

/**
 * Compares two orderings (`less_than < `equal_to < `greater_than)
 */
let compare: (t, t) => t =
  (o1, o2) =>
    switch (o1, o2) {
    | (`less_than, `less_than) => `equal_to
    | (`less_than, `equal_to) => `less_than
    | (`less_than, `greater_than) => `less_than
    | (`equal_to, `less_than) => `greater_than
    | (`equal_to, `equal_to) => `equal_to
    | (`equal_to, `greater_than) => `less_than
    | (`greater_than, `less_than) => `greater_than
    | (`greater_than, `equal_to) => `greater_than
    | (`greater_than, `greater_than) => `equal_to
    };

module Ord: BsBastet.Interface.ORD with type t = t = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

let top = `greater_than;

let bottom = `less_than;

module Bounded: BsBastet.Interface.BOUNDED with type t = t = {
  include Ord;
  let top = top;
  let bottom = bottom;
};
include Relude_Extensions_Bounded.BoundedExtensions(Bounded);

let pred: t => option(t) =
  fun
  | `less_than => None
  | `equal_to => Some(`less_than)
  | `greater_than => Some(`equal_to);

let succ: t => option(t) =
  fun
  | `less_than => Some(`equal_to)
  | `equal_to => Some(`greater_than)
  | `greater_than => None;

module Enum: Relude_Interface.ENUM with type t = t = {
  include Ord;
  let pred = pred;
  let succ = succ;
};
include Relude_Extensions_Enum.EnumExtensions(Enum);

module BoundedEnum: Relude_Interface.BOUNDED_ENUM with type t = t = {
  include Bounded;
  include (Enum: Relude_Interface.ENUM with type t := t);
  let cardinality = 3;
  let toEnum = i => Some(fromInt(i));
  let fromEnum = toInt;
};
include Relude_Extensions_BoundedEnum.BoundedEnumExtensions(BoundedEnum);
