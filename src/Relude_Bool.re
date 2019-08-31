/**
 * Folds a bool value into a value of a different type, using a function
 * for the true and false cases.
 */
let ifElse: (unit => 'a, unit => 'a, bool) => 'a =
  (onTrue, onFalse, value) =>
    if (value) {
      onTrue();
    } else {
      onFalse();
    };

/**
 * Negates the bool value
 * 
 * Not named `not`, because refmt rewrites this as (!)
 */
let inverse: bool => bool = (!);

/**
 * Negates the bool value
 *
 * Alias for `inverse`.  Not named `not`, because refmt rewrites this as (!)
 */
let not_: bool => bool = inverse;

/**
 * Combines two boolean using an AND
 */
let and_: (bool, bool) => bool = (&&);

/**
 * Combines two boolean using an OR
 */
let or_: (bool, bool) => bool = (||);

/**
 * Combines two booleans using an NAND
 */
let nand: (bool, bool) => bool = (a, b) => !(a && b);

/**
 * Combines two booleans using an NOR
 */
let nor: (bool, bool) => bool = (a, b) => !(a || b);

/**
 * Combines two booleans using an XOR
 */
let xor: (bool, bool) => bool = (a, b) => !a && b || a && !b;

/**
 * Combines two booleans using an XNOR
 */
let xnor: (bool, bool) => bool = (a, b) => !xor(a, b);

/**
 * Combines two booleans using an implication
 */
let implies: (bool, bool) => bool = (a, b) => !a || b;

/**
 * EQ instance for booleans
 */
module Eq = BsAbstract.Bool.Eq;

/**
 * Compares two booleans for equality
 */
let eq: (bool, bool) => bool = Eq.eq;

/**
 * ORD instance for booleans
 */
module Ord = BsAbstract.Bool.Ord;

/**
 * Compares two booleans for equality
 */
let compare: (bool, bool) => BsAbstract.Interface.ordering = Ord.compare;

/**
 * SHOW instance for booleans
 */
module Show = BsAbstract.Bool.Show;

/**
 * Converts a boolean value to a string
 */
let show: bool => string = Show.show;

module Conjunctive = BsAbstract.Bool.Conjunctive;
module And = Conjunctive;
module Disjunctive = BsAbstract.Bool.Disjunctive;
module Or = Disjunctive;
/* TODO: include other modules from BsAbstract? */

module Infix = BsAbstract.Bool.Infix;