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
 */
let inverse: bool => bool = (!);

module Conjunctive = BsAbstract.Bool.Conjunctive;

module And = Conjunctive;

module Disjunctive = BsAbstract.Bool.Disjunctive;

module Or = Disjunctive;

module Eq = BsAbstract.Bool.Eq;

module Ord = BsAbstract.Bool.Ord;

/* TODO: include other modules from BsAbstract? */

module Show = BsAbstract.Bool.Show;

module Infix = BsAbstract.Bool.Infix;
