/**
 * Converts an int to a type-safe ordering value
 */
let fromInt: int => BsAbstract.Interface.ordering = BsAbstract.Interface.int_to_ordering;

/**
 * Converts a type-safe ordering value to an int
 */
let toInt: BsAbstract.Interface.ordering => int =
  fun
  | `less_than => (-1)
  | `equal_to => 0
  | `greater_than => 1;
