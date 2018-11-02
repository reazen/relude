let fromInt: int => BsAbstract.Interface.ordering = BsAbstract.Interface.int_to_ordering;

let toInt: BsAbstract.Interface.ordering => int =
  fun
  | `less_than => (-1)
  | `equal_to => 0
  | `greater_than => 1;
