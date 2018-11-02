type ordering = BsAbstract.Interface.ordering;

let eq: (int, int) => bool = BsAbstract.Int.Eq.eq;

let compare: (int, int) => ordering = BsAbstract.Int.Ord.compare;

let compareAsInt: (int, int) => int = (a, b) => compare(a, b) |> Ordering.toInt;

let rec range = (start: int, end_: int): list(int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  }

module Eq = BsAbstract.Int.Eq;

module Ord = BsAbstract.Int.Ord;

