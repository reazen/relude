module Ordering = Relude_Ordering;

type ordering = BsAbstract.Interface.ordering;

let rec rangeAsList = (start: int, end_: int): list(int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...rangeAsList(start + 1, end_)];
  };

let rec rangeAsArray: (int, int) => array(int) =
  (start, end_) =>
    if (start >= end_) {
      [||];
    } else {
      Belt.Array.concat([|start|], rangeAsArray(start + 1, end_));
    };

let eq: (int, int) => bool = BsAbstract.Int.Eq.eq;

let compare: (int, int) => ordering = BsAbstract.Int.Ord.compare;

let compareAsInt: (int, int) => int =
  (a, b) => compare(a, b) |> Ordering.toInt;

module Additive = BsAbstract.Int.Additive;

module Multiplicative = BsAbstract.Int.Multiplicative;

module Subtractive = BsAbstract.Int.Subtractive;

module Divisive = BsAbstract.Int.Divisive;

module Eq = BsAbstract.Int.Eq;

module Ord = BsAbstract.Int.Ord;

module Show = BsAbstract.Int.Show;

module Infix = BsAbstract.Int.Infix;
