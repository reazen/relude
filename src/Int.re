open BsAbstract.Interface;

module Eq = BsAbstract.Int.Eq;
module Ord = BsAbstract.Int.Ord;

let eq = Eq.eq;
let compare: (int, int) => ordering = Ord.compare;
