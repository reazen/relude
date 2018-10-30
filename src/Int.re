open BsAbstract.Interface;

/* Functions */
let eq = BsAbstract.Int.Eq.eq;

let compare: (int, int) => ordering = BsAbstract.Int.Ord.compare;

/* Modules */
module Eq = BsAbstract.Int.Eq;

module Ord = BsAbstract.Int.Ord;

