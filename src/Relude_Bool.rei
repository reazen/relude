let ifElse: (unit => 'a, unit => 'a, bool) => 'a;
module Conjunctive = BsAbstract.Bool.Conjunctive;
module And = Conjunctive;
module Disjunctive = BsAbstract.Bool.Disjunctive;
module Or = Disjunctive;
module Eq = BsAbstract.Bool.Eq;
module Ord = BsAbstract.Bool.Ord;
module Show = BsAbstract.Bool.Show;
module Infix = BsAbstract.Bool.Infix;
