open BsAbstract.Interface;

let minBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `greater_than => b
    | `less_than
    | `equal_to => a
    };

let maxBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `less_than => b
    | `greater_than
    | `equal_to => a
    };

let min =
    (type a, ord: (module BsAbstract.Interface.ORD with type t = a), a, b) => {
  module Ord = (val ord);
  minBy(Ord.compare, a, b);
};

let max =
    (type a, ord: (module BsAbstract.Interface.ORD with type t = a), a, b) => {
  module Ord = (val ord);
  maxBy(Ord.compare, a, b);
};

module Make = (O: ORD) => {
  let min = (a, b) => minBy(O.compare, a, b);
  let max = (a, b) => maxBy(O.compare, a, b);
};
