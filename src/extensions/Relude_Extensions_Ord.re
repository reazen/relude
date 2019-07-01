open BsAbstract.Interface;

let minBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `greater_than => b
    | `less_than
    | `equal_to => a
    };

let min = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  minBy(Ord.compare, a, b);
};

let maxBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `less_than => b
    | `greater_than
    | `equal_to => a
    };

let max = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  maxBy(Ord.compare, a, b);
};

let lessThanBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `less_than;

let lessThan = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  lessThanBy(Ord.compare, a, b);
};

let lessThanOrEqBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `greater_than;

let lessThanOrEq = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  lessThanOrEqBy(Ord.compare, a, b);
};

let greaterThanBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `greater_than;

let greaterThan = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  greaterThanBy(Ord.compare, a, b);
};

let greaterThanOrEqBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `less_than;

let greaterThanOrEq = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  greaterThanBy(Ord.compare, a, b);
};

let lt = lessThan;
let lte = lessThanOrEq;
let gt = greaterThan;
let gte = greaterThanOrEq;

module Make = (O: ORD) => {
  let min = (a, b) => minBy(O.compare, a, b);
  let max = (a, b) => maxBy(O.compare, a, b);
  let lessThan = (a, b) => lessThanBy(O.compare, a, b);
  let lessThanOrEq = (a, b) => lessThanOrEqBy(O.compare, a, b);
  let greaterThan = (a, b) => greaterThanBy(O.compare, a, b);
  let greaterThanOrEq = (a, b) => greaterThanOrEqBy(O.compare, a, b);
  let lt = lessThan;
  let lte = lessThanOrEq;
  let gt = greaterThan;
  let gte = greaterThanOrEq;
};
