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
  greaterThanOrEqBy(Ord.compare, a, b);
};

let lt = lessThan;
let lte = lessThanOrEq;
let gt = greaterThan;
let gte = greaterThanOrEq;

/**
  Ensure a provided value falls between a max and min (inclusive). Note that if
  the provided min is greater than the provided max, the max is always returned.
  This is considered an incorrect use of `clamp`.

  ```
  let clamp = clampBy(Int.compare);
  clamp(~min=0, ~max=5, 3) == 3;
  clamp(~min=0, ~max=5, 0) == 0;
  clamp(~min=0, ~max=3, 4) == 3;
  clamp(~min=1, ~max=0, 2) == 0; // don't do this
  ```
 */
let clampBy: 'a. (('a, 'a) => ordering, ~min: 'a, ~max: 'a, 'a) => 'a =
  (compare, ~min, ~max, v) => minBy(compare, max, maxBy(compare, min, v));

let clamp = (type a, ord: (module ORD with type t = a), ~min: a, ~max: a, x) => {
  module Ord = (val ord);
  clampBy(Ord.compare, ~min, ~max, x);
};

/**
  Determine whether a provided value falls between a min and max.
 */
let betweenBy: 'a. (('a, 'a) => ordering, ~min: 'a, ~max: 'a, 'a) => bool =
  (compare, ~min, ~max, v) =>
    greaterThanOrEqBy(compare, v, min) && lessThanOrEqBy(compare, v, max);

let between = (type a, ord: (module ORD with type t = a), ~min: a, ~max: a, x) => {
  module Ord = (val ord);
  betweenBy(Ord.compare, ~min, ~max, x);
};

/**
  Absolute value: if x is gte zero, return zero, otherwise negate x.
 */
let abs =
    (
      type a,
      ord: (module ORD with type t = a),
      ring: (module RING with type t = a),
      x,
    ) => {
  module Ring = (val ring);
  gte(ord, x, Ring.zero) ? x : Ring.subtract(Ring.zero, x);
};

/**
  Sign function, evaluates to one for values >= zero, and negative one for
  values less than zero.
 */
let signum =
    (
      type a,
      ord: (module ORD with type t = a),
      ring: (module RING with type t = a),
      x,
    )
    : a => {
  module Ring = (val ring);
  Ring.(gte(ord, x, zero) ? one : subtract(zero, one));
};

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
  let clamp = (~min, ~max, v) => clampBy(O.compare, ~min, ~max, v);
  let between = (~min, ~max, v) => betweenBy(O.compare, ~min, ~max, v);
};

module MakeWithRing = (O: ORD, R: RING with type t = O.t) => {
  let abs = v => abs((module O), (module R), v);
  let signum = v => signum((module O), (module R), v);
};
