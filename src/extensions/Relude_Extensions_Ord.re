open BsAbstract.Interface;

/**
 * Compares two values using the given compare function
 */
let compareAsIntBy: 'a. (('a, 'a) => ordering, 'a, 'a) => int =
  (compare, a, b) => compare(a, b) |> Relude_Ordering.toInt;

/**
 * Compares two values using the given ORD module
 */
let compareAsInt =
    (type a, ord: (module ORD with type t = a), a: a, b: a): int => {
  module Ord = (val ord);
  compareAsIntBy(Ord.compare, a, b);
};

/**
 * Finds the minimum of two values using the given compare function
 */
let minBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `greater_than => b
    | `less_than
    | `equal_to => a
    };

/**
 * Finds the minimum of two values using the given ORD module
 */
let min = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  minBy(Ord.compare, a, b);
};

/**
 * Finds the maximum of two values using the given compare function
 */
let maxBy: 'a. (('a, 'a) => ordering, 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `less_than => b
    | `greater_than
    | `equal_to => a
    };

/**
 * Finds the maximum of two values using the given ORD module
 */
let max = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  maxBy(Ord.compare, a, b);
};

/**
 * Indicates if the item on the left is less than the item on the right using the given compare function
 */
let lessThanBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `less_than;

/**
 * Alias for `lessThanBy`
 */
let ltBy = lessThanBy;

/**
 * Indicates if the item on the left is less than the item on the right using the given ORD module
 */
let lessThan = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  lessThanBy(Ord.compare, a, b);
};

/**
 * Alias for `lessThan`
 */
let lt = lessThan;

/**
 * Indicates if the item on the left is less than or equal to the item on the right using the given compare function
 */
let lessThanOrEqBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `greater_than;

/**
 * Alias for lessThanOrEqBy`
 */
let lteBy = lessThanOrEqBy;

/**
 * Indicates if the item on the left is less than or equal to the item on the right using the given ORD module
 */
let lessThanOrEq = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  lessThanOrEqBy(Ord.compare, a, b);
};

/**
 * Alias for `lessThanOrEq`
 */
let lte = lessThanOrEq;

/**
 * Indicates if the item on the left is greater than the item on the right using the given compare function
 */
let greaterThanBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `greater_than;

/**
 * Alias for `greaterThanBy`
 */
let gtBy = greaterThanBy;

/**
 * Indicates if the item on the left is greater than the item on the right using the given ORD module
 */
let greaterThan = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  greaterThanBy(Ord.compare, a, b);
};

/**
 * Alias for `greaterThan`
 */
let gt = greaterThan;

/**
 * Indicates if the item on the left is greater than or equal to the item on the right using the given compare function
 */
let greaterThanOrEqBy: 'a. (('a, 'a) => ordering, 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `less_than;

/**
 * Alias for `greaterThanOrEqBy`
 */
let gteBy = greaterThanOrEqBy;

/**
 * Indicates if the item on the left is greater than or equal to the item on the right using the given ORD module
 */
let greaterThanOrEq = (type a, ord: (module ORD with type t = a), a, b) => {
  module Ord = (val ord);
  greaterThanOrEqBy(Ord.compare, a, b);
};

/**
 * Alias for `greaterThanOrEq`
 */
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

module OrdExtensions = (O: ORD) => {
  let compareAsInt = (a, b) => compareAsIntBy(O.compare, a, b);
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

  module OrdRingExtensions = (R: RING with type t = O.t) => {
    let abs = v => abs((module O), (module R), v);
    let signum = v => signum((module O), (module R), v);
  };
};

module OrdInfix = (O: ORD) => {
  module OrdExtensions = OrdExtensions(O);

  // Note: if we want to change these, try for consistency with EQ operators

  /**
   * Less-than operator
   */
  let (|<|) = OrdExtensions.lessThan;

  /**
   * Less-than-or-equal operator
   */
  let (|<=|) = OrdExtensions.lessThanOrEq;

  /**
   * Greater-than operator
   */
  let (|>|) = OrdExtensions.greaterThan;

  /**
   * Greater-than-or-equal operator
   */
  let (|>=|) = OrdExtensions.greaterThanOrEq;
};