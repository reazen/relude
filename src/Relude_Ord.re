/**
 * Contains functions and modules for to help with comparing values
 */;

/**
 * The ordering type represents the result of a comparison - `less_than, `equal_to, or `greater_than
 */
type ordering = BsBastet.Interface.ordering;

/**
 * The compare function is the heart of all comparison functions
 */
type compare('a) = ('a, 'a) => ordering;

/**
 * Converts an compare function for a type 'a to a compare function of type 'b
 * using a function from 'b => 'a.
 *
 * If we know how to compare 'as and we know how to convert 'b to 'a, we can
 * compare 'bs by first converting our 'bs to 'as, then comparing them with the 'a
 * compare function.
 *
 * Example:
 * ```
 * let userCompare: compare(user) = Relude.Ord.by(user => user.name, Relude.String.compare);
 * ```
 *
 * This is the contravariant map for the compare function.
 */
let by: 'a 'b. ('b => 'a, compare('a)) => compare('b) =
  (bToA, compareA, b1, b2) => compareA(bToA(b1), bToA(b2));

/**
 * Alias of by
 */
let cmap = by;

module Contravariant:
  BsBastet.Interface.CONTRAVARIANT with type t('a) = compare('a) = {
  type nonrec t('a) = compare('a);
  let cmap = by;
};
include Relude_Extensions_Contravariant.ContravariantExtensions(
          Contravariant,
        );

/**
 * Creates a new compare function that is the reverse of the given compare function
 */
let reverse: 'a. compare('a) => compare('a) =
  (compare, a1, a2) =>
    switch (compare(a1, a2)) {
    | `less_than => `greater_than
    | `equal_to => `equal_to
    | `greater_than => `less_than
    };

/**
 * Compares two values using the given compare function
 */
let compareAsIntBy: 'a. (compare('a), 'a, 'a) => int =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `less_than => (-1)
    | `equal_to => 0
    | `greater_than => 1
    };

/**
 * Compares two values using the given ORD module
 */
let compareAsInt =
    (type a, ord: (module BsBastet.Interface.ORD with type t = a), a: a, b: a)
    : int => {
  module Ord = (val ord);
  compareAsIntBy(Ord.compare, a, b);
};

/**
 * Finds the minimum of two values using the given compare function
 */
let minBy: 'a. (compare('a), 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `greater_than => b
    | `less_than
    | `equal_to => a
    };

/**
 * Finds the minimum of two values using the given ORD module
 */
let min = (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
  module Ord = (val ord);
  minBy(Ord.compare, a, b);
};

/**
 * Finds the maximum of two values using the given compare function
 */
let maxBy: 'a. (compare('a), 'a, 'a) => 'a =
  (compare, a, b) =>
    switch (compare(a, b)) {
    | `less_than => b
    | `greater_than
    | `equal_to => a
    };

/**
 * Finds the maximum of two values using the given ORD module
 */
let max = (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
  module Ord = (val ord);
  maxBy(Ord.compare, a, b);
};

/**
 * Indicates if the item on the left is less than the item on the right using the given compare function
 */
let lessThanBy: 'a. (compare('a), 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `less_than;

/**
 * Alias for `lessThanBy`
 */
let ltBy = lessThanBy;

/**
 * Indicates if the item on the left is less than the item on the right using the given ORD module
 */
let lessThan =
    (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
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
let lessThanOrEqBy: 'a. (compare('a), 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `greater_than;

/**
 * Alias for lessThanOrEqBy`
 */
let lteBy = lessThanOrEqBy;

/**
 * Indicates if the item on the left is less than or equal to the item on the right using the given ORD module
 */
let lessThanOrEq =
    (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
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
let greaterThanBy: 'a. (compare('a), 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) == `greater_than;

/**
 * Alias for `greaterThanBy`
 */
let gtBy = greaterThanBy;

/**
 * Indicates if the item on the left is greater than the item on the right using the given ORD module
 */
let greaterThan =
    (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
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
let greaterThanOrEqBy: 'a. (compare('a), 'a, 'a) => bool =
  (compare, a, b) => compare(a, b) != `less_than;

/**
 * Alias for `greaterThanOrEqBy`
 */
let gteBy = greaterThanOrEqBy;

/**
 * Indicates if the item on the left is greater than or equal to the item on the right using the given ORD module
 */
let greaterThanOrEq =
    (type a, ord: (module BsBastet.Interface.ORD with type t = a), a, b) => {
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
let clampBy: 'a. (compare('a), ~min: 'a, ~max: 'a, 'a) => 'a =
  (compare, ~min, ~max, v) => minBy(compare, max, maxBy(compare, min, v));

/**
 * First-class module version of clampBy
 */
let clamp =
    (
      type a,
      ord: (module BsBastet.Interface.ORD with type t = a),
      ~min: a,
      ~max: a,
      x,
    ) => {
  module Ord = (val ord);
  clampBy(Ord.compare, ~min, ~max, x);
};

/**
  Determine whether a provided value falls between a min and max.
 */
let betweenBy: 'a. (compare('a), ~min: 'a, ~max: 'a, 'a) => bool =
  (compare, ~min, ~max, v) =>
    greaterThanOrEqBy(compare, v, min) && lessThanOrEqBy(compare, v, max);

/**
 * First-class module version of betweenBy
 */
let between =
    (
      type a,
      ord: (module BsBastet.Interface.ORD with type t = a),
      ~min: a,
      ~max: a,
      x,
    ) => {
  module Ord = (val ord);
  betweenBy(Ord.compare, ~min, ~max, x);
};

/**
  Absolute value: if x is gte zero, return zero, otherwise negate x.
 */
let abs =
    (
      type a,
      ord: (module BsBastet.Interface.ORD with type t = a),
      ring: (module BsBastet.Interface.RING with type t = a),
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
      ord: (module BsBastet.Interface.ORD with type t = a),
      ring: (module BsBastet.Interface.RING with type t = a),
      x,
    )
    : a => {
  module Ring = (val ring);
  Ring.(gte(ord, x, zero) ? one : subtract(zero, one));
};
