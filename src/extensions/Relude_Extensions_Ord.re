type ordering = BsAbstract.Interface.ordering;
type compare('a) = ('a, 'a) => ordering;

module OrdExtensions = (O: BsAbstract.Interface.ORD) => {
  let compareWithConversion: ('b => O.t) => compare('b) =
    bToA => Relude_Ord.by(bToA, O.compare);

  let compareReversed = O.compare |> Relude_Ord.reverse;

  /**
   * Creates a new Ord module which is the reverse of the given Ord
   */
  module OrdReversed: BsAbstract.Interface.ORD with type t = O.t = {
    type t = O.t;
    let eq = O.eq;
    let compare = compareReversed;
  };

  let compareAsInt: (O.t, O.t) => int =
    (a, b) => Relude_Ord.compareAsIntBy(O.compare, a, b);

  let min: (O.t, O.t) => O.t = (a, b) => Relude_Ord.minBy(O.compare, a, b);

  let max: (O.t, O.t) => O.t = (a, b) => Relude_Ord.maxBy(O.compare, a, b);

  let lessThan: (O.t, O.t) => bool =
    (a, b) => Relude_Ord.lessThanBy(O.compare, a, b);

  let lt: (O.t, O.t) => bool = lessThan;

  let lessThanOrEq: (O.t, O.t) => bool =
    (a, b) => Relude_Ord.lessThanOrEqBy(O.compare, a, b);

  let lte: (O.t, O.t) => bool = lessThanOrEq;

  let greaterThan: (O.t, O.t) => bool =
    (a, b) => Relude_Ord.greaterThanBy(O.compare, a, b);

  let gt: (O.t, O.t) => bool = greaterThan;

  let greaterThanOrEq: (O.t, O.t) => bool =
    (a, b) => Relude_Ord.greaterThanOrEqBy(O.compare, a, b);

  let gte: (O.t, O.t) => bool = greaterThanOrEq;

  let clamp: (~min: O.t, ~max: O.t, O.t) => O.t =
    (~min, ~max, v) => Relude_Ord.clampBy(O.compare, ~min, ~max, v);

  let between: (~min: O.t, ~max: O.t, O.t) => bool =
    (~min, ~max, v) => Relude_Ord.betweenBy(O.compare, ~min, ~max, v);

  module OrdRingExtensions = (R: BsAbstract.Interface.RING with type t = O.t) => {
    let abs: R.t => R.t = v => Relude_Ord.abs((module O), (module R), v);

    let signum: R.t => R.t =
      v => Relude_Ord.signum((module O), (module R), v);
  };

  /**
   * OrdNamed contains versions of the comparison functions that have a named argument
   * to disambiguate what's on the lhs vs. rhs of the comparison.  The ~compareTo values
   * are on the rhs.
   *
   * Example:
   * ```
   * // 10 < 100
   * let result = 10 |> Int.OrdNamed.lessThan(~compareTo=100);
   * ```
   */
  module OrdNamed = {
    let lessThan = (~compareTo, a) =>
      Relude_Ord.lessThanBy(O.compare, a, compareTo);

    let lessThanOrEq = (~compareTo, a) =>
      Relude_Ord.lessThanOrEqBy(O.compare, a, compareTo);

    let greaterThan = (~compareTo, a) =>
      Relude_Ord.greaterThanBy(O.compare, a, compareTo);

    let greaterThanOrEq = (~compareTo, a) =>
      Relude_Ord.greaterThanOrEqBy(O.compare, a, compareTo);

    let lt = lessThan;

    let lte = lessThanOrEq;

    let gt = greaterThan;

    let gte = greaterThanOrEq;
  };

  module type ORD_BY_F =
    (A: Relude_Interface.ARROW with type b = O.t) =>
     BsAbstract.Interface.ORD with type t = A.a;

  /**
   * Creates an ORD for type b given this ORD of type a and an ARROW `b => a` to act as the contravariant
   *
   * Example:
   * ```
   * // Create an Ord for a user using an Ord for string and a function from User => string
   * module UserOrd = String.OrdBy({
   *   type a = User.t;
   *   type b = string;
   *   let f = user => user.email;
   * });
   * ```
   */
  module OrdBy: ORD_BY_F =
    (A: Relude_Interface.ARROW with type b = O.t) => {
      include Relude_Extensions_Eq.EqExtensions(O); // Get the EqBy module functor, so we can get the EQ bits using the Arrow
      include EqBy(A); // Get the type t and eq functions using the Arrow
      let compare: (t, t) => ordering =
        (b1, b2) => O.compare(A.f(b1), A.f(b2));
    };
};

module OrdInfix = (O: BsAbstract.Interface.ORD) => {
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