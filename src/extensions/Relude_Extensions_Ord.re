open BsBastet.Interface;

/**
[Extensions.Ord] is a module functor that accepts any [ORD]-compatible module
and returns a collection of extra helper functions specific to that type. For
example, imagine we define a custom [weekday] type that can be compared for
ordering:

{[
  type weekday = Sun | Mon | Tue | Wed | Thu | Fri | Sat;

  // simple variants like this can be safely compared with the comparison
  // operations from the OCaml stdlib. This comparison uses the order in which
  // the constructors were defined, and we convert to Bastet ordering
  let compare = (a: weekday, b: weekday) =>
    a < b ? `less_than : a > b ? `greater_than : equal_to;

  let eq = (a: weekday, b: weekday) => a == b;

  // now we can construct a module of extra Ord functions
  module WeekdayOrd = Relude.Extensions.Ord.OrdExtensions({
    type t = weekday;
    let eq = eq;
    let compare = compare;
  });

  // and we can use these functions with our weekday type
  let isBetween = WeekdayOrd.between(~min=Tue, ~max=Fri, Wed); // true
]}
*/
module OrdExtensions = (O: ORD) => {
  let compareWithConversion: ('b => O.t) => Relude_Ord.compare('b) =
    bToA => Relude_Ord.by(bToA, O.compare);

  let compareReversed = O.compare |> Relude_Ord.reverse;

  /**
  Creates a new Ord module which is the reverse of the given Ord
  */
  module OrdReversed: ORD with type t = O.t = {
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

  module OrdRingExtensions = (R: RING with type t = O.t) => {
    let abs: R.t => R.t = v => Relude_Ord.abs((module O), (module R), v);

    let signum: R.t => R.t =
      v => Relude_Ord.signum((module O), (module R), v);
  };

  /**
  OrdNamed contains versions of the comparison functions that have a named argument
  to disambiguate what's on the lhs vs. rhs of the comparison.  The ~compareTo values
  are on the rhs.

  {[
    // 10 < 100
    let result = 10 |> Int.OrdNamed.lessThan(~compareTo=100);
  ]}
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
    (A: Relude_Interface.FUNCTION_1 with type b = O.t) =>
     ORD with type t = A.a;

  /**
  Creates an ORD for type b given this ORD of type a and an ARROW `b => a` to act as the contravariant

  {[
    // Create an Ord for a user using an Ord for string and a function from User => string
    module UserOrd = String.OrdBy({
      type a = User.t;
      type b = string;
      let f = user => user.email;
    });
  ]}
  */
  module OrdBy: ORD_BY_F =
    (A: Relude_Interface.FUNCTION_1 with type b = O.t) => {
      include Relude_Extensions_Eq.EqExtensions(O); // Get the EqBy module functor, so we can get the EQ bits using the Arrow
      include EqBy(A); // Get the type t and eq functions using the Arrow
      let compare: (t, t) => ordering =
        (b1, b2) => O.compare(A.f(b1), A.f(b2));
    };
};

module OrdInfix = (O: ORD) => {
  module OrdExtensions = OrdExtensions(O);

  // Note: if we want to change these, try for consistency with EQ operators

  /**
  Less-than operator
  */
  let (|<|) = OrdExtensions.lessThan;

  /**
  Less-than-or-equal operator
  */
  let (|<=|) = OrdExtensions.lessThanOrEq;

  /**
  Greater-than operator
  */
  let (|>|) = OrdExtensions.greaterThan;

  /**
  Greater-than-or-equal operator
  */
  let (|>=|) = OrdExtensions.greaterThanOrEq;
};
