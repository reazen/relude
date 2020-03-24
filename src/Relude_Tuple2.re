open BsBastet.Interface;

/**
 * Constructs a tuple-2 from 2 values
 */
let make: 'a 'b. ('a, 'b) => ('a, 'b) = (a, b) => (a, b);

/**
 * Constructs a tuple-2 from an array of exactly 2 values
 */
let fromArray: 'a. array('a) => option(('a, 'a)) =
  fun
  | [|a, b|] => Some((a, b))
  | _ => None;

/**
 * Constructs a tuple-2 from an array of at least 2 values
 */
let fromArrayAtLeast: 'a. array('a) => option(('a, 'a)) =
  xs => Relude_Array.take(2, xs) |> fromArray;

/**
 * Constructs a tuple-2 from a list of exactly 2 values
 */
let fromList: 'a. list('a) => option(('a, 'a)) =
  xs => Relude_List.(take(3, xs) |> toArray) |> fromArray;

/**
 * Constructs a tuple-2 from a list of at least 2 values
 */
let fromListAtLeast: 'a. list('a) => option(('a, 'a)) =
  xs => Relude_List.take(2, xs) |> fromList;

/**
 * Shows a tuple ('a, 'b) using show functions for 'a and 'b
 */
let showBy: 'a 'b. ('a => string, 'b => string, ('a, 'b)) => string =
  (showA, showB, (a, b)) => "(" ++ showA(a) ++ ", " ++ showB(b) ++ ")";

/**
 * Compares two tuples of type ('a, 'b) for equality using equality functions for 'a and 'b
 */
let eqBy:
  'a 'b.
  (('a, 'a) => bool, ('b, 'b) => bool, ('a, 'b), ('a, 'b)) => bool
 =
  (eqA, eqB, (a1, b1), (a2, b2)) => eqA(a1, a2) && eqB(b1, b2);

/**
 * Creates a module containing helpers and an Eq module and extensions for the type (a, b)
 */
module WithEqs = (EqA: EQ, EqB: EQ) => {
  type t = (EqA.t, EqB.t);
  let eq = eqBy(EqA.eq, EqB.eq);
  module Eq: EQ with type t = t = {
    type nonrec t = t;
    let eq = eq;
  };
  include Relude_Extensions_Eq.EqExtensions(Eq);
};

/**
 * Creates an Eq module for a type t, given an Arrow from t => (a, b) and EQ modules for a and b
 */
module type EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t),
  ) =>
   EQ with type t = A.a;

module EqBy: EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t),
  ) => {
    type t = A.a;
    let eq = (t1, t2) => {
      eqBy(EqA.eq, EqB.eq, A.f(t1), A.f(t2));
    };
  };

/**
 * Compares two tuples of type ('a, 'b) for ordering, given comparison functions for 'a and 'b
 *
 * The 'a value is checked first, and if the 'as are equal, the 'b compare is checked.
 */
let compareBy:
  'a 'b.
  (
    ('a, 'a) => Relude_Ordering.t,
    ('b, 'b) => Relude_Ordering.t,
    ('a, 'b),
    ('a, 'b)
  ) =>
  Relude_Ordering.t
 =
  (compareA, compareB, (a1, b1), (a2, b2)) =>
    switch (compareA(a1, a2)) {
    | `less_than => `less_than
    | `equal_to => compareB(b1, b2)
    | `greater_than => `greater_than
    };

/**
 * Creates an Ord module for (a, b), given Ord instances for a and b
 */
module WithOrds = (OrdA: ORD, OrdB: ORD) => {
  include WithEqs(OrdA, OrdB);
  let compare = compareBy(OrdA.compare, OrdB.compare);
  module Ord: ORD with type t = t = {
    include Eq;
    let compare = compare;
  };
  include Relude_Extensions_Ord.OrdExtensions(Ord);
};

/**
 * Creates an Ord instance for a type t, given an arrow from t => (a, b) and Ord instances for a and b
 */
module type ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    A: Relude_Interface.FUNCTION_1 with type b = (OrdA.t, OrdB.t),
  ) =>
   ORD with type t = A.a;

module OrdBy: ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    A: Relude_Interface.FUNCTION_1 with type b = (OrdA.t, OrdB.t),
  ) => {
    include EqBy(OrdA, OrdB, A);
    let compare = (t1, t2) => {
      compareBy(OrdA.compare, OrdB.compare, A.f(t1), A.f(t2));
    };
  };
