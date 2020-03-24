open BsBastet.Interface;

/**
 * Constructs a tuple-3 from 3 values
 */
let make: 'a 'b 'c. ('a, 'b, 'c) => ('a, 'b, 'c) = (a, b, c) => (a, b, c);

/**
 * Constructs a tuple-3 from an array of exactly 3 values
 */
let fromArray: 'a. array('a) => option(('a, 'a, 'a)) =
  fun
  | [|a, b, c|] => Some((a, b, c))
  | _ => None;

/**
 * Constructs a tuple-3 from an array of at least 3 values
 */
let fromArrayAtLeast: 'a. array('a) => option(('a, 'a, 'a)) =
  xs => Relude_Array.take(3, xs) |> fromArray;

/**
 * Constructs a tuple-3 from a list of exactly 3 values
 */
let fromList: 'a. list('a) => option(('a, 'a, 'a)) =
  xs => Relude_List.(take(4, xs) |> toArray) |> fromArray;

/**
 * Constructs a tuple-3 from a list of at least 3 values
 */
let fromListAtLeast: 'a. list('a) => option(('a, 'a, 'a)) =
  xs => Relude_List.take(3, xs) |> fromList;

let showBy:
  'a 'b 'c.
  ('a => string, 'b => string, 'c => string, ('a, 'b, 'c)) => string
 =
  (showA, showB, showC, (a, b, c)) =>
    "(" ++ showA(a) ++ ", " ++ showB(b) ++ ", " ++ showC(c) ++ ")";

let eqBy:
  'a 'b 'c.
  (
    ('a, 'a) => bool,
    ('b, 'b) => bool,
    ('c, 'c) => bool,
    ('a, 'b, 'c),
    ('a, 'b, 'c)
  ) =>
  bool
 =
  (eqA, eqB, eqC, (a1, b1, c1), (a2, b2, c2)) =>
    eqA(a1, a2) && eqB(b1, b2) && eqC(c1, c2);

module WithEqs = (EqA: EQ, EqB: EQ, EqC: EQ) => {
  type t = (EqA.t, EqB.t, EqC.t);
  let eq = eqBy(EqA.eq, EqB.eq, EqC.eq);

  module Eq: EQ with type t = t = {
    type nonrec t = t;
    let eq = eq;
  };
  include Relude_Extensions_Eq.EqExtensions(Eq);
};

module type EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    EqC: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t, EqC.t),
  ) =>
   EQ with type t = A.a;

module EqBy: EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    EqC: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t, EqC.t),
  ) => {
    type t = A.a;
    let eq = (t1, t2) => {
      eqBy(EqA.eq, EqB.eq, EqC.eq, A.f(t1), A.f(t2));
    };
  };

let compareBy:
  'a 'b 'c.
  (
    ('a, 'a) => Relude_Ordering.t,
    ('b, 'b) => Relude_Ordering.t,
    ('c, 'c) => Relude_Ordering.t,
    ('a, 'b, 'c),
    ('a, 'b, 'c)
  ) =>
  Relude_Ordering.t
 =
  (compareA, compareB, compareC, (a1, b1, c1), (a2, b2, c2)) =>
    switch (compareA(a1, a2)) {
    | `less_than => `less_than
    | `greater_than => `greater_than
    | `equal_to =>
      switch (compareB(b1, b2)) {
      | `less_than => `less_than
      | `greater_than => `greater_than
      | `equal_to => compareC(c1, c2)
      }
    };

module WithOrds = (OrdA: ORD, OrdB: ORD, OrdC: ORD) => {
  include WithEqs(OrdA, OrdB, OrdC);
  let compare = compareBy(OrdA.compare, OrdB.compare, OrdC.compare);

  module Ord: ORD with type t = t = {
    include Eq;
    let compare = compare;
  };
  include Relude_Extensions_Ord.OrdExtensions(Ord);
};

module type ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    OrdC: ORD,
    A: Relude_Interface.FUNCTION_1 with type b = (OrdA.t, OrdB.t, OrdC.t),
  ) =>
   ORD with type t = A.a;

module OrdBy: ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    OrdC: ORD,
    A: Relude_Interface.FUNCTION_1 with type b = (OrdA.t, OrdB.t, OrdC.t),
  ) => {
    include EqBy(OrdA, OrdB, OrdC, A);
    let compare = (t1, t2) => {
      compareBy(OrdA.compare, OrdB.compare, OrdC.compare, A.f(t1), A.f(t2));
    };
  };
