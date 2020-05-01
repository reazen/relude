open BsBastet.Interface;

/**
Constructs a tuple-4 from 4 values
*/
let make: 'a 'b 'c 'd. ('a, 'b, 'c, 'd) => ('a, 'b, 'c, 'd) =
  (a, b, c, d) => (a, b, c, d);

/**
Constructs a tuple-4 from an array of exactly 4 values
*/
let fromArray: 'a. array('a) => option(('a, 'a, 'a, 'a)) =
  fun
  | [|a, b, c, d|] => Some((a, b, c, d))
  | _ => None;

/**
Constructs a tuple-4 from an array of at least 4 values
*/
let fromArrayAtLeast: 'a. array('a) => option(('a, 'a, 'a, 'a)) =
  xs => Relude_Array.take(4, xs) |> fromArray;

/**
Constructs a tuple-4 from a list of exactly 4 values
*/
let fromList: 'a. list('a) => option(('a, 'a, 'a, 'a)) =
  xs => Relude_List.(take(5, xs) |> toArray) |> fromArray;

/**
Constructs a tuple-4 from a list of at least 4 values
*/
let fromListAtLeast: 'a. list('a) => option(('a, 'a, 'a, 'a)) =
  xs => Relude_List.take(4, xs) |> fromList;

let showBy:
  'a 'b 'c 'd.
  (
    'a => string,
    'b => string,
    'c => string,
    'd => string,
    ('a, 'b, 'c, 'd)
  ) =>
  string
 =
  (showA, showB, showC, showD, (a, b, c, d)) =>
    "("
    ++ showA(a)
    ++ ", "
    ++ showB(b)
    ++ ", "
    ++ showC(c)
    ++ ", "
    ++ showD(d)
    ++ ")";

let eqBy:
  'a 'b 'c 'd.
  (
    ('a, 'a) => bool,
    ('b, 'b) => bool,
    ('c, 'c) => bool,
    ('d, 'd) => bool,
    ('a, 'b, 'c, 'd),
    ('a, 'b, 'c, 'd)
  ) =>
  bool
 =
  (eqA, eqB, eqC, eqD, (a1, b1, c1, d1), (a2, b2, c2, d2)) =>
    eqA(a1, a2) && eqB(b1, b2) && eqC(c1, c2) && eqD(d1, d2);

module WithEqs = (EqA: EQ, EqB: EQ, EqC: EQ, EqD: EQ) => {
  type t = (EqA.t, EqB.t, EqC.t, EqD.t);
  let eq = eqBy(EqA.eq, EqB.eq, EqC.eq, EqD.eq);

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
    EqD: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t, EqC.t, EqD.t),
  ) =>
   EQ with type t = A.a;

module EqBy: EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    EqC: EQ,
    EqD: EQ,
    A: Relude_Interface.FUNCTION_1 with type b = (EqA.t, EqB.t, EqC.t, EqD.t),
  ) => {
    type t = A.a;
    let eq = (t1, t2) => {
      eqBy(EqA.eq, EqB.eq, EqC.eq, EqD.eq, A.f(t1), A.f(t2));
    };
  };

let compareBy:
  'a 'b 'c 'd.
  (
    ('a, 'a) => Relude_Ordering.t,
    ('b, 'b) => Relude_Ordering.t,
    ('c, 'c) => Relude_Ordering.t,
    ('d, 'd) => Relude_Ordering.t,
    ('a, 'b, 'c, 'd),
    ('a, 'b, 'c, 'd)
  ) =>
  Relude_Ordering.t
 =
  (
    compareA,
    compareB,
    compareC,
    compareD,
    (a1, b1, c1, d1),
    (a2, b2, c2, d2),
  ) =>
    switch (compareA(a1, a2)) {
    | `less_than => `less_than
    | `greater_than => `greater_than
    | `equal_to =>
      switch (compareB(b1, b2)) {
      | `less_than => `less_than
      | `greater_than => `greater_than
      | `equal_to =>
        switch (compareC(c1, c2)) {
        | `less_than => `less_than
        | `greater_than => `greater_than
        | `equal_to => compareD(d1, d2)
        }
      }
    };

module WithOrds = (OrdA: ORD, OrdB: ORD, OrdC: ORD, OrdD: ORD) => {
  include WithEqs(OrdA, OrdB, OrdC, OrdD);
  let compare =
    compareBy(OrdA.compare, OrdB.compare, OrdC.compare, OrdD.compare);

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
    OrdD: ORD,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (OrdA.t, OrdB.t, OrdC.t, OrdD.t),
  ) =>
   ORD with type t = A.a;

module OrdBy: ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    OrdC: ORD,
    OrdD: ORD,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (OrdA.t, OrdB.t, OrdC.t, OrdD.t),
  ) => {
    include EqBy(OrdA, OrdB, OrdC, OrdD, A);
    let compare = (t1, t2) => {
      compareBy(
        OrdA.compare,
        OrdB.compare,
        OrdC.compare,
        OrdD.compare,
        A.f(t1),
        A.f(t2),
      );
    };
  };
