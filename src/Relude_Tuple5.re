open Bastet.Interface;

/**
Constructs a tuple-5 from 5 values
*/
let make: 'a 'b 'c 'd 'e. ('a, 'b, 'c, 'd, 'e) => ('a, 'b, 'c, 'd, 'e) =
  (a, b, c, d, e) => (a, b, c, d, e);

/**
Constructs a tuple-5 from an array of exactly 5 values
*/
let fromArray: 'a. array('a) => option(('a, 'a, 'a, 'a, 'a)) =
  fun
  | [|a, b, c, d, e|] => Some((a, b, c, d, e))
  | _ => None;

/**
Constructs a tuple-5 from an array of at least 5 values
*/
let fromArrayAtLeast: 'a. array('a) => option(('a, 'a, 'a, 'a, 'a)) =
  xs => Relude_Array.take(5, xs) |> fromArray;

/**
Constructs a tuple-5 from a list of exactly 5 values
*/
let fromList: 'a. list('a) => option(('a, 'a, 'a, 'a, 'a)) =
  xs => Relude_List.(take(6, xs) |> toArray) |> fromArray;

/**
Constructs a tuple-5 from a list of at least 5 values
*/
let fromListAtLeast: 'a. list('a) => option(('a, 'a, 'a, 'a, 'a)) =
  xs => Relude_List.take(5, xs) |> fromList;

/**
Applies a normal 5-argument function to arguments contained in a tuple-5
*/
let apply:
  'a 'b 'c 'd 'e 'f.
  (('a, 'b, 'c, 'd, 'e) => 'f, ('a, 'b, 'c, 'd, 'e)) => 'f
 =
  (f, (a, b, c, d, e)) => f(a, b, c, d, e);

/**
Gets the first value of a tuple-5
*/
let first: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e)) => 'a =
  ((a, _b, _c, _d, _e)) => a;

/**
Gets the second value of a tuple-5
*/
let second: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e)) => 'b =
  ((_a, b, _c, _d, _e)) => b;

/**
Gets the third value of a tuple-5
*/
let third: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e)) => 'c =
  ((_a, _b, c, _d, _e)) => c;

/**
Gets the fourth value of a tuple-5
*/
let fourth: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e)) => 'd =
  ((_a, _b, _c, d, _e)) => d;

/**
Gets the fifth value of a tuple-5
*/
let fifth: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e)) => 'e =
  ((_a, _b, _c, _d, e)) => e;

let showBy:
  'a 'b 'c 'd 'e.
  (
    'a => string,
    'b => string,
    'c => string,
    'd => string,
    'e => string,
    ('a, 'b, 'c, 'd, 'e)
  ) =>
  string
 =
  (showA, showB, showC, showD, showE, (a, b, c, d, e)) =>
    "("
    ++ showA(a)
    ++ ", "
    ++ showB(b)
    ++ ", "
    ++ showC(c)
    ++ ", "
    ++ showD(d)
    ++ ", "
    ++ showE(e)
    ++ ")";

let eqBy:
  'a 'b 'c 'd 'e.
  (
    ('a, 'a) => bool,
    ('b, 'b) => bool,
    ('c, 'c) => bool,
    ('d, 'd) => bool,
    ('e, 'e) => bool,
    ('a, 'b, 'c, 'd, 'e),
    ('a, 'b, 'c, 'd, 'e)
  ) =>
  bool
 =
  (eqA, eqB, eqC, eqD, eqE, (a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
    eqA(a1, a2)
    && eqB(b1, b2)
    && eqC(c1, c2)
    && eqD(d1, d2)
    && eqE(e1, e2);

module WithEqs = (EqA: EQ, EqB: EQ, EqC: EQ, EqD: EQ, EqE: EQ) => {
  type t = (EqA.t, EqB.t, EqC.t, EqD.t, EqE.t);
  let eq = eqBy(EqA.eq, EqB.eq, EqC.eq, EqD.eq, EqE.eq);

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
    EqE: EQ,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (EqA.t, EqB.t, EqC.t, EqD.t, EqE.t),
  ) =>
   EQ with type t = A.a;

module EqBy: EQ_BY_F =
  (
    EqA: EQ,
    EqB: EQ,
    EqC: EQ,
    EqD: EQ,
    EqE: EQ,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (EqA.t, EqB.t, EqC.t, EqD.t, EqE.t),
  ) => {
    type t = A.a;
    let eq = (t1, t2) => {
      eqBy(EqA.eq, EqB.eq, EqC.eq, EqD.eq, EqE.eq, A.f(t1), A.f(t2));
    };
  };

let compareBy:
  'a 'b 'c 'd 'e.
  (
    ('a, 'a) => Relude_Ordering.t,
    ('b, 'b) => Relude_Ordering.t,
    ('c, 'c) => Relude_Ordering.t,
    ('d, 'd) => Relude_Ordering.t,
    ('e, 'e) => Relude_Ordering.t,
    ('a, 'b, 'c, 'd, 'e),
    ('a, 'b, 'c, 'd, 'e)
  ) =>
  Relude_Ordering.t
 =
  (
    compareA,
    compareB,
    compareC,
    compareD,
    compareE,
    (a1, b1, c1, d1, e1),
    (a2, b2, c2, d2, e2),
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
        | `equal_to =>
          switch (compareD(d1, d2)) {
          | `less_than => `less_than
          | `greater_than => `greater_than
          | `equal_to => compareE(e1, e2)
          }
        }
      }
    };

module WithOrds = (OrdA: ORD, OrdB: ORD, OrdC: ORD, OrdD: ORD, OrdE: ORD) => {
  include WithEqs(OrdA, OrdB, OrdC, OrdD, OrdE);
  let compare =
    compareBy(
      OrdA.compare,
      OrdB.compare,
      OrdC.compare,
      OrdD.compare,
      OrdE.compare,
    );

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
    OrdE: ORD,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (OrdA.t, OrdB.t, OrdC.t, OrdD.t, OrdE.t),
  ) =>
   ORD with type t = A.a;

module OrdBy: ORD_BY_F =
  (
    OrdA: ORD,
    OrdB: ORD,
    OrdC: ORD,
    OrdD: ORD,
    OrdE: ORD,
    A:
      Relude_Interface.FUNCTION_1 with
        type b = (OrdA.t, OrdB.t, OrdC.t, OrdD.t, OrdE.t),
  ) => {
    include EqBy(OrdA, OrdB, OrdC, OrdD, OrdE, A);
    let compare = (t1, t2) => {
      compareBy(
        OrdA.compare,
        OrdB.compare,
        OrdC.compare,
        OrdD.compare,
        OrdE.compare,
        A.f(t1),
        A.f(t2),
      );
    };
  };
