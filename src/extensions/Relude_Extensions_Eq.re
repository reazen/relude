open BsBastet.Interface;

/**
Extensions for any EQ
*/
module EqExtensions = (Eq: EQ) => {
  /**
  Creates a new equality function by contramapping the given conversion function
  */
  let eqWithConversion: 'b. ('b => Eq.t) => Relude_Eq.eq('b) =
    bToA => Relude_Eq.by(bToA, Eq.eq);

  /**
  Indicates if the two items are not equal
  */
  let notEq = Eq.eq |> Relude_Eq.invert;

  /**
  Alias for notEq
  */
  let eqInverted = notEq;

  /**
  An Eq module which is the inverse of the given Eq module
  */
  module EqInverted: EQ with type t = Eq.t = {
    type t = Eq.t;
    let eq = eqInverted;
  };

  module type EQ_BY_F =
    (A: Relude_Interface.FUNCTION_1 with type b = Eq.t) =>
     EQ with type t = A.a;

  /**
  Creates a new Eq for a type [b], given an Eq for type [a] and a function
  [b => a].

  {[
    // Create an Eq for User, given an Eq for string and a function from User => string
    module UserEq = String.EqBy({
      type a = User.t,
      type b = string;
      let f = user => user.email;
    });
  ]}
  */
  module EqBy: EQ_BY_F =
    (A: Relude_Interface.FUNCTION_1 with type b = Eq.t) => {
      type t = A.a;

      let eq: (t, t) => bool = (b1, b2) => Eq.eq(A.f(b1), A.f(b2));
    };
};

/**
 Infix operator extensions for any EQ
*/
module EqInfix = (Eq: EQ) => {
  module EqExtensions = EqExtensions(Eq);

  // Note: if we want to change these, try for consistency with ORD operators

  /**
  Equals operator
  */
  let (|=|) = Eq.eq;

  /**
  Not-equals operator
  */
  let (|!=|) = EqExtensions.notEq;
};
