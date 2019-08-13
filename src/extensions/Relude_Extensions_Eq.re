/**
 * Extensions for any EQ
 */
module EqExtensions = (Eq: BsAbstract.Interface.EQ) => {
  /**
   * Indicates if the two items are not equal
   */
  let notEq: (Eq.t, Eq.t) => bool = (a, b) => !Eq.eq(a, b);
};

/**
 * Infix operator extensions for any EQ
 */
module EqInfix = (Eq: BsAbstract.Interface.EQ) => {
  module EqExtensions = EqExtensions(Eq);

  // Note: if we want to change these, try for consistency with ORD operators

  /**
   * Equals operator
   */
  let (|=|) = Eq.eq;

  /**
   * Not-equals operator
   */
  let (|!=|) = EqExtensions.notEq;
};