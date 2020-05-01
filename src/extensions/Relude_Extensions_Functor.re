/**
Extensions for any FUNCTOR
*/
module FunctorExtensions = (F: BsBastet.Interface.FUNCTOR) => {
  module BsFunctorExtensions = BsBastet.Functions.Functor(F);

  /**
  Flipped version of the map function which has the functor on the left, and the
  function on the right.
  */
  let flipMap: 'a 'b. (F.t('a), 'a => 'b) => F.t('b) =
    (fa, f) => F.map(f, fa);

  /**
  Clears the value(s) of a functor by mapping a function that produces unit for
  each value in the functor.
  */
  let void: 'a. F.t('a) => F.t(unit) = BsFunctorExtensions.void;

  /**
  Replaces the value(s) of the functor on the right with the value on the left.
  */
  let voidRight: 'a 'b. ('a, F.t('b)) => F.t('a) = BsFunctorExtensions.void_right;

  /**
  Replaces the value(s) of the functor on the left with the value on the right.
  */
  let voidLeft: 'a 'b. (F.t('a), 'b) => F.t('b) = BsFunctorExtensions.void_left;

  /**
  Applies an argument of type ['a] to a functor of ['a => 'b] functions, to
  produce a functor of ['b]
  */
  let flap: 'a 'b. (F.t('a => 'b), 'a) => F.t('b) = BsFunctorExtensions.flap;
};

/**
Infix operator extensions for any FUNCTOR
*/
module FunctorInfix = (F: BsBastet.Interface.FUNCTOR) => {
  module FunctorExtensions = FunctorExtensions(F);

  /**
  Operator version of the [map] function, which has the function on the left,
  and functor on the right.
  */
  let (<$>) = F.map;

  /**
  Operator version of the [flipMap] function, which has the functor on the left,
  and function on the right.
  */
  let (<#>) = FunctorExtensions.flipMap;

  /**
  Operator version of voidRight, which replaces the values of the functor on the
  right with the value on the left.
  */
  let (<$) = FunctorExtensions.voidRight;

  /**
  Operator version of voidLeft, which replaces the values of the functor on the
  left with the value on the right.
  */
  let ($>) = FunctorExtensions.voidLeft;

  /**
  Operator version of flap, which takes a functor of functions [F.t('a => 'b)]
  on the left, and an ['a] on the right, and applies the value ['a] to each
  function, producing a functor of ['b] ([F.t('b)]).
  */
  let (<@>) = FunctorExtensions.flap;
};
