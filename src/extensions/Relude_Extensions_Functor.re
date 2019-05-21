module FunctorExtensions = (F: BsAbstract.Interface.FUNCTOR) => {
  module BsFunctorExtensions = BsAbstract.Functions.Functor(F);

  let flipMap: 'a 'b. (F.t('a), 'a => 'b) => F.t('b) =
    (fa, f) => F.map(f, fa);

  let void: 'a. F.t('a) => F.t(unit) = BsFunctorExtensions.void;

  let voidRight: 'a 'b. ('a, F.t('b)) => F.t('a) = BsFunctorExtensions.void_right;

  let voidLeft: 'a 'b. (F.t('a), 'b) => F.t('b) = BsFunctorExtensions.void_left;

  let flap: 'a 'b. (F.t('a => 'b), 'a) => F.t('b) = BsFunctorExtensions.flap;
};

module FunctorInfix = (F: BsAbstract.Interface.FUNCTOR) => {
  module FunctorExtensions = FunctorExtensions(F);

  let (<$>) = F.map;

  let (<#>) = FunctorExtensions.flipMap;

  let (<$) = FunctorExtensions.voidRight;

  let ($>) = FunctorExtensions.voidLeft;

  let (<@>) = FunctorExtensions.flap;
};

// Experimental extensions for FUNCTORs that have 2 type params

module Functor2Extensions = (F: Relude_Interface.FUNCTOR2) => {
  let flipMap: 'a 'b 'e. (F.t('a, 'e), 'a => 'b) => F.t('b, 'e) =
    (fa, f) => F.map(f, fa);

  let void: 'a 'e. F.t('a, 'e) => F.t(unit, 'e) = fa => F.map(_ => (), fa);

  let voidRight: 'a 'b 'e. ('a, F.t('b, 'e)) => F.t('a, 'e) =
    (a, fb) => F.map(_ => a, fb);

  let voidLeft: 'a 'b 'e. (F.t('a, 'e), 'b) => F.t('b, 'e) =
    (fa, b) => F.map(_ => b, fa);

  let flap: 'a 'b. (F.t('a => 'b, 'e), 'a) => F.t('b, 'e) =
    (ff, a) => F.map(f => f(a), ff);
};

module Functor2Infix = (F: Relude_Interface.FUNCTOR2) => {
  module Functor2Extensions = Functor2Extensions(F);

  let (<$>) = F.map;

  let (<#>) = Functor2Extensions.flipMap;

  let (<$) = Functor2Extensions.voidRight;

  let ($>) = Functor2Extensions.voidLeft;

  let (<@>) = Functor2Extensions.flap;
};