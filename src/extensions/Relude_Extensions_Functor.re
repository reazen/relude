module FunctorExtensions = (F: BsAbstract.Interface.FUNCTOR) => {
  module BsFunctorExtensions = BsAbstract.Functions.Functor(F);

  //let map: 'a 'b. ('a => 'b, F.t('a)) => F.t('b) = F.map;

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