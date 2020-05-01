open BsBastet.Interface;
open Relude_Function.Infix;

module WithFunctor = (F: FUNCTOR) => {
  type t('a) =
    | Pure('a): t('a)
    | Apply(F.t('x), t('x => 'a)): t('a);

  let rec map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (aToB, freeA) =>
      switch (freeA) {
      | Pure(a) => Pure(aToB(a))
      | Apply(fx, freeXToA) =>
        Apply(fx, freeXToA |> map(xToA => xToA >> aToB))
      };

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  let rec apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
    (freeAToB, freeA) =>
      switch (freeAToB) {
      | Pure(aToB) => freeA |> map(aToB)
      | Apply(fx, freeXToAToB) =>
        let freeAToXToB = freeXToAToB |> map(Relude_Function.flip);
        let freeXToB = apply(freeAToXToB, freeA);
        Apply(fx, freeXToB);
      };

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  let pure: 'a. 'a => t('a) = a => Pure(a);

  module Applicative: APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  /**
  Lifts a value of our algebra into our free monad type

  This is useful to creating "smart constructors" for our algebra
  */
  let liftF: 'a. F.t('a) => t('a) = fa => Apply(fa, Pure(a => a));

  /**
  Specifies an applicative into which we will interpret our free monadic program

  We also need a Natural Transformation module here, because putting that
  function inline doesn't seem to work with the existential type captured by the
  FreeAp (at least I couldn't figure it out).
  */
  module WithApplicative = (A: APPLICATIVE) => {
    /**
    We also need a natural transformation in order to create our interpreters
    for the free applicative. Because of the existential type captured by the
    free applicative and what appear to be limitations of OCaml, the NT is
    provided via a module, so that we can achieve the [forall a. f a -> g a].

    I tried to get this to work using a [F.t('a) => A.t('a)] function as a
    normal function argument, but I couldn't figure it out. This worked for free
    monad, but I think the problem here is the existential "x" type that is
    captured by this type.
    */
    module WithNT =
           (
             NT:
               Relude_Interface.NATURAL_TRANSFORMATION with
                 type f('a) = F.t('a) and type g('a) = A.t('a),
           ) => {
      /**
      Applies an interpreter function (natural transformation) to interpret each
      value of our algebra into a target applicative.
      */
      let rec foldFree: 'a. t('a) => A.t('a) =
        freeA =>
          switch (freeA) {
          | Pure(a) => A.pure(a)
          | Apply(fx, freeXToA) => A.apply(foldFree(freeXToA), NT.f(fx))
          };
    };
  };

  module WithApplicativeAndNT =
         (
           A: APPLICATIVE,
           NT:
             Relude_Interface.NATURAL_TRANSFORMATION with
               type f('a) = F.t('a) and type g('a) = A.t('a),
         ) => {
    module A = WithApplicative(A);
    include A.WithNT(NT);
  };

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
  };
};
