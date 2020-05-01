open BsBastet.Interface;

/**
Creates a ResultT Monad with the given outer Monad module.
*/
module WithMonad = (M: MONAD) => {
  type t('a, 'e) =
    | ResultT(M.t(result('a, 'e)));

  let make: 'a 'e. M.t(result('a, 'e)) => t('a, 'e) =
    mResult => ResultT(mResult);

  let runResultT: 'a 'e. t('a, 'e) => M.t(result('a, 'e)) =
    (ResultT(mResult)) => mResult;

  let withResultT: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
    (e1ToE2, ResultT(mResultE1)) =>
      ResultT(
        M.map(
          resultE1 => Relude_Result.mapError(e1ToE2, resultE1),
          mResultE1,
        ),
      );

  let mapResultT:
    'a 'b 'e.
    (M.t(result('a, 'e)) => M.t(result('b, 'e)), t('a, 'e)) => t('b, 'e)
   =
    (mResultAToMResultB, ResultT(mResultA)) =>
      ResultT(mResultAToMResultB(mResultA));

  let fromResult: 'a 'e. result('a, 'e) => t('a, 'e) =
    result => ResultT(M.pure(result));

  let liftF: 'a 'e. M.t('a) => t('a, 'e) =
    mA => ResultT(M.map(a => Ok(a), mA));

  let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
    (aToB, ResultT(mResultA)) =>
      ResultT(M.map(resultA => Relude_Result.map(aToB, resultA), mResultA));

  let subflatMap: 'a 'b 'e. ('a => result('b, 'e), t('a, 'e)) => t('b, 'e) =
    (aToB, ResultT(mResultA)) =>
      ResultT(
        M.map(resultA => Relude_Result.flatMap(aToB, resultA), mResultA),
      );

  let cond: 'a 'e. ('a => bool, 'a, 'e, t('a, 'e)) => t('a, 'e) =
    (aToBool, success, err, ResultT(mResultA)) =>
      ResultT(
        M.map(
          resultA =>
            Relude_Result.flatMap(
              a =>
                aToBool(a)
                  ? Relude_Result.pure(success) : Relude_Result.error(err),
              resultA,
            ),
          mResultA,
        ),
      );

  let condError: 'a 'b 'e. ('a => bool, 'e, t('a, 'e)) => t('a, 'e) =
    (aToBool, err, ResultT(mResultA)) =>
      ResultT(
        M.map(
          resultA =>
            Relude_Result.flatMap(
              a =>
                aToBool(a)
                  ? Relude_Result.pure(a) : Relude_Result.error(err),
              resultA,
            ),
          mResultA,
        ),
      );

  let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) = withResultT;

  let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
    (aToB, e1ToE2, ResultT(mResultAE1)) => {
      ResultT(
        M.map(
          resultAE1 => Relude_Result.bimap(aToB, e1ToE2, resultAE1),
          mResultAE1,
        ),
      );
    };

  let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
    (ResultT(mResultAToB), ResultT(mResultA)) =>
      ResultT(
        M.apply(
          M.map(
            (resultAToB, resultA) =>
              Relude_Result.apply(resultAToB, resultA),
            mResultAToB,
          ),
          mResultA,
        ),
      );

  let pure: 'a 'e. 'a => t('a, 'e) =
    a => ResultT(M.pure(Relude_Result.ok(a)));

  let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
    (ResultT(mResultA), aToResultTB) => {
      ResultT(
        M.flat_map(mResultA, resultA =>
          switch (resultA) {
          | Ok(a) =>
            let ResultT(mResultB) = aToResultTB(a);
            mResultB;
          | Error(_) as e => M.pure(e)
          }
        ),
      );
    };

  let semiflatMap: 'a 'b 'e. ('a => M.t('b), t('a, 'e)) => t('b, 'e) =
    (aToMB, resultTA) => bind(resultTA, a => liftF(aToMB(a)));

  module WithError = (E: TYPE) => {
    let make = make;
    let runResultT = runResultT;
    let withResultT = withResultT;
    let mapResultT = mapResultT;
    let fromResult = fromResult;
    let liftF = liftF;
    let subflatMap = subflatMap;
    let semiflatMap = semiflatMap;
    let cond = cond;
    let condError = condError;

    module Functor: FUNCTOR with type t('a) = t('a, E.t) = {
      type nonrec t('a) = t('a, E.t);
      let map = map;
    };
    let map = Functor.map;
    include Relude_Extensions_Functor.FunctorExtensions(Functor);

    module Bifunctor: BIFUNCTOR with type t('a, 'e) = t('a, 'e) = {
      type nonrec t('a, 'e) = t('a, 'e);
      let bimap = bimap;
    };
    let bimap = Bifunctor.bimap;
    include Relude_Extensions_Bifunctor.BifunctorExtensions(Bifunctor);

    module Apply: APPLY with type t('a) = t('a, E.t) = {
      include Functor;
      let apply = apply;
    };
    let apply = Apply.apply;
    include Relude_Extensions_Apply.ApplyExtensions(Apply);

    module Applicative: APPLICATIVE with type t('a) = t('a, E.t) = {
      include Apply;
      let pure = pure;
    };
    let pure = Applicative.pure;
    include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

    module Monad: MONAD with type t('a) = t('a, E.t) = {
      include Applicative;
      let flat_map = bind;
    };
    let bind = Monad.flat_map;
    include Relude_Extensions_Monad.MonadExtensions(Monad);

    module Infix = {
      include Relude_Extensions_Functor.FunctorInfix(Functor);
      include Relude_Extensions_Bifunctor.BifunctorInfix(Bifunctor);
      include Relude_Extensions_Apply.ApplyInfix(Apply);
      include Relude_Extensions_Monad.MonadInfix(Monad);
    };
  };
};

module WithMonadAndError = (M: MONAD, E: TYPE) => {
  module WithMonad = WithMonad(M);
  include WithMonad.WithError(E);
};
