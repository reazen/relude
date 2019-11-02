let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);

module IO = Relude_IO;

module WithError = (ERR: BsAbstract.Interface.TYPE) => {
  module IOE = IO.WithError(ERR);
  module M = IOE.MonadError;

  type t('r, 'a) =
    | RIO('r => M.t('a));

  let make: 'r 'a. ('r => M.t('a)) => t('r, 'a) = rToMA => RIO(rToMA);

  let runReaderT: 'r 'a. ('r, t('r, 'a)) => M.t('a) =
    (r, RIO(rToMA)) => rToMA(r);

  let mapReaderT: 'r 'a 'b. (M.t('a) => M.t('b), t('r, 'a)) => t('r, 'b) =
    (maToMB, RIO(rToMA)) => RIO(maToMB << rToMA);

  let withReaderT: 'r1 'r2 'a. ('r2 => 'r1, t('r1, 'a)) => t('r2, 'a) =
    (r2ToR1, RIO(r1ToMA)) => RIO(r1ToMA << r2ToR1);

  let ask: 'r. t('r, 'r) = RIO(r => M.pure(r));

  let asks: 'r 'a. ('r => 'a) => t('r, 'a) =
    rToA => RIO(r => M.pure(rToA(r)));

  let local: 'r 'a. ('r => 'r, t('r, 'a)) => t('r, 'a) =
    (rToR, RIO(rToMA)) => RIO(r => rToMA(rToR(r)));

  let map: 'r 'a 'b. ('a => 'b, t('r, 'a)) => t('r, 'b) =
    (aToB, RIO(rToMA)) => RIO(r => M.map(aToB, rToMA(r)));

  let apply: 'r 'a 'b. (t('r, 'a => 'b), t('r, 'a)) => t('r, 'b) =
    (RIO(rToMAToB), RIO(rToMA)) =>
      RIO(r => M.apply(rToMAToB(r), rToMA(r)));

  let pure: 'r 'a. 'a => t('r, 'a) = a => RIO(_ => M.pure(a));

  let bind: 'r 'a 'b. (t('r, 'a), 'a => t('r, 'b)) => t('r, 'b) =
    (RIO(rToMA), aToReaderB) =>
      RIO(
        r =>
          M.flat_map(
            rToMA(r),
            a => {
              let RIO(rToMB) = aToReaderB(a);
              rToMB(r);
            },
          ),
      )

  let semiflatMap: 'r 'a 'b. ('a => M.t('b), t('r, 'a)) => t('r, 'b) =
    (aToMA, RIO(rToMA)) =>
      RIO(
        r =>
          M.flat_map(
            rToMA(r),
            a => aToMA(a),
          ),
      );

  /**
   * Locks in the reader environment type, so that we can implement
   * the single-type-parameter typeclasses.
   */
  module WithEnv = (R: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t(R.t, 'a);

    let make = make;
    let runReaderT = runReaderT;
    let mapReaderT = mapReaderT;
    let withReaderT = withReaderT;
    let ask = ask;
    let asks = asks;
    let local = local;
    let semiflatMap = semiflatMap;

    module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
      type nonrec t('a) = t('a);
      let map = map;
    };
    let map = Functor.map;
    include Relude_Extensions_Functor.FunctorExtensions(Functor);

    module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
      include Functor;
      let apply = apply;
    };
    let apply = Apply.apply;
    include Relude_Extensions_Apply.ApplyExtensions(Apply);

    module Applicative:
      BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
      include Apply;
      let pure = pure;
    };
    let pure = Applicative.pure;
    include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

    module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
      include Applicative;
      let flat_map = bind;
    };
    let bind = Monad.flat_map;
    include Relude_Extensions_Monad.MonadExtensions(Monad);

    module Infix = {
      include Relude_Extensions_Functor.FunctorInfix(Functor);
      include Relude_Extensions_Apply.ApplyInfix(Apply);
      include Relude_Extensions_Monad.MonadInfix(Monad);
    };
  };
};

/**
 * Creates a RIO Monad with the given error and environment.
 * e.g. WithErrorAndEnv(ERR, ENV) = Rio(ENV.t => IO.t('a, ERR.t))
 */
module WithErrorAndEnv =
       (ERR: BsAbstract.Interface.TYPE, ENV: BsAbstract.Interface.TYPE) => {
  module WithMonad = WithError(ERR);
  include WithMonad.WithEnv(ENV);
};
