let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);

// TODO: not sure whether to just make this functor include the "Env" type R here along with the Monad, or
// have it nested inside.  I have a convenience variant called WithMonadAndEnv, but maybe that should
// just be the only thing.

/**
 * Creates a ReaderT Monad with the given Monad module
 */
module WithMonad = (M: BsAbstract.Interface.MONAD) => {
  type t('r, 'a) =
    | ReaderT('r => M.t('a));

  let make: 'r 'a. ('r => M.t('a)) => t('r, 'a) = rToMA => ReaderT(rToMA);

  let runReaderT: 'r 'a. ('r, t('r, 'a)) => M.t('a) =
    (r, ReaderT(rToMA)) => rToMA(r);

  let mapReaderT: 'r 'a 'b. (M.t('a) => M.t('b), t('r, 'a)) => t('r, 'b) =
    (maToMB, ReaderT(rToMA)) => ReaderT(maToMB << rToMA);

  let withReaderT: 'r1 'r2 'a. ('r2 => 'r1, t('r1, 'a)) => t('r2, 'a) =
    (r2ToR1, ReaderT(r1ToMA)) => ReaderT(r1ToMA << r2ToR1);

  let ask: 'r. t('r, 'r) = ReaderT(r => M.pure(r));

  let asks: 'r 'a. ('r => 'a) => t('r, 'a) =
    rToA => ReaderT(r => M.pure(rToA(r)));

  let local: 'r 'a. ('r => 'r, t('r, 'a)) => t('r, 'a) =
    (rToR, ReaderT(rToMA)) => ReaderT(r => rToMA(rToR(r)));

  let map: 'r 'a 'b. ('a => 'b, t('r, 'a)) => t('r, 'b) =
    (aToB, ReaderT(rToMA)) => ReaderT(r => M.map(aToB, rToMA(r)));

  let apply: 'r 'a 'b. (t('r, 'a => 'b), t('r, 'a)) => t('r, 'b) =
    (ReaderT(rToMAToB), ReaderT(rToMA)) =>
      ReaderT(r => M.apply(rToMAToB(r), rToMA(r)));

  let pure: 'r 'a. 'a => t('r, 'a) = a => ReaderT(_ => M.pure(a));

  let bind: 'r 'a 'b. (t('r, 'a), 'a => t('r, 'b)) => t('r, 'b) =
    (ReaderT(rToMA), aToReaderB) =>
      ReaderT(
        r =>
          M.flat_map(
            rToMA(r),
            a => {
              let ReaderT(rToMB) = aToReaderB(a);
              rToMB(r);
            },
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

module WithMonadAndEnv =
       (M: BsAbstract.Interface.MONAD, E: BsAbstract.Interface.TYPE) => {
  module WithMonad = WithMonad(M);
  include WithMonad.WithEnv(E);
};

module Reader = WithMonad(Relude_Identity.Monad);