open Bastet.Interface;

module RWSResult = {
  type t('a, 's, 'w) =
    | RWSResult('a, 's, 'w);
};

/**
Creates a RWST (Reader/Writer/State) Monad with the given Monad module.
*/
module WithMonad = (M: MONAD) => {
  open RWSResult;

  type t('a, 'r, 's, 'w) =
    | RWST(('r, 's) => M.t(RWSResult.t('a, 's, 'w)));

  /**
  Given a reader environment, and state, run the RWST to produce the monadic
  value.
  */
  let runRWST:
    'a 'r 's 'w.
    ('r, 's, t('a, 'r, 's, 'w)) => M.t(RWSResult.t('a, 's, 'w))
   =
    (r, s, RWST(f)) => f(r, s);

  /**
  Same as runRWST, but discards the final state value, only returning the final
  result, and the writer log.
  */
  let evalRWST: 'a 'r 's 'w. ('r, 's, t('a, 'r, 's, 'w)) => M.t(('a, 'w)) =
    (r, s, RWST(f)) => {
      f(r, s) |> M.map((RWSResult(a, _s, w)) => (a, w));
    };

  /**
  Same as runRWST, but discards the final result value, only returning the final
  state, and the writer log.
  */
  let execRWST: 'a 'r 's 'w. ('r, 's, t('a, 'r, 's, 'w)) => M.t(('s, 'w)) =
    (r, s, RWST(f)) => {
      f(r, s) |> M.map((RWSResult(_a, s, w)) => (s, w));
    };

  /**
  Change the result type and writer log type.  Note: this should normally allow
  changing the monad, but we're not doing that for simplicity.
  */
  let mapRWST:
    'a1 'a2 'r 's 'w1 'w2.
    (
      M.t(RWSResult.t('a1, 's, 'w1)) => M.t(RWSResult.t('a2, 's, 'w2)),
      t('a1, 'r, 's, 'w1)
    ) =>
    t('a2, 'r, 's, 'w2)
   =
    (f, RWST(m)) => RWST((r, s) => f(m(r, s)));

  /**
  Changes the reader environment and state values for an RWST using a function
  */
  let withRWST:
    'a 'r1 'r2 's 'w.
    (('r2, 's) => ('r1, 's), t('a, 'r1, 's, 'w)) => t('a, 'r2, 's, 'w)
   =
    (convert, RWST(f)) =>
      RWST(
        (r1, s1) => {
          let (r2, s2) = convert(r1, s1);
          f(r2, s2);
        },
      );

  let map: 'a 'b 'r 's 'w. ('a => 'b, t('a, 'r, 's, 'w)) => t('b, 'r, 's, 'w) =
    (aToB, RWST(f)) =>
      RWST(
        (r, s) =>
          f(r, s)
          |> M.map((RWSResult.RWSResult(a, s, w)) =>
               RWSResult.RWSResult(aToB(a), s, w)
             ),
      );

  let applyWithAppendLog:
    'a 'b 'r 's 'w.
    (('w, 'w) => 'w, t('a => 'b, 'r, 's, 'w), t('a, 'r, 's, 'w)) =>
    t('b, 'r, 's, 'w)
   =
    (appendLog, RWST(runAToB), RWST(runA)) =>
      RWST(
        (r, s) => {
          let mAToB = runAToB(r, s);
          let mA = runA(r, s);
          let f = (RWSResult(aToB, _s1, w1), RWSResult(a, s2, w2)) =>
            RWSResult(aToB(a), s2, appendLog(w1, w2));
          M.apply(M.map(f, mAToB), mA);
        },
      );

  let pureWithEmptyLog: 'a 'r 's 'w. ('w, 'a) => t('a, 'r, 's, 'w) =
    (w, a) => RWST((_r, s) => M.pure(RWSResult(a, s, w)));

  let bindWithAppendLog:
    'a 'b 'r 's 'w.
    (('w, 'w) => 'w, t('a, 'r, 's, 'w), 'a => t('b, 'r, 's, 'w)) =>
    t('b, 'r, 's, 'w)
   =
    (appendLog, RWST(runA), aToRWSTB) =>
      RWST(
        (r1, s1) =>
          runA(r1, s1)
          ->M.flat_map((RWSResult(a, s2, w1)) => {
              let RWST(runB) = aToRWSTB(a);
              runB(r1, s2)
              |> M.map((RWSResult(b, s3, w2)) =>
                   RWSResult(b, s3, appendLog(w1, w2))
                 );
            }),
      );

  module WithEnvAndStateAndLog =
         (R: TYPE, S: TYPE, Log: Relude_WriterT.WriterLog.LOG) => {
    let runRWST = runRWST;
    let evalRWST = evalRWST;
    let execRWST = execRWST;
    let mapRWST = mapRWST;
    let withRWST = withRWST;

    module Functor: FUNCTOR with type t('a) = t('a, R.t, S.t, Log.t) = {
      type nonrec t('a) = t('a, R.t, S.t, Log.t);
      let map = map;
    };
    let map = Functor.map;
    include Relude_Extensions_Functor.FunctorExtensions(Functor);

    module Apply: APPLY with type t('a) = t('a, R.t, S.t, Log.t) = {
      include Functor;
      let apply = (ff, fa) => applyWithAppendLog(Log.Monoid.append, ff, fa);
    };
    let apply = Apply.apply;
    include Relude_Extensions_Apply.ApplyExtensions(Apply);

    module Applicative: APPLICATIVE with type t('a) = t('a, R.t, S.t, Log.t) = {
      include Apply;
      let pure = a => pureWithEmptyLog(Log.Monoid.empty, a);
    };
    let pure = Applicative.pure;
    include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

    module Monad: MONAD with type t('a) = t('a, R.t, S.t, Log.t) = {
      include Applicative;
      let flat_map = (f, ma) => bindWithAppendLog(Log.Monoid.append, f, ma);
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
