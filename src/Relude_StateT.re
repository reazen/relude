let (<<) = Relude_Function.Infix.(<<);

module WithMonad = (M: BsAbstract.Interface.MONAD) => {
  /**
  StateT represents an effectful function from a state value of type 's to a result value 'a and a next state of type 's.
  This version of StateT has 'a on the left for consistency with other Reason types like Result/IO/Validation/etc.
  */
  type t('a, 's) =
    | StateT('s => M.t(('a, 's)));

  /**
  Applies an initial state value to run the StateT, producing the resulting value a and new state s in the result Monad.
  */
  let runStateT: 's 'a. ('s, t('a, 's)) => M.t(('a, 's)) =
    (s, StateT(sToMAS)) => sToMAS(s);

  /**
  Same as runStateT, but discards the final state, only returning the result a.
  */
  let evalStateT: 's 'a. (t('a, 's), 's) => M.t('a) =
    (StateT(sToMAS), s) => M.map(fst, sToMAS(s));

  /**
  Same as runStateT, but discards the final result a, only returning the final state s.
  */
  let execStateT: 's 'a. (t('a, 's), 's) => M.t('s) =
    (StateT(sToMAS), s) => M.map(snd, sToMAS(s));

  /**
  Change the result type in the StateT.
  Note: for simplicity, we don't allow changing the monad here.
  */
  let mapStateT:
    's 'a 'b.
    (M.t(('a, 's)) => M.t(('b, 's)), t('a, 's)) => t('b, 's)
   =
    (mASToMBS, StateT(sToMAS)) => StateT(mASToMBS << sToMAS);

  /**
  Modifies the state of a StateT
  */
  let withStateT: 's 'a. ('s => 's, t('a, 's)) => t('a, 's) =
    (sToS, StateT(sToMAS)) => StateT(sToMAS << sToS);

  /**
  Surfaces the current state as the result value a.
  */
  let get: 's 'a. t('s, 's) = StateT(s => M.pure((s, s)));

  /**
  Extracts a value from the current state and surfaces it in the result value a.
  */
  let gets: 's 'a. ('s => 'a) => t('a, 's) =
    sToA => StateT(s => M.pure((sToA(s), s)));

  /**
  Overwrites the current state with a new state s.
  */
  let put: 's. 's => t(unit, 's) = s => StateT(_ => M.pure(((), s)));

  /**
  Modifies the current state with a function, and also returns it in the result value a.
  */
  let modify: 's. ('s => 's) => t('s, 's) =
    sToS =>
      StateT(
        s => {
          let s' = sToS(s);
          M.pure((s', s'));
        },
      );

  /**
  Same as modify, but doesn't return the new state as the result value a.
  */
  let modify_: 's. ('s => 's) => t(unit, 's) =
    sToS => StateT(s => M.pure(((), sToS(s))));

  let map: 's 'a 'b. ('a => 'b, t('a, 's)) => t('b, 's) =
    (aToB, StateT(sToMAS)) =>
      StateT(s => sToMAS(s) |> M.map(((a, s)) => (aToB(a), s)));

  let pure: 's 'a. 'a => t('a, 's) = a => StateT(s => M.pure((a, s)));

  let apply: 's 'a 'b. (t('a => 'b, 's), t('a, 's)) => t('b, 's) =
    (StateT(sToMAToBS), StateT(sToMAS)) => {
      StateT(
        s => {
          let mAToBS = sToMAToBS(s);
          let mAS = sToMAS(s);
          let f: 's 'a 'b. (('a => 'b, 's), ('a, 's)) => ('b, 's) =
            // Note: the states from the two sides are not merged here - we take the state from one side or the other.
            // State is intended to be a sequential data flow, not parallel.
            ((aToB, _s1), (a, s2)) => (aToB(a), s2);
          M.apply(M.map(f, mAToBS), mAS);
        },
      );
    };

  let bind: 's 'a 'b. (t('a, 's), 'a => t('b, 's)) => t('b, 's) =
    (StateT(sToMAS), aToStateTBS) => {
      StateT(
        s => {
          let mAS = sToMAS(s);
          M.flat_map(
            mAS,
            ((a, s')) => {
              let StateT(sToMBS) = aToStateTBS(a);
              sToMBS(s');
            },
          );
        },
      );
    };

  module WithState = (S: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t('a, S.t);

    let runStateT = runStateT;
    let evalStateT = evalStateT;
    let execStateT = execStateT;
    let mapStateT = mapStateT;
    let withStateT = withStateT;
    let get = get;
    let gets = gets;
    let put = put;
    let modify = modify;
    let modify_ = modify_;

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

module State = WithMonad(Relude_Identity.Monad);