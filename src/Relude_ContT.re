module WithMonad = (M: BsAbstract.Interface.MONAD) => {
  type t('r, 'a) =
    | ContT(('a => M.t('r)) => M.t('r));

  /**
  Runs the computation by providing the continuation callback
  */
  let runContT: 'a 'r. (t('r, 'a), 'a => M.t('r)) => M.t('r) =
    (ContT(f), k) => f(k);

  /**
  Modify the underlying action in a ContT monad
  */
  let mapContT: 'a 'r. (M.t('r) => M.t('r), t('r, 'a)) => t('r, 'a) =
    (f, ContT(m)) => ContT(k => f(m(k)));

  /**
  Modify the continuation in a ContT monad
  */
  let withContT:
    'r 'a 'b.
    (('b => M.t('r), 'a) => M.t('r), t('r, 'a)) => t('r, 'b)
   =
    (f, ContT(m)) => ContT(k => m(f(k)));

  /**
  Functor map
  */
  let map: 'r 'a 'b. ('a => 'b, t('r, 'a)) => t('r, 'b) =
    (aToB, ContT(aToMRToMR)) =>
      ContT(bToMR => aToMRToMR(a => bToMR(aToB(a))));

  /**
  Apply apply
  */
  let apply: 'r 'a 'b. (t('r, 'a => 'b), t('r, 'a)) => t('r, 'b) =
    (ContT(aToBToMRToMR), ContT(aToMRToMR)) =>
      ContT(bToMR => aToBToMRToMR(aToB => aToMRToMR(a => bToMR(aToB(a)))));

  /**
  Applicative pure
  */
  let pure: 'r 'a. 'a => t('r, 'a) = a => ContT(aToMR => aToMR(a));

  /**
  Monad bind
  */
  let bind: 'r 'a 'b. (t('r, 'a), 'a => t('r, 'b)) => t('r, 'b) =
    (ContT(aToMRToMR), aToContTB) =>
      ContT(
        bToMR =>
          aToMRToMR(a =>
            switch (aToContTB(a)) {
            | ContT(bToMRToMR) => bToMRToMR(bToMR)
            }
          ),
      );

  module WithEnv = (R: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t(R.t, 'a);

    let runContT = runContT;
    let mapContT = mapContT;
    let withContT = withContT;
    let map = map;
    let apply = apply;
    let pure = pure;
    let bind = bind;

    module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
      type nonrec t('a) = t('a);
      let map = map;
    };
    include Relude_Extensions.Functor.FunctorExtensions(Functor);

    module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
      include Functor;
      let apply = apply;
    };
    include Relude_Extensions.Apply.ApplyExtensions(Apply);

    module Applicative:
      BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
      include Apply;
      let pure = pure;
    };
    include Relude_Extensions.Applicative.ApplicativeExtensions(Applicative);

    module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
      include Applicative;
      let flat_map = bind;
    };
    include Relude_Extensions.Monad.MonadExtensions(Monad);

    module Infix = {
      include Relude_Extensions.Functor.FunctorInfix(Functor);
      include Relude_Extensions.Apply.ApplyInfix(Apply);
      include Relude_Extensions.Monad.MonadInfix(Monad);
    };
  };
};

module WithMonadAndEnv =
       (M: BsAbstract.Interface.MONAD, E: BsAbstract.Interface.TYPE) => {
  module WithMonad = WithMonad(M);
  include WithMonad.WithEnv(E);
};

module Cont = WithMonad(Relude_Identity.Monad);