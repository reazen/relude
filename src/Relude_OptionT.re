open BsBastet.Interface;

/**
Creates an OptionT monad with the given outer Monad
*/
module WithMonad = (M: MONAD) => {
  type t('a) =
    | OptionT(M.t(option('a)));

  let make: 'a. M.t(option('a)) => t('a) = mOptionA => OptionT(mOptionA);

  let runOptionT: 'a. t('a) => M.t(option('a)) =
    (OptionT(mOptionA)) => mOptionA;

  let mapOptionT:
    'a 'b.
    (M.t(option('a)) => M.t(option('b)), t('a)) => t('b)
   =
    (mOptionAToMOptionB, OptionT(mOptionA)) =>
      OptionT(mOptionAToMOptionB(mOptionA));

  let fromOption: 'a. option('a) => t('a) =
    optionA => OptionT(M.pure(optionA));

  let liftF: 'a. M.t('a) => t('a) =
    mA => OptionT(M.map(a => Some(a), mA));

  let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (aToB, OptionT(mOptionA)) =>
      OptionT(M.map(optionA => Relude_Option.map(aToB, optionA), mOptionA));

  let subflatMap: 'a 'b. ('a => option('b), t('a)) => t('b) =
    (aToB, OptionT(mOptionA)) =>
      OptionT(
        M.map(optionA => Relude_Option.flatMap(aToB, optionA), mOptionA),
      );

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
    (OptionT(mOptionAToB), OptionT(mOptionA)) => {
      OptionT(
        M.apply(
          M.map(
            (optionAToB, optionA) =>
              Relude_Option.apply(optionAToB, optionA),
            mOptionAToB,
          ),
          mOptionA,
        ),
      );
    };

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  let pure: 'a. 'a => t('a) = a => OptionT(M.pure(Some(a)));

  module Applicative: APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
    (OptionT(mOptionA), aToOptionTB) => {
      OptionT(
        M.flat_map(mOptionA, optionA =>
          switch (optionA) {
          | Some(a) =>
            let OptionT(mOptionB) = aToOptionTB(a);
            mOptionB;
          | None => M.pure(None)
          }
        ),
      );
    };

  let semiflatMap: 'a 'b. ('a => M.t('b), t('a)) => t('b) =
    (aToMB, optionTA) => bind(optionTA, a => liftF(aToMB(a)));

  module Monad: MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = bind;
  };
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
  };
};
