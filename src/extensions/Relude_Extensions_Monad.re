module MonadExtensions = (M: BsAbstract.Interface.MONAD) => {
  module BsMonadExtensions = BsAbstract.Functions.Monad(M);

  let flatMap: 'a 'b. ('a => M.t('b), M.t('a)) => M.t('b) =
    (f, ma) => M.flat_map(ma, f);

  let flatten: 'a. M.t(M.t('a)) => M.t('a) = mma => M.flat_map(mma, v => v);

  let composeKleisli:
    'a 'b 'c.
    ('a => M.t('b), 'b => M.t('c), 'a) => M.t('c)
   = BsMonadExtensions.compose_kliesli;

  let flipComposeKleisli:
    'a 'b 'c.
    ('b => M.t('c), 'a => M.t('b), 'a) => M.t('c)
   = BsMonadExtensions.compose_kliesli_flipped;

  let liftM1: 'a 'b. ('a => 'b, M.t('a)) => M.t('b) = BsMonadExtensions.liftM1;

  let when_: (M.t(bool), M.t(unit)) => M.t(unit) = BsMonadExtensions.when_;

  let unless: (M.t(bool), M.t(unit)) => M.t(unit) = BsMonadExtensions.unless;
};

module MonadInfix = (M: BsAbstract.Interface.MONAD) => {
  module MonadExtensions = MonadExtensions(M);

  let (>>=) = M.flat_map;

  let (=<<) = MonadExtensions.flatMap;

  let (>=>) = MonadExtensions.composeKleisli;

  let (<=<) = MonadExtensions.flipComposeKleisli;
};