/**
[Extensions.Monad] contains a module functor [MonadExtensions] which gives you
access to a wide variety of functions and infix operators that you can get "for
free" when you have a [Monad] typeclass instance.
*/
/**
Extensions for any MONAD
*/
module MonadExtensions = (M: BsBastet.Interface.MONAD) => {
  module BsMonadExtensions = BsBastet.Functions.Monad(M);

  /**
  Flipped version of [bind] which has the function on the left and the monad on
  the right. We're calling this [flatMap] because the signature closely
  resembles the signature of [map] with the function on the left, and [flatMap]
  is another commonly used name for [bind].
  */
  let flatMap: 'a 'b. ('a => M.t('b), M.t('a)) => M.t('b) =
    (f, ma) => M.flat_map(ma, f);

  /**
  Flattens a nested monadic structure one time.
  */
  let flatten: 'a. M.t(M.t('a)) => M.t('a) = mma => M.flat_map(mma, v => v);

  /**
  Creates a new monadic function by composing two monadic functions from
  left-to-right.
  */
  let composeKleisli:
    'a 'b 'c.
    ('a => M.t('b), 'b => M.t('c), 'a) => M.t('c)
   = BsMonadExtensions.compose_kliesli;

  /**
  Creates a new monadic function by composing two monadic functions from
  right-to-left.
  */
  let flipComposeKleisli:
    'a 'b 'c.
    ('b => M.t('c), 'a => M.t('b), 'a) => M.t('c)
   = BsMonadExtensions.compose_kliesli_flipped;

  /**
  Lifts a pure function ['a => 'b] into a monadic context [M.t('a) => M.t('b)]
  */
  let liftM1: 'a 'b. ('a => 'b, M.t('a)) => M.t('b) = BsMonadExtensions.liftM1;

  /**
  Runs a monadic effect if the given monadic condition is evaluated to true. If
  the condition is false, a pure unit value is returned with no effect being
  run.
  */
  let when_: (M.t(bool), M.t(unit)) => M.t(unit) = BsMonadExtensions.when_;

  /**
  Runs a monadic effect if the given monadic condition is evaluated to false. If
  the condition is true, a pure unit value is returned with no effect being run.
  */
  let unless: (M.t(bool), M.t(unit)) => M.t(unit) = BsMonadExtensions.unless;
};

/**
Infix operator extensions for MONAD
*/
module MonadInfix = (M: BsBastet.Interface.MONAD) => {
  module MonadExtensions = MonadExtensions(M);

  /**
  Operator version of applying a monadic function with the monad value on the
  left, and the function on the right.
  */
  let (>>=) = M.flat_map;

  /**
  Operator version of applying a monadic function with the monad value on the
  right, and the function on the left.
  */
  let (=<<) = MonadExtensions.flatMap;

  /**
  Creates a new monadic function by composing two monadic functions from
  left-to-right.
  */
  let (>=>) = MonadExtensions.composeKleisli;

  /**
  Creates a new monadic function by composing two monadic functions from
  right-to-left.
  */
  let (<=<) = MonadExtensions.flipComposeKleisli;
};
