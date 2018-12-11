/**
 * This is a collection of functions (and more modules that produce functions)
 * that you get for free if your outer type is a monad. This includes all free
 * functions from functor/apply/applicative as well.
 */
module Functions = (M: BsAbstract.Interface.MONAD) => {
  module FunctorFunctions = BsAbstract.Functions.Functor(M);
  module ApplyFunctions = BsAbstract.Functions.Apply(M);

  include FunctorFunctions;
  include ApplyFunctions;

  let flatMap: 'a 'b. ('a => M.t('b), M.t('a)) => M.t('b) = (f, ma) => M.flat_map(ma, f);
  let flatten: 'a. M.t(M.t('a)) => M.t('a) = mma => M.flat_map(mma, v => v);
};
