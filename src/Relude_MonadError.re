module type MONAD_THROW = (E: BsAbstract.Interface.TYPE) => {
  include BsAbstract.Interface.MONAD;
  let throwError: E.t => t('a)
};

module type MONAD_ERROR = (E: BsAbstract.Interface.TYPE) => {
  /* TODO: not sure how to make this require/include MONAD_THROW with the module functor syntax */
  /*
  include MONAD_THROW(E); /* ??? */
  */
  include BsAbstract.Interface.MONAD;
  let throwError: E.t => t('a)
  let catchError: (E.t => t('a), t('a)) => t('a)
}
