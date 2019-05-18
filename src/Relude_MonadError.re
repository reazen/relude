module type MONAD_THROW = {
  include BsAbstract.Interface.MONAD;
  type e;
  let throwError: e => t('a);
};

module type MONAD_ERROR = {
  include MONAD_THROW;
  let catchError: (e => t('a), t('a)) => t('a);
};