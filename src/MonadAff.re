/**
Typeclass that provides the ability to lift an Aff into another effect monad.
*/
module type MONAD_AFF = {
  type t('a, 'e);
  let liftAff: Aff.t('a, 'e) => t('a, 'e);
}

module MonadAffAff: MONAD_AFF with type t('a, 'e) = Aff.t('a, 'e) {
  type t('a, 'e) = Aff.t('a, 'e);
  let liftAff = Function.identity
}
