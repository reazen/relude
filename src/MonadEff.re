/**
Typeclass that provides the ability to lift an Eff into another effect monad.
*/
module type MONAD_EFF = {
  type t('a);
  let liftEff: Eff.t('a) => t('a);
};

module MonadEffEff: MONAD_EFF with type t('a) = Eff.t('a) {
  type t('a) = Eff.t('a)
  let liftEff = Function.identity
}
