type t('a) = 'a; // Or other implementation: type t('a) = | Identity('a);

let wrap: 'a. 'a => t('a) = a => a;

let unwrap: 'a. t('a) => 'a = a => a;

let map: 'a 'b. ('a => 'b, t('a)) => t('b) = (f, fa) => f(fa);

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) = (ff, fa) => ff(fa);

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

let pure: 'a. 'a => t('a) = wrap;

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) = (fa, f) => f(fa);

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

let eq =
    (
      type a,
      eq: (module BsAbstract.Interface.EQ with type t = a),
      fa: t(a),
      fb: t(a),
    )
    : bool => {
  module AEq = (val eq);
  AEq.eq(unwrap(fa), unwrap(fb));
};

let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool =
  (f, fa, fb) => f(unwrap(fa), unwrap(fb));

module type EQ_F =
  (EQ: BsAbstract.Interface.EQ) =>
   BsAbstract.Interface.EQ with type t = t(EQ.t);

module Eq: EQ_F =
  (EQ: BsAbstract.Interface.EQ) => {
    type nonrec t = t(EQ.t);
    let eq: (t, t) => bool = (fa, fb) => eqBy(EQ.eq, fa, fb);
  };

let show =
    (
      type a,
      show: (module BsAbstract.Interface.SHOW with type t = a),
      fa: t(a),
    )
    : string => {
  module AShow = (val show);
  AShow.show(unwrap(fa));
};

let showBy: 'a. ('a => string, t('a)) => string = (f, fa) => f(unwrap(fa));

module type SHOW_F =
  (S: BsAbstract.Interface.SHOW) =>
   BsAbstract.Interface.SHOW with type t = t(S.t);

module Show: SHOW_F =
  (S: BsAbstract.Interface.SHOW) => {
    type nonrec t = t(S.t);
    let show: t => string = fa => showBy(S.show, fa);
  };

// TODO: semigroup/monoid/plus/alt/etc.