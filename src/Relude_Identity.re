type t('a) = 'a; // Or other implementation: type t('a) = | Identity('a);
type t_('a) = t('a); // alias to fix .rei generation

let pure: 'a. 'a => t('a) = a => a;

let unwrap: 'a. t('a) => 'a = a => a;

let map: 'a 'b. ('a => 'b, t('a)) => t('b) = (f, fa) => f(fa);

let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) = (ff, fa) => ff(fa);

let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) = (fa, f) => f(fa);

// flatMap comes in via extenions now
//let flatMap: ('a => t('b), t('a)) => t('b) = (f, fa) => bind(fa, f);

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

/* TODO: semigroup/monoid/plus/alt/etc. */

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type t('a) = t_('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

module type EQ_F =
  (EQ: BsAbstract.Interface.EQ) =>
   BsAbstract.Interface.EQ with type t = t(EQ.t);

module Eq: EQ_F =
  (EQ: BsAbstract.Interface.EQ) => {
    type nonrec t = t(EQ.t);
    let eq: (t, t) => bool = (fa, fb) => eqBy(EQ.eq, fa, fb);
  };

module type SHOW_F =
  (S: BsAbstract.Interface.SHOW) =>
   BsAbstract.Interface.SHOW with type t = t(S.t);

module Show: SHOW_F =
  (S: BsAbstract.Interface.SHOW) => {
    type nonrec t = t(S.t);
    let show: t => string = fa => showBy(S.show, fa);
  };