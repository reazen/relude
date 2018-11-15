/* TODO: not sure if we should use this type with the constructor, or if `type t('a) = 'a` is sufficient. */
/*
 type t('a) =
   | Identity('a);

 let pure: 'a => t('a) = a => Identity(a);

 let wrap: 'a => t('a) = pure;

 let unwrap: t('a) => 'a =
   fun
   | Identity(v) => v;

 let map: ('a => 'b, t('a)) => t('b) =
   (f, fa) =>
     switch (fa) {
     | Identity(a) => Identity(f(a))
     };

 let apply: (t('a => 'b), t('a)) => t('b) =
   (ff, fa) =>
     switch (ff, fa) {
     | (Identity(f), Identity(a)) => Identity(f(a))
     };

 let flatMap: (t('a), 'a => t('b)) => t('b) =
   (fa, f) =>
     switch (fa) {
     | Identity(a) => f(a)
     };
 */

type t('a) = 'a;

let pure: 'a => t('a) = a => a;

let unwrap: t('a) => 'a = a => a;

let map: ('a => 'b, t('a)) => t('b) = (f, fa) => f(fa);

let apply: (t('a => 'b), t('a)) => t('b) = (ff, fa) => ff(fa);

let flatMap: (t('a), 'a => t('b)) => t('b) = (fa, f) => f(fa);

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

let eqF: (('a, 'a) => bool, t('a), t('a)) => bool =
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

let showF: ('a => string, t('a)) => string = (f, fa) => f(unwrap(fa));

/* TODO: semigroup/monoid/plus/alt/etc. */

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = flatMap;
};

module type EQ_F =
  (E: BsAbstract.Interface.EQ) =>
   BsAbstract.Interface.EQ with type t = t(E.t);

module Eq: EQ_F =
  (E: BsAbstract.Interface.EQ) => {
    type nonrec t = t(E.t);
    let eq: (t, t) => bool = (fa, fb) => eqF(E.eq, fa, fb);
  };

module type SHOW_F =
  (E: BsAbstract.Interface.SHOW) =>
   BsAbstract.Interface.SHOW with type t = t(E.t);

module Show: SHOW_F =
  (E: BsAbstract.Interface.SHOW) => {
    type nonrec t = t(E.t);
    let show: t => string = fa => showF(E.show, fa);
  };
