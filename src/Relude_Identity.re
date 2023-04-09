open Bastet.Interface;

[@ocaml.text
  {|
[Relude.Identity] is a type which is useful for acting as an identity [Functor],
[Monad], etc. in cases where you need to provide such a thing, but you don't
need any extra effects or behavior.

A common use of [Identity] is to create a plain [Reader] monad from a [ReaderT]
transformer by using the [Identity] monad as the [ReaderT] monad type. You can
see this technique in action in {!module:Relude_ReaderT} and
{!module:Relude_OptionT}.
|}
];

/**
The identity type. This is useful for contexts that require a type constructor,
but you don't actually need any extra functionality.
*/
type t('a) = 'a;

/**
Lifts a pure value in the Identity context.
*/
let wrap: 'a. 'a => t('a) = a => a;

/**
Unwraps a value in the Identity context.
*/
let unwrap: 'a. t('a) => 'a = a => a;

/**
Maps a pure function over the value contained in the Identity.
*/
let map: 'a 'b. ('a => 'b, t('a)) => t('b) = (f, fa) => f(fa);

module Functor: FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
Applies a wrapped function to the value contained in the Identity.
*/
let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) = (ff, fa) => ff(fa);

module Apply: APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
Lifts a pure value into the Identity.

Alias for [wrap]
*/
let pure: 'a. 'a => t('a) = wrap;

module Applicative: APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
Applies a monadic function to the value contained in the Identity context.
*/
let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) = (fa, f) => f(fa);

module Monad: MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

/**
Indicates if two Identity values are equal
*/
let eq = (type a, eq: (module EQ with type t = a), fa: t(a), fb: t(a)): bool => {
  module AEq = (val eq);
  AEq.eq(unwrap(fa), unwrap(fb));
};

/**
Indicates if two Identity values are equal using the given equality function
*/
let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool =
  (f, fa, fb) => f(unwrap(fa), unwrap(fb));

module type EQ_F = (EQ: EQ) => EQ with type t = t(EQ.t);

module Eq: EQ_F =
  (EQ: EQ) => {
    type nonrec t = t(EQ.t);
    let eq: (t, t) => bool = (fa, fb) => eqBy(EQ.eq, fa, fb);
  };

/**
Converts the Identity value to a string, using the given SHOW module
*/
let show = (type a, show: (module SHOW with type t = a), fa: t(a)): string => {
  module AShow = (val show);
  AShow.show(unwrap(fa));
};

/**
Converts the Identity value to a string, using the given show function
*/
let showBy: 'a. ('a => string, t('a)) => string = (f, fa) => f(unwrap(fa));

module type SHOW_F = (S: SHOW) => SHOW with type t = t(S.t);

module Show: SHOW_F =
  (S: SHOW) => {
    type nonrec t = t(S.t);
    let show: t => string = fa => showBy(S.show, fa);
  } /* TODO: semigroup/monoid/plus/alt/etc*/;
