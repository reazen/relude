/**
Eff is a synchronous effect monad.

This is inspired by bs-effects `Effect`, purescript Eff and Effect, and John
De Goes' basic synchronous IO monad described here:
http://degoes.net/articles/only-one-io
*/

type t('a) = unit => 'a;

/**
Unsafely runs the effect or chain of effects. For Eff, this is the same as
just calling the effect as a function.
 */
let run: t('a) => 'a = eff => eff();

/*
 Lifts a value into the Eff monad.  Note this isn't lazy, so the given value will be evaluated prior to being wrapped in Eff.
 */
let pure: 'a => t('a) = (a, ()) => a;

let fromThunk: (unit => 'a) => t('a) = Relude_Function.identity;

/**
Attempts to run an Eff.t('a) and catches any exn thrown by the effect and lifts it into Eff.t(Belt.Result.t('a, exn))
*/
let attempt: t('a) => t(Belt.Result.t('a, exn)) =
  (effA, ()) =>
    try (Belt.Result.Ok(effA |> run)) {
    | exn => Belt.Result.Error(exn)
    };

/*
 Attempts to run an Eff.t('a) and catches any JS exception thrown by the effect and lifts into into Eff.t(Belt.Result.t('a, Js.Exn.t))
 */
let attemptJS: t('a) => t(Belt.Result.t('a, Js.Exn.t)) =
  (effA, ()) =>
    try (Belt.Result.Ok(effA |> run)) {
    | Js.Exn.Error(e) => Belt.Result.Error(e)
    };

let map: ('a => 'b, t('a)) => t('b) = (f, effA, ()) => f(effA());

let apply: (t('a => 'b), t('a)) => t('b) =
  (effAToB, effA, ()) => effAToB((), effA());

let flatMap: (t('a), 'a => t('b)) => t('b) =
  (effA, aToEffB, ()) => aToEffB(effA(), ());

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

module Infix = {
  include BsAbstract.Infix.Monad(Monad);
};
