/**
Eff is a pure, lazy, synchronous effect monad that allows for chaining of
synchronous effectful functions that are not expected to fail.

It's basically just a thunk or a lazy function that produces a value.
Laziness is the key to achieving referential transparency and delaying side
effects until the monadic chain is run by calling the effect with `myEff()`
or equivalently `myEff |> Eff.run`

This is inspired by bs-effects `Effect` and John De Goes' basic synchronous
IO monad described here: http://degoes.net/articles/only-one-io

Eff should be similar in spirit to the Eff type of purescript (minus the
effect row) for encoding synchronous effects with no accomodation for errors.

console.log is a good example of a function that can be wrapped in Eff - it
is purely synchronous and is not likely to ever fail with an exception.
Certain DOM functions might also work well with Eff - the key is that Eff
cannot deal with errors nor exceptions.

If your action can fail, you should use Aff instead.
*/

type t('a) = unit => 'a;

/**
Unsafely runs the effect or chain of effects. For Eff, this is the same as
just calling the effect as a function. If any of the thunks throw an
exception, the exception will not be caught here.
 */
let run: t('a) => 'a = eff => eff();

let pure: 'a => t('a) = (a, ()) => a;

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
