/**
Eff is a pure, lazy, synchronous effect monad that allows for monadic chaining of purely synchronous
actions that are not expected to ever fail (or failure are not recoverable).

console.log is a prime example of such a function that can be wrapped in Eff - it is purely synchronous and is highly likely
to never fail with an exception.

If your action can fail, you should use Aff instead.
*/

type t('a) = unit => 'a;

/**
Unsafe run effect.  For Eff, this is the same as just calling the effect as a function.
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
