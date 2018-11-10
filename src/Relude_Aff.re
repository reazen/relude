module Eff = Relude_Eff;
module Option = Relude_Option;
module Result = Relude_Result;

/*
 Aff is an pure, lazy, asynchronous effect monad that should be able to
 handle any type of effect a program could possibly need, including effects
 like Option, Result, Js.Promise, Future, or any combination of these. It
 basically boils down to a continuation callback, where the `onDone` callback
 is invoked with a `Result.t('a, 'e)` to either provide a value or an error.
 The error type is kept open as a type parameter, so that the user of Aff can
 decide exactly how to encode errors in his/her program. For many JS FFI
 functions, the error type will be Js.Exn.t, but this can be wrapped or converted
 into another error type using `Aff.mapError`.

 This is inspired by bs-effects `Affect` and John De Goes' basic Async IO
 monad described here: http://degoes.net/articles/only-one-io

 Aff should be similar in spirit to the `IO` type Haskell, or the new
 row-less Effect type of purescript.

 See the unit tests (Aff_test.re) for an example of how this works with
 asynchronous functions that can fail.

 TODO:

 I'm not sure whether we need/want to return Eff.t(unit) in these methods,
 but that's what John De Goes' basic Async IO type is doing. I think the idea
 is that the callback is itself a synchronous effect, so it should be treated
 as an effect rather than just a function that returns `unit`.

 However, making the callback return Eff and the onDone function return Eff
 means we have to add extra layers of `() => ...` and `onDone(...)()` in
 various places. I'm not sure if that's useful or not.
 */

type t('a, 'e) = (Belt.Result.t('a, 'e) => Eff.t(unit)) => Eff.t(unit);

let run: t(unit, 'e) => unit = onDone => onDone(_ => Eff.pure(), ());

let pure: 'a => t('a, 'e) = (a, onDone) => onDone(Ok(a));

let ok: 'a => t('a, 'e) = pure;

let error: 'e => t('a, 'e) = (e, onDone) => onDone(Error(e));

let fromOption: ('e, option('a)) => t('a, 'e) =
  (e, opt) => opt |> Option.fold(error(e), ok);

let fromResult: Belt.Result.t('a, 'e) => t('a, 'e) =
  r => r |> Result.fold(ok, error);

let fromEff: Eff.t('a) => t('a, 'e) =
  (eff, onDone) => onDone(Ok(eff |> Eff.run));

let fromEffAttemptJS: Eff.t(Belt.Result.t('a, Js.Exn.t)) => t('a, Js.Exn.t) =
  (eff, onDone) => onDone(eff |> Eff.run);

let map: ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, onDoneA, onDoneB) =>
    onDoneA(resultA =>
      switch (resultA) {
      | Ok(a) => onDoneB(Ok(f(a)))
      | Error(_) as err => onDoneB(err)
      }
    );

let mapError: ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, onDoneA, onDoneB) =>
    onDoneA(resultA =>
      switch (resultA) {
      | Ok(_) as okA => onDoneB(okA)
      | Error(e) => onDoneB(Error(f(e)))
      }
    );

let voidError: t('a, 'e1) => t('a, unit) = a => mapError(_ => (), a);

let apply: (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (onDoneF, onDoneA, onDoneB) =>
    onDoneF(resultF =>
      switch (resultF) {
      | Ok(f) =>
        onDoneA(resultA =>
          switch (resultA) {
          | Ok(a) => onDoneB(Ok(f(a)))
          | Error(_) as err => onDoneB(err)
          }
        )
      | Error(_) as err => onDoneB(err)
      }
    );

let flatMap: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (onDoneA, aToAffB, onDoneB) =>
    onDoneA(resultA =>
      switch (resultA) {
      | Ok(a) => aToAffB(a, onDoneB)
      | Error(_) as err => onDoneB(err)
      }
    );

module type FUNCTOR_F =
  (Error: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.FUNCTOR with type t('a) = t('a, Error.t);

module Functor: FUNCTOR_F =
  (Error: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t('a, Error.t);
    let map = map;
  };

module type APPLY_F =
  (Error: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLY with type t('a) = t('a, Error.t);

module Apply: APPLY_F =
  (Error: BsAbstract.Interface.TYPE) => {
    include Functor(Error);
    let apply = apply;
  };

module type APPLICATIVE_F =
  (Error: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, Error.t);

module Applicative: APPLICATIVE_F =
  (Error: BsAbstract.Interface.TYPE) => {
    include Apply(Error);
    let pure = pure;
  };

module type MONAD_F =
  (Error: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.MONAD with type t('a) = t('a, Error.t);

module Monad: MONAD_F =
  (Error: BsAbstract.Interface.TYPE) => {
    include Applicative(Error);
    let flat_map = flatMap;
  };

module Infix = (Error: BsAbstract.Interface.TYPE) => {
  module Monad = Monad(Error);
  include BsAbstract.Infix.Monad(Monad);
};

module InfixJsExn {
  module JsExnType: BsAbstract.Interface.TYPE { type t = Js.Exn.t };
  include Infix(JsExnType);
}
