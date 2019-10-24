module Function = Relude_Function;
module JsExn = Relude_Js_Exn;
module Option = Relude_Option;
module Result = Relude_Result;
module Void = Relude_Void;

let (<<) = Function.Infix.(<<);
let (>>) = Function.Infix.(>>);

/**
IO is a bi-functor effect type that supports both synchronous and asynchronous effects, with explicit error handling.

This is inspired by the following libraries/articles:

* John De Goes - http://degoes.net/articles/only-one-io and http://degoes.net/articles/bifunctor-io
* ZIO/Scalaz 8 IO (Scala) - https://github.com/scalaz/scalaz-zio
* cats-bio (Scala) - https://github.com/LukaJCB/cats-bio
* purescript-aff discussion (Purescript) - https://github.com/slamdata/purescript-aff/issues/137
 */
type t('a, 'e) =
  | Pure('a): t('a, 'e)
  | Throw('e): t('a, 'e)
  | Suspend(unit => 'a): t('a, 'e)
  | SuspendIO(unit => t('a, 'e)): t('a, 'e)
  | Async((Result.t('a, 'e) => unit) => unit): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | FlatMap('r => t('a, 'e), t('r, 'e)): t('a, 'e);

/**
Wraps a strictly-evaluated value in an `IO`

Prefer `suspend` (or variants) for values that are expensive to construct or may have side-effects.
*/
let pure: 'a 'e. 'a => t('a, 'e) = a => Pure(a);

/**
Wraps a non-failing, strictly-evaluated value in an `IO`

Prefer `suspend` for values that are expensive to construct or may have side-effects.
*/
let pureWithVoid: 'a. 'a => t('a, Void.t) = a => Pure(a);

/**
Wraps a strictly-evaluated unit value `()` in an `IO`
*/
let unit: 'e. t(unit, 'e) = Pure();

/**
Wraps a non-failing, strictly-evaluated unit value `()` in an `IO`
*/
let unitWithVoid: t(unit, Void.t) = Pure();

/**
Wraps a strictly-evaluated error value in an `IO`

Prefer `suspendThrow` (or variants) for values that are expensive to construct or may have side-effects.
*/
let throw: 'a 'e. 'e => t('a, 'e) = e => Throw(e);

/**
Wraps a non-succeeding, strictly-evaluated error value in an `IO`
*/
let throwWithVoid: 'e. 'e => t(Void.t, 'e) = e => Throw(e);

/**
Wraps a lazily-evaluated value in an `IO`
*/
let suspend: 'a 'e. (unit => 'a) => t('a, 'e) = getA => Suspend(getA);

/**
Wraps a non-failing, lazily-evaluated value in an `IO`
*/
let suspendWithVoid: 'a. (unit => 'a) => t('a, Void.t) =
  getA => Suspend(getA);

/**
Wraps a lazily-evaluated error in an `IO`
*/
let suspendThrow: 'a 'e. (unit => 'e) => t('a, 'e) =
  getError => SuspendIO(() => Throw(getError()));

/**
Wraps a lazily-evaluated `IO` value in an `IO`

This can be useful if you are dealing with an effectful value that is
normally eagerly or strictly evaluated, like a
`Result`/`option`/`Js.Promise`/etc. In this case, you would typically convert
the effectful value into an `IO` using one of the other functions in `IO`
like `pure` or `throw`, but doing this strict conversion inside a `suspendIO`
function makes the conversion lazy.
*/
let suspendIO: 'a 'e. (unit => t('a, 'e)) => t('a, 'e) =
  getIO => SuspendIO(getIO);

/**
Creates an async `IO` value that is run by invoking a callback `Result.t('a, 'e) => unit`

This is useful for lifting other types of async effects into `IO`, like `Js.Promise` or
a Node.js-style callback API.
*/
let async: 'a 'e. ((Result.t('a, 'e) => unit) => unit) => t('a, 'e) =
  onDone => Async(onDone);

/**
Converts an `option('a)` to an `IO.t('a, 'e) by providing a callback to use when the `option` is `None`

Because the option is already evaluated, no effort is made to suspend any effects.
*/
let fromOption: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, option) =>
    option |> Option.foldLazy(() => throw(getError()), pure);

/**
Converts an `Result.t('a, 'e)` to an `IO.t('a, 'e)

Because the result is already evaluated, no effort is made to suspend any effects.
*/
let fromResult: 'a 'e. Result.t('a, 'e) => t('a, 'e) =
  res => res |> Result.fold(throw, pure);

/**
Applies a function `a => `b on an `IO.t('a, 'e)` to produce an `IO.t('b, 'e)`
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, io) => Map(f, io);

/**
Applies a side-effect function ``a => unit` on an `IO.t('a, 'e)`, and propagates the `'a` value unchanged.

This is useful for doing things like logging the value inside the `IO`.
*/
let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> map(a => {
         f(a);
         a;
       });

/**
Applies an effectful function `'a => IO.t('b, 'e)` on the `'a` value inside the `IO` to produce an `IO.t('b, 'e)`
*/
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (rToIOA, ioR) => FlatMap(rToIOA, ioR);

/**
Same as `flatMap`, but with the argument order reversed.  Also an alias for the `>>=` "bind" operator.
*/
let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ioA, aToIOB) => flatMap(aToIOB, ioA);

/**
Applicative `apply` function
*/
let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ioF, ioA) => ioF |> flatMap(f => ioA |> map(f));

/**
Unsafely runs the `IO.t('a, 'e)` to produce a final `Result.t('a, 'e)`, which is provided to the caller via
a callback of type `Result.t('a, 'e) => unit`.

This function should be run "at the end of the world" to evaluate the suspended side-effects in
the `IO` and produce either an error of type `'e` or a successful value of type `'a`.  Ideally in simple apps,
the "end of the world" is the end of your main function, but when using `IO` inside existing frameworks that
don't natively support `IO`, the "end of the world" might be the context of a reducer side effect, or a web app
controller function.

The function uses the term "unsafe" because calling this function causes all of the suspended side effects
to actually be executed.  It is not "unsafe" in that it can throw an exception - it is just a convention in FP
libraries to denote these types of functions as unsafe.
*/
let rec unsafeRunAsync: 'a 'e. (Result.t('a, 'e) => unit, t('a, 'e)) => unit =
  (onDone, ioA) =>
    switch (ioA) {
    | Pure(a) => onDone(Result.ok(a))
    | Throw(e) => onDone(Result.error(e))
    | Suspend(getA) => onDone(Result.ok(getA()))
    | SuspendIO(getIOA) => getIOA() |> unsafeRunAsync(onDone)
    | Async(onDoneA) => onDoneA(onDone)
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => onDone(Result.ok(r0ToA(r0)))
      | Throw(e) => onDone(Result.error(e))
      | Suspend(getR0) => onDone(Result.ok(r0ToA(getR0())))
      | SuspendIO(getIOR0) =>
        getIOR0()
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r0) => onDone(Result.ok(r0ToA(r0))),
           )
      | Async(onDoneR0) =>
        onDoneR0(
          fun
          | Error(_) as resultE => onDone(resultE)
          | Ok(r0) => onDone(Result.ok(r0ToA(r0))),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r1) => onDone(Result.ok(r0ToA(r1ToR0(r1)))),
           )
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r1) =>
               r1ToIOR0(r1)
               |> unsafeRunAsync(
                    fun
                    | Error(_) as resultE => onDone(resultE)
                    | Ok(r0) => onDone(Result.ok(r0ToA(r0))),
                  ),
           )
      }
    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone)
      | Throw(e) => onDone(Result.error(e))
      | Suspend(getR0) => r0ToIOA(getR0()) |> unsafeRunAsync(onDone)
      | SuspendIO(getIOR0) =>
        getIOR0()
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone),
           )
      | Async(onDoneR0) =>
        onDoneR0(
          fun
          | Error(_) as resultE => onDone(resultE)
          | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r1) => r1 |> r1ToR0 |> r0ToIOA |> unsafeRunAsync(onDone),
           )
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resultE => onDone(resultE)
             | Ok(r1) =>
               r1ToIOR0(r1)
               |> unsafeRunAsync(
                    fun
                    | Error(_) as resE => onDone(resE)
                    | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone),
                  ),
           )
      }
    };

/**
Same as `map`, but operates on the error channel.
*/
let rec mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (e1ToE2, ioA) =>
    switch (ioA) {
    | Pure(a) => pure(a)
    | Throw(e1) => throw(e1ToE2(e1))
    | Suspend(getA) => suspend(getA)
    | SuspendIO(getIOA) => suspendIO(() => getIOA() |> mapError(e1ToE2))
    | Async(onDoneA) =>
      Async(
        onDone =>
          onDoneA(resultA => resultA |> Result.mapError(e1ToE2) |> onDone),
      )
    | Map(rToA, ioR) => ioR |> mapError(e1ToE2) |> map(rToA)
    | FlatMap(rToIOA, ioR) =>
      ioR |> mapError(e1ToE2) |> flatMap(r => rToIOA(r) |> mapError(e1ToE2))
    };

/**
Same as `tap`, but operates on the error channel.
*/
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> mapError(e => {
         f(e);
         e;
       });

/**
Handles an error of types `'e1` from an `IO.t('a, 'e1)` and converts it into a
new `IO.t('a, 'e1)` value. This is much like `flatMap`/`bind` but works for
the error channel of the `IO`.
*/
let rec catchError:
  'a 'e1 'e2.
  ('e1 => t('a, 'e2), t('a, 'e1)) => t('a, 'e2)
 =
  (eToIOA, ioA) =>
    switch (ioA) {
    | Pure(a) => a |> pure
    | Throw(e) => eToIOA(e)
    | Suspend(getA) => suspend(getA)
    | SuspendIO(getIOA) => suspendIO(() => getIOA() |> catchError(eToIOA))
    | Async(onDoneA) =>
      Async(
        onDone =>
          onDoneA(
            fun
            | Ok(a) => a |> Result.ok |> onDone
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0 |> r0ToA |> pure |> catchError(eToIOA)
      | Throw(e) => eToIOA(e)
      | Suspend(getR0) =>
        suspendIO(() => getR0() |> r0ToA |> pure |> catchError(eToIOA))
      | SuspendIO(getIOR0) =>
        suspendIO(() => getIOR0() |> map(r0ToA) |> catchError(eToIOA))
      | Async(onDoneR0) =>
        async(onDone =>
          onDoneR0(
            fun
            | Ok(r0) => r0 |> r0ToA |> Result.ok |> onDone
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone),
          )
        )

      | Map(r1ToR0, ioR1) =>
        ioR1 |> map(r1 => r1 |> r1ToR0 |> r0ToA) |> catchError(eToIOA)
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> flatMap(r1 => r1 |> r1ToIOR0 |> map(r0ToA))
        |> catchError(eToIOA)
      }
    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0 |> r0ToIOA |> catchError(eToIOA)
      | Throw(e) => eToIOA(e)
      | Suspend(getR0) =>
        suspendIO(() => getR0() |> r0ToIOA |> catchError(eToIOA))
      | SuspendIO(getIOR0) =>
        suspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> catchError(eToIOA))
      | Async(onDoneR0) =>
        async(onDone =>
          onDoneR0(
            fun
            | Ok(r0) =>
              r0ToIOA(r0) |> catchError(eToIOA) |> unsafeRunAsync(onDone)
            | Error(e) => eToIOA(e) |> unsafeRunAsync(onDone),
          )
        )
      | Map(r1ToR0, ioR1) =>
        ioR1 |> flatMap(r1 => r1 |> r1ToR0 |> r0ToIOA) |> catchError(eToIOA)
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> flatMap(r1 => r1 |> r1ToIOR0 |> flatMap(r0ToIOA))
        |> catchError(eToIOA)
      }
    };

/**
 * Uses a function to convert an error value to a success value, which serves to "clear" the error
 * in the IO, thereby making the error type `Void.t`.
 */
let rec handleError: 'a 'e. ('e => 'a, t('a, 'e)) => t('a, Relude_Void.t) =
  (eToA, ioA) =>
    switch (ioA) {
    | Pure(a) => pure(a)
    | Throw(e) => pure(eToA(e))
    | Suspend(getA) => suspend(getA)
    | SuspendIO(getIOA) => suspendIO(() => getIOA() |> handleError(eToA))
    | Async(onDoneA) =>
      Async(
        onDone =>
          onDoneA(
            fun
            | Ok(a) => a |> Result.ok |> onDone
            | Error(e) => e |> eToA |> Result.ok |> onDone,
          ),
      )
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0 |> r0ToA |> pure
      | Throw(e) => e |> eToA |> pure
      | Suspend(getR0) => suspend(() => getR0() |> r0ToA)
      | SuspendIO(getIOR0) =>
        suspendIO(() => getIOR0() |> map(r0ToA) |> handleError(eToA))
      | Async(onDoneR0) =>
        async(onDone =>
          onDoneR0(
            fun
            | Ok(r0) => r0 |> r0ToA |> Result.ok |> onDone
            | Error(e) => e |> eToA |> Result.ok |> onDone,
          )
        )
      | Map(r1ToR0, ioR1) =>
        ioR1 |> map(r1 => r1 |> r1ToR0 |> r0ToA) |> handleError(eToA)
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> flatMap(r1 => r1 |> r1ToIOR0 |> map(r0ToA))
        |> handleError(eToA)
      }
    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> handleError(eToA)
      | Throw(e) => e |> eToA |> pure
      | Suspend(getR0) =>
        SuspendIO(() => getR0() |> r0ToIOA |> handleError(eToA))
      | SuspendIO(getIOR0) =>
        suspendIO(() =>
          getIOR0() |> flatMap(r0 => r0 |> r0ToIOA) |> handleError(eToA)
        )
      | Async(onDoneR0) =>
        Async(
          onDone =>
            onDoneR0(
              fun
              | Ok(r0) =>
                r0 |> r0ToIOA |> handleError(eToA) |> unsafeRunAsync(onDone)
              | Error(e) => e |> eToA |> Result.ok |> onDone,
            ),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1 |> flatMap(r1 => r1 |> r1ToR0 |> r0ToIOA) |> handleError(eToA)
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> flatMap(r1 => r1 |> r1ToIOR0 |> flatMap(r0 => r0 |> r0ToIOA))
        |> handleError(eToA)
      }
    };

/**
Applies functions on both the success and error channels of the `IO`.
*/
let rec bimap:
  'a 'b 'e1 'e2.
  ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2)
 =
  (aToB, e1ToE2, io) =>
    switch (io) {
    | Pure(a) => a |> aToB |> pure
    | Throw(e1) => e1 |> e1ToE2 |> throw
    | Suspend(getA) => suspend(() => getA() |> aToB)
    | SuspendIO(getIOA) => suspendIO(() => getIOA() |> bimap(aToB, e1ToE2))
    | Async(onDoneA) =>
      Async(onDone => onDoneA(Result.bimap(aToB, e1ToE2) >> onDone))
    | Map(rToA, ioR) => Map(rToA >> aToB, ioR |> mapError(e1ToE2))
    | FlatMap(rToIOA, ioR) =>
      FlatMap(
        r => rToIOA(r) |> bimap(aToB, e1ToE2),
        ioR |> mapError(e1ToE2),
      )
    };

/**
Same as `tap`, but works on both the success and error channels simultaneously.
*/
let bitap: 'a 'e. ('a => unit, 'e => unit, t('a, 'e)) => t('a, 'e) =
  (f, g, io) =>
    io
    |> bimap(
         a => {
           f(a);
           a;
         },
         e => {
           g(e);
           e;
         },
       );

/**
Lifts a side-effect function that might throw an exception into a suspended `IO.t('a, exn)` value.

The `exn` type is OCaml's extensible error type.
*/
let tries: 'a. (unit => 'a) => t('a, exn) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | exn => Throw(exn)
        },
    );

/**
Lifts a side-effect function that might throw an JS exception into a suspended `IO.t('a, Js.Exn.t)` value.

If a normal `Js.Exn.t` is throw, it is captured as-is, but if the thrown object is not a `Js.Exn.t` it is
unsafely coerced into a `Js.Exn.t`.
*/
let triesJS: 'a. (unit => 'a) => t('a, Js.Exn.t) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | Js.Exn.Error(jsExn) => Throw(jsExn)
        | exn =>
          let jsExn = JsExn.unsafeFromExn(exn);
          Throw(jsExn);
        },
    );

/**
Flips the values between the success and error channels.
*/
let rec flip: 'a 'e. t('a, 'e) => t('e, 'a) =
  ioAE => {
    switch (ioAE) {
    | Pure(a) => throw(a)
    | Throw(e) => pure(e)
    | Suspend(getA) => suspendIO(() => getA() |> throw)
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> flip)
    | Async(onDoneA) =>
      Async(onDoneE => onDoneA(resultA => onDoneE(resultA |> Result.flip)))
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => Throw(r0ToA(r0))
      | Throw(e) => Pure(e)
      | Suspend(getR0) => SuspendIO(() => Throw(r0ToA(getR0())))
      | SuspendIO(getIOR0) =>
        SuspendIO(() => getIOR0() |> map(r0ToA) |> flip)
      | Async(onDoneR0) =>
        Async(
          onDoneE =>
            onDoneR0(resultR0 =>
              onDoneE(resultR0 |> Result.map(r0ToA) |> Result.flip)
            ),
        )
      | Map(rToR0, ioR) => ioR |> map(rToR0 >> r0ToA) |> flip
      | FlatMap(rToIOR0, ioR) =>
        ioR |> flatMap(r => rToIOR0(r) |> map(r0ToA)) |> flip
      }
    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> flip
      | Throw(e) => Pure(e)
      | Suspend(getR0) => SuspendIO(() => r0ToIOA(getR0()) |> flip)
      | SuspendIO(getIOR0) =>
        SuspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> flip)
      | Async(onDoneR0) =>
        Async(
          onDoneE =>
            onDoneR0(resultR0 =>
              switch (resultR0) {
              | Ok(r0) => r0ToIOA(r0) |> flip |> unsafeRunAsync(onDoneE)
              | Error(e) => onDoneE(Result.ok(e))
              }
            ),
        )
      | Map(rToR0, ioR) => ioR |> flatMap(rToR0 >> r0ToIOA) |> flip
      | FlatMap(rToIOR0, ioR) =>
        ioR |> flatMap(r => rToIOR0(r) |> flatMap(r0ToIOA)) |> flip
      }
    };
  };

/**
Summons an error of type `'e` from the error channel into the success channel as a `Result.t('a, 'e)`.
The error channel becomes `Void.t` because the error has been (re)moved.
*/
let rec summonError: 'a 'e. t('a, 'e) => t(Result.t('a, 'e), Void.t) =
  ioA =>
    switch (ioA) {
    | Pure(a) => Pure(Result.ok(a))
    | Throw(e) => Pure(Result.error(e))
    | Suspend(getA) => Suspend(() => Result.ok(getA()))
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> summonError)
    | Async(onDoneA) =>
      Async(onDone => onDoneA(result => onDone(Result.ok(result))))
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => Pure(Result.ok(r0ToA(r0)))
      | Throw(e) => Pure(Result.error(e))
      | Suspend(getR0) => Suspend(() => Result.ok(r0ToA(getR0())))
      | SuspendIO(getIOR0) =>
        SuspendIO(() => getIOR0() |> map(r0ToA) |> summonError)
      | Async(onDoneR0) =>
        Async(
          onDone =>
            onDoneR0(resR0 => onDone(Result.ok(Result.map(r0ToA, resR0)))),
        )
      | Map(r1ToR0, ioR1) => ioR1 |> map(r1ToR0 >> r0ToA) |> summonError
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1 |> flatMap(r1 => r1ToIOR0(r1) |> map(r0ToA)) |> summonError
      }

    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> summonError
      | Throw(e) => Pure(Result.error(e))
      | Suspend(getR0) => SuspendIO(() => r0ToIOA(getR0()) |> summonError)
      | SuspendIO(getIOR0) =>
        SuspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> summonError)
      | Async(onDoneR0) =>
        Async(
          onDone =>
            onDoneR0(
              fun
              | Error(e) => onDone(Result.ok(Result.error(e)))
              | Ok(r0) =>
                r0ToIOA(r0) |> summonError |> unsafeRunAsync(onDone),
            ),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1 |> flatMap(r1ToR0 >> r0ToIOA) |> summonError
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOA(r0)))
        |> summonError
      }
    };

/**
Unsummons an error from a success channel `Result.t('a, 'e)` back into the error channel of the `IO`.
*/
let rec unsummonError: 'a 'e. t(Result.t('a, 'e), Void.t) => t('a, 'e) =
  fun
  | Pure(resultA) => resultA |> Result.fold(throw, pure)
  | Throw(void) => Void.absurd(void)
  | Suspend(getResultA) =>
    SuspendIO(() => getResultA() |> Result.fold(throw, pure))
  | SuspendIO(getIOResultA) =>
    SuspendIO(() => getIOResultA() |> unsummonError)
  | Async(onDoneResultResultA) =>
    Async(
      onDoneResultA =>
        onDoneResultResultA(resultResultA =>
          switch (resultResultA) {
          | Ok(resultA) => onDoneResultA(resultA)
          | Error(void) => Void.absurd(void)
          }
        ),
    )
  | Map(r0ToResultA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0ToResultA(r0) |> Result.fold(throw, pure)
    | Throw(absurd) => Void.absurd(absurd)
    | Suspend(getR0) =>
      SuspendIO(() => getR0() |> r0ToResultA |> Result.fold(throw, pure))
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> map(r0ToResultA) |> unsummonError)
    | Async(onDoneR0) =>
      Async(
        onDoneResultA =>
          onDoneR0(
            fun
            | Ok(r0) => onDoneResultA(r0ToResultA(r0))
            | Error(void) => Void.absurd(void),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> map(r1ToR0 >> r0ToResultA) |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1ToIOR0) |> map(r0ToResultA) |> unsummonError
    }
  | FlatMap(r0ToIOResultA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0ToIOResultA(r0) |> unsummonError
    | Throw(absurd) => Void.absurd(absurd)
    | Suspend(getR0) =>
      SuspendIO(() => getR0() |> r0ToIOResultA |> unsummonError)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> flatMap(r0ToIOResultA) |> unsummonError)
    | Async(onDoneR0) =>
      Async(
        onDoneResultA =>
          onDoneR0(
            fun
            | Ok(r0) =>
              r0
              |> r0ToIOResultA
              |> unsummonError
              |> unsafeRunAsync(onDoneResultA)
            | Error(void) => Void.absurd(void),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> flatMap(r1ToR0 >> r0ToIOResultA) |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOResultA(r0)))
      |> unsummonError
    };

/**
Creates an async `IO` that waits for the given millisecond timeout before completing with a unit value.
*/
let delay: 'e. int => t(unit, 'e) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Result.ok()), millis) |> ignore
    );

/**
Creates an async non-failing `IO` that waits for the given millisecond timeout before completing with a unit value.
*/
let delayWithVoid: int => t(unit, Void.t) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Result.ok()), millis) |> ignore
    );

/**
Injects a delay in milliseconds after the given IO.  The value or error from the previous IO is propagated after the delay.

Example

```re
IO.pure(4) |> IO.withDelay(2000) |> ...
```
*/
let withDelay: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => delay(millis) |> flatMap(_ => io);

/**
This will "debounce" an IO so that it will only allow the latest call within some interval to go through.
All other calls will be cancelled.

Example

```re
let ioLog = messageToLog => IO.pure() |> IO.map(() => Js.log(messageToLog));
let debouncedIoLog = IO.debounce(ioLog);

"This message will not get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
"This message will also not get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
"This message will get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
```
*/
let debounce:
  'r 'a 'e.
  (~immediate: bool=?, ~intervalMs: int=?, 'r => t('a, 'e), 'r) =>
  t(option('a), 'e)
 =
  (~immediate=false, ~intervalMs=150, io) => {
    let currerntlyDebouncedIO = ref(None);
    let startDebouncedIO = () => {
      let debouncedIO = delay(intervalMs);
      currerntlyDebouncedIO := debouncedIO |> Option.pure;
      debouncedIO
      |> map(() => {
           let shouldRunIO =
             currerntlyDebouncedIO^ |> Option.fold(false, (===)(debouncedIO));
           if (shouldRunIO) {
             currerntlyDebouncedIO := None;
           };
           shouldRunIO;
         });
    };

    a => {
      let immediatelyRanIO =
        switch (immediate, currerntlyDebouncedIO^) {
        | (true, None) =>
          suspendIO(() => a |> io |> map(Option.pure)) |> Option.pure
        | (true, Some(_))
        | (false, None)
        | (false, Some(_)) => None
        };
      let debouncedIO =
        startDebouncedIO()
        |> flatMap(shouldRunIO =>
             shouldRunIO && immediatelyRanIO |> Option.isNone
               ? a |> io |> map(Option.pure) : None |> pure
           );

      immediatelyRanIO |> Option.getOrElse(debouncedIO);
    };
  };

/**
This will "throttle" an IO so that it will only allow subsequent calls to go through after some period of time
has elapsed.

Example

```re
let ioLog = messageToLog => IO.pure() |> IO.map(() => Js.log(messageToLog));
let throttledIoLog = IO.throttled(ioLog);

"This message will get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
"This message will not get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
"This message will also not get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
```
*/
let throttle:
  'r 'a 'e.
  (~intervalMs: int=?, 'r => t('a, 'e), 'r) => t(option('a), 'e)
 =
  (~intervalMs=150, io) => {
    let currentlyThrottled = ref(false);
    let startThrottle = () => {
      currentlyThrottled := true;
      Js.Global.setTimeout(() => currentlyThrottled := false, intervalMs)
      |> ignore;
    };

    a =>
      if (currentlyThrottled^) {
        None |> pure;
      } else {
        startThrottle();
        a |> io |> map(Option.pure);
      };
  };

/**
Because this is a bifunctor, we need to use a module functor to lock in the error type,
so we can implement many of the single-type parameter typeclasses.
*/
module WithError = (E: BsAbstract.Interface.TYPE) => {
  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t) = {
    type nonrec t('a) = t('a, E.t);
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Bifunctor:
    BsAbstract.Interface.BIFUNCTOR with type t('a, 'e) = t('a, 'e) = {
    type nonrec t('a, 'e) = t('a, 'e);
    let bimap = bimap;
  };
  let bimap = Bifunctor.bimap;
  include Relude_Extensions_Bifunctor.BifunctorExtensions(Bifunctor);

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a, E.t) = {
    include Functor;
    let apply = apply;
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, E.t) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a, E.t) = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module MonadThrow:
    Relude_Interface.MONAD_THROW with
      type t('a) = t('a, E.t) and type e = E.t = {
    include Monad;
    type e = E.t;
    let throwError = throw;
  };
  let throwError = MonadThrow.throwError;
  include Relude_Extensions_MonadThrow.MonadThrowExtensions(MonadThrow);

  module MonadError:
    Relude_Interface.MONAD_ERROR with
      type t('a) = t('a, E.t) and type e = E.t = {
    include MonadThrow;
    let catchError = catchError;
  };
  let catchError = MonadError.catchError;
  include Relude_Extensions_MonadError.MonadErrorExtensions(MonadError);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Bifunctor.BifunctorInfix(Bifunctor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
  };
};
