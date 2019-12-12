open Relude_Function.Infix;

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
  | Async((Belt.Result.t('a, 'e) => unit) => unit): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | Apply(t('r => 'a, 'e), t('r, 'e)): t('a, 'e)
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
let pureWithVoid: 'a. 'a => t('a, Relude_Void.t) = a => Pure(a);

/**
Wraps a strictly-evaluated unit value `()` in an `IO`
*/
let unit: 'e. t(unit, 'e) = Pure();

/**
Wraps a non-failing, strictly-evaluated unit value `()` in an `IO`
*/
let unitWithVoid: t(unit, Relude_Void.t) = Pure();

/**
Wraps a strictly-evaluated error value in an `IO`

Prefer `suspendThrow` (or variants) for values that are expensive to construct or may have side-effects.
*/
let throw: 'a 'e. 'e => t('a, 'e) = e => Throw(e);

/**
Wraps a non-succeeding, strictly-evaluated error value in an `IO`
*/
let throwWithVoid: 'e. 'e => t(Relude_Void.t, 'e) = e => Throw(e);

/**
Wraps a lazily-evaluated value in an `IO`
*/
let suspend: 'a 'e. (unit => 'a) => t('a, 'e) = getA => Suspend(getA);

/**
Wraps a non-failing, lazily-evaluated value in an `IO`
*/
let suspendWithVoid: 'a. (unit => 'a) => t('a, Relude_Void.t) =
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
let async: 'a 'e. ((Belt.Result.t('a, 'e) => unit) => unit) => t('a, 'e) =
  onDone => Async(onDone);

/**
Converts an `option('a)` to an `IO.t('a, 'e) by providing a callback to use when the `option` is `None`

Because the option is already evaluated, no effort is made to suspend any effects.
*/
let fromOption: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, option) =>
    option |> Relude_Option_Base.foldLazy(() => throw(getError()), pure);

/**
Converts an `Result.t('a, 'e)` to an `IO.t('a, 'e)

Because the result is already evaluated, no effort is made to suspend any effects.
*/
let fromResult: 'a 'e. Belt.Result.t('a, 'e) => t('a, 'e) =
  res => res |> Relude_Result.fold(throw, pure);

/**
Applies a function `a => `b on an `IO.t('a, 'e)` to produce an `IO.t('b, 'e)`
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, io) => Map(f, io);

let (<$>) = map;

let (<#>) = (ioA, aToB) => map(aToB, ioA);

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
Applicative `apply` function
*/
let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ioAToB, ioA) => Apply(ioAToB, ioA);

let (<*>) = apply;

/**
Applies an effectful function `'a => IO.t('b, 'e)` on the `'a` value inside the `IO` to produce an `IO.t('b, 'e)`
*/
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (rToIOA, ioR) => FlatMap(rToIOA, ioR);

/**
Same as `flatMap`, but with the argument order reversed.  Also an alias for the `>>=` "bind" operator.
*/
let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ioA, aToIOB) => FlatMap(aToIOB, ioA);

let (>>=) = bind;

/**
 Conditional map.
 If the condition is satisfied, return the given `'a` in lifted into a successful IO, otherwise,
 return the given `e` Lifted to the error side.
 */
let cond: 'a 'e. ('a => bool, 'a, 'e, t('a, 'e)) => t('a, 'e) =
  (f, newA, err, ioA) =>
    ioA |> flatMap(a => f(a) ? pure(newA) : throw(err));

/**
 As `cond`, but only maps the 'e side when the condition fails.
 */
let condError: 'a 'e. ('a => bool, 'e, t('a, 'e)) => t('a, 'e) =
  (f, err, ioA) => ioA |> flatMap(a => f(a) ? pure(a) : throw(err));

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
let rec unsafeRunAsync: 'a 'e. (Belt.Result.t('a, 'e) => unit, t('a, 'e)) => unit =
  (onDone, ioA) =>
    switch (ioA) {
    | Pure(a) => onDone(Belt.Result.Ok(a))
    | Throw(e) => onDone(Belt.Result.Error(e))
    | Suspend(getA) => onDone(Belt.Result.Ok(getA()))
    | SuspendIO(getIOA) => getIOA() |> unsafeRunAsync(onDone)
    | Async(onDoneA) => onDoneA(onDone)
    | Map(r0ToA, ioR0) =>
      ioR0
      |> unsafeRunAsync(
           fun
           | Error(_) as resultE => onDone(resultE)
           | Ok(r0) => onDone(Ok(r0ToA(r0))),
         )
    | Apply(ioR0ToA, ioR0) =>
      unsafeRunAsyncPar2(
        (resultR0ToA, resultR0) =>
          onDone(Relude_Result.apply(resultR0ToA, resultR0)),
        ioR0ToA,
        ioR0,
      )
    | FlatMap(r0ToIOA, ioR0) =>
      ioR0
      |> unsafeRunAsync(
           fun
           | Error(_) as resultE => onDone(resultE)
           | Ok(r0) =>
             r0
             |> r0ToIOA
             |> unsafeRunAsync(
                  fun
                  | Error(_) as resultE => onDone(resultE)
                  | Ok(a) => onDone(Ok(a)),
                ),
         )
    }

/**
 * Runs two IOs in parallel, and invokes the given done callback when all complete
 *
 * Note that applicative uses of IO (apply/map2/map3/traverse/etc.) will run
 * the IOs in parallel, so it's rarely necessary for the end-user to call this
 * directly.
 */
and unsafeRunAsyncPar2:
  'a 'b 'e.
  ((Belt.Result.t('a, 'e), Belt.Result.t('b, 'e)) => unit, t('a, 'e), t('b, 'e)) =>
  unit
 =
  (onDone, ioA, ioB) => {
    let refA = ref(None);
    let refB = ref(None);
    ioA
    |> unsafeRunAsync(resultA =>
         switch (refB^) {
         | Some(resultB) => onDone(resultA, resultB)
         | None => refA := Some(resultA)
         }
       );
    ioB
    |> unsafeRunAsync(resultB =>
         switch (refA^) {
         | Some(resultA) => onDone(resultA, resultB)
         | None => refB := Some(resultB)
         }
       );
  }

/**
 * Runs three IOs in parallel, and invokes the given done callback when all complete
 *
 * Note that applicative uses of IO (apply/map2/map3/traverse/etc.) will run
 * the IOs in parallel, so it's rarely necessary for the end-user to call this
 * directly.
 */
and unsafeRunAsyncPar3:
  'a 'b 'c 'e.
  (
    (Belt.Result.t('a, 'e), Belt.Result.t('b, 'e), Belt.Result.t('c, 'e)) => unit,
    t('a, 'e),
    t('b, 'e),
    t('c, 'e)
  ) =>
  unit
 =
  (onDone, ioA, ioB, ioC) => {
    let refA = ref(None);
    let refB = ref(None);
    let refC = ref(None);
    ioA
    |> unsafeRunAsync(resultA =>
         switch (refB^, refC^) {
         | (Some(resultB), Some(resultC)) =>
           onDone(resultA, resultB, resultC)
         | _ => refA := Some(resultA)
         }
       );
    ioB
    |> unsafeRunAsync(resultB =>
         switch (refA^, refC^) {
         | (Some(resultA), Some(resultC)) =>
           onDone(resultA, resultB, resultC)
         | _ => refB := Some(resultB)
         }
       );
    ioC
    |> unsafeRunAsync(resultC =>
         switch (refA^, refB^) {
         | (Some(resultA), Some(resultB)) =>
           onDone(resultA, resultB, resultC)
         | _ => refC := Some(resultC)
         }
       );
  };

/**
 * Creates a new IO value that contains the composition of functions from two
 * input IO values. Composition is done from right-to-left with this function - see
 * andThen for left-to-right.
 */
let rec compose:
  'a 'b 'c 'e.
  (t('b => 'c, 'e), t('a => 'b, 'e)) => t('a => 'c, 'e)
 =
  (ioBToC, ioAToB) =>
    switch (ioAToB) {
    | Pure(aToB) => composePure(aToB, ioBToC)
    | Throw(e) => composeThrow(e, ioBToC)
    | Suspend(getAToB) => composeSuspend(getAToB, ioBToC)
    | SuspendIO(getIOAToB) => composeSuspendIO(getIOAToB, ioBToC)
    | Async(onDoneAToB) => composeAsync(onDoneAToB, ioBToC)
    | Map(r0ToAToB, ioR0) => composeMap(r0ToAToB, ioR0, ioBToC)
    | Apply(ioR0ToAToB, ioR0) => composeApply(ioR0ToAToB, ioR0, ioBToC)
    | FlatMap(r0ToIOAToB, ioR0) => composeFlatMap(r0ToIOAToB, ioR0, ioBToC)
    }

/**
 * compose specialization for a left-hand-side Pure('a => 'b)
 */
and composePure: 'a 'b 'c 'e. ('a => 'b, t('b => 'c, 'e)) => t('a => 'c, 'e) =
  (aToB, ioBToC) =>
    switch (ioBToC) {
    | Pure(bToC) => Pure(aToB >> bToC)
    | Throw(_) as t => t
    | Suspend(getBToC) => Suspend(() => aToB >> getBToC())
    | SuspendIO(getIOBToC) =>
      SuspendIO(() => getIOBToC() |> map(bToC => aToB >> bToC))
    | Async(onDoneBToC) =>
      Async(
        onDone =>
          onDoneBToC(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(bToC) => onDone(Ok(aToB >> bToC)),
          ),
      )
    | Map(r0ToBToC, ioR0) => ioR0 |> map(r0 => aToB >> r0ToBToC(r0))
    | Apply(ioR0ToBToC, ioR0) =>
      ioR0
      |> apply(ioR0ToBToC |> map((r0ToBToC, r0) => aToB >> r0ToBToC(r0)))
    | FlatMap(r0ToIOBToC, ioR0) =>
      ioR0 |> flatMap(r0 => r0ToIOBToC(r0) |> map(bToC => aToB >> bToC))
    }

/**
 * compose specialization for a left-hand-side Throw
 */
and composeThrow: 'a 'b 'c 'e. ('e, t('b => 'c, 'e)) => t('a => 'c, 'e) =
  (e, ioBToC) =>
    switch (ioBToC) {
    | Pure(_) => Throw(e)
    | Throw(_) => Throw(e)
    | Suspend(_) => Throw(e)
    | SuspendIO(_) => Throw(e)
    | Async(_) => Throw(e)
    | Map(_, _) => Throw(e)
    | Apply(_, _) => Throw(e)
    | FlatMap(_, _) => Throw(e)
    }

/**
 * compose specialization for a left-hand-side Suspend
 */
and composeSuspend:
  'a 'b 'c 'e.
  ((unit, 'a) => 'b, t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (getAToB, ioBToC) =>
    switch (ioBToC) {
    | Pure(bToC) => Suspend(() => getAToB() >> bToC)
    | Throw(_) as t => t
    | Suspend(getBToC) => Suspend(() => getAToB() >> getBToC())
    | SuspendIO(getIOBToC) =>
      SuspendIO(() => getIOBToC() |> map(bToC => getAToB() >> bToC))
    | Async(onDoneBToC) =>
      Async(
        onDone =>
          onDoneBToC(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(bToC) => onDone(Ok(getAToB() >> bToC)),
          ),
      )
    | Map(r0ToBToC, ioR0) => ioR0 |> map(r0 => getAToB() >> r0ToBToC(r0))
    | Apply(ioR0ToBToC, ioR0) =>
      ioR0
      |> apply(
           ioR0ToBToC |> map((r0ToBToC, r0) => getAToB() >> r0ToBToC(r0)),
         )
    | FlatMap(r0ToIOBToC, ioR0) =>
      ioR0 |> flatMap(r0 => r0ToIOBToC(r0) |> map(bToC => getAToB() >> bToC))
    }

/**
 * compose specialization for a left-hand-side SuspendIO
 */
and composeSuspendIO:
  'a 'b 'c 'e.
  (unit => t('a => 'b, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (getIOAToB, ioBToC) =>
    switch (ioBToC) {
    | Pure(bToC) => SuspendIO(() => getIOAToB() |> map(aToB => aToB >> bToC))
    | Throw(_) as t => t
    | Suspend(getBToC) =>
      SuspendIO(() => getIOAToB() |> map(aToB => aToB >> getBToC()))
    | SuspendIO(getIOBToC) =>
      SuspendIO(
        () =>
          getIOAToB()
          |> flatMap(aToB => getIOBToC() |> map(bToC => aToB >> bToC)),
      )
    | Async(onDoneBToC) =>
      Async(
        onDone =>
          getIOAToB()
          |> unsafeRunAsync(
               fun
               | Error(_) as resultE => onDone(resultE)
               | Ok(aToB) =>
                 onDoneBToC(
                   fun
                   | Error(_) as resultE => onDone(resultE)
                   | Ok(bToC) => onDone(Ok(aToB >> bToC)),
                 ),
             ),
      )
    | Map(r0ToBToC, ioR0) =>
      SuspendIO(
        () =>
          getIOAToB()
          |> flatMap(aToB => ioR0 |> map(r0 => aToB >> r0ToBToC(r0))),
      )
    | Apply(ioR0ToBToC, ioR0) =>
      SuspendIO(
        () =>
          getIOAToB()
          |> flatMap(aToB =>
               ioR0ToBToC
               |> flatMap(r0ToBToC => ioR0 |> map(r0 => aToB >> r0ToBToC(r0)))
             ),
      )
    | FlatMap(r0ToIOBToC, ioR0) =>
      SuspendIO(
        () =>
          getIOAToB()
          |> flatMap(aToB =>
               ioR0
               |> flatMap(r0 => r0ToIOBToC(r0) |> map(bToC => aToB >> bToC))
             ),
      )
    }

/**
 * compose specialization for a left-hand-side Async
 */
and composeAsync:
  'a 'b 'c 'e.
  ((Belt.Result.t('a => 'b, 'e) => unit) => unit, t('b => 'c, 'e)) =>
  t('a => 'c, 'e)
 =
  (onDoneAToB, ioBToC) =>
    switch (ioBToC) {
    | Pure(bToC) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) => onDone(Ok(aToB >> bToC)),
          ),
      )
    | Throw(_) as t => t
    | Suspend(getBToC) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) => onDone(Ok(aToB >> getBToC())),
          ),
      )
    | SuspendIO(getIOBToC) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) =>
              getIOBToC()
              |> unsafeRunAsync(
                   fun
                   | Error(_) as resultE => onDone(resultE)
                   | Ok(bToC) => onDone(Ok(aToB >> bToC)),
                 ),
          ),
      )
    | Async(onDoneBToC) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) =>
              onDoneBToC(
                fun
                | Error(_) as resultE => onDone(resultE)
                | Ok(bToC) => onDone(Ok(aToB >> bToC)),
              ),
          ),
      )
    | Map(r0ToBToC, ioR0) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) =>
              ioR0
              |> map(r0 => aToB >> r0ToBToC(r0))
              |> unsafeRunAsync(onDone),
          ),
      )
    | Apply(ioR0ToBToC, ioR0) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) =>
              ioR0ToBToC
              |> flatMap(r0ToBToC => ioR0 |> map(r0 => aToB >> r0ToBToC(r0)))
              |> unsafeRunAsync(onDone),
          ),
      )
    | FlatMap(r0ToIOBToC, ioR0) =>
      Async(
        onDone =>
          onDoneAToB(
            fun
            | Error(_) as resultE => onDone(resultE)
            | Ok(aToB) =>
              ioR0
              |> flatMap(r0 => r0ToIOBToC(r0) |> map(bToC => aToB >> bToC))
              |> unsafeRunAsync(onDone),
          ),
      )
    }

/**
 * compose specialization for a left-hand-side Map
 */
and composeMap:
  'a 'b 'c 'r0 'e.
  (('r0, 'a) => 'b, t('r0, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (r0ToAToB, ioR0, ioBToC) =>
    ioR0 |> flatMap(r0 => ioBToC |> map(bToC => r0ToAToB(r0) >> bToC))

/**
 * compose specialization for a left-hand-side Apply
 */
and composeApply:
  'a 'b 'c 'r0 'e.
  (t(('r0, 'a) => 'b, 'e), t('r0, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (ioR0ToAToB, ioR0, ioBToC) =>
    ioR0ToAToB
    |> flatMap(r0ToAToB =>
         ioR0 |> flatMap(r0 => ioBToC |> map(bToC => r0ToAToB(r0) >> bToC))
       )

/**
 * compose specialization for a left-hand-side FlatMap
 */
and composeFlatMap:
  'a 'b 'c 'r0 'e.
  ('r0 => t('a => 'b, 'e), t('r0, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (r0ToIOAToB, ioR0, ioBToC) =>
    ioR0
    |> flatMap(r0 =>
         r0ToIOAToB(r0)
         |> flatMap(aToB => ioBToC |> map(bToC => aToB >> bToC))
       );

/**
 * Operator for IO's compose right-to-left composition function
 *
 * (using triple <<< to disambiguate from function compose <<)
 */
let (<<<) = compose;

/**
 * Flipped version of compose for left-to-right usage
 */
let andThen:
  'a 'b 'c 'e.
  (t('a => 'b, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (ioAToB, ioBToC) => compose(ioBToC, ioAToB);

/**
 * Operator for IO's andThen left-to-right composition function
 *
 * (using triple >>> to disambiguate from function andThen >>)
 */
let (>>>) = andThen;

/**
Same as `map`, but operates on the error channel.
*/
let rec mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (e1ToE2, ioA) =>
    switch (ioA) {
    | Pure(a) => Pure(a)
    | Throw(e1) => Throw(e1ToE2(e1))
    | Suspend(getA) => Suspend(getA)
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> mapError(e1ToE2))
    | Async(onDoneA) =>
      Async(
        onDone =>
          onDoneA(resultA => resultA |> Relude_Result.mapError(e1ToE2) |> onDone),
      )
    | Map(rToA, ioR) => ioR |> mapError(e1ToE2) |> map(rToA)
    | Apply(ioRToA, ioR) =>
      Apply(ioRToA |> mapError(e1ToE2), ioR |> mapError(e1ToE2))
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
    | Pure(a) => Pure(a)
    | Throw(e) => eToIOA(e)
    | Suspend(getA) => Suspend(getA)
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> catchError(eToIOA))
    | Async(onDoneA) =>
      Async(
        onDone =>
          onDoneA(
            fun
            | Ok(a) => onDone(Ok(a))
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r0ToA, ioR0) => catchErrorMap(eToIOA, r0ToA, ioR0)
    | Apply(ioR0ToA, ioR0) => catchErrorApply(eToIOA, ioR0ToA, ioR0)
    | FlatMap(r0ToIOA, ioR0) => catchErrorFlatMap(eToIOA, r0ToIOA, ioR0)
    }

and catchErrorMap:
  'a 'r0 'e1 'e2.
  ('e1 => t('a, 'e2), 'r0 => 'a, t('r0, 'e1)) => t('a, 'e2)
 =
  (eToIOA, r0ToA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => Pure(r0 |> r0ToA) |> catchError(eToIOA)
    | Throw(e) => eToIOA(e)
    | Suspend(getR0) =>
      Suspend(() => getR0() |> r0ToA) |> catchError(eToIOA)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> map(r0ToA)) |> catchError(eToIOA)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Ok(r0) => onDone(Ok(r0ToA(r0)))
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> map(r1ToR0 >> r0ToA) |> catchError(eToIOA)
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> pure(r0ToA)) |> catchError(eToIOA)
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1ToIOR0 >> map(r0ToA)) |> catchError(eToIOA)
    }

and catchErrorApply:
  'a 'r0 'e1 'e2.
  ('e1 => t('a, 'e2), t('r0 => 'a, 'e1), t('r0, 'e1)) => t('a, 'e2)
 =
  (eToIOA, ioR0ToA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => ioR0ToA |> map(r0ToA => r0 |> r0ToA) |> catchError(eToIOA)
    | Throw(e) => eToIOA(e)
    | Suspend(getR0) =>
      ioR0ToA |> map(r0ToA => getR0() |> r0ToA) |> catchError(eToIOA)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> apply(ioR0ToA)) |> catchError(eToIOA)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone)
            | Ok(r0) =>
              Apply(ioR0ToA, pure(r0))
              |> catchError(eToIOA)
              |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> apply(pure(r1ToR0) >>> ioR0ToA) |> catchError(eToIOA)
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> ioR0ToA) |> catchError(eToIOA)
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR0ToA
      |> flatMap(r0ToA =>
           ioR1 |> flatMap(r1 => r1 |> r1ToIOR0 |> map(r0 => r0 |> r0ToA))
         )
      |> catchError(eToIOA)
    }

and catchErrorFlatMap:
  'a 'r0 'e1 'e2.
  ('e1 => t('a, 'e2), 'r0 => t('a, 'e1), t('r0, 'e1)) => t('a, 'e2)
 =
  (eToIOA, r0ToIOA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0 |> r0ToIOA |> catchError(eToIOA)
    | Throw(e) => eToIOA(e)
    | Suspend(getR0) =>
      SuspendIO(() => getR0() |> r0ToIOA) |> catchError(eToIOA)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> flatMap(r0ToIOA)) |> catchError(eToIOA)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Ok(r0) =>
              r0 |> r0ToIOA |> catchError(eToIOA) |> unsafeRunAsync(onDone)
            | Error(e) => e |> eToIOA |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> flatMap(r1ToR0 >> r0ToIOA) |> catchError(eToIOA)
    | Apply(ioR1ToR0, ioR1) =>
      ioR1
      |> flatMap(r1 => ioR1ToR0 |> flatMap(r1ToR0 => r1 |> r1ToR0 |> r0ToIOA))
      |> catchError(eToIOA)
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 => r1 |> r1ToIOR0 |> flatMap(r0ToIOA))
      |> catchError(eToIOA)
    };

/**
 * Uses a function to convert an error value to a success value, which serves to "clear" the error
 * in the IO, thereby making the error type `Void.t`.
 */
let handleError: 'a 'e. ('e => 'a, t('a, 'e)) => t('a, Relude_Void.t) =
  (eToA, ioA) => ioA |> catchError(e => Pure(eToA(e)));

/**
Applies functions on both the success and error channels of the `IO`.
*/
let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (aToB, e1ToE2, io) => io |> map(aToB) |> mapError(e1ToE2);

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
 * Returns a new `IO` that when run, will attempt the `IO` given as the first argument,
 * and if it fails, will attempt the `IO` given as the second argument. The second `IO`
 * is only run if the first fails.
 *
 * The <|> operator version of `alt` can be accessed via the `IO.WithError` module functor,
 * like this:
 *
 * ```reason
 * module IOE = IO.WithError({ type t = string; });
 *
 * let a = ref(false);
 * let b = ref(false);
 *
 * IOE.Infix.(
 *   IO.suspend(() => a := true)
 *   <|> IO.suspend(() => b := true) // this effect won't run in this case, because the previous IO succeeds
 *   |> IO.unsafeRunAsync(...)
 * );
 * ```
 */
let alt: 'a 'e. (t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (io1, io2) => io1 |> catchError(_ => io2);

/**
 * Returns a new `IO` that when run, will attempt the `IO` given as the second, un-labeled argument,
 * and if it fails, will attempt the `IO` given as the first argument with the label ~fallback.
 *
 * This is intended to be used with the `|>` pipe operator, like this:
 *
 * ```reason
 * IO.suspend(() => a := true)
 * |> IO.orElse(~fallback=IO.suspend(() => b := true))
 * |> IO.unsafeRunAsync(...)
 * ```
 */
let orElse: 'a 'e. (~fallback: t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (~fallback, io) => {
    alt(io, fallback);
  };

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
          let jsExn = Relude_Js_Exn.unsafeFromExn(exn);
          Throw(jsExn);
        },
    );

/**
Flips the values between the success and error channels.
*/
let rec flip: 'a 'e. t('a, 'e) => t('e, 'a) =
  ioA => {
    switch (ioA) {
    | Pure(a) => throw(a)
    | Throw(e) => pure(e)
    | Suspend(getA) => suspendIO(() => getA() |> throw)
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> flip)
    | Async(onDoneA) =>
      Async(onDone => onDoneA(resultA => onDone(resultA |> Relude_Result.flip)))
    | Map(r0ToA, ioR0) => flipMap(r0ToA, ioR0)
    | Apply(ioR0ToA, ioR0) => flipApply(ioR0ToA, ioR0)
    | FlatMap(r0ToIOA, ioR0) => flipFlatMap(r0ToIOA, ioR0)
    };
  }

and flipMap: 'a 'r0 'e. ('r0 => 'a, t('r0, 'e)) => t('e, 'a) =
  (r0ToA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => Throw(r0ToA(r0))
    | Throw(e) => Pure(e)
    | Suspend(getR0) => SuspendIO(() => Throw(r0ToA(getR0())))
    | SuspendIO(getIOR0) => SuspendIO(() => getIOR0() |> map(r0ToA) |> flip)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(resultR0 =>
            onDone(resultR0 |> Relude_Result.map(r0ToA) |> Relude_Result.flip)
          ),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> map(r1ToR0 >> r0ToA) |> flip
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> pure(r0ToA)) |> flip
    | FlatMap(rToIOR0, ioR) =>
      ioR |> flatMap(r => rToIOR0(r) |> map(r0ToA)) |> flip
    }

and flipApply: 'a 'r0 'e. (t('r0 => 'a, 'e), t('r0, 'e)) => t('e, 'a) =
  (ioR0ToA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => ioR0ToA |> map(r0ToA => r0 |> r0ToA) |> flip
    | Throw(e) => Pure(e)
    | Suspend(getR0) => ioR0ToA |> map(r0ToA => getR0() |> r0ToA) |> flip
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> apply(ioR0ToA) |> flip)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Error(e) => onDone(Ok(e))
            | Ok(r0) =>
              ioR0ToA
              |> map(r0ToA => r0 |> r0ToA)
              |> unsafeRunAsync(
                   fun
                   | Error(e) => onDone(Ok(e))
                   | Ok(a) => onDone(Error(a)),
                 ),
          ),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> apply(pure(r1ToR0) >>> ioR0ToA) |> flip
    | Apply(ioR1ToR0, ioR1) => ioR1 |> apply(ioR1ToR0 >>> ioR0ToA) |> flip
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1 => r1 |> r1ToIOR0 |> apply(ioR0ToA)) |> flip
    }

and flipFlatMap: 'a 'r0 'e. ('r0 => t('a, 'e), t('r0, 'e)) => t('e, 'a) =
  (r0ToIOA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0 |> r0ToIOA |> flip
    | Throw(e) => Pure(e)
    | Suspend(getR0) => SuspendIO(() => getR0() |> r0ToIOA |> flip)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> flip)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(resultR0 =>
            switch (resultR0) {
            | Ok(r0) => r0ToIOA(r0) |> flip |> unsafeRunAsync(onDone)
            | Error(e) => onDone(Belt.Result.Ok(e))
            }
          ),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> flatMap(r1ToR0 >> r0ToIOA) |> flip
    | Apply(ioR1ToR0, ioR1) =>
      ioR1
      |> flatMap(r1 => ioR1ToR0 |> flatMap(r1ToR0 => r1 |> r1ToR0 |> r0ToIOA))
      |> flip
    | FlatMap(rToIOR0, ioR) =>
      ioR |> flatMap(r => rToIOR0(r) |> flatMap(r0ToIOA)) |> flip
    };

/**
Summons an error of type `'e` from the error channel into the success channel as a `Result.t('a, 'e)`.
The error channel becomes `Void.t` because the error has been (re)moved.
*/
let rec summonError: 'a 'e. t('a, 'e) => t(Belt.Result.t('a, 'e), Relude_Void.t) =
  ioA =>
    switch (ioA) {
    | Pure(a) => Pure(Belt.Result.Ok(a))
    | Throw(e) => Pure(Belt.Result.Error(e))
    | Suspend(getA) => Suspend(() => Belt.Result.Ok(getA()))
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> summonError)
    | Async(onDoneA) =>
      Async(onDone => onDoneA(result => onDone(Belt.Result.Ok(result))))
    | Map(r0ToA, ioR0) => summonErrorMap(r0ToA, ioR0)
    | Apply(ioR0ToA, ioR0) => summonErrorApply(ioR0ToA, ioR0)
    | FlatMap(r0ToIOA, ioR0) => summonErrorFlatMap(r0ToIOA, ioR0)
    }

and summonErrorMap:
  'a 'r0 'e.
  ('r0 => 'a, t('r0, 'e)) => t(Belt.Result.t('a, 'e), Relude_Void.t)
 =
  (r0ToA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => Pure(Belt.Result.Ok(r0ToA(r0)))
    | Throw(e) => Pure(Belt.Result.Error(e))
    | Suspend(getR0) => Suspend(() => Belt.Result.Ok(r0ToA(getR0())))
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> map(r0ToA) |> summonError)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(resR0 => onDone(Belt.Result.Ok(Relude_Result.map(r0ToA, resR0)))),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> map(r1ToR0 >> r0ToA) |> summonError
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> pure(r0ToA)) |> summonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1 => r1ToIOR0(r1) |> map(r0ToA)) |> summonError
    };
  }

and summonErrorApply:
  'a 'r0 'e.
  (t('r0 => 'a, 'e), t('r0, 'e)) => t(Belt.Result.t('a, 'e), Relude_Void.t)
 =
  (ioR0ToA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => ioR0ToA |> map(r0ToA => r0ToA(r0)) |> summonError
    | Throw(e) => Pure(Belt.Result.Error(e))
    | Suspend(getR0) =>
      SuspendIO(() => ioR0ToA |> map(r0ToA => r0ToA(getR0())) |> summonError)
    | SuspendIO(getIOR0) =>
      SuspendIO(
        () =>
          ioR0ToA |> flatMap(r0ToA => getIOR0() |> map(r0ToA)) |> summonError,
      )
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Error(_) as resultE => onDone(Ok(resultE))
            | Ok(r0) =>
              ioR0ToA
              |> map(r0ToA => r0ToA(r0))
              |> summonError
              |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> apply(pure(r1ToR0) >>> ioR0ToA) |> summonError
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> ioR0ToA) |> summonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 =>
           r1ToIOR0(r1) |> flatMap(r0 => ioR0ToA |> map(r0ToA => r0ToA(r0)))
         )
      |> summonError
    };
  }

and summonErrorFlatMap:
  'a 'r0 'e.
  ('r0 => t('a, 'e), t('r0, 'e)) => t(Belt.Result.t('a, 'e), Relude_Void.t)
 =
  (r0ToIOA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => r0ToIOA(r0) |> summonError
    | Throw(e) => Pure(Belt.Result.Error(e))
    | Suspend(getR0) => SuspendIO(() => r0ToIOA(getR0()) |> summonError)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> summonError)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Error(e) => onDone(Belt.Result.Ok(Belt.Result.Error(e)))
            | Ok(r0) => r0ToIOA(r0) |> summonError |> unsafeRunAsync(onDone),
          ),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> flatMap(r1ToR0 >> r0ToIOA) |> summonError
    | Apply(ioR1ToR0, ioR1) =>
      ioR1
      |> flatMap(r1 => ioR1ToR0 |> flatMap(r1ToR0 => r1 |> r1ToR0 |> r0ToIOA))
      |> summonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOA(r0)))
      |> summonError
    };
  };

/**
Unsummons an error from a success channel `Result.t('a, 'e)` back into the error channel of the `IO`.
*/
let rec unsummonError: 'a 'e. t(Belt.Result.t('a, 'e), Relude_Void.t) => t('a, 'e) =
  ioResultA =>
    switch (ioResultA) {
    | Pure(resultA) => resultA |> Relude_Result.fold(throw, pure)
    | Throw(void) => Relude_Void.absurd(void)
    | Suspend(getResultA) =>
      SuspendIO(() => getResultA() |> Relude_Result.fold(throw, pure))
    | SuspendIO(getIOResultA) =>
      SuspendIO(() => getIOResultA() |> unsummonError)
    | Async(onDoneResultResultA) =>
      Async(
        onDoneResultA =>
          onDoneResultResultA(resultResultA =>
            switch (resultResultA) {
            | Ok(resultA) => onDoneResultA(resultA)
            | Error(void) => Relude_Void.absurd(void)
            }
          ),
      )
    | Map(r0ToResultA, ioR0) => unsummonErrorMap(r0ToResultA, ioR0)
    | Apply(ioR0ToResultA, ioR0) => unsummonErrorApply(ioR0ToResultA, ioR0)
    | FlatMap(r0ToIOResultA, ioR0) =>
      unsummonErrorFlatMap(r0ToIOResultA, ioR0)
    }

and unsummonErrorMap:
  'r0 'a 'e.
  ('r0 => Belt.Result.t('a, 'e), t('r0, Relude_Void.t)) => t('a, 'e)
 =
  (r0ToResultA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => r0ToResultA(r0) |> Relude_Result.fold(throw, pure)
    | Throw(void) => Relude_Void.absurd(void)
    | Suspend(getR0) =>
      SuspendIO(() => getR0() |> r0ToResultA |> Relude_Result.fold(throw, pure))
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> map(r0ToResultA) |> unsummonError)
    | Async(onDoneR0) =>
      Async(
        onDoneResultA =>
          onDoneR0(
            fun
            | Ok(r0) => onDoneResultA(r0ToResultA(r0))
            | Error(void) => Relude_Void.absurd(void),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> map(r1 => r1 |> r1ToR0 |> r0ToResultA) |> unsummonError
    | Apply(ioR1ToR0, ioR1) =>
      ioR1 |> apply(ioR1ToR0 >>> pure(r0ToResultA)) |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 => r1 |> r1ToIOR0 |> map(r0ToResultA))
      |> unsummonError
    };
  }

and unsummonErrorApply:
  'r0 'a 'e.
  (t('r0 => Belt.Result.t('a, 'e), Relude_Void.t), t('r0, Relude_Void.t)) => t('a, 'e)
 =
  (ioR0ToResultA, ioR0) => {
    ioR0ToResultA
    |> flatMap(r0ToResultA =>
         ioR0 |> flatMap(r0 => r0 |> r0ToResultA |> pure)
       )
    |> unsummonError;
  }

and unsummonErrorFlatMap:
  'r0 'a 'e.
  ('r0 => t(Belt.Result.t('a, 'e), Relude_Void.t), t('r0, Relude_Void.t)) => t('a, 'e)
 =
  (r0ToIOResultA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => r0ToIOResultA(r0) |> unsummonError
    | Throw(absurd) => Relude_Void.absurd(absurd)
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
            | Error(void) => Relude_Void.absurd(void),
          ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> flatMap(r1ToR0 >> r0ToIOResultA) |> unsummonError
    | Apply(ioR1ToR0, ioR1) =>
      ioR1ToR0
      |> flatMap(r1ToR0 =>
           ioR1
           |> flatMap(r1 => r1 |> r1ToR0 |> r0ToIOResultA |> flatMap(pure))
         )
      |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1
      |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOResultA(r0)))
      |> unsummonError
    };
  };

/**
Creates an async `IO` that waits for the given millisecond timeout before completing with a unit value.
*/
let delay: 'e. int => t(unit, 'e) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Belt.Result.Ok()), millis) |> ignore
    );

/**
Creates an async non-failing `IO` that waits for the given millisecond timeout before completing with a unit value.
*/
let delayWithVoid: int => t(unit, Relude_Void.t) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Belt.Result.Ok()), millis) |> ignore
    );

/**
Injects a delay in milliseconds after the given IO.  The value or error from the previous IO is propagated after the delay.

When run, the given IO will be run first, then the delay will be run after the IO finishes.

Example

```re
IO.pure(4) |> IO.withDelayAfter(2000) |> ...
```
*/
let withDelayAfter: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => io |> flatMap(a => delay(millis) |> map(_ => a));

/**
 * Alias for withDelayAfter
 */
let withDelay = withDelayAfter;

/**
 * Injects a delay before the given IO.
 *
 * When run, the delay will be executed first, then the given IO
 */
let withDelayBefore: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => delay(millis) |> flatMap(_ => io);

/**
This will "debounce" an IO so that it will only allow the latest call within some interval to go through.
All other calls will be cancelled.

Note: that the IO produced by this function is not referentially transparent, because
the IO chain is manipulated at runtime via a mutable ref.

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
    let currentlyDebouncedIO = ref(None);
    let startDebouncedIO = () => {
      let debouncedIO = delay(intervalMs);
      currentlyDebouncedIO := debouncedIO |> Relude_Option_Instances.pure;
      debouncedIO
      |> map(() => {
           let shouldRunIO =
             currentlyDebouncedIO^
             |> Relude_Option_Base.fold(false, (===)(debouncedIO));
           if (shouldRunIO) {
             currentlyDebouncedIO := None;
           };
           shouldRunIO;
         });
    };

    a => {
      let immediatelyRanIO =
        switch (immediate, currentlyDebouncedIO^) {
        | (true, None) =>
          suspendIO(() => a |> io |> map(Relude_Option_Instances.pure))
          |> Relude_Option_Instances.pure
        | (true, Some(_))
        | (false, None)
        | (false, Some(_)) => None
        };
      let debouncedIO =
        startDebouncedIO()
        |> flatMap(shouldRunIO =>
             shouldRunIO && immediatelyRanIO |> Relude_Option_Base.isNone
               ? a |> io |> map(Relude_Option_Instances.pure) : None |> pure
           );

      immediatelyRanIO |> Relude_Option_Base.getOrElse(debouncedIO);
    };
  };

/**
This will "throttle" an IO so that it will only allow subsequent calls to go through after some period of time
has elapsed.

Note: that the IO produced by this function is not referentially transparent, because
the IO chain is manipulated at runtime via a mutable ref.

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
        a |> io |> map(Relude_Option_Instances.pure);
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
  let bimap = bimap;
  include Relude_Extensions_Bifunctor.BifunctorExtensions(Bifunctor);

  module Alt: BsAbstract.Interface.ALT with type t('a) = t('a, E.t) = {
    include Functor;
    let alt = alt;
  };
  let alt = alt;
  include Relude_Extensions_Alt.AltExtensions(Alt);

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

  // Not sure if this is valid, but I'll leave it for now
  module Semigroupoid:
    BsAbstract.Interface.SEMIGROUPOID with type t('a, 'b) = t('a => 'b, E.t) = {
    type nonrec t('a, 'b) = t('a => 'b, E.t);
    let compose = compose;
  };
  include Relude_Extensions_Semigroupoid.SemigroupoidExtensions(Semigroupoid);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Bifunctor.BifunctorInfix(Bifunctor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
    include Relude_Extensions_Alt.AltInfix(Alt);
    include Relude_Extensions_Semigroupoid.SemigroupoidInfix(Semigroupoid);
  };
};