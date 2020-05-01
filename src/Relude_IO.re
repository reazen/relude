open BsBastet.Interface;
open Relude_Function.Infix;

[@ocaml.text
  {|
[Relude.IO] contains a type [t('a, 'e)] and related functions for representing
and controlling the execution of side effects.

{2 What is IO?}

Side effects are a normal part of building a useful program. Side effects cover
everything from reading user input to making HTTP requests to drawing output on
the screen. Really, a side effect is any change (in application state or in the
universe) that can be observed outside of a function other than that function's
returned value.

While side effects are important for building a program that actually {e does}
something, they are inherently impure. This is obviously at odds with writing
{{: https://en.wikipedia.org/wiki/Pure_function} pure} code. IO seeks to restore
this purity by {e describing} these effects (and transformations on top of these
effects) as data. Doing so allows us to be much more precise about when our
effects actually run, and we can more easily reuse, reason about, and
confidently refactor these descriptions of effects.

[IO] can represent any type of synchronous or asynchronous side effect that can
either produce a value (in the success channel ['a]) or an error (in the error
channel ['e]). The main purpose of [IO] is to make side-effectful code
{{: https://en.wikipedia.org/wiki/Referential_transparency} referentially
transparent}. When you construct an [IO], in most cases you are just suspending
the execution of a side effect, rather than allowing it to execute immediately.

In the case of a synchronous effect like a [Js.log(...)], which writes to the
console, or [getCurrentDate()], which reads the current clock time from the
system, or [Math.random()], which generates a random number out of the blue,
it simply makes the action lazy, using a thunk like [unit => effect].

In the case of asynchronous side effects, the execution of the effect is
suspended until the [IO] is run using [unsafeRunAsync]. Because [IO] can
represent both synchronous and asynchronous side effects, the end result of
executing an [IO] must always be handled via a callback, as if the effect were
async.

{2 Why is running an [IO] considered unsafe?}

An [IO] value represents a "description" or "recipe" of one or more side effects
to perform. When you run an [IO], all of the suspended effects are executed, and
all of the side effects will occur. Naming the run function [unsafeRunAsync] is
intended to warn you that invoking the function will execute your side effects.

Describing these functions as "unsafe" is a convention in the FP community for
performing [IO] effects:

- {{: http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Unsafe.html} Haskell IO},
- {{: https://pursuit.purescript.org/packages/purescript-effect/2.0.1/docs/Effect.Unsafe#v:unsafePerformEffect} Purescript Effect},
- {{: https://typelevel.org/cats-effect/datatypes/io.html#unsafe-operations} Cats Effect IO}.

{2 Relation to [Js.Promise]}

If you are coming from the JavaScript world, don't be afraid of [IO] - in many
ways, you can basically just think of it as a construct similar to [Promise],
with the following key differences:

{3 Eager vs lazy}

[Promise] is eagerly-executed and [IO] is lazily-executed. When you construct
a [Promise], its execution is immediately started, and it will invoke its
success or failure callbacks whenever it finishes. [IO] is lazily-executed, so
it will not do anything until you "run" the [IO] using [unsafeRunAsync], which
provides you with the resulting value or error using a callback from
[result('a, 'e) => unit].

As a result, functions that return a [Promise] are inherently impure (they've
already started performing an effect), while functions that return [IO] are not
(they've returned a data structure describing how an effect will be performed in
the future).

{3 Memoization}

[Promise] {{: https://en.wikipedia.org/wiki/Memoization} memoizes} its result
value, and [IO] does not. This means that once a [Promise] is resolved or
rejected, it will hold onto the resulting value or error, and any future uses
of [then] or [catch] will produce that same value or error, rather than
re-running the [Promise] and producing a new value or error.

[IO] doesn't have built-in memoization of the value, so re-running an [IO] will
cause all the effects to be re-run, and a new value (or error) will be produced.

{3 Error handling}

A [Promise] can fail (be rejected) with any type of value, and in JavaScript,
the only way to tell what type of error occurred is to inspect the error value
and/or its type at runtime. For this reason, the BuckleScript bindings choose
to represent a [Promise] error as an {{: https://bucklescript.github.io/bucklescript/api/Js.Promise.html#TYPEerror} opaque type}.
Opaque types require runtime inspection or unsafe casting before they can be
used.

[IO.t('a, 'e)] uses the ['e] type parameter to let you express to the type
system whatever type of error makes most sense for your needs. In some cases,
you may not care about the details of the error and can choose to use an opaque
type or [unit]. In other cases, you may wish to pass along custom errors that
carry lots of extra information. Either way, the choice is yours, and that
flexibility makes errors much easier to work with,

{2 More about "effects"}

The word "effect" is a bit overloaded in the functional programming world. We
often hear about "side effects," and this term is typically referring to things
like writing to stdout, making a network call, getting the system time,
generating a random number - these are all things that either send information
to the outside world, or pull information in from the outside world. They are
called "side effects" because the function has an effect on the system beyond
its input arguments and output return value - it does something that is not
represented by the input and output types.

Another place in functional programming where the term "effect" appears is when
we talk about data types like [option('a)] or [result('a, 'e)]. These types of
values are often called "effectful" values, because they represent some sort of
additional "effect" that may have occurred. For example if you have a function
[a => b], assuming it's a pure function, the only possible value you can get out
of the function is a value of type ['b] - there's no way to represent a failure,
an asynchronous computation, or a case where the function can't produce a value.
If instead the function is ['a => option('b)], the function can now either
produce a value of type ['a], or fail to produce a value ([None]).

This behavior is often called the "effect" of producing a value with the
possibility of not being able to produce a value. For ['a => result('b, 'e)],
we have the "effect" of either producing a value or an error. [Js.Promise] is
another effectful value in that it has the "effect" of asynchronous computation
(a value will be produced at some time later), and the "effect" of possible
failure. In terms of [IO], this is important because not only can [IO] suspend
actual "side effects" (like stdout/network/etc.), but it can also represent the
types of "effects" like [option('a)], [result('a, 'e)], [Js.Promise.t], etc.

All of these types of "effectful" values can be converted to an [IO], so that
you can easily compose all of these different effects in a single expression.
When you try to write code in the monadic style (e.g. using chained [flatMap] or
[>>=] operations), it can be quite convenient to be able to deal with various
types of side effects and "functional effects" using a single Monad.

{2 Further reading}

This type is inspired by and based on the recent work that has gone into the
concept of bi-functor IO in the FP community:

{ul
 {- John De Goes' blog posts
    - {{: http://degoes.net/articles/bifunctor-io} Bifunctor IO: A Step Away from Dynamically-Typed Error Handling}
    - {{: http://degoes.net/articles/zio-environment} Beautiful, Simple, Testable Functional Effects for Scala}}
 {- {{: https://github.com/scalaz/scalaz-zio} ZIO}}
 {- {{: https://github.com/LukaJCB/cats-bio} cats-bio}}
 {- {{: https://github.com/slamdata/purescript-aff/issues/137} PureScript Aff Discussion}}}
|}
];

/**
IO is a bi-functor effect type that supports both synchronous and asynchronous effects, with explicit error handling.

This is inspired by the following libraries/articles:

  - John De Goes - http://degoes.net/articles/only-one-io and http://degoes.net/articles/bifunctor-io
  - ZIO/Scalaz 8 IO (Scala) - https://github.com/scalaz/scalaz-zio
  - cats-bio (Scala) - https://github.com/LukaJCB/cats-bio
  - purescript-aff discussion (Purescript) - https://github.com/slamdata/purescript-aff/issues/137
*/
type t('a, 'e) =
  | Pure('a): t('a, 'e)
  | Throw('e): t('a, 'e)
  | Suspend(unit => 'a): t('a, 'e)
  | SuspendIO(unit => t('a, 'e)): t('a, 'e)
  | Async((result('a, 'e) => unit) => unit): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | Apply(t('r => 'a, 'e), t('r, 'e)): t('a, 'e)
  | FlatMap('r => t('a, 'e), t('r, 'e)): t('a, 'e);

/**
[IO.pure] constructs an [IO] from the provided value. However, the fact that you
are able to provide a value suggests that the effect has already run.

In many cases, it's preferable to use [suspend], especailly for values that are
expensive to construct or may produce observable effects.
*/
let pure: 'a 'e. 'a => t('a, 'e) = a => Pure(a);

/**
[IO.pureWithVoid] Wraps a non-failing, strictly-evaluated value in an [IO].
Unlike the normal [pure] function, this version uses [Void.t] in the error
channel to indicate that no unhandled error exists.

As with [pure], you should prefer [suspend] for values that are expensive to
construct or may have side-effects.
*/
let pureWithVoid: 'a. 'a => t('a, Relude_Void.t) = a => Pure(a);

/**
[IO.unit] represents an [IO] that successfully resolves to [()].
*/
let unit: 'e. t(unit, 'e) = Pure();

/**
[IO.unitWithVoid] represents an [IO] that successfully resolves to [()]. Unlike
[IO.unit], this function indicates that it has no unhandled errors by using
[Void.t] in the error channel.
*/
let unitWithVoid: t(unit, Relude_Void.t) = Pure();

/**
[IO.throw] wraps a strictly-evaluated error value in an [IO].

Prefer [suspendThrow] (or variants) for values that are expensive to construct
or may have side-effects.
*/
let throw: 'a 'e. 'e => t('a, 'e) = e => Throw(e);

/**
[IO.throwWithVoid] wraps a strictly-evaluated error value in an [IO], indicating
that no success can exist with a [Void.t] type in the success channel.
*/
let throwWithVoid: 'e. 'e => t(Relude_Void.t, 'e) = e => Throw(e);

/**
[IO.suspend] wraps a lazily-evaluated value in an [IO].
*/
let suspend: 'a 'e. (unit => 'a) => t('a, 'e) = getA => Suspend(getA);

/**
[IO.suspendWithVoid] wraps a lazily-evaluated value in an [IO]. This function
cannot fail, as indicated by the [Void.t] type in the error channel.
*/
let suspendWithVoid: 'a. (unit => 'a) => t('a, Relude_Void.t) =
  getA => Suspend(getA);

/**
[IO.suspendThrow] wraps a lazily-evaluated error in an [IO]
*/
let suspendThrow: 'a 'e. (unit => 'e) => t('a, 'e) =
  getError => SuspendIO(() => Throw(getError()));

/**
[IO.suspendIO] wraps a lazily-evaluated [IO] value in an [IO].

This can be useful if you are dealing with an effectful value that is normally
eagerly or strictly evaluated, like a [result] or [option] or [Js.Promise].
In this case, you would typically convert the effectful value into an [IO] using
a function like [pure] or [throw], but doing this strict conversion inside a
[suspendIO] function makes the conversion lazy.
*/
let suspendIO: 'a 'e. (unit => t('a, 'e)) => t('a, 'e) =
  getIO => SuspendIO(getIO);

/**
[IO.async] creates an async [IO] value that is run by invoking a callback
[result('a, 'e) => unit].

This is useful for lifting other types of async effects into [IO], like
[Js.Promise] or a Node.js-style callback API.
*/
let async: 'a 'e. ((result('a, 'e) => unit) => unit) => t('a, 'e) =
  onDone => Async(onDone);

/**
[IO.fromOption] onverts an [option('a)] to an [IO.t('a, 'e)] by providing a
callback to use when the [option] is [None].

Because the option is already evaluated, no effort is made to suspend any
effects.
*/
let fromOption: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, option) =>
    option |> Relude_Option_Base.foldLazy(() => throw(getError()), pure);

/**
[IO.fromResult] converts a [result('a, 'e)] to an [IO.t('a, 'e)].

Because the result is already evaluated, no effort is made to suspend any
effects.
*/
let fromResult: 'a 'e. result('a, 'e) => t('a, 'e) =
  res => res |> Relude_Result.fold(throw, pure);

/**
[IO.map] applies a function ['a => 'b] on an [IO.t('a, 'e)] to produce an
[IO.t('b, 'e)].
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, io) => Map(f, io);

let (<$>) = map;

let (<#>) = (ioA, aToB) => map(aToB, ioA);

/**
Applies a side-effect function ['a => unit] on an [IO.t('a, 'e)], and propagates
the ['a] value unchanged.

This is useful for doing things like logging the value inside the [IO].
*/
let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  f =>
    map(a => {
      f(a);
      a;
    });

/**
Applicative [apply] function
*/
let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ioAToB, ioA) => Apply(ioAToB, ioA);

let (<*>) = apply;

/**
[IO.flatMap] pplies an effectful function ['a => IO.t('b, 'e)] on the ['a] value
inside the [IO] to produce an [IO.t('b, 'e)].
*/
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (rToIOA, ioR) => FlatMap(rToIOA, ioR);

/**
[IO.bind] is [flatMap] with the argument order reversed. It's also an alias for
the [>>=] "bind" operator.
*/
let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ioA, aToIOB) => FlatMap(aToIOB, ioA);

let (>>=) = bind;

/**
[IO.cond] applies the provided predicate function to the success value in the
[IO]. If the condition is satisfied, the provided ['a] value is lifted into the
[IO] and returned. If the condition fails, the provided ['e] value is lifted
into the [IO] and returned. If the [IO] was already an error, the predicate
function isn't run and the existing error is preserved.
*/
let cond: 'a 'e. ('a => bool, 'a, 'e, t('a, 'e)) => t('a, 'e) =
  (f, newA, err, ioA) =>
    ioA |> flatMap(a => f(a) ? pure(newA) : throw(err));

/**
[IO.condError] tests the success value against a provided predicate, as [cond]
does. Unlike [cond], if the predicate passes, the existing ['a] is returned. If
the condition does not pass, the new error is used instead.
*/
let condError: 'a 'e. ('a => bool, 'e, t('a, 'e)) => t('a, 'e) =
  (f, err, ioA) => ioA |> flatMap(a => f(a) ? pure(a) : throw(err));

/**
[IO.unsafeRunAsync] runs the [IO.t('a, 'e)] to produce a final [result('a, 'e)],
which is provided to the caller via a callback of type [result('a, 'e) => unit].

This function should be run "at the edge of the program" to evaluate the
suspended side-effects in the [IO] and produce either an error of type ['e] or a
successful value of type ['a]. Ideally, in simple apps, this execution happens
at the end of your main function, but when using [IO] inside existing frameworks
that don't natively support [IO], the most appropriate place may be inside the
context of a reducer side effect or a web app controller function.

The function uses the term "unsafe" because calling this function causes all of
the suspended side effects to actually be executed. It is not "unsafe" in that
it can throw an exception - it is just a convention in FP libraries to denote
these types of functions as unsafe.
*/
let rec unsafeRunAsync: 'a 'e. (result('a, 'e) => unit, t('a, 'e)) => unit =
  (onDone, ioA) =>
    switch (ioA) {
    | Pure(a) => onDone(Ok(a))
    | Throw(e) => onDone(Error(e))
    | Suspend(getA) => onDone(Ok(getA()))
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
Runs two IOs in parallel, and invokes the given done callback when all complete

Note that applicative uses of IO (apply/map2/map3/traverse/etc.) will run
the IOs in parallel, so it's rarely necessary for the end-user to call this
directly.
*/
and unsafeRunAsyncPar2:
  'a 'b 'e.
  ((result('a, 'e), result('b, 'e)) => unit, t('a, 'e), t('b, 'e)) => unit
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
Runs three IOs in parallel, and invokes the given done callback when all
complete.

Note that applicative uses of IO (apply/map2/map3/traverse/etc.) will run
the IOs in parallel, so it's rarely necessary for the end-user to call this
directly.
*/
and unsafeRunAsyncPar3:
  'a 'b 'c 'e.
  (
    (result('a, 'e), result('b, 'e), result('c, 'e)) => unit,
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
Creates a new IO value that contains the composition of functions from two
input IO values. Composition is done from right-to-left with this function - see
andThen for left-to-right.
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
compose specialization for a left-hand-side Pure('a => 'b)
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
compose specialization for a left-hand-side Throw
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
compose specialization for a left-hand-side Suspend
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
compose specialization for a left-hand-side SuspendIO
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
compose specialization for a left-hand-side Async
*/
and composeAsync:
  'a 'b 'c 'e.
  ((result('a => 'b, 'e) => unit) => unit, t('b => 'c, 'e)) =>
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
compose specialization for a left-hand-side Map
*/
and composeMap:
  'a 'b 'c 'r0 'e.
  (('r0, 'a) => 'b, t('r0, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (r0ToAToB, ioR0, ioBToC) =>
    ioR0 |> flatMap(r0 => ioBToC |> map(bToC => r0ToAToB(r0) >> bToC))

/**
compose specialization for a left-hand-side Apply
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
compose specialization for a left-hand-side FlatMap
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
Operator for IO's compose right-to-left composition function

This uses triple <<< to disambiguate from function compose <<
*/
let (<<<) = compose;

/**
Flipped version of compose for left-to-right usage
*/
let andThen:
  'a 'b 'c 'e.
  (t('a => 'b, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (ioAToB, ioBToC) => compose(ioBToC, ioAToB);

/**
Operator for IO's andThen left-to-right composition function

This uses triple >>> to disambiguate from function andThen >>
*/
let (>>>) = andThen;

/**
Same as [map], but operates on the error channel.
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
          onDoneA(resultA =>
            resultA |> Relude_Result.mapError(e1ToE2) |> onDone
          ),
      )
    | Map(rToA, ioR) => ioR |> mapError(e1ToE2) |> map(rToA)
    | Apply(ioRToA, ioR) =>
      Apply(ioRToA |> mapError(e1ToE2), ioR |> mapError(e1ToE2))
    | FlatMap(rToIOA, ioR) =>
      ioR |> mapError(e1ToE2) |> flatMap(r => rToIOA(r) |> mapError(e1ToE2))
    };

/**
Same as [tap], but operates on the error channel.
*/
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> mapError(e => {
         f(e);
         e;
       });

/**
Handles an error of types ['e1] from an [IO.t('a, 'e1)] and converts it into a
new [IO.t('a, 'e1)] value. This is much like [flatMap]/[bind] but works for
the error channel of the [IO].
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
Uses a function to convert an error value to a success value, which serves to
"clear" the error in the IO, thereby making the error type [Void.t].
*/
let handleError: 'a 'e. ('e => 'a, t('a, 'e)) => t('a, Relude_Void.t) =
  (eToA, ioA) => ioA |> catchError(e => Pure(eToA(e)));

/**
Maps the success channel and handles an error on the error channel to end up
with an IO of a new type with a voided error channel
*/
let mapHandleError:
  'a 'e 'b.
  ('a => 'b, 'e => 'b, t('a, 'e)) => t('b, Relude_Void.t)
 =
  (aToB, eToB, ioAE) => ioAE |> map(aToB) |> handleError(eToB);

/**
Applies functions on both the success and error channels of the [IO].
*/
let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (aToB, e1ToE2, io) => io |> map(aToB) |> mapError(e1ToE2);

/**
Same as [tap], but works on both the success and error channels simultaneously.
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
Returns a new [IO] that when run, will attempt the [IO] given as the first
argument, and if it fails, will attempt the [IO] given as the second argument.
The second [IO] is only run if the first fails.

The [<|>] operator version of [alt] can be accessed via the [IO.WithError]
module functor, like this:

{[
  module IOE = IO.WithError({ type t = string; });

  let a = ref(false);
  let b = ref(false);

  IOE.Infix.(
    IO.suspend(() => a := true)
    <|> IO.suspend(() => b := true) // this effect won't run in this case, because the previous IO succeeds
    |> IO.unsafeRunAsync(...)
  );
]}
*/
let alt: 'a 'e. (t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (io1, io2) => io1 |> catchError(_ => io2);

/**
Creates a new IO that will run the two input IO effects in parallel, and resolve
if either or both succeed.
*/
let align:
  'a 'b 'e.
  (t('a, 'e), t('b, 'e)) => t(Relude_Ior_Type.t('a, 'b), 'e)
 =
  (ioA, ioB) => {
    Async(
      onDone =>
        // Not sure if there's a better way to do this
        unsafeRunAsyncPar2(
          (resultA, resultB) => {
            switch (resultA, resultB) {
            | (Ok(a), Ok(b)) => onDone(Ok(Relude_Ior_Type.Both(a, b)))
            | (Ok(a), Error(_)) => onDone(Ok(Relude_Ior_Type.This(a)))
            | (Error(_), Ok(b)) => onDone(Ok(Relude_Ior_Type.That(b)))
            | (Error(_) as e, Error(_)) => onDone(e)
            }
          },
          ioA,
          ioB,
        ),
    );
  };

/**
Creates a new IO that runs two effects in parallel, and if either or both
succeed, convert the result into another type 'c
*/
let alignWith:
  'a 'b 'c 'e.
  (Relude_Ior_Type.t('a, 'b) => 'c, t('a, 'e), t('b, 'e)) => t('c, 'e)
 =
  (f, fa, fb) => {
    align(fa, fb) |> map(f);
  };

/**
Returns a new [IO] that when run, will attempt the [IO] given as the second,
un-labeled argument, and if it fails, will attempt the [IO] given as the first
argument with the label ~fallback.

This is intended to be used with the [|>] pipe operator, like this:

{[
  IO.suspend(() => a := true)
  |> IO.orElse(~fallback=IO.suspend(() => b := true))
  |> IO.unsafeRunAsync(...)
]}
*/
let orElse: 'a 'e. (~fallback: t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (~fallback, io) => {
    alt(io, fallback);
  };

/**
Lifts a side-effect function that might throw an exception into a suspended
[IO.t('a, exn)] value.

The [exn] type is OCaml's extensible error type.
*/
let tries: 'a. (unit => 'a) => t('a, exn) =
  getA =>
    SuspendIO(
      () =>
        try(Pure(getA())) {
        | exn => Throw(exn)
        },
    );

/**
Lifts a side-effect function that might throw an JS exception into a suspended
[IO.t('a, Js.Exn.t)] value.

If a normal [Js.Exn.t] is throw, it is captured as-is, but if the thrown object
is not a [Js.Exn.t] it is unsafely coerced into a [Js.Exn.t].
*/
let triesJS: 'a. (unit => 'a) => t('a, Js.Exn.t) =
  getA =>
    SuspendIO(
      () =>
        try(Pure(getA())) {
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
      Async(
        onDone => onDoneA(resultA => onDone(resultA |> Relude_Result.flip)),
      )
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
            onDone(
              resultR0 |> Relude_Result.map(r0ToA) |> Relude_Result.flip,
            )
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
            | Error(e) => onDone(Ok(e))
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
Summons an error of type ['e] from the error channel into the success channel as
a [result('a, 'e)]. The error channel becomes [Void.t] because the error has
been (re)moved.
*/
let rec summonError: 'a 'e. t('a, 'e) => t(result('a, 'e), Relude_Void.t) =
  ioA =>
    switch (ioA) {
    | Pure(a) => Pure(Ok(a))
    | Throw(e) => Pure(Error(e))
    | Suspend(getA) => Suspend(() => Ok(getA()))
    | SuspendIO(getIOA) => SuspendIO(() => getIOA() |> summonError)
    | Async(onDoneA) =>
      Async(onDone => onDoneA(result => onDone(Ok(result))))
    | Map(r0ToA, ioR0) => summonErrorMap(r0ToA, ioR0)
    | Apply(ioR0ToA, ioR0) => summonErrorApply(ioR0ToA, ioR0)
    | FlatMap(r0ToIOA, ioR0) => summonErrorFlatMap(r0ToIOA, ioR0)
    }

and summonErrorMap:
  'a 'r0 'e.
  ('r0 => 'a, t('r0, 'e)) => t(result('a, 'e), Relude_Void.t)
 =
  (r0ToA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => Pure(Ok(r0ToA(r0)))
    | Throw(e) => Pure(Error(e))
    | Suspend(getR0) => Suspend(() => Ok(r0ToA(getR0())))
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> map(r0ToA) |> summonError)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(resR0 => onDone(Ok(Relude_Result.map(r0ToA, resR0)))),
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
  (t('r0 => 'a, 'e), t('r0, 'e)) => t(result('a, 'e), Relude_Void.t)
 =
  (ioR0ToA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => ioR0ToA |> map(r0ToA => r0ToA(r0)) |> summonError
    | Throw(e) => Pure(Error(e))
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
  ('r0 => t('a, 'e), t('r0, 'e)) => t(result('a, 'e), Relude_Void.t)
 =
  (r0ToIOA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => r0ToIOA(r0) |> summonError
    | Throw(e) => Pure(Error(e))
    | Suspend(getR0) => SuspendIO(() => r0ToIOA(getR0()) |> summonError)
    | SuspendIO(getIOR0) =>
      SuspendIO(() => getIOR0() |> flatMap(r0ToIOA) |> summonError)
    | Async(onDoneR0) =>
      Async(
        onDone =>
          onDoneR0(
            fun
            | Error(e) => onDone(Ok(Error(e)))
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
Unsummons an error from a success channel [result('a, 'e)] back into the error
channel of the [IO].
*/
let rec unsummonError: 'a 'e. t(result('a, 'e), Relude_Void.t) => t('a, 'e) =
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
  ('r0 => result('a, 'e), t('r0, Relude_Void.t)) => t('a, 'e)
 =
  (r0ToResultA, ioR0) => {
    switch (ioR0) {
    | Pure(r0) => r0ToResultA(r0) |> Relude_Result.fold(throw, pure)
    | Throw(void) => Relude_Void.absurd(void)
    | Suspend(getR0) =>
      SuspendIO(
        () => getR0() |> r0ToResultA |> Relude_Result.fold(throw, pure),
      )
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
  (t('r0 => result('a, 'e), Relude_Void.t), t('r0, Relude_Void.t)) =>
  t('a, 'e)
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
  ('r0 => t(result('a, 'e), Relude_Void.t), t('r0, Relude_Void.t)) =>
  t('a, 'e)
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
Creates an async [IO] that waits for the given millisecond timeout before
completing with a unit value.
*/
let delay: 'e. int => t(unit, 'e) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Ok()), millis) |> ignore
    );

/**
Creates an async non-failing [IO] that waits for the given millisecond timeout
before completing with a unit value.
*/
let delayWithVoid: int => t(unit, Relude_Void.t) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Ok()), millis) |> ignore
    );

/**
Injects a delay in milliseconds after the given IO.  The value or error from the
previous IO is propagated after the delay.

When run, the given IO will be run first, then the delay will be run after the
IO finishes.

{[
  IO.pure(4) |> IO.withDelayAfter(2000) |> ...
]}
*/
let withDelayAfter: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => io |> flatMap(a => delay(millis) |> map(_ => a));

/**
Alias for withDelayAfter
*/
let withDelay = withDelayAfter;

/**
Injects a delay before the given IO.

When run, the delay will be executed first, then the given IO
*/
let withDelayBefore: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => delay(millis) |> flatMap(_ => io);

/**
This will "debounce" an IO so that it will only allow the latest call within some interval to go through.
All other calls will be cancelled.

Note: that the IO produced by this function is not referentially transparent, because
the IO chain is manipulated at runtime via a mutable ref.

{[
  let ioLog = messageToLog => IO.pure() |> IO.map(() => Js.log(messageToLog));
  let debouncedIoLog = IO.debounce(ioLog);

  "This message will not get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
  "This message will also not get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
  "This message will get logged" |> debouncedIoLog |> IO.unsafeRunAsync(ignore);
]}
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

{[
let ioLog = messageToLog => IO.pure() |> IO.map(() => Js.log(messageToLog));
let throttledIoLog = IO.throttled(ioLog);

"This message will get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
"This message will not get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
"This message will also not get logged" |> throttledIoLog |> IO.unsafeRunAsync(ignore);
]}
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

module Bifunctor: BIFUNCTOR with type t('a, 'e) = t('a, 'e) = {
  type nonrec t('a, 'e) = t('a, 'e);
  let bimap = bimap;
};
let bimap = bimap;
include Relude_Extensions_Bifunctor.BifunctorExtensions(Bifunctor);

/**
Because this is a bifunctor, we need to use a module functor to lock in the
error type, so we can implement many of the single-type parameter typeclasses.
*/
module WithError = (E: TYPE) => {
  type nonrec t('a) = t('a, E.t);

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  let map = Functor.map;
  let mapError = mapError;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Alt: ALT with type t('a) = t('a) = {
    include Functor;
    let alt = alt;
  };
  let alt = alt;
  include Relude_Extensions_Alt.AltExtensions(Alt);

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative: APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Semialign: Relude_Interface.SEMIALIGN with type t('a) = t('a) = {
    include Functor;
    let align = align;
    let alignWith = alignWith;
  };
  include Relude_Extensions_Semialign.SemialignExtensions(Semialign);

  module Monad: MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module MonadThrow:
    Relude_Interface.MONAD_THROW with type t('a) = t('a) and type e = E.t = {
    include Monad;
    type e = E.t;
    let throwError = throw;
  };
  let throwError = MonadThrow.throwError;
  include Relude_Extensions_MonadThrow.MonadThrowExtensions(MonadThrow);

  module MonadError:
    Relude_Interface.MONAD_ERROR with type t('a) = t('a) and type e = E.t = {
    include MonadThrow;
    let catchError = catchError;
  };
  let catchError = MonadError.catchError;
  include Relude_Extensions_MonadError.MonadErrorExtensions(MonadError);

  // Not sure if this is valid, but I'll leave it for now
  module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = t('a => 'b) = {
    type nonrec t('a, 'b) = t('a => 'b);
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
