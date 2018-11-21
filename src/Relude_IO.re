let (<<) = Relude_Function.compose;
let (>>) = Relude_Function.andThen;

type t('a, 'e) =
  | Pure('a) : t('a, 'e)
  | Throw('e) : t('a, 'e)
  | Suspend(unit => 'a) : t('a, 'e)
  | SuspendIO(unit => t('a, 'e)) : t('a, 'e)
  | Async((Belt.Result.t('a, 'e) => unit) => unit) : t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | FlatMap('r => t('a, 'e), t('r, 'e)): t('a, 'e);

let pure: 'a => t('a, 'e) = a => Pure(a);

let pureWithVoid: 'a => t('a, Relude_Void.t) = a => Pure(a);

let throw: 'e => t('a, 'e) = e => Throw(e);

let throwWithVoid: 'e => t(Relude_Void.t, 'e) = e => Throw(e);

let suspend: (unit => 'a) => t('a, 'e) = getA => Suspend(getA);

let suspendWithVoid: (unit => 'a) => t('a, Relude_Void.t) =
  getA => Suspend(getA);

let suspendIO: (unit => t('a, 'e)) => t('a, 'e) = getIO => SuspendIO(getIO);

let async: ((Belt.Result.t('a, 'e) => unit) => unit) => t('a, 'e) =
  onDone => Async(onDone);

let map: ('a => 'b, t('a, 'e)) => t('b, 'e) = (f, io) => Map(f, io);

let tap: ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> map(a => {
         f(a);
         a;
       });

let flatMap: ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (rToIOA, ioR) => FlatMap(rToIOA, ioR);

let bind: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ioA, aToIOB) => flatMap(aToIOB, ioA);

let apply: (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ioF, ioA) => ioF |> flatMap(f => ioA |> map(f));

let rec mapError: ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (e1ToE2, ioA) =>
    switch (ioA) {
    | Pure(_) as io => io /*|> mapError(e1ToE2)*/
    | Throw(e1) => Throw(e1ToE2(e1))
    | Suspend(_) as io => io |> mapError(e1ToE2)
    | SuspendIO(_) as io => io |> mapError(e1ToE2)
    | Async(onDone) =>
      Async(
        (
          onDone' =>
            onDone(
              fun
              | Ok(a) => onDone'(Belt.Result.Ok(a))
              | Error(e) => onDone'(Belt.Result.Error(e1ToE2(e))),
            )
        ),
      )
    | Map(_, _) as io => io |> mapError(e1ToE2)
    | FlatMap(_, _) as io => io |> mapError(e1ToE2)
    };

let tapError: ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> mapError(e => {
         f(e);
         e;
       });

let rec bimap: ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (aToB, e1ToE2, io) =>
    switch (io) {
    | Pure(a) => Pure(aToB(a)) |> mapError(e1ToE2)
    | Throw(e1) => Throw(e1ToE2(e1)) |> mapError(e1ToE2)
    | Suspend(getA) => Suspend((() => aToB(getA()))) |> mapError(e1ToE2)
    | SuspendIO(getIOA) =>
      SuspendIO((() => getIOA() |> bimap(aToB, e1ToE2)))
    | Async(onDone) =>
      Async(
        (
          onDoneB =>
            onDone(resultA =>
              onDoneB(Relude_Result.bimap(aToB, e1ToE2, resultA))
            )
        ),
      )
    | Map(rToA, ioR) => Map(rToA >> aToB, ioR) |> mapError(e1ToE2)
    | FlatMap(rToIOA, ioR) =>
      FlatMap((r => rToIOA(r) |> bimap(aToB, e1ToE2)), ioR)
    };

let bitap: ('a => unit, 'e => unit, t('a, 'e)) => t('a, 'e) =
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

let catchError: ('e => t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (eToIOA, ioA) =>
    switch (ioA) {
    | Throw(e) => eToIOA(e)
    | _ => ioA
    };

let tries: (unit => 'a) => t('a, exn) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | exn => Throw(exn)
        },
    );

let triesJS: (unit => 'a) => t('a, Js.Exn.t) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | Js.Exn.Error(jsExn) => Throw(jsExn)
        | exn =>
          let jsExn = Relude_JsExn.exnToJsExn(exn);
          Throw(jsExn);
        },
    );

let rec materializeError:
  t('a, 'e) => t(Belt.Result.t('a, 'e), Relude_Void.t) =
  ioA =>
    switch (ioA) {
    | Pure(a) => Pure(Belt.Result.Ok(a))

    | Throw(e) => Pure(Belt.Result.Error(e))

    | Suspend(getA) => Suspend((() => Belt.Result.Ok(getA())))

    | SuspendIO(getIOA) => SuspendIO((() => getIOA() |> materializeError))

    | Async(onDone) =>
      Async(
        (onDoneMat => onDone(result => onDoneMat(Belt.Result.Ok(result)))),
      )

    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => Pure(Belt.Result.Ok(r0ToA(r0)))

      | Throw(e) => Pure(Belt.Result.Error(e))

      | Suspend(getR0) => Suspend((() => Belt.Result.Ok(r0ToA(getR0()))))

      | SuspendIO(getIOR0) =>
        SuspendIO((() => getIOR0() |> map(r0ToA) |> materializeError))

      | Async(onDoneR0) =>
        Async(
          (
            onDoneResA =>
              onDoneR0(resultR0 =>
                resultR0
                |> Relude_Result.map(r0ToA)
                |> (resultA => onDoneResA(Belt.Result.Ok(resultA)))
              )
          ),
        )

      | Map(r1ToR0, ioR1) => Map(r1ToR0 >> r0ToA, ioR1) |> materializeError

      | FlatMap(r1ToIOR0, ioR1) =>
        FlatMap((r1 => r1ToIOR0(r1) |> map(r0ToA)), ioR1)
        |> materializeError
      }

    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> materializeError

      | Throw(e) => Pure(Belt.Result.Error(e))

      | Suspend(getR0) =>
        SuspendIO((() => r0ToIOA(getR0()) |> materializeError))

      | SuspendIO(getIOR0) =>
        SuspendIO((() => getIOR0() |> flatMap(r0ToIOA) |> materializeError))

      | Async(onDoneR0) =>
        Async(
          (
            onDoneResA =>
              onDoneR0(resultR0 =>
                switch (resultR0) {
                | Ok(r0) =>
                  r0ToIOA(r0)
                  |> materializeError
                  |> map(resA => onDoneResA(Belt.Result.Ok(resA)))
                  |> ignore
                | Error(e) =>
                  onDoneResA(Belt.Result.Ok(Belt.Result.Error(e)))
                }
              )
          ),
        )

      | Map(r1ToR0, ioR1) =>
        FlatMap((r1 => r1ToR0(r1) |> r0ToIOA |> materializeError), ioR1)

      | FlatMap(r1ToIOR0, ioR1) =>
        FlatMap(
          (r1 => r1 |> r1ToIOR0 |> flatMap(r0ToIOA) |> materializeError),
          ioR1,
        )
      }
    };

let rec unsafeRunAsync: (Belt.Result.t('a, 'e) => unit, t('a, 'e)) => unit =
  (onDone, ioA) =>
    switch (ioA) {
    | Pure(a) => onDone(Belt.Result.Ok(a))

    | Throw(e) => onDone(Belt.Result.Error(e))

    | Suspend(getA) => onDone(Belt.Result.Ok(getA()))

    | SuspendIO(getIOA) => getIOA() |> unsafeRunAsync(onDone)

    | Async(onDone') => onDone'(onDone)

    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => onDone(Belt.Result.Ok(r0ToA(r0)))

      | Throw(e) => onDone(Belt.Result.Error(e))

      | Suspend(getR0) => onDone(Belt.Result.Ok(r0ToA(getR0())))

      | SuspendIO(getIOR0) =>
        Map(r0ToA, getIOR0()) |> unsafeRunAsync(onDone)

      | Async(onDoneR0) =>
        onDoneR0(resultR0 => onDone(Relude_Result.map(r0ToA, resultR0)))

      | Map(r1ToR0, ioR1) =>
        Map(r1ToR0 >> r0ToA, ioR1) |> unsafeRunAsync(onDone)

      | FlatMap(r1ToIOR0, ioR1) =>
        FlatMap((r1 => r1ToIOR0(r1) |> map(r0ToA)), ioR1)
        |> unsafeRunAsync(onDone)
      }

    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone)

      | Throw(e) => onDone(Belt.Result.Error(e))

      | Suspend(getR0) => r0ToIOA(getR0()) |> unsafeRunAsync(onDone)

      | SuspendIO(getIOR0) =>
        getIOR0() |> flatMap(r0ToIOA) |> unsafeRunAsync(onDone)

      | Async(onDoneR0) =>
        onDoneR0(resultR0 =>
          switch (resultR0) {
          | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone)
          | Error(e) => onDone(Belt.Result.Error(e))
          }
        )

      | Map(r1ToR0, ioR1) =>
        flatMap(r1 => (r1ToR0 >> r0ToIOA)(r1), ioR1)
        |> unsafeRunAsync(onDone)

      | FlatMap(r1ToIOR0, ioR1) =>
        FlatMap((r1 => r1ToIOR0(r1) |> flatMap(r0ToIOA)), ioR1)
        |> unsafeRunAsync(onDone)
      }
    };

let delay: int => t(unit, 'e) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Belt.Result.Ok()), millis) |> ignore
    );

let delayWithVoid: int => t(unit, Relude_Void.t) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Belt.Result.Ok()), millis) |> ignore
    );

let withDelay: (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => delay(millis) |> flatMap(_ => io);

module type FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t);

module Functor: FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t('a, E.t);
    let map = map;
  };

module type APPLY_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLY with type t('a) = t('a, E.t);

module Apply: APPLY_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Functor(E);
    let apply = apply;
  };

module type APPLICATIVE_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, E.t);

module Applicative: APPLICATIVE_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Apply(E);
    let pure = pure;
  };

module type MONAD_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.MONAD with type t('a) = t('a, E.t);

module Monad: MONAD_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Applicative(E);
    let flat_map = bind;
  };

module Infix = {
  module Functor = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Functor((Functor(E)));
  module Apply = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Apply((Apply(E)));
  module Monad = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Monad((Monad(E)));
};
