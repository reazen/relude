module Function = Relude_Function;
module JsExn = Relude_JsExn;
module Option = Relude_Option;
module Result = Relude_Result;
module Void = Relude_Void;

let (<<) = Function.Infix.(<<);
let (>>) = Function.Infix.(>>);

type t('a, 'e) =
  | Pure('a): t('a, 'e)
  | Throw('e): t('a, 'e)
  | Suspend(unit => 'a): t('a, 'e)
  | SuspendIO(unit => t('a, 'e)): t('a, 'e)
  | Async((Result.t('a, 'e) => unit) => unit): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | FlatMap('r => t('a, 'e), t('r, 'e)): t('a, 'e);

let pure: 'a 'e. 'a => t('a, 'e) = a => Pure(a);

let pureWithVoid: 'a. 'a => t('a, Void.t) = a => Pure(a);

let unit: 'e. t(unit, 'e) = Pure();

let unitWithVoid: t(unit, Void.t) = Pure();

let throw: 'a 'e. 'e => t('a, 'e) = e => Throw(e);

let throwWithVoid: 'e. 'e => t(Void.t, 'e) = e => Throw(e);

let suspend: 'a 'e. (unit => 'a) => t('a, 'e) = getA => Suspend(getA);

let suspendWithVoid: 'a. (unit => 'a) => t('a, Void.t) =
  getA => Suspend(getA);

let suspendThrow: 'a 'e. (unit => 'e) => t('a, 'e) =
  getError => SuspendIO(() => Throw(getError()));

let suspendIO: 'a 'e. (unit => t('a, 'e)) => t('a, 'e) =
  getIO => SuspendIO(getIO);

let async: 'a 'e. ((Result.t('a, 'e) => unit) => unit) => t('a, 'e) =
  onDone => Async(onDone);

let fromOption: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, option) =>
    option |> Option.fold(() => throw(getError()), pure);

let fromResult: 'a 'e. Result.t('a, 'e) => t('a, 'e) =
  res => res |> Result.fold(pure, throw);

let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, io) => Map(f, io);

let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> map(a => {
         f(a);
         a;
       });

let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (rToIOA, ioR) => FlatMap(rToIOA, ioR);

let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ioA, aToIOB) => flatMap(aToIOB, ioA);

let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ioF, ioA) => ioF |> flatMap(f => ioA |> map(f));

let rec mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (e1ToE2, ioA) =>
    switch (ioA) {
    | Pure(a) => Pure(a)
    | Throw(e1) => Throw(e1ToE2(e1))
    | Suspend(getA) => Suspend(getA)
    | SuspendIO(getIOA) => SuspendIO((() => getIOA() |> mapError(e1ToE2)))
    | Async(onDone) =>
      Async((onDone' => onDone(Result.mapError(e1ToE2) >> onDone')))
    | Map(rToA, ioR) => Map(rToA, ioR |> mapError(e1ToE2))
    | FlatMap(rToIOA, ioR) =>
      FlatMap(
        (r => rToIOA(r) |> mapError(e1ToE2)),
        ioR |> mapError(e1ToE2),
      )
    };

let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, io) =>
    io
    |> mapError(e => {
         f(e);
         e;
       });

let rec bimap:
  'a 'b 'e1 'e2.
  ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2)
 =
  (aToB, e1ToE2, io) =>
    switch (io) {
    | Pure(a) => Pure(aToB(a))
    | Throw(e1) => Throw(e1ToE2(e1))
    | Suspend(getA) => Suspend((() => aToB(getA())))
    | SuspendIO(getIOA) =>
      SuspendIO((() => getIOA() |> bimap(aToB, e1ToE2)))
    | Async(onDone) =>
      Async((onDone' => onDone(Result.bimap(aToB, e1ToE2) >> onDone')))
    | Map(rToA, ioR) => Map(rToA >> aToB, ioR |> mapError(e1ToE2))
    | FlatMap(rToIOA, ioR) =>
      FlatMap(
        (r => rToIOA(r) |> bimap(aToB, e1ToE2)),
        ioR |> mapError(e1ToE2),
      )
    };

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

let catchError: 'a 'e. ('e => t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (eToIOA, ioA) =>
    switch (ioA) {
    | Throw(e) => eToIOA(e)
    | _ => ioA
    };

let tries: 'a. (unit => 'a) => t('a, exn) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | exn => Throw(exn)
        },
    );

let triesJS: 'a. (unit => 'a) => t('a, Js.Exn.t) =
  getA =>
    SuspendIO(
      () =>
        try (Pure(getA())) {
        | Js.Exn.Error(jsExn) => Throw(jsExn)
        | exn =>
          let jsExn = JsExn.fromExn(exn);
          Throw(jsExn);
        },
    );

let rec summonError: 'a 'e. t('a, 'e) => t(Result.t('a, 'e), Void.t) =
  ioA =>
    switch (ioA) {
    | Pure(a) => Pure(Result.ok(a))
    | Throw(e) => Pure(Result.error(e))
    | Suspend(getA) => Suspend((() => Result.ok(getA())))
    | SuspendIO(getIOA) => SuspendIO((() => getIOA() |> summonError))
    | Async(onDone) =>
      Async((onDoneMat => onDone(result => onDoneMat(Result.ok(result)))))
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => Pure(Result.ok(r0ToA(r0)))
      | Throw(e) => Pure(Result.error(e))
      | Suspend(getR0) => Suspend((() => Result.ok(r0ToA(getR0()))))
      | SuspendIO(getIOR0) =>
        SuspendIO((() => getIOR0() |> map(r0ToA) |> summonError))
      | Async(onDoneR0) =>
        Async(
          (
            onDoneMat =>
              onDoneR0(resR0 =>
                onDoneMat(Result.ok(Result.map(r0ToA, resR0)))
              )
          ),
        )
      | Map(r1ToR0, ioR1) => ioR1 |> map(r1ToR0 >> r0ToA) |> summonError
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1 |> flatMap(r1 => r1ToIOR0(r1) |> map(r0ToA)) |> summonError
      }

    | FlatMap(r0ToIOA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => r0ToIOA(r0) |> summonError
      | Throw(e) => Pure(Result.error(e))
      | Suspend(getR0) => SuspendIO((() => r0ToIOA(getR0()) |> summonError))
      | SuspendIO(getIOR0) =>
        SuspendIO((() => getIOR0() |> flatMap(r0ToIOA) |> summonError))
      | Async(onDoneR0) =>
        Async(
          (
            onDoneMat =>
              onDoneR0(
                fun
                | Error(e) => onDoneMat(Result.ok(Result.error(e)))
                | Ok(r0) =>
                  r0ToIOA(r0)
                  |> summonError
                  |> map(resA => onDoneMat(Result.ok(resA)))
                  |> ignore,
              )
          ),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1 |> flatMap(r1ToR0 >> r0ToIOA) |> summonError
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1 |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOA(r0))) |> summonError
      }
    };

let rec unsummonError: 'a 'e. t(Result.t('a, 'e), Void.t) => t('a, 'e) =
  fun
  | Pure(resA) => resA |> Result.fold(pure, throw)
  | Throw(absurd) => Void.absurd(absurd)
  | Suspend(getResA) =>
    SuspendIO((() => getResA() |> Result.fold(pure, throw)))
  | SuspendIO(getIOResA) => SuspendIO((() => getIOResA() |> unsummonError))
  | Async(onDoneResResA) =>
    Async(
      (
        onDoneResA =>
          onDoneResResA(resResA =>
            switch (resResA) {
            | Ok(resA) => onDoneResA(resA)
            | Error(absurd) => Void.absurd(absurd)
            }
          )
      ),
    )
  | Map(r0ToResA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0ToResA(r0) |> Result.fold(pure, throw)
    | Throw(absurd) => Void.absurd(absurd)
    | Suspend(getR0) =>
      SuspendIO((() => getR0() |> r0ToResA |> Result.fold(pure, throw)))
    | SuspendIO(getIOR0) =>
      SuspendIO((() => getIOR0() |> map(r0ToResA) |> unsummonError))
    | Async(onDoneR0) =>
      Async(
        (
          onDoneResA =>
            onDoneR0(
              fun
              | Ok(r0) => onDoneResA(r0ToResA(r0))
              | Error(absurd) => Void.absurd(absurd),
            )
        ),
      )
    | Map(r1ToR0, ioR1) => ioR1 |> map(r1ToR0 >> r0ToResA) |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1ToIOR0) |> map(r0ToResA) |> unsummonError
    }
  | FlatMap(r0ToIOResA, ioR0) =>
    switch (ioR0) {
    | Pure(r0) => r0ToIOResA(r0) |> unsummonError
    | Throw(absurd) => Void.absurd(absurd)
    | Suspend(getR0) =>
      SuspendIO((() => getR0() |> r0ToIOResA |> unsummonError))
    | SuspendIO(getIOR0) =>
      SuspendIO((() => getIOR0() |> flatMap(r0ToIOResA) |> unsummonError))
    | Async(onDoneR0) =>
      Async(
        (
          onDoneResA =>
            onDoneR0(
              fun
              | Ok(r0) => r0 |> r0ToIOResA |> map(onDoneResA) |> ignore
              | Error(absurd) => Void.absurd(absurd),
            )
        ),
      )
    | Map(r1ToR0, ioR1) =>
      ioR1 |> flatMap(r1ToR0 >> r0ToIOResA) |> unsummonError
    | FlatMap(r1ToIOR0, ioR1) =>
      ioR1 |> flatMap(r1 => r1ToIOR0(r1) |> flatMap(r0 => r0ToIOResA(r0))) |> unsummonError
    };

let rec unsafeRunAsync: 'a 'e. (Result.t('a, 'e) => unit, t('a, 'e)) => unit =
  (onDone, ioA) =>
    switch (ioA) {
    | Pure(a) => onDone(Result.ok(a))
    | Throw(e) => onDone(Result.error(e))
    | Suspend(getA) => onDone(Result.ok(getA()))
    | SuspendIO(getIOA) => getIOA() |> unsafeRunAsync(onDone)
    | Async(onDone') => onDone'(onDone)
    | Map(r0ToA, ioR0) =>
      switch (ioR0) {
      | Pure(r0) => onDone(Result.ok(r0ToA(r0)))
      | Throw(e) => onDone(Result.error(e))
      | Suspend(getR0) => onDone(Result.ok(r0ToA(getR0())))
      | SuspendIO(getIOR0) =>
        getIOR0()
        |> unsafeRunAsync(
             fun
             | Error(_) as resE => onDone(resE)
             | Ok(r0) => onDone(Result.ok(r0ToA(r0))),
           )
      | Async(onDoneR0) =>
        onDoneR0(
          fun
          | Error(_) as resE => onDone(resE)
          | Ok(r0) => onDone(Result.ok(r0ToA(r0))),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resE => onDone(resE)
             | Ok(r1) => onDone(Result.ok(r0ToA(r1ToR0(r1)))),
           )
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resE => onDone(resE)
             | Ok(r1) =>
               r1ToIOR0(r1)
               |> unsafeRunAsync(
                    fun
                    | Error(_) as resE => onDone(resE)
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
             | Error(_) as resE => onDone(resE)
             | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone),
           )
      | Async(onDoneR0) =>
        onDoneR0(
          fun
          | Error(_) as resE => onDone(resE)
          | Ok(r0) => r0ToIOA(r0) |> unsafeRunAsync(onDone),
        )
      | Map(r1ToR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resE => onDone(resE)
             | Ok(r1) => r1 |> r1ToR0 |> r0ToIOA |> unsafeRunAsync(onDone),
           )
      | FlatMap(r1ToIOR0, ioR1) =>
        ioR1
        |> unsafeRunAsync(
             fun
             | Error(_) as resE => onDone(resE)
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

let delay: 'e. int => t(unit, 'e) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Result.ok()), millis) |> ignore
    );

let delayWithVoid: int => t(unit, Void.t) =
  millis =>
    async(onDone =>
      Js.Global.setTimeout(_ => onDone(Result.ok()), millis) |> ignore
    );

let withDelay: 'a 'e. (int, t('a, 'e)) => t('a, 'e) =
  (millis, io) => delay(millis) |> flatMap(_ => io);

module type FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t);

module Functor: FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t('a, E.t);
    let map = map;
  };

module Bifunctor: BsAbstract.Interface.BIFUNCTOR = {
  type nonrec t('a, 'e) = t('a, 'e);
  let bimap = bimap;
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

module MonadThrow: Relude_MonadError.MONAD_THROW =
  (E: BsAbstract.Interface.TYPE) => {
    include Monad(E);
    let throwError = throw;
  };

module MonadError: Relude_MonadError.MONAD_ERROR =
  (E: BsAbstract.Interface.TYPE) => {
    include Monad(E);
    let throwError = throw;
    let catchError = catchError;
  };

module Infix = {
  module Functor = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Functor((Functor(E)));
  module Apply = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Apply((Apply(E)));
  module Monad = (E: BsAbstract.Interface.TYPE) =>
    BsAbstract.Infix.Monad((Monad(E)));
};
