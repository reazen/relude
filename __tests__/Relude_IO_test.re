open Jest;
open Expect;
open Relude.Globals;

let throwJSError: unit => int = [%bs.raw
  {| function() { throw new Error("Error from JS"); } |}
];

Jest.useFakeTimers(); // This applies to the whole file, so any tests that use delay must use the mock timer manipulation functions

describe("IO basics", () => {
  testAsync("pure unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspend unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO pure unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO suspend unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.suspend(() => 42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO suspendIO pure unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.suspendIO(() => IO.pure(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO async unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.async(onDone => onDone(Belt.Result.Ok(42))))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async Ok unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Belt.Result.Ok(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async Error unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Belt.Result.Error("it failed")))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(msg) => onDone(expect(msg) |> toEqual("it failed")),
       )
  );

  testAsync("pure map unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("pure flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync(
    "pure flatMap pure flatMap pure flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(72))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("pure flatMap suspend flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a =>
         IO.suspend(_ => a + 10) |> IO.flatMap(b => IO.pure(b * 2))
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(104))
         | Error(_) => onDone(fail("Failed")),
       )
  );
});

describe("IO fromOption", () => {
  testAsync("fromOption Some unsafeRunAsync", onDone =>
    IO.fromOption(() => "Failed", Some(32))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(32))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("fromOption None unsafeRunAsync", onDone =>
    IO.fromOption(() => "Messed up", None)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(error) => onDone(expect(error) |> toEqual("Messed up")),
       )
  );
});

describe("IO fromResult", () => {
  testAsync("fromResult Ok unsafeRunAsync", onDone =>
    IO.fromResult(Ok(32))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(32))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("fromResult Error unsafeRunAsync", onDone =>
    IO.fromResult(Error("Messed up"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(error) => onDone(expect(error) |> toEqual("Messed up")),
       )
  );
});

describe("IO cond", () => {
  testAsync("pure cond map unsafeRunAsync", onDone =>
    IO.pure("hello")
    |> IO.cond(a => a |> String.length == 5, "is five", "boom explosions")
    |> IO.map(a => expect(a) |> toEqual("is five"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );

  testAsync("pure condError mapError unsafeRunAsync", onDone =>
    IO.pure("hello world")
    |> IO.condError(a => a |> String.length == 5, "string is too long")
    |> IO.mapError(a => expect(a) |> toEqual("string is too long"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("fail"))
         | Error(assertion) => onDone(assertion),
       )
  );
});

describe("IO compose", () =>
  testAsync("compose pure pure", onDone =>
    IO.(IO.pure(int_of_string) >>> IO.pure(i => i > 0) <*> IO.pure("42"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(true))
         | Error(_) => onDone(fail("fail")),
       )
  )
);

describe("IO mapError", () => {
  testAsync("pure mapError unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.mapError(_ => "error")
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("throw mapError unsafeRunAsync", onDone =>
    IO.throw("this is a test")
    |> IO.mapError(msg => Relude_Js_Exn.make(msg))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_a) => onDone(fail("Failed"))
         | Error(_err) => onDone(pass),
       )
  );
});

describe("IO catchError", () => {
  testAsync("pure catchError unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.catchError((e: string) => IO.throw(e ++ e))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("throw catchError unsafeRunAsync", onDone =>
    IO.throw("42")
    |> IO.catchError((e: string) => {
         let intValue =
           Relude.Int.fromString(e) |> Relude.Option.getOrElse(0);
         IO.throw(intValue * 2);
       })
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(v) => onDone(expect(v) |> toEqual(84)),
       )
  );

  testAsync("pure flatMap catchError unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.throw(string_of_int(a)))
    |> IO.catchError(_ => IO.pure(55))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(55))
         | Error(_) => onDone(fail("Fail")),
       )
  );
});

describe("IO handleError", () =>
  testAsync("throw handleError unsafeRunAsync", onDone =>
    IO.throw("42")
    |> IO.handleError(e => int_of_string(e))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Fail")),
       )
  )
);

describe("IO bimap/bitap", () =>
  testAsync("suspend bimap bimap unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    |> IO.bimap(a => a * 2, e => e ++ e)
    |> IO.bimap(a => expect(a) |> toEqual(84), _ => fail("fail"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  )
);

describe("IO alt", () => {
  testAsync("alt success success", onDone => {
    let a = ref(false);
    let b = ref(false);

    IO.alt(IO.suspend(() => a := true), IO.suspend(() => b := true))
    |> IO.bimap(
         _ => expect((a^, b^)) |> toEqual((true, false)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       );
  });

  testAsync("alt fail success", onDone => {
    let a = ref(false);
    let b = ref(false);

    IO.alt(
      IO.suspendIO(() => {
        a := true;
        IO.throw("Failed!");
      }),
      IO.suspend(() => b := true),
    )
    |> IO.bimap(
         _ => expect((a^, b^)) |> toEqual((true, true)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       );
  });

  testAsync("orElse success success", onDone => {
    let a = ref(false);
    let b = ref(false);
    IO.suspend(() => a := true)
    |> IO.orElse(~fallback=IO.suspend(() => b := true))
    |> IO.bimap(
         _ => expect((a^, b^)) |> toEqual((true, false)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       );
  });

  testAsync("orElse fail success", onDone => {
    let a = ref(false);
    let b = ref(false);
    IO.suspendIO(() => {
      a := true;
      IO.throw("Failed!");
    })
    |> IO.orElse(~fallback=IO.suspend(() => b := true))
    |> IO.bimap(
         _ => expect((a^, b^)) |> toEqual((true, true)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       );
  });

  testAsync("<|> alt operator success success", onDone => {
    let a = ref(false);
    let b = ref(false);
    module IOE =
      IO.WithError({
        type t = string;
      });
    IOE.Infix.(
      IO.suspend(() => a := true)
      <|> IO.suspend(() => b := true)
      |> IO.bimap(
           _ => expect((a^, b^)) |> toEqual((true, false)),
           _ => fail("Failed"),
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });

  testAsync("<|> alt operator fail success", onDone => {
    let a = ref(false);
    let b = ref(false);
    module IOE =
      IO.WithError({
        type t = string;
      });
    IOE.Infix.(
      IO.suspendIO(() => {
        a := true;
        IO.throw("Darn");
      })
      <|> IO.suspend(() => b := true)
      |> IO.bimap(
           _ => expect((a^, b^)) |> toEqual((true, true)),
           _ => fail("Failed"),
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });
});

describe("IO tries/exceptions", () => {
  testAsync("tries unsafeRunAsync", onDone =>
    IO.tries(throwJSError)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Should not be Ok"))
         | Error(Js.Exn.Error(jsExn)) => {
             let msg = Js.Exn.message(jsExn);
             onDone(expect(msg) |> toEqual(Some("Error from JS")));
           }
         | Error(_) => onDone(fail("Should have been an Js.Exn")),
       )
  );

  testAsync("triesJS unsafeRunAsync", onDone =>
    IO.triesJS(throwJSError)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Should not be Ok"))
         | Error(jsExn) => {
             let msg = Js.Exn.message(jsExn);
             onDone(expect(msg) |> toEqual(Some("Error from JS")));
           },
       )
  );

  testAsync("triesJS with Reason Js.Exn.raiseError", onDone =>
    IO.triesJS(() => Js.Exn.raiseError("Crap the pants"))
    |> IO.unsafeRunAsync(result =>
         switch (result) {
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) =>
           onDone(
             expect(Js.Exn.message(e)) |> toEqual(Some("Crap the pants")),
           )
         }
       )
  );

  testAsync("triesJS with raw JS function that throws", onDone => {
    let jsThrow = [%raw
      {|
      function() {
        throw new Error("This sucks");
      }
    |}
    ];
    IO.triesJS(() => jsThrow(.))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) =>
           onDone(
             expect(Js.Exn.message(e)) |> toEqual(Some("This sucks")),
           ),
       );
  });
});

describe("IO flip", () => {
  testAsync("pure flip unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("throw flip unsafeRunAsync", onDone =>
    IO.throw("my error")
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual("my error"))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspend flip unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("suspendIO flip unsafeRunAsync", onDone =>
    IO.suspendIO(() => Pure(42))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("async flip unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Ok(42)))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("pure map flip unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(52)),
       )
  );

  testAsync("pure flatMap flip unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => Pure(a + 10))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(52)),
       )
  );

  testAsync("pure flatMap flatMap map flatMap flip unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => Pure(a + 10))
    |> IO.flatMap(a => Pure(a + 100))
    |> IO.map(a => a + 1000)
    |> IO.flatMap(a => IO.async(onDone => onDone(Result.ok(a + 10000))))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(11152)),
       )
  );

  testAsync("async flatMap suspend flip bimap unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok(42)))
    |> IO.flatMap(a => IO.suspend(() => a))
    |> IO.flip
    |> IO.bimap(_ => fail("fail"), e => expect(e) |> toEqual(42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );
});

describe("IO summonError", () => {
  testAsync("pure summonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Ok(42)),
         _e => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspend summonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Ok(42)),
         _e => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspendIO pure summonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(42))
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Ok(42)),
         _e => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspendIO throw summonError unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.throw("error!"))
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Error("error!")),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync(
    "suspendIO pure map flatMap pure summonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() =>
      IO.pure(42) |> IO.map(a => a + 10) |> IO.flatMap(a => IO.pure(a + 11))
    )
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Ok(63)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("async flatMap summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => 42 |> Result.ok |> onDone)
    |> IO.flatMap(IO.pure)
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Ok(42)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("async flatMap throw summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => 42 |> Result.ok |> onDone)
    |> IO.flatMap(IO.throw)
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Belt.Result.Error(42)),
         _ => fail("Failed"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("async flatMap suspend summonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok(42)))
    |> IO.flatMap(a => IO.suspend(() => a))
    |> IO.summonError
    |> IO.bimap(
         res => expect(res) |> toEqual(Belt.Result.Ok(42)),
         Relude.Void.absurd,
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );
});

describe("IO unsummonError", () => {
  testAsync("pure Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Belt.Result.Ok(42))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Belt.Result.Error("e!"))
    |> IO.unsummonError
    |> IO.bimap(
         _ => fail("Failed"),
         error => expect(error) |> toEqual("e!"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspend Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => Belt.Result.Ok(42))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspend Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => Belt.Result.Error("e!"))
    |> IO.unsummonError
    |> IO.bimap(
         _ => fail("Failed"),
         error => expect(error) |> toEqual("e!"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspendIO pure Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(Belt.Result.Ok(42)))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspendIO pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(Belt.Result.Error("e!")))
    |> IO.unsummonError
    |> IO.bimap(
         _ => fail("Failed"),
         error => expect(error) |> toEqual("e!"),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("async flatMap suspend unsummonError bimap unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok(Result.ok(42))))
    |> IO.flatMap(a => IO.suspend(() => a))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("fail"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync(
    "pure map flatMap pure summonError unsummonError bimap unsafeRunAsync",
    onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.flatMap(a => IO.pure(a + 11))
    |> IO.summonError
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(63), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );
});

describe("IO delay", () => {
  testAsync("delay unsafeRunAsync", onDone => {
    IO.delay(10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("Failed")),
       );
    Jest.advanceTimersByTime(10);
  });

  testAsync("pure withDelay unsafeRunAsync", onDone => {
    IO.pure(42)
    |> IO.withDelay(10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("fail")),
       );
    Jest.advanceTimersByTime(10);
  });
});

describe("IO debounce", () => {
  // TODO: need to use fake timers
  Skip.testAsync("debounce", onDone => {
    // This will test that when a debounced IO is called, it will only let the most recent one go through
    // after some predetermined amount of time. After that call has gone through the time should reset and
    // the next time the function is called it will have to wait that amount of time all over again.
    let getTimestamp = () => Js.Date.make() |> Js.Date.getTime;
    let timeIntervals = ref([getTimestamp()]);
    let intervalMs = 100;
    let areTimestampsSpacedCorrectly = (x1, x2) =>
      x2 -. x1 >= (intervalMs |> float_of_int);
    let debouncedIO =
      IO.debounce(
        ~intervalMs,
        () => {
          timeIntervals := [getTimestamp(), ...timeIntervals^];
          IO.pure();
        },
      );

    let checkNonRunIO =
      IO.unsafeRunAsync(
        Result.fold(
          _ => "IO should not have failed" |> fail |> onDone,
          Relude_Option.foldLazy(ignore, () =>
            "IO should not have been run" |> fail |> onDone
          ),
        ),
      );

    debouncedIO() |> checkNonRunIO;

    debouncedIO() |> checkNonRunIO;

    debouncedIO()
    |> IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO))
    |> checkNonRunIO;

    debouncedIO()
    |> IO.flatMap(ignore >> debouncedIO)
    |> IO.flatMap(_ => IO.delay(300))
    |> IO.unsafeRunAsync(_ =>
         (
           switch (timeIntervals^) {
           | [x2, x1, x0]
               when
                 areTimestampsSpacedCorrectly(x0, x1)
                 && areTimestampsSpacedCorrectly(x1, x2) => pass
           | [_, _, _] =>
             fail("debounced IO did not time executions correctly")
           | [_]
           | [_, _] => fail("debounced IO was executed too few times")
           | xs =>
             fail(
               "debounced IO was executed too many times. Was executed "
               ++ ((xs |> List.length) - 1 |> string_of_int),
             )
           }
         )
         |> onDone
       );
  });

  // TODO: need to use fake timers
  Skip.testAsync("debounce immediate", onDone => {
    // This tests a debounced IO that has an execution on the leading edge of the timing interval. It will
    // test that the first execution immediately goes out and that only the latest execution that happens
    // within the time interval goes out. After that time interval is up, the next time the IO is executed it
    // should go out immediately
    let getTimestamp = () => Js.Date.make() |> Js.Date.getTime;
    let timeIntervals = ref([getTimestamp()]);
    let intervalMs = 100;
    let areTimestampsSpacedCorrectly = (x1, x2) =>
      x2 -. x1 >= (intervalMs |> float_of_int);

    let debouncedIO =
      IO.debounce(
        ~immediate=true,
        ~intervalMs,
        () => {
          timeIntervals := [getTimestamp(), ...timeIntervals^];
          IO.pure();
        },
      );

    debouncedIO() |> IO.unsafeRunAsync(ignore);

    debouncedIO() |> IO.unsafeRunAsync(ignore);

    debouncedIO()
    |> IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO))
    |> IO.unsafeRunAsync(ignore);

    debouncedIO()
    |> IO.flatMap(Relude_Option.fold(IO.pure(None), debouncedIO))
    |> IO.flatMap(_ => IO.delay(300))
    |> IO.unsafeRunAsync(_ =>
         (
           switch (timeIntervals^) {
           | [x3, x2, x1, x0]
               when
                 Float.approximatelyEqual(~tolerance=2.0, x1, x0)
                 && areTimestampsSpacedCorrectly(x1, x2)
                 && Float.approximatelyEqual(~tolerance=2.0, x2, x3) => pass
           | [_, _, _, _] =>
             fail("debounced IO did not time executions correctly")
           | [_]
           | [_, _] => fail("debounced IO was executions too few times")
           | xs =>
             fail(
               "debounced IO was executions too many times. Was executed "
               ++ ((xs |> List.length) - 1 |> string_of_int),
             )
           }
         )
         |> onDone
       );
  });
});

describe("IO throttle", () => {
  // TODO: need to use fake timers
  Skip.testAsync("throttle", onDone => {
    let getTimestamp = () => Js.Date.make() |> Js.Date.getTime;
    let timeIntervals = ref([getTimestamp()]);
    let intervalMs = 100;
    let throttledIO =
      IO.throttle(
        ~intervalMs,
        () => {
          timeIntervals := [getTimestamp(), ...timeIntervals^];
          IO.pure();
        },
      );

    throttledIO() |> IO.unsafeRunAsync(ignore);

    throttledIO() |> IO.unsafeRunAsync(ignore);

    throttledIO()
    |> IO.flatMap(ignore >> throttledIO)
    |> IO.unsafeRunAsync(ignore);

    IO.delay(300)
    |> IO.flatMap(throttledIO)
    |> IO.unsafeRunAsync(_ =>
         (
           switch (timeIntervals^) {
           | [x2, x1, x0]
               when
                 x2
                 -. x1 >= (intervalMs |> float_of_int)
                 && Float.approximatelyEqual(~tolerance=2.0, x1, x0) => pass
           | [_, _, _] =>
             fail("throttled IO did not time executions correctly")
           | [_]
           | [_, _] => fail("throttled IO was executed too few times")
           | xs =>
             fail(
               "throttled IO was executed too many times. Was executed "
               ++ ((xs |> List.length) - 1 |> string_of_int),
             )
           }
         )
         |> onDone
       );
  });

  testAsync("all", onDone => {
    module IOE =
      IO.WithError({
        type t = string;
      });
    [IO.pure(1), IO.pure(2), IO.pure(3)]
    |> IOE.all
    |> IO.bimap(a => expect(a) |> toEqual([1, 2, 3]), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       );
  });
});

describe("IO parallel", () =>
  testAsync("parallel", onDone => {
    module IOE =
      IO.WithError({
        type t = string;
      });

    let a = ref(false);
    let b = ref(false);
    let c = ref(false);

    let ioA =
      IO.suspend(() => {
        a := true;
        (a^, b^, c^);
      })
      |> IO.withDelay(100)
      |> IO.map(t => {
           a := false;
           (t, (a^, b^, c^));
         });

    let ioB =
      IO.suspend(() => {
        b := true;
        (a^, b^, c^);
      })
      |> IO.withDelay(100)
      |> IO.map(t => {
           b := false;
           (t, (a^, b^, c^));
         });

    let ioC =
      IO.suspend(() => {
        c := true;
        (a^, b^, c^);
      })
      |> IO.withDelay(100)
      |> IO.map(t => {
           c := false;
           (t, (a^, b^, c^));
         });

    let a0 = (a^, b^, c^);

    let ioAll =
      (ioA, ioB, ioC)
      |> IOE.mapTuple3(((a1, a2), (b1, b2), (c1, c2)) =>
           expect((a0, a1, a2, b1, b2, c1, c2))
           |> toEqual((
                (false, false, false), // before starting, none are running
                (true, false, false), // when a is run, a sees itself running, but not yet b and c
                (false, true, true), // after delay, a completes, but sees b and c still running (they have not completed yet)
                (true, true, false), // when b is run, b sees both a and b running, but not yet c
                (false, false, true), // after delay, b completes, but sees c still running (it has not completed yet)
                (true, true, true), // when c is run c sees all a, b, and c running
                (false, false, false) // after delay c completes and sees a, b, c not running
              ))
         );

    ioAll
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("Failed")),
       );

    // We should only need to advance by a total of 100ms to get all the IOs to
    // start and complete (if running in parallel)
    Jest.advanceTimersByTime(50);
    Jest.advanceTimersByTime(100);
  })
);


type getError =
  | GetError(string);

type parseError =
  | ParseError(string);

type printError =
  | PrintError(string);

type appError =
  | EGet(getError)
  | EParse(parseError)
  | EPrint(printError);

let eGet = e => EGet(e);
let eParse = e => EParse(e);
let ePrint = e => EPrint(e);

module AppErrorType: BsAbstract.Interface.TYPE with type t = appError = {
  type t = appError;
};
module IOAppError = IO.WithError(AppErrorType);

let (>>=) = IOAppError.Infix.(>>=);
let (>=>) = IOAppError.Infix.(>=>);

let getData: IO.t(string, getError) = IO.suspendIO(() => IO.pure("data"));

let parseData: string => IO.t(int, parseError) =
  data => {
    let l = Relude.String.length(data);
    if (l > 0) {
      IO.pure(l);
    } else {
      IO.throw(ParseError("Bad data: " ++ data));
    };
  };

let printNumber: int => IO.t(unit, printError) = _num => IO.unit;

describe("IO realish examples", () => {
  testAsync("example >>=", onDone =>
    getData
    |> IO.mapError(eGet)
    >>= (parseData >> IO.mapError(eParse))
    >>= (printNumber >> IO.mapError(ePrint))
    >>= (_ => IO.pure(pass))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("example >=>", onDone => {
    let getIO =
      (_ => getData |> IO.mapError(eGet))
      >=> (parseData >> IO.mapError(eParse))
      >=> (printNumber >> IO.mapError(ePrint))
      >=> (_ => IO.pure(pass));

    getIO()
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("Failed")),
       );
  });

  testAsync("example flatMap", onDone =>
    getData
    |> IO.mapError(e => EGet(e))
    |> IO.flatMap(str => parseData(str) |> IO.mapError(e => EParse(e)))
    |> IO.flatMap(num => printNumber(num) |> IO.mapError(e => EPrint(e)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("Failed")),
       )
  );
});

let testFilePath = FS.testFilePath("Eff_test.txt");

module JsExnType: BsAbstract.Interface.TYPE with type t = Js.Exn.t = {
  type t = Js.Exn.t;
};
module IOJsExn = IO.WithError(JsExnType);

let (>>=) = IOJsExn.Infix.(>>=);
let (>=>) = IOJsExn.Infix.(>=>);

describe("IO FS examples", () => {
  beforeAll(() =>
    FS.IO.writeFileSync(testFilePath, "") |> IO.unsafeRunAsync(ignore)
  );

  testAsync("read and writeFileSync", onDone =>
    FS.IO.writeFileSync(testFilePath, "IO Eff test")
    >>= (_ => FS.IO.readFileSync(testFilePath))
    >>= (content => IO.pure(expect(content) |> toEqual("IO Eff test")))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_jsExn) => onDone(fail("Failed")),
       )
  );

  testAsync("readFile", onDone =>
    FS.IO.writeFile(testFilePath, "IO Aff test")
    >>= (_ => FS.IO.readFile(testFilePath))
    >>= (
      content =>
        Relude_String.toNonWhitespace(content)
        |> IO.fromOption(_ =>
             Relude_Js_Exn.make("Failed to get non-empty file content")
           )
    )
    >>= (content => IO.pure(expect(content) |> toEqual("IO Aff test")))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("Failed")),
       )
  );
});