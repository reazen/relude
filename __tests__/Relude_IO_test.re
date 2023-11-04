open Jest;
open Expect;
open! Relude.Globals;

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

  testAsync("pureWithVoid unsafeRunAsync", onDone =>
    IO.pureWithVoid(42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("throw unsafeRunAsync", onDone =>
    IO.throwWithVoid("this is a test")
    |> IO.unsafeRunAsync(
         fun
         | Ok(_a) => onDone(fail("Failed"))
         | Error(_err) => onDone(pass),
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

  testAsync("suspendWithVoid unsafeRunAsync", onDone =>
    IO.suspendWithVoid(() => 42)
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
    IO.suspendIO(() => IO.async(onDone => onDone(Ok(42))))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async Ok unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Ok(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async Error unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Error("it failed")))
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

  testAsync("pure map <$$> unsafeRunAsync", onDone => {
    let (<$$>) = IO.(<$$>);

    IO.pure(42)
    <$$> (a => a + 10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       );
  });

  testAsync("pure tap unsafeRunAsync", onDone => {
    let a = ref(0);

    IO.pure(42)
    |> IO.tap(b => a := b)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_value) => onDone(expect(a^) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       );
  });

  testAsync("pure tapError unsafeRunAsync", onDone => {
    let a = ref(0);

    IO.throw(42)
    |> IO.tapError(b => a := b)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(_) => onDone(expect(a^) |> toEqual(42)),
       );
  });

  testAsync("pure apply unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.apply(IO.pure(a => a * 2))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(84))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("align pure pure", onDone =>
    IO.align(IO.pure(42), IO.pure("a"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(ior) =>
           onDone(expect(ior) |> toEqual(Relude.Ior_Type.Both(42, "a")))
         | Error(_) => onDone(fail("Fail")),
       )
  );

  testAsync("align pure throw", onDone =>
    IO.align(IO.pure(42), IO.throw("e2"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(ior) =>
           onDone(expect(ior) |> toEqual(Relude.Ior_Type.This(42)))
         | Error(_) => onDone(fail("Fail")),
       )
  );

  testAsync("align throw pure", onDone =>
    IO.align(IO.throw("e1"), IO.pure(99))
    |> IO.unsafeRunAsync(
         fun
         | Ok(ior) =>
           onDone(expect(ior) |> toEqual(Relude.Ior_Type.That(99)))
         | Error(_) => onDone(fail("Fail")),
       )
  );

  testAsync("align throw throw", onDone =>
    IO.align(IO.throw("e1"), IO.throw("e2"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Fail"))
         | Error(e) => onDone(expect(e) |> toEqual("e1")),
       )
  );

  testAsync("alignWith pure pure", onDone => {
    let f =
      fun
      | Relude.Ior_Type.This(a) => a
      | Relude.Ior_Type.That(b) => int_of_string(b)
      | Relude.Ior_Type.Both(a, b) => a + int_of_string(b);
    IO.alignWith(f, IO.pure(42), IO.pure("99"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(v) => onDone(expect(v) |> toEqual(141))
         | Error(_) => onDone(fail("Fail")),
       );
  });

  testAsync("alignWith pure throw", onDone => {
    let f =
      fun
      | Relude.Ior_Type.This(a) => a
      | Relude.Ior_Type.That(b) => int_of_string(b)
      | Relude.Ior_Type.Both(a, b) => a + int_of_string(b);
    IO.alignWith(f, IO.pure(42), IO.throw("e2"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(v) => onDone(expect(v) |> toEqual(42))
         | Error(_) => onDone(fail("Fail")),
       );
  });

  testAsync("alignWith throw pure", onDone => {
    let f =
      fun
      | Relude.Ior_Type.This(a) => a
      | Relude.Ior_Type.That(b) => int_of_string(b)
      | Relude.Ior_Type.Both(a, b) => a + int_of_string(b);
    IO.alignWith(f, IO.throw("e1"), IO.pure("99"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(v) => onDone(expect(v) |> toEqual(99))
         | Error(_) => onDone(fail("Fail")),
       );
  });

  testAsync("alignWith throw throw", onDone => {
    let f =
      fun
      | Relude.Ior_Type.This(a) => a
      | Relude.Ior_Type.That(b) => int_of_string(b)
      | Relude.Ior_Type.Both(a, b) => a + int_of_string(b);
    IO.alignWith(f, IO.throw("e1"), IO.throw("e2"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Fail"))
         | Error(e) => onDone(expect(e) |> toEqual("e1")),
       );
  });

  testAsync("pure flatMap pure unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("throw flatMap unsafeRunAsync", onDone =>
    IO.throw(42)
    |> IO.flatMap(a => IO.pure(a + 1))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(value) => onDone(expect(value) |> toEqual(42)),
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

  testAsync("throw cond error unsafeRunAsync", onDone =>
    IO.pure("hel")
    |> IO.cond(a => a |> String.length == 5, "is five", "boom explosions")
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("fail"))
         | Error(a) => onDone(expect(a) |> toEqual("boom explosions")),
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

  testAsync("pure condError ok unsafeRunAsync", onDone =>
    IO.pure("hello")
    |> IO.condError(a => a |> String.length == 5, "string is too long")
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual("hello"))
         | Error(_) => onDone(fail("fail")),
       )
  );
});

describe("IO compose", () => {
  describe("pure", () => {
    let ioAToB = IO.pure(a => a ++ "1");

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.(ioBToC <<< ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("pure (andThen)", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.(ioAToB >>> ioBToC)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Result.ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Result.error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));
      let ioAToB = IO.pure(a => a ++ "1");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a("0")) |> toEqual("012"))
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("throw", () => {
    let ioAToB = IO.throw("error");

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(i => i + 42);

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error 2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), i) => i + 42);

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(i => i + 42));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Result.ok(i => i + 42)));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Result.error("error 2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((a, b) => a + b, IO.pure(1));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((a, b) => a + b), IO.pure(1));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(a => IO.pure(b => a + b), IO.pure(0));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => onDone(expect(e) |> toEqual("error")),
         );
    });
  });

  describe("suspend", () => {
    let ioAToB = IO.suspend(((), a) => a ++ "1");

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("suspendIO", () => {
    let ioAToB = IO.suspendIO(() => IO.pure(a => a ++ "1"));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("async (ok)", () => {
    let ioAToB = IO.async(onDone => onDone(Result.ok(a => a ++ "1")));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO (error)", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(_b => Result.error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) =>
             expect(a("0")) |> toEqual(Result.error("error")) |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("async (error)", () => {
    let ioAToB = IO.async(onDone => onDone(Result.error("error")));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });
  });

  describe("map", () => {
    let ioAToB = IO.map((one, a) => a ++ one, IO.pure("1"));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("apply", () => {
    let ioAToB = IO.apply(IO.pure((one, a) => a ++ one), IO.pure("1"));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });

  describe("flatMap", () => {
    let ioAToB = IO.flatMap(one => IO.pure(a => a ++ one), IO.pure("1"));

    testAsync("pure", onDone => {
      let ioBToC = IO.pure(b => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("throw", onDone => {
      let ioBToC = IO.throw("error");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("suspend", onDone => {
      let ioBToC = IO.suspend(((), b) => b ++ "2");

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("suspendIO", onDone => {
      let ioBToC = IO.suspendIO(() => IO.pure(b => b ++ "2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (ok)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Ok(b => b ++ "2")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("async (error)", onDone => {
      let ioBToC = IO.async(onDone => onDone(Error("error")));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("fail"))
           | Error(e) => expect(e) |> toEqual("error") |> onDone,
         );
    });

    testAsync("map", onDone => {
      let ioBToC = IO.map((two, b) => b ++ two, IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("apply", onDone => {
      let ioBToC = IO.apply(IO.pure((two, b) => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });

    testAsync("flatMap", onDone => {
      let ioBToC = IO.flatMap(two => IO.pure(b => b ++ two), IO.pure("2"));

      IO.compose(ioBToC, ioAToB)
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => expect(a("0")) |> toEqual("012") |> onDone
           | Error(_) => onDone(fail("fail")),
         );
    });
  });
});

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
    |> IO.mapError(msg => Relude.Js_Exn.make(msg))
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

  testAsync("async (ok) catchError unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.ok("0")))
    |> IO.catchError((e: string) => IO.throw(e ++ "1"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual("0"))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async (error) catchError unsafeRunAsync", onDone =>
    IO.async(onDone => onDone(Result.error("0")))
    |> IO.catchError((e: string) => IO.throw(e ++ "1"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual("01")),
       )
  );

  describe("map catchError unsafeRunAsync", () => {
    let r0ToA = a => a ++ "1";

    testAsync("pure", onDone =>
      IO.map(r0ToA, IO.pure("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("throw", onDone =>
      IO.map(r0ToA, IO.throw("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("suspend", onDone =>
      IO.map(r0ToA, IO.suspend(() => "0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.map(r0ToA, IO.suspendIO(() => IO.pure("0")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.map(r0ToA, IO.async(onDone => onDone(Result.ok("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (error)", onDone =>
      IO.map(r0ToA, IO.async(onDone => onDone(Result.error("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("map", onDone =>
      IO.map(r0ToA, IO.map(a => a ++ "0", IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("apply", onDone =>
      IO.map(r0ToA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("flatMap", onDone =>
      IO.map(r0ToA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );
  });

  describe("apply catchError unsafeRunAsync", () => {
    let ioR0ToA = IO.pure(a => a ++ "1");

    testAsync("pure", onDone =>
      IO.apply(ioR0ToA, IO.pure("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("throw", onDone =>
      IO.apply(ioR0ToA, IO.throw("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("suspend", onDone =>
      IO.apply(ioR0ToA, IO.suspend(() => "0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.apply(ioR0ToA, IO.suspendIO(() => IO.pure("0")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.apply(ioR0ToA, IO.async(onDone => onDone(Result.ok("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (error)", onDone =>
      IO.apply(ioR0ToA, IO.async(onDone => onDone(Result.error("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("map", onDone =>
      IO.apply(ioR0ToA, IO.map(a => a ++ "0", IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(ioR0ToA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("flatMap", onDone =>
      IO.apply(ioR0ToA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );
  });

  describe("flatMap catchError unsafeRunAsync", () => {
    let r0ToIOA = a => IO.pure(a ++ "1");

    testAsync("pure", onDone =>
      IO.flatMap(r0ToIOA, IO.pure("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("throw", onDone =>
      IO.flatMap(r0ToIOA, IO.throw("0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("suspend", onDone =>
      IO.flatMap(r0ToIOA, IO.suspend(() => "0"))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.flatMap(r0ToIOA, IO.suspendIO(() => IO.pure("0")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.flatMap(r0ToIOA, IO.async(onDone => onDone(Result.ok("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("async (error)", onDone =>
      IO.flatMap(r0ToIOA, IO.async(onDone => onDone(Result.error("0"))))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual("02")),
         )
    );

    testAsync("map", onDone =>
      IO.flatMap(r0ToIOA, IO.map(a => a ++ "0", IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("apply", onDone =>
      IO.flatMap(r0ToIOA, IO.apply(IO.pure(a => a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(r0ToIOA, IO.flatMap(a => IO.pure(a ++ "0"), IO.pure("+")))
      |> IO.catchError((e: string) => IO.throw(e ++ "2"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual("+01"))
           | Error(_) => onDone(fail("Failed")),
         )
    );
  });
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

describe("IO mapHandleError", () => {
  testAsync("pure mapHandleError unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.mapHandleError(a => a * 2, int_of_string)
    |> IO.unsafeRunAsync(
         fun
         | Ok(b) => onDone(expect(b) |> toEqual(84))
         | Error(_) => onDone(fail("Fail")),
       )
  );

  testAsync("pure mapHandleError unsafeRunAsync", onDone =>
    IO.throw("42")
    |> IO.mapHandleError(a => a * 2, int_of_string)
    |> IO.unsafeRunAsync(
         fun
         | Ok(b) => onDone(expect(b) |> toEqual(42))
         | Error(_) => onDone(fail("Fail")),
       )
  );
});

describe("IO bimap/bitap", () => {
  testAsync("suspend bimap bimap unsafeRunAsync", onDone =>
    IO.suspend(() => 42)
    |> IO.bimap(a => a * 2, e => e ++ e)
    |> IO.bimap(a => expect(a) |> toEqual(84), _ => fail("fail"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspend bitap (ok) unsafeRunAsync", onDone => {
    let a = ref(0);

    IO.pure(42)
    |> IO.bitap(b => a := b + 1, e => a := e - 1)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(expect(a^) |> toEqual(43))
         | Error(_) => onDone(fail("fail")),
       );
  });

  testAsync("suspend bitap (error) unsafeRunAsync", onDone => {
    let a = ref(0);

    IO.throw(42)
    |> IO.bitap(b => a := b + 1, e => a := e - 1)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("fail"))
         | Error(_) => onDone(expect(a^) |> toEqual(41)),
       );
  });
});

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
    IO.triesJS(() => Js.Exn.raiseError("Fail"))
    |> IO.unsafeRunAsync(result =>
         switch (result) {
         | Ok(_) => onDone(fail("Should not be Ok"))
         | Error(e) =>
           onDone(expect(Js.Exn.message(e)) |> toEqual(Some("Fail")))
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
         | Ok(_) => onDone(fail("Should not be Ok"))
         | Error(e) =>
           onDone(
             expect(Js.Exn.message(e)) |> toEqual(Some("This sucks")),
           ),
       );
  });

  // Something must have changed in how custom exceptions are raised from OCaml,
  // because the message inside the error appears to be `[Object object]`. For
  // now, we can just skip this test.
  Skip.testAsync("triesJS with exn", onDone => {
    exception MyExn(string);

    IO.triesJS(() => raise(MyExn("Custom error")))
    |> IO.unsafeRunAsync(result =>
         switch (result) {
         | Ok(_) => onDone(fail("Should not be Ok"))
         | Error(e) =>
           Js.log(e);
           onDone(
             expect(Js.Exn.message(e))
             |> toEqual(Some("Unexpected error: MyExn,8,Custom error")),
           );
         }
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

  describe("map flip unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.pure(42)
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("throw", onDone =>
      IO.throw(42)
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual(42))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspend", onDone =>
      IO.suspend(() => 42)
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(42))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.async(onDone => onDone(Result.ok(42)))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("async (error)", onDone =>
      IO.async(onDone => onDone(Result.error(42)))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual(42))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("map", onDone =>
      IO.map(b => b + 42, IO.pure(0))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(b => b + 42), IO.pure(0))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(b => IO.pure(b + 42), IO.pure(0))
      |> IO.map(a => a + 10)
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );
  });

  describe("apply flip unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.pure(42)
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("throw", onDone =>
      IO.throw(42)
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual(42))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspend", onDone =>
      IO.suspend(() => 42)
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(42))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.async(onDone => onDone(Result.ok(42)))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("async (error)", onDone =>
      IO.async(onDone => onDone(Result.error(42)))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual(42))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("map", onDone =>
      IO.map(b => b + 42, IO.pure(0))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(b => b + 42), IO.pure(0))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("map", onDone =>
      IO.map(b => b + 42, IO.pure(0))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(b => IO.pure(b + 42), IO.pure(0))
      |> IO.apply(IO.pure(a => a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );
  });

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

  describe("flatMap flip unsafeRunAsync", () => {
    testAsync("pure flatMap map", onDone =>
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

    testAsync("throw", onDone =>
      IO.throw(42)
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) => onDone(expect(a) |> toEqual(42))
           | Error(_) => onDone(fail("Failed")),
         )
    );

    testAsync("suspend", onDone =>
      IO.suspend(() => 42)
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(42))
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
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

    testAsync("map", onDone =>
      IO.map(a => a + 42, IO.pure(0))
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(a => a + 42), IO.pure(0))
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(a => IO.pure(a + 42), IO.pure(0))
      |> IO.flatMap(a => Pure(a + 10))
      |> IO.flip
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(fail("Failed"))
           | Error(e) => onDone(expect(e) |> toEqual(52)),
         )
    );
  });
});

describe("IO summonError", () => {
  testAsync("pure summonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.summonError
    |> IO.bimap(
         resA => expect(resA) |> toEqual(Ok(42)),
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
         resA => expect(resA) |> toEqual(Ok(42)),
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
         resA => expect(resA) |> toEqual(Ok(42)),
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
         resA => expect(resA) |> toEqual(Error("error!")),
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
         resA => expect(resA) |> toEqual(Ok(63)),
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
         resA => expect(resA) |> toEqual(Ok(42)),
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
         resA => expect(resA) |> toEqual(Error(42)),
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
    |> IO.bimap(res => expect(res) |> toEqual(Ok(42)), Relude.Void.absurd)
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  describe("map unsafeRunAsync", () => {
    testAsync("throw", onDone =>
      IO.map(a => a + 42, IO.throw(1))
      |> IO.summonError
      |> IO.unsafeRunAsync(
           fun
           | Ok(a) =>
             expect(a |> Result.getError) |> toEqual(Some(1)) |> onDone
           | Error(_) => fail("Failed") |> onDone,
         )
    );

    testAsync("suspend", onDone =>
      IO.map(a => a + 42, IO.suspend(() => 1))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(43)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.map(a => a + 42, IO.suspendIO(() => IO.pure(1)))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(43)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async", onDone =>
      IO.map(a => a + 42, IO.async(onDone => onDone(Result.ok(1))))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(43)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("map", onDone =>
      IO.map(a => a + 42, IO.map(b => b + 2, IO.pure(1)))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(45)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("apply", onDone =>
      IO.map(a => a + 42, IO.apply(IO.pure(b => b + 2), IO.pure(1)))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(45)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("flatMap", onDone =>
      IO.map(a => a + 42, IO.flatMap(b => IO.pure(b + 2), IO.pure(1)))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(45)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });

  describe("apply unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.pure(42)
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("throw", onDone => {
      let a = ref(None);

      IO.throw(2)
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.tap(b => a := b |> Result.getError)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(expect(a^) |> toEqual(Some(2)))
           | Error(_) => onDone(fail("Failed")),
         );
    });

    testAsync("suspend", onDone =>
      IO.suspend(() => 42)
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(42))
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.async(onDone => Ok(42) |> onDone)
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async (error)", onDone => {
      let a = ref(None);

      IO.async(onDone => Error(42) |> onDone)
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.tap(b => a := b |> Result.getError)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(expect(a^) |> toEqual(Some(42)))
           | Error(_) => onDone(fail("Failed")),
         );
    });

    testAsync("map", onDone =>
      IO.map(a => a + 1, IO.pure(41))
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(a => a + 1), IO.pure(41))
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(a => IO.pure(a + 1), IO.pure(41))
      |> IO.apply(IO.pure(a => a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });

  describe("flatMap unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.pure(42)
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("throw", onDone => {
      let a = ref(None);

      IO.throw(2)
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.tap(b => a := b |> Result.getError)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(expect(a^) |> toEqual(Some(2)))
           | Error(_) => onDone(fail("Failed")),
         );
    });

    testAsync("suspend", onDone =>
      IO.suspend(() => 42)
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(42))
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.async(onDone => Ok(42) |> onDone)
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async (error)", onDone => {
      let a = ref(None);

      IO.async(onDone => Error(42) |> onDone)
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.tap(b => a := b |> Result.getError)
      |> IO.unsafeRunAsync(
           fun
           | Ok(_) => onDone(expect(a^) |> toEqual(Some(42)))
           | Error(_) => onDone(fail("Failed")),
         );
    });

    testAsync("map", onDone =>
      IO.map(a => a + 1, IO.pure(41))
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(a => a + 1), IO.pure(41))
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(a => IO.pure(a + 1), IO.pure(41))
      |> IO.flatMap(a => IO.pure(a * 2))
      |> IO.summonError
      |> IO.bimap(
           res => expect(res) |> toEqual(Ok(84)),
           Relude.Void.absurd,
         )
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });
});

describe("IO unsummonError", () => {
  testAsync("pure Ok unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Ok(42))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(Error("e!"))
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
    IO.suspend(() => Ok(42))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspend Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspend(() => Error("e!"))
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
    IO.suspendIO(() => IO.pure(Ok(42)))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("suspendIO pure Error unsummonError bimap unsafeRunAsync", onDone =>
    IO.suspendIO(() => IO.pure(Error("e!")))
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

  describe("flatMap unsummonError unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.pure(0)
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspend", onDone =>
      IO.suspend(() => 0)
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.suspendIO(() => IO.pure(0))
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync(
      "async flatMap suspend unsummonError bimap unsafeRunAsync", onDone =>
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

    testAsync("map", onDone =>
      IO.map(a => a + 1, IO.pure(0))
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(43), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("apply", onDone =>
      IO.apply(IO.pure(a => a + 1), IO.pure(0))
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(43), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("flatMap", onDone =>
      IO.flatMap(a => IO.pure(a + 1), IO.pure(0))
      |> IO.flatMap(a => IO.pure(Ok(a + 42)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(43), _ => fail("fail"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });

  describe("map unsummonError bimap unsafeRunAsync", () => {
    testAsync("pure", onDone =>
      IO.map(a => Ok(a + 1), IO.pure(0))
      |> IO.unsummonError
      |> IO.bimap(_ => pass, _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspend", onDone =>
      IO.map(a => Ok(a + 1), IO.suspend(() => 0))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(1), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("suspendIO", onDone =>
      IO.map(a => Ok(a + 1), IO.suspendIO(() => IO.pure(0)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(1), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("async (ok)", onDone =>
      IO.map(a => Ok(a + 1), IO.async(onDone => onDone(Ok(0))))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(1), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("map", onDone =>
      IO.map(a => Ok(a + 1), IO.map(a => a + 1, IO.pure(0)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(2), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("apply", onDone =>
      IO.map(a => Ok(a + 1), IO.apply(IO.pure(a => a + 1), IO.pure(0)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(2), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );

    testAsync("flatMap", onDone =>
      IO.map(a => Ok(a + 1), IO.flatMap(a => IO.pure(a + 1), IO.pure(0)))
      |> IO.unsummonError
      |> IO.bimap(a => expect(a) |> toEqual(2), _ => fail("Failed"))
      |> IO.unsafeRunAsync(
           fun
           | Ok(assertion) => onDone(assertion)
           | Error(assertion) => onDone(assertion),
         )
    );
  });

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

  testAsync(
    "pure apply summonError apply unsummonError bimap unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.apply(IO.pure(a => a * 2))
    |> IO.summonError
    |> IO.apply(IO.pure(res => res |> Result.map(a => a * 3)))
    |> IO.unsummonError
    |> IO.bimap(a => expect(a) |> toEqual(252), _ => fail("Failed"))
    |> IO.unsafeRunAsync(res => res |> Result.merge |> onDone)
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

testAsync("IO delayWithVoid unsafeRunAsync", onDone => {
  IO.delayWithVoid(10)
  |> IO.unsafeRunAsync(
       fun
       | Ok(_) => onDone(pass)
       | Error(_) => onDone(fail("Failed")),
     );
  Jest.advanceTimersByTime(10);
});

testAsync("IO withDelayBefore unsafeRunAsync", onDone => {
  IO.pure(0)
  |> IO.withDelayBefore(10)
  |> IO.unsafeRunAsync(
       fun
       | Ok(_) => onDone(pass)
       | Error(_) => onDone(fail("Failed")),
     );
  Jest.advanceTimersByTime(10);
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
          Relude.Option.foldLazy(ignore, () =>
            "IO should not have been run" |> fail |> onDone
          ),
        ),
      );

    debouncedIO() |> checkNonRunIO;

    debouncedIO() |> checkNonRunIO;

    debouncedIO()
    |> IO.flatMap(Relude.Option.fold(IO.pure(None), debouncedIO))
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
    |> IO.flatMap(Relude.Option.fold(IO.pure(None), debouncedIO))
    |> IO.unsafeRunAsync(ignore);

    debouncedIO()
    |> IO.flatMap(Relude.Option.fold(IO.pure(None), debouncedIO))
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

module AppErrorType: BsBastet.Interface.TYPE with type t = appError = {
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

module JsExnType: BsBastet.Interface.TYPE with type t = Js.Exn.t = {
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
        Relude.String.toNonWhitespace(content)
        |> IO.fromOption(_ =>
             Relude.Js_Exn.make("Failed to get non-empty file content")
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
