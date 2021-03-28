open Jest;
open Expect;

type env = {
  intValue: int,
  stringValue: string,
};

let testEnv: env = {intValue: 42, stringValue: "abc"};

type error = {message: string};

module ReaderT = Relude.ReaderT;

module Reader =
  Relude.Reader.WithEnv({
    type t = env;
  });

describe("Reader", () => {
  test("make", () =>
    expect(Reader.make(r => r.intValue * 2) |> Reader.runReaderT(testEnv))
    |> toEqual(84)
  );

  test("ask", () =>
    expect(
      Reader.ask |> Reader.map(a => a.intValue) |> Reader.runReaderT(testEnv),
    )
    |> toEqual(42)
  );

  test("asks", () =>
    expect(
      Reader.asks(r => r.intValue * 2)
      |> Reader.map(a => a * 10)
      |> Reader.runReaderT(testEnv),
    )
    |> toEqual(840)
  );

  test("local", () =>
    expect(
      Reader.local(
        r => {intValue: r.intValue * 2, stringValue: r.stringValue ++ "!"},
        Reader.ask
        |> Reader.map(a => string_of_int(a.intValue) ++ a.stringValue),
      )
      |> Reader.runReaderT(testEnv),
    )
    |> toEqual("84abc!")
  );

  test("map", () =>
    expect(
      Reader.pure(42) |> Reader.map(a => a * 2) |> Reader.runReaderT(testEnv),
    )
    |> toEqual(84)
  );

  test("apply", () =>
    expect(
      Reader.pure(42)
      |> Reader.apply(Reader.make((r, a) => a * r.intValue * 2))
      |> Reader.runReaderT(testEnv),
    )
    |> toEqual(3528)
  );

  test("pure", () =>
    expect(Reader.pure(42) |> Reader.runReaderT(testEnv)) |> toEqual(42)
  );

  test("flatMap", () =>
    expect(
      Reader.pure(42)
      |> Reader.flatMap(a => Reader.make(r => r.intValue * a))
      |> Reader.runReaderT(testEnv),
    )
    |> toEqual(42 * 42)
  );
});

module IO = Relude.IO;

module IOE =
  IO.WithError({
    type t = error;
  });

module ReaderIOE =
  ReaderT.WithMonadAndEnv(
    IOE.Monad,
    {
      type t = env;
    },
  );

let ((<$>), (<$$>), (>>=)) = ReaderIOE.Infix.((<$>), (<$$>), (>>=));

describe("Reader IO", () =>
  testAsync("test flow", onDone =>
    ReaderIOE.ask
    >>= (
      env => {
        ReaderIOE.pure((-1) * env.intValue)
        <$$> string_of_int
        <$$> (a => a ++ env.stringValue);
      }
    )
    |> ReaderIOE.semiflatMap(c => IOE.pure(c ++ "semi"))
    |> ReaderIOE.runReaderT(testEnv)
    |> IOE.map(a => expect(a) |> toEqual("-42abcsemi"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("fail")),
       )
  )
);
