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

module RIO = Relude_RIO.WithErrorAndEnv(
  {
    type t = error;
  },
  {
    type t = env;
  }
);


let ((<$>), (<$$>), (>>=)) = RIO.Infix.((<$>), (<$$>), (>>=));

describe("Reader IO", () =>
  testAsync("test flow", onDone =>
    RIO.ask
    >>= (
      env => {
        RIO.pure((-1) * env.intValue)
        <$$> string_of_int
        <$$> (a => a ++ env.stringValue);
      }
    )
    |> RIO.semiflatMap(c => Relude_IO.pure(c ++ "semi"))
    |> RIO.runRIO(testEnv)
    |> Relude_IO.map(a => expect(a) |> toEqual("-42abcsemi"))
    |> Relude_IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("fail")),
       )
  )
);
