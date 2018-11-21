open Jest;
open Expect;

module IO = Relude_IO;

let throwJSError: unit => int = [%bs.raw
  {| function() { throw new Error("Error from JS"); } |}
];

describe("IO", () => {
  testAsync("pure", onDone =>
    IO.pure(42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspend", onDone =>
    IO.suspend(() => 42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO - pure", onDone =>
    IO.suspendIO(() => IO.pure(42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO - suspend", onDone =>
    IO.suspendIO(() => IO.suspend(() => 42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO - suspendIO - pure", onDone =>
    IO.suspendIO(() => IO.suspendIO(() => IO.pure(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("suspendIO - async", onDone =>
    IO.suspendIO(() => IO.async(onDone => onDone(Belt.Result.Ok(42))))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async - synchronous Ok", onDone =>
    IO.async(onDone => onDone(Belt.Result.Ok(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("async - synchronous Error", onDone =>
    IO.async(onDone => onDone(Belt.Result.Error("it failed")))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(msg) => onDone(expect(msg) |> toEqual("it failed")),
       )
  );

  testAsync("pure - map", onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("pure - flatMap pure", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("pure - flatMap pure - flatMap pure - flatMap pure", onDone =>
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

  testAsync("pure - flatMap supend - flatMap pure", onDone =>
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

  testAsync("pure - mapError", onDone =>
    IO.pure(42)
    |> IO.mapError(_ => "error")
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("tries", onDone =>
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

  testAsync("triesJS", onDone =>
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

  testAsync("delay", onDone =>
    IO.delay(10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("Failed")),
       )
  );
});

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

module AppErrorType: BsAbstract.Interface.TYPE = {
  type t = appError;
};
module IOMonad = IO.Monad(AppErrorType);
module IOInfix = IO.Infix.Monad(AppErrorType);

let (>>=) = IOInfix.(>>=);
let (>=>) = IOInfix.(>=>);

let getData: unit => IO.t(string, getError) =
  () => {
    Js.Console.log("getData");
    /*
     IO.delay(10)
     |> IO.map(_ => "data")
     */
    IO.suspend(_ => "data");
  };

let parseData: string => IO.t(int, parseError) =
  data => {
    Js.Console.log("parseData");
    let l = Relude.String.length(data);
    if (l > 0) {
      IO.pure(l);
    } else {
      IO.throw(ParseError("Bad data: " ++ data));
    };
  };

let printNumber: int => IO.t(unit, printError) =
  num => {
    Js.Console.log("printNumber");
    Relude.Js.Console.IO.log(string_of_int(num));
  };

describe("examples", () => {
  testAsync("example >>=", onDone => onDone(pass));

  /*
  testAsync("example flatMap", onDone => {
    Js.Console.log("example flatMap");
    let io = getData() |> IO.mapError(e => EGet(e));
    /*
     |> IO.flatMap(str => parseData(str) |> IO.mapError(e => EParse(e)))
     |> IO.flatMap(num => printNumber(num) |> IO.mapError(e => EPrint(e)))
     */

    Js.Console.log("example flatMap: created io, running it now");

    io
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) =>
           onDone(
             {
               Js.Console.log("pass");
               pass;
             },
           )
         | Error(_) =>
           onDone(
             {
               Js.Console.log("failed");
               fail("Failed");
             },
           ),
       );
  });
  */
});
