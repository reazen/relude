open Jest;
open Expect;

module IO = Relude_IO;
module Result = Relude_Result;

/* Test helpers */
let throwJSError: unit => int = [%bs.raw
  {| function() { throw new Error("Error from JS"); } |}
];

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
let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);

let getData: IO.t(string, getError) =
  IO.suspendIO(() => IO.delay(2) |> IO.map(_ => "data"));

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

describe("IO", () => {
  testAsync("flip pure", onDone =>
    IO.pure(42)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("flip throw", onDone =>
    IO.throw("my error")
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual("my error"))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("flip suspend", onDone =>
    IO.suspend(() => 42)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("flip suspendIO", onDone =>
    IO.suspendIO(() => Pure(42))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("flip async", onDone =>
    IO.async(onDone => onDone(Ok(42)))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(42)),
       )
  );

  testAsync("flip map", onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(52)),
       )
  );

  testAsync("flip flatMap", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => Pure(a + 10))
    |> IO.flip
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(e) => onDone(expect(e) |> toEqual(52)),
       )
  );

  testAsync("flip multi", onDone =>
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

  testAsync("catchError success", onDone =>
    IO.pure(42)
    |> IO.catchError((e: string) => IO.throw(e ++ e))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("catchError failure", onDone =>
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

  testAsync("unsafeRunAsync pure", onDone =>
    IO.pure(42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync suspend", onDone =>
    IO.suspend(() => 42)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync suspendIO - pure", onDone =>
    IO.suspendIO(() => IO.pure(42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync suspendIO - suspend", onDone =>
    IO.suspendIO(() => IO.suspend(() => 42))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync suspendIO - suspendIO - pure", onDone =>
    IO.suspendIO(() => IO.suspendIO(() => IO.pure(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync suspendIO - async", onDone =>
    IO.suspendIO(() => IO.async(onDone => onDone(Belt.Result.Ok(42))))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync async - synchronous Ok", onDone =>
    IO.async(onDone => onDone(Belt.Result.Ok(42)))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync async - synchronous Error", onDone =>
    IO.async(onDone => onDone(Belt.Result.Error("it failed")))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(msg) => onDone(expect(msg) |> toEqual("it failed")),
       )
  );

  testAsync("unsafeRunAsync pure - map", onDone =>
    IO.pure(42)
    |> IO.map(a => a + 10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync pure - flatMap pure", onDone =>
    IO.pure(42)
    |> IO.flatMap(a => IO.pure(a + 10))
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(52))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync(
    "unsafeRunAsync pure - flatMap pure - flatMap pure - flatMap pure", onDone =>
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

  testAsync("unsafeRunAsync pure - flatMap supend - flatMap pure", onDone =>
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

  testAsync("unsafeRunAsync pure - mapError", onDone =>
    IO.pure(42)
    |> IO.mapError(_ => "error")
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("unsafeRunAsync throw - mapError", onDone =>
    IO.throw("this is a test")
    |> IO.mapError(msg => Relude_Js_Exn.make(msg))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_a) => onDone(fail("Failed"))
         | Error(_err) => onDone(pass),
       )
  );

  testAsync("fromOption Some", onDone =>
    IO.fromOption(() => "Failed", Some(32))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(32))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("fromOption None", onDone =>
    IO.fromOption(() => "Messed up", None)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(error) => onDone(expect(error) |> toEqual("Messed up")),
       )
  );

  testAsync("fromResult Ok", onDone =>
    IO.fromResult(Ok(32))
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(32))
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("fromResult Error", onDone =>
    IO.fromResult(Error("Messed up"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("Failed"))
         | Error(error) => onDone(expect(error) |> toEqual("Messed up")),
       )
  );

  testAsync("summonError pure", onDone =>
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

  testAsync("summonError suspend", onDone =>
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

  testAsync("summonError suspendIO pure", onDone =>
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

  testAsync("summonError suspendIO throw", onDone =>
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

  testAsync("summonError suspendIO pure - map - flatMap pure", onDone =>
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

  testAsync("summonError async - flatMap pure", onDone =>
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

  testAsync("summonError async - flatMap throw", onDone =>
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

  testAsync("unsummonError pure Ok", onDone =>
    IO.unsummonError(IO.pure(Belt.Result.Ok(42)))
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("unsummonError pure Error", onDone =>
    IO.unsummonError(IO.pure(Belt.Result.Error("e!")))
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

  testAsync("unsummonError suspend Ok", onDone =>
    IO.unsummonError(IO.suspend(() => Belt.Result.Ok(42)))
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("unsummonError suspend Error", onDone =>
    IO.unsummonError(IO.suspend(() => Belt.Result.Error("e!")))
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

  testAsync("unsummonError suspendIO pure Ok", onDone =>
    IO.unsummonError(IO.suspendIO(() => IO.pure(Belt.Result.Ok(42))))
    |> IO.bimap(a => expect(a) |> toEqual(42), _ => fail("Failed"))
    |> IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(assertion) => onDone(assertion),
       )
  );

  testAsync("unsummonError suspendIO pure Error", onDone =>
    IO.unsummonError(IO.suspendIO(() => IO.pure(Belt.Result.Error("e!"))))
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

  testAsync("unsummonError suspendIO pure Ok - map - flatMap", onDone =>
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

describe("IO examples", () => {
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
    let runIO =
      (_ => getData |> IO.mapError(eGet))
      >=> (parseData >> IO.mapError(eParse))
      >=> (printNumber >> IO.mapError(ePrint))
      >=> (_ => IO.pure(pass));

    runIO()
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
