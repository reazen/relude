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

  testAsync("pure flatMap unsafeRunAsync", onDone =>
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

  // Note: this test fails without the unsafeRunAsync in summonError FlatMap/Async case
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

  testAsync("delay unsafeRunAsync", onDone =>
    IO.delay(10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("Failed")),
       )
  );

  testAsync("pure withDelay unsafeRunAsync", onDone =>
    IO.pure(42)
    |> IO.withDelay(10)
    |> IO.unsafeRunAsync(
         fun
         | Ok(a) => onDone(expect(a) |> toEqual(42))
         | Error(_) => onDone(fail("fail")),
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