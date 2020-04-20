open Jest;
open Expect;

afterAll(Bisect.Runtime.write_coverage_data);

module FilePath = {
  type t =
    | FilePath(string);
};

module Error = {
  type t =
    | Error(string);

  module Type: BsBastet.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};

module Unit = {
  type t = unit;

  module Type: BsBastet.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};

module Cont = Relude.Cont.WithResult(Unit.Type);

/**
 * Callback-based API for reading a file (with a success)
 */
let readFileSuccess =
    (
      FilePath(filePath): FilePath.t,
      onDone: result(string, Error.t) => unit,
    )
    : unit =>
  onDone(Ok("Read file: " ++ filePath));

/**
 * Callback-based API for reading a file (with an error)
 */
let readFileError =
    (
      FilePath(filePath): FilePath.t,
      onDone: result(string, Error.t) => unit,
    )
    : unit =>
  onDone(Error(Error("Failed to read file: " ++ filePath)));

/**
 * Callback-based API for writing a file (with success)
 */
let writeFileSuccess =
    (
      FilePath(_filePath): FilePath.t,
      _content: string,
      onDone: result(unit, Error.t) => unit,
    )
    : unit =>
  onDone(Ok());

/**
 * Callback-based API for writing a file (with an error)
 */
let writeFileError =
    (
      FilePath(filePath): FilePath.t,
      _content: string,
      onDone: result(unit, Error.t) => unit,
    )
    : unit =>
  onDone(Error(Error("Failed to write file: " ++ filePath)));

/**
 * Continuation Monad for the callback-based API for reading a file (success)
 */
let readFileSuccessCont: FilePath.t => Cont.t(result(string, Error.t)) =
  filePath => Cont.make(readFileSuccess(filePath));

/**
 * Continuation Monad for the callback-based API for reading a file (error)
 */
let readFileError: FilePath.t => Cont.t(result(string, Error.t)) =
  filePath => Cont.make(readFileError(filePath));

/**
 * Continuation Monad for the callback-based API for writing a file (success)
 */
let writeFileSuccessCont:
  (FilePath.t, string) => Cont.t(result(unit, Error.t)) =
  (filePath, content) => Cont.make(writeFileSuccess(filePath, content));

/**
 * Continuation Monad for the callback-based API for writing a file (error)
 */
let writeFileErrorCont: (FilePath.t, string) => Cont.t(result(unit, Error.t)) =
  (filePath, content) => Cont.make(writeFileError(filePath, content));

describe("ContT Identity", () => {
  open Cont.Infix;

  testAsync("pure", onDone =>
    Cont.pure(42)
    <#> (value => expect(value) |> toEqual(42))
    <#> onDone
    |> Cont.runContT(() => ())
  );

  testAsync("map (<#>) success", onDone => {
    let filePath = FilePath.FilePath("test.txt");
    readFileSuccessCont(filePath)
    <#> (
      fun
      | Ok(value) => expect(value) |> toEqual("Read file: test.txt")
      | Error(_) => fail("Failed")
    )
    <#> onDone
    |> Cont.runContT(() => ());
  });

  testAsync("bind (>>=) success", onDone => {
    let filePath1 = FilePath.FilePath("test1.txt");
    let filePath2 = FilePath.FilePath("test2.txt");
    let filePath3 = FilePath.FilePath("test3.txt");
    let expected = "Read file: test1.txt, Read file: test2.txt, Read file: test3.txt";

    readFileSuccessCont(filePath1)
    >>= (
      fun
      | Ok(content1) =>
        readFileSuccessCont(filePath2)
        >>= (
          fun
          | Ok(content2) =>
            readFileSuccessCont(filePath3)
            <#> (
              fun
              | Ok(content3) => {
                  let result =
                    Relude.List.String.joinWith(
                      ", ",
                      [content1, content2, content3],
                    );
                  expect(result) |> toEqual(expected);
                }
              | Error(_) => fail("Failed 3")
            )
          | Error(_) => Cont.pure(fail("Failed 2"))
        )
      | Error(_) => Cont.pure(fail("Failed 1"))
    )
    <#> onDone
    |> Cont.runContT(() => ());
  });
});

// Below is an extremely contrived example of using IO with ContT.  I'm not sure if it actually
// demonstrates anything useful, or is a correct/expected usage of ContT...

module ContT = Relude.ContT;
module Result = Relude.Result;
module IO = Relude.IO;
module IOE = IO.WithError(Error.Type);
module ContIO = ContT.WithMonadAndResult(IOE.Monad, Unit.Type);

/**
 * A callback-based API for converting a string to an int and running some IO effect
 * on the result.
 */
let stringToIntCB:
  (string, Result.t(int, Error.t) => IO.t(unit, Error.t)) =>
  IO.t(unit, Error.t) =
  (str, onDone) => {
    Relude.Int.fromString(str)
    |> Relude.Option.fold(IO.throw(Error.Error("Failed")), i => IO.pure(i))
    |> IO.flatMap(i => onDone(Ok(i)));
  };

/**
 * A ContT version of the above ContT(IO) CB API
 */
let stringToIntCont: string => ContIO.t(result(int, Error.t)) =
  str => ContIO.make(stringToIntCB(str));

/**
 * Simulates an effectful assertion function that compares two ints
 */
let assertIntIO =
    (expected: int, actual: int, onDone: assertion => unit)
    : IO.t(unit, Error.t) => {
  onDone(expect(actual) |> toEqual(expected));
  IO.unit;
};

/**
 * Simulates an effectful assertion function that fails
 */
let failIO = (onDone: assertion => unit): IO.t(unit, Error.t) => {
  onDone(fail("Failed"));
  IO.unit;
};

describe("ContT IO", () => {
  open ContIO.Infix;

  testAsync("map", onDone =>
    stringToIntCont("42")
    <#> (res => res |> Result.map(a => a * 2))
    |> ContIO.runContT(
         fun
         | Ok(i) => assertIntIO(84, i, onDone)
         | Error(_) => failIO(onDone),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok () => ()
         | Error(_) => (),
       )
  );

  testAsync("Cont.map2", onDone =>
    ContIO.map2(
      (res1, res2) =>
        switch (res1, res2) {
        | (Ok(a), Ok(b)) => Ok(a + b)
        | _ => Error(Error.Error("error"))
        },
      stringToIntCont("42"),
      stringToIntCont("43"),
    )
    |> ContIO.runContT(
         fun
         | Ok(a) => assertIntIO(85, a, onDone)
         | Error(_) => failIO(onDone),
       )
    |> IO.unsafeRunAsync(
         fun
         | Ok () => ()
         | Error(_) => (),
       )
  );
});
