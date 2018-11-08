open Jest;
open Expect;

/*
 This is how you might define all of the possible errors that you might expect from a program.
 This is done because the Aff needs a consistent error type throughout the monadic chain.

 Js.Exn.t or just a string error might make sense as a general-purpose error type, but it's possible
 to encode all possible errors in a sum type.
 */
type affTestError =
  | JsExn(Js.Exn.t)
  | ErrorMessage(string)
  | Unknown;

module AffTestErrorInfix =
  Aff.Infix({
    type t = affTestError;
  });

let (>>=) = AffTestErrorInfix.(>>=);

/* TODO: remove live file tests, and replace with in-memory async tests */
let testFilePath = Fs.testFilePath("Aff_test.txt");

describe("Aff", () => {
  beforeAll(() => Fs.Eff.writeFileSync(testFilePath, "") |> Eff.run);

  /*
   test("Sandbox", () => {
     let x: Aff.t(unit, Js.Exn.t) = Fs.Aff.writeFile(testFilePath, "Aff test");
     let y: Aff.t(unit, affError) = x |> Aff.mapError(e => JsExn(e));
     expect(true) |> toBe(true);
   });
   */

  testAsync("readFile", onDone =>
    Fs.Aff.writeFile(testFilePath, "Aff test")
    |> Aff.mapError(e => JsExn(e))  /* Fs methods have error type Js.Exn.t, but we want to work in our affTestError type, so we need to wrap the Js.Exn.t */
    >>= (_ => Fs.Aff.readFile(testFilePath) |> Aff.mapError(e => JsExn(e)))
    >>= (content => Strings.toNonWhitespace(content) |> Aff.fromOption(ErrorMessage("Failed to get non-empty file content")))
    >>= (content => Aff.pure(expect(content) |> toEqual("Aff test")))
    >>= (
      assertion => {
        onDone(assertion) |> ignore;
        Aff.pure();
      }
    )
    |> Aff.run
  );
});
