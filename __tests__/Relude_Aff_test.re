open Jest;
open Expect;

module Aff = Relude_Aff;
module Eff = Relude_Eff;

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

module AffInfix = Aff.Infix({ type t = affTestError; });

let (>>=) = AffInfix.Monad.(>>=);

/* TODO: remove live file tests, and replace with in-memory async tests */
let testFilePath = FS.testFilePath("Aff_test.txt");

describe("Aff", () => {
  beforeAll(() => FS.Eff.writeFileSync(testFilePath, "") |> Eff.run);

  testAsync("readFile", onDone =>
    FS.Aff.writeFile(testFilePath, "Aff test")
    |> Aff.mapError(e => JsExn(e))  /* FS methods have error type Js.Exn.t, but we want to work in our affTestError type, so we need to wrap the Js.Exn.t */
    >>= (_ => FS.Aff.readFile(testFilePath) |> Aff.mapError(e => JsExn(e)))
    >>= (content => Relude_String.toNonWhitespace(content) |> Aff.fromOption(ErrorMessage("Failed to get non-empty file content")))
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
