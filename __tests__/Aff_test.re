open Jest;
open Expect;

/*
This is how you might define all of the possible errors that you might expect from a program.
This is done because the Aff needs a consistent error type throughout the monadic chain.

Js.Exn.t or just a string error might make sense as a general-purpose error type, but it's possible
to encode all possible errors in a sum type.
*/
type affError =
  | JsExn(Js.Exn.t)
  | ErrorMessage(string)
  | Unknown;

module AffErrorInfix =
  Aff.Infix({
    type t = affError;
  });

let (>>=) = AffErrorInfix.(>>=);

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
    |> Aff.mapError(e => JsExn(e))
    >>= (_ => Fs.Aff.readFile(testFilePath) |> Aff.mapError(e => JsExn(e)))
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
