open Jest;
open Expect;

let (>>=) = Eff.Infix.(>>=);

/* Note: these tests currently write an actual file - get rid of this and use an in-memory test instead */
let testFilePath = Fs.testFilePath("Eff_test.txt");

describe("Eff", () => {
  beforeAll(() => Fs.Eff.writeFileSync(testFilePath, "") |> Eff.run);

  test("read and writeFileSync", () =>
    Fs.Eff.writeFileSync(testFilePath, "Eff test")
    >>= (_ => Fs.Eff.readFileSync(testFilePath))
    >>= (content => Eff.pure(expect(content) |> toEqual("Eff test")))
    |> Eff.run
  );

  test("attemptJS with Reason Js.Exn.raiseError", () => {
    expect(Eff.attemptJS(() => {
      raise(Js.Exn.raiseError("Crap the pants"))
      }) |> Eff.run |> Result.fold(_ => None, Js.Exn.message)) |> toEqual(Some("Crap the pants"))
  });

  test("attemptJS with raw JS function that throws", () => {
    let jsThrow = [%raw {|
      function() {
        throw new Error("This sucks");
      }
    |}];
    expect(Eff.attemptJS(() => {
      jsThrow(.)
    }) |> Eff.run |> Result.fold(_ => None, Js.Exn.message)) |> toEqual(Some("This sucks"));
  });
});
