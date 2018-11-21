open Jest;
open Expect;

module Eff = Relude_Eff;

let (>>=) = Eff.Infix.(>>=);

describe("Eff", () => {
  test("pure", () => {
    expect(Eff.pure(1) |> Eff.run) |> toEqual(1)
  });

  test("fromThunk", () => {
    expect(Eff.fromThunk(_ => 1) |> Eff.run) |> toEqual(1)
  });

  test("map", () => {
    expect(Eff.map(a => a + 2, Eff.pure(1)) |> Eff.run) |> toEqual(3)
  });

  test("apply", () => {
    expect(Eff.apply(Eff.pure(a => a + 2), Eff.pure(1)) |> Eff.run) |> toEqual(3)
  });

  test("bind", () => {
    expect(Eff.bind(Eff.pure(1), i => Eff.pure(i + 2)) |> Eff.run) |> toEqual(3)
  });
});

/* Note: these tests currently write an actual file - get rid of this and use an in-memory test instead */
let testFilePath = FS.testFilePath("Eff_test.txt");

describe("Eff file test", () => {
  beforeAll(() => FS.Eff.writeFileSync(testFilePath, "") |> Eff.run);

  test("read and writeFileSync", () =>
    FS.Eff.writeFileSync(testFilePath, "Eff test")
    >>= (_ => FS.Eff.readFileSync(testFilePath))
    >>= (content => Eff.pure(expect(content) |> toEqual("Eff test")))
    |> Eff.run
  );

  test("attemptJS with Reason Js.Exn.raiseError", () => {
    expect(Eff.attemptJS(() => {
      raise(Js.Exn.raiseError("Crap the pants"))
      }) |> Eff.run |> Relude_Result.fold(_ => None, Js.Exn.message)) |> toEqual(Some("Crap the pants"))
  });

  test("attemptJS with raw JS function that throws", () => {
    let jsThrow = [%raw {|
      function() {
        throw new Error("This sucks");
      }
    |}];
    expect(Eff.attemptJS(() => {
      jsThrow(.)
    }) |> Eff.run |> Relude_Result.fold(_ => None, Js.Exn.message)) |> toEqual(Some("This sucks"));
  });
});
