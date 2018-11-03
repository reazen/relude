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
});
