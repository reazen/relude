
open Jest;
open Expect;

describe("Validation", () => {
  test("isOk success", () => {
    expect(Validation.VOk(123)->Validation.isOk) |> toBe(true);
  });
});
