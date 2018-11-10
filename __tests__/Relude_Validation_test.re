
open Jest;
open Expect;

module Validation = Relude_Validation;

describe("Validation", () => {
  test("isOk success", () => {
    expect(Validation.VOk(123)->Validation.isOk) |> toBe(true);
  });
});
