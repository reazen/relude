
open Jest;
open Expect;

describe("Validation", () => {
  test("traverseValidation success", () => {
    expect(ListF.Validation.traverse(a => VOk(a), [1, 2, 3, 4, 5])) |> toEqual(Validation.VOk([1, 2, 3, 4, 5]));
  });

  test("traverseValidation failure", () => {
    expect(ListF.Validation.traverse(a => VError([string_of_int(a) ++ " is bad"]), [1, 2, 3, 4, 5])) |> toEqual(Validation.VError(["1 is bad", "2 is bad", "3 is bad", "4 is bad", "5 is bad"]));
  });
});
