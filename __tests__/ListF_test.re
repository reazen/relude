open Jest;
open Expect;

describe("ListF", () => {
  test("Option.traverse success", () => {
    expect([1, 2, 3, 4] |> ListF.Option.traverse(a => Some(a))) |> toEqual(Some([1, 2, 3, 4]))
  });

  test("Option.traverse failure", () => {
    expect([1, 2, 3, 4] |> ListF.Option.traverse(a => a mod 2 == 0 ? Some(a) : None)) |> toEqual(None)
  });

  test("Validation.traverse success", () => {
    expect(ListF.Validation.traverse(a => Ok(a), [1, 2, 3, 4, 5])) |> toEqual(Validation.VOk([1, 2, 3, 4, 5]));
  });

  test("Validation.traverse failure", () => {
    expect(ListF.Validation.traverse(a => Error(string_of_int(a) ++ " is bad"), [1, 2, 3, 4, 5]))
      |> toEqual(Validation.VError(NonEmpty.List.make("1 is bad", ["2 is bad", "3 is bad", "4 is bad", "5 is bad"])));
  });
});
