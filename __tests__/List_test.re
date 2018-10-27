open Jest;
open Expect;

describe("List", () => {
  test("isEmpty is true for empty list", () => {
    expect(List.isEmpty([])) |> toBe(true);
  });

  test("isEmpty is false for non-empty list", () => {
    expect(List.isEmpty([1])) |> toBe(false);
  });

  test("traverseOption success", () => {
    expect([1, 2, 3, 4] |> List.traverseOption(a => Some(a))) |> toEqual(Some([1, 2, 3, 4]))
  });

  test("traverseOption failure", () => {
    expect([1, 2, 3, 4] |> List.traverseOption(a => a mod 2 == 0 ? Some(a) : None)) |> toEqual(None)
  });
});
