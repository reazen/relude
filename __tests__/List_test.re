open Jest;
open Expect;

describe("List", () => {
  test("isEmpty is true for empty list", () => {
    expect(List.isEmpty([])) |> toBe(true);
  });

  test("isEmpty is false for non-empty list", () => {
    expect(List.isEmpty([1])) |> toBe(false);
  });
});
