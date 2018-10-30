open Jest;
open Expect;

describe("List", () => {
  test("isEmpty is true for empty list", () => {
    expect(List.isEmpty([])) |> toBe(true);
  });

  test("isEmpty is false for non-empty list", () => {
    expect(List.isEmpty([1])) |> toBe(false);
  });

  test("eq returns true if list items are equal", () => {
    expect(List.eq([1, 2, 3], [1, 2, 3], Int.eq)) |> toBe(true);
  });

  test("eq returns false if list items are not equal", () => {
    expect(List.eq([1, 2, 3], [1, 2, 4], Int.eq)) |> toBe(false);
  });

  test("eqM returns true if list items are equal", () => {
    expect(List.eqM([1, 2, 3], [1, 2, 3], (module Int.Eq))) |> toBe(true);
  });

  test("eqM returns false if list items are not equal", () => {
    expect(List.eqM([1, 2, 3], [1, 2, 4], (module Int.Eq))) |> toBe(false);
  });
});
