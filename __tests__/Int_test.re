
open Jest;
open Expect;

describe("Int", () => {
  test("range for valid range", () => {
    expect(Int.range(0, 5)) |> toEqual([0, 1, 2, 3, 4]);
  });

  test("range for invalid range", () => {
    expect(Int.range(5, 0)) |> toEqual([]);
  });
});
