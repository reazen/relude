
open Jest;
open Expect;

describe("Int", () => {
  test("rangeAsList for valid range", () => {
    expect(Int.rangeAsList(0, 5)) |> toEqual([0, 1, 2, 3, 4]);
  });

  test("rangeAsList for invalid range", () => {
    expect(Int.rangeAsList(5, 0)) |> toEqual([]);
  });
});
