open Jest;
open Expect;

describe("Float", () => {
  test("approximatelyEqual", () => {
    expect(Float.approximatelyEqual(~tolerance=0.01, 0.111, 0.112)) |> toEqual(true);
  });
});
