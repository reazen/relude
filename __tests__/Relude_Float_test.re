open Jest;
open Expect;

module Float = Relude_Float;

describe("Float", () => {
  test("approximatelyEqual", () => {
    expect(Float.approximatelyEqual(~tolerance=0.01, 0.111, 0.112)) |> toEqual(true);
  });
});
