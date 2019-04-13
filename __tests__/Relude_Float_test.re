open Jest;
open Expect;

module Float = Relude_Float;

describe("Float", () => {
  test("approximatelyEqual", () =>
    expect(Float.approximatelyEqual(~tolerance=0.01, 0.111, 0.112))
    |> toEqual(true)
  );

  test("fromString success", () =>
    expect(Float.fromString("3.14")) |> toEqual(Some(3.14))
  );

  test("fromString success on int", () =>
    expect(Float.fromString("3")) |> toEqual(Some(3.0))
  );

  test("fromString failure on empty", () =>
    expect(Float.fromString("")) |> toEqual(None)
  );

  test("fromString failure on mixed", () =>
    expect(Float.fromString("3.14a")) |> toEqual(None)
  );

  test("fromString failure on alpha", () =>
    expect(Float.fromString("abc")) |> toEqual(None)
  );

  test("toString", () =>
    expect(Float.toString(1.0)) |> toEqual("1")
  );
});
