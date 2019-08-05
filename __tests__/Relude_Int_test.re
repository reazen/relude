open Jest;
open Expect;

module Int = Relude_Int;

describe("Int", () => {
  test("toFloat", () =>
    expect(Int.toFloat(2)) |> toEqual(2.0)
  );

  test("toFloat (negative)", () =>
    expect(Int.toFloat(-20)) |> toEqual(-20.0)
  );

  test("fromFloat", () =>
    expect(Int.fromFloat(4.9999)) |> toEqual(4)
  );

  test("fromFloat (negative)", () =>
    expect(Int.fromFloat(-4.9999)) |> toEqual(-4)
  );

  test("fromFloat (infinity)", () =>
    expect(Int.fromFloat(infinity)) |> toEqual(0)
  );

  test("fromFloat (nan)", () =>
    expect(Int.fromFloat(nan)) |> toEqual(0)
  );

  test("rangeAsList for valid range", () =>
    expect(Int.rangeAsList(0, 5)) |> toEqual([0, 1, 2, 3, 4])
  );

  test("rangeAsList for invalid range", () =>
    expect(Int.rangeAsList(5, 0)) |> toEqual([])
  );

  test("rangeAsArray for valid range", () =>
    expect(Int.rangeAsArray(0, 5)) |> toEqual([|0, 1, 2, 3, 4|])
  );

  test("rangeAsArray for invalid range", () =>
    expect(Int.rangeAsArray(5, 0)) |> toEqual([||])
  );

  test("add", () =>
    expect(Int.Semiring.add(4, 5)) |> toEqual(9)
  );

  test("multiply", () =>
    expect(Int.Semiring.multiply(4, 5)) |> toEqual(20)
  );

  test("subtract", () =>
    expect(Int.Ring.subtract(4, 5)) |> toEqual(-1)
  );

  test("fromString success", () =>
    expect(Int.fromString("3")) |> toEqual(Some(3))
  );

  test("fromString failure on empty", () =>
    expect(Int.fromString("")) |> toEqual(None)
  );

  test("fromString failure on mixed", () =>
    expect(Int.fromString("3a")) |> toEqual(None)
  );

  test("fromString failure on float", () =>
    expect(Int.fromString("3.14")) |> toEqual(None)
  );

  test("fromString failure on alpha", () =>
    expect(Int.fromString("abc")) |> toEqual(None)
  );

  test("toString", () =>
    expect(Int.toString(1)) |> toEqual("1")
  );
});
