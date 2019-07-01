open Jest;
open Expect;

module Float = Relude.Float;

describe("Float", () => {
  test("eq true", () =>
    expect(Float.eq(1.1, 1.1)) |> toEqual(true)
  );

  test("eq false", () =>
    expect(Float.eq(1.1, 1.2)) |> toEqual(false)
  );

  test("compare equal_to", () =>
    expect(Float.compare(1.1, 1.1)) |> toEqual(`equal_to)
  );

  test("compare greater_than", () =>
    expect(Float.compare(1.2, 1.1)) |> toEqual(`greater_than)
  );

  test("compare less_than", () =>
    expect(Float.compare(1.1, 1.2)) |> toEqual(`less_than)
  );

  test("min (smaller first)", () =>
    expect(Float.min(3.1, 3.2)) |> toEqual(3.1)
  );

  test("min (smaller second)", () =>
    expect(Float.min(3.2, 3.1)) |> toEqual(3.1)
  );

  test("max (larger second)", () =>
    expect(Float.max(-1.0, 1.0)) |> toEqual(1.0)
  );

  test("max (values equal)", () =>
    expect(Float.max(10.0, 10.0)) |> toEqual(10.0)
  );

  test("lessThan (first smaller)", () =>
    expect(Float.lessThan(3.14, 3.15)) |> toEqual(true)
  );

  test("lessThan (values equal)", () =>
    expect(Float.lessThan(3.14, 3.14)) |> toEqual(false)
  );

  test("lessThanOrEq (first smaller)", () =>
    expect(Float.lessThanOrEq(1.0, 2.0)) |> toEqual(true)
  );

  test("lessThanOrEq (first larger)", () =>
    expect(Float.lessThanOrEq(3.0, 0.0)) |> toEqual(false)
  );

  test("greaterThan (first larger)", () =>
    expect(Float.greaterThan(1.0, 0.1)) |> toEqual(true)
  );

  test("greaterThan (values equal)", () =>
    expect(Float.greaterThan(1.0, 1.0)) |> toEqual(false)
  );

  test("greaterThanOrEq (eq)", () =>
    expect(Float.greaterThanOrEq(0.0, 0.0)) |> toEqual(true)
  );

  test("greaterThanOrEq (first larger)", () =>
    expect(Float.greaterThanOrEq(1.0, 0.0)) |> toEqual(true)
  );

  test("greaterThanOrEq (first smaller)", () =>
    expect(Float.greaterThanOrEq(1.0, 2.0)) |> toEqual(false)
  );

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

  test("show 1.0", () =>
    expect(Float.show(1.0)) |> toEqual("1")
  );

  test("toString 1.0", () =>
    expect(Float.toString(1.0)) |> toEqual("1")
  );

  test("show 1.123", () =>
    expect(Float.show(1.123)) |> toEqual("1.123")
  );

  test("toString 1.123", () =>
    expect(Float.toString(1.123)) |> toEqual("1.123")
  );
});
