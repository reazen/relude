open Jest;
open Expect;

describe("Option", () => {
  test("fold maps value when option is Some", () =>
    expect(Option.fold("", string_of_int, Some(1))) |> toEqual("1")
  );

  test("fold uses default when option is None", () =>
    expect(Option.fold("", string_of_int, None)) |> toEqual("")
  );

  test("getOrElse extracts value when option is Some", () =>
    expect(Option.getOrElse(0, Some(1))) |> toEqual(1)
  );

  test("getOrElse uses default when option is None", () =>
    expect(Option.getOrElse(0, None)) |> toEqual(0)
  );

  test("toList has one item when option is Some", () =>
    expect(Option.toList(Some("a"))) |> toEqual(["a"])
  );

  test("toList is empty when option is None", () =>
    expect(Option.toList(None)) |> toEqual([])
  );

  test("toArray has one item when option is Some", () =>
    expect(Option.toArray(Some([|0|]))) |> toEqual([|[|0|]|])
  );

  test("toArray is empty when option is None", () =>
    expect(Option.toArray(None)) |> toEqual([||])
  );

  test("isSome is true for Some", () =>
    expect(Option.isSome(Some(3))) |> toEqual(true)
  );

  test("isSome is false for None", () =>
    expect(Option.isSome(None)) |> toEqual(false)
  );

  test("isNone is true for None", () =>
    expect(Option.isNone(None)) |> toEqual(true)
  );

  test("isNone is false for Some", () =>
    expect(Option.isNone(Some(0))) |> toEqual(false)
  );

  /**
   * TODO: spot-check functions from bs-abstract
   * - map, apply, pure, flatMap
   * - map2...map5
   * - foldLeft, alt, empty
   */
  test("filter is None when option is None", () =>
    expect(Option.filter(v => v == 0, None)) |> toEqual(None)
  );

  test("filter is None when value does not pass predicate", () =>
    expect(Option.filter(v => v == 0, Some(1))) |> toEqual(None)
  );

  test("filter is Some when value passes predicate", () =>
    expect(Option.filter(v => v == 0, Some(0))) |> toEqual(Some(0))
  );

  test("flatten is None when option is None", () =>
    expect(Option.flatten(None)) |> toEqual(None)
  );

  test("flatten is None when inner option is None", () =>
    expect(Option.flatten(Some(None))) |> toEqual(None)
  );

  test("flatten is Some when both options are Some", () =>
    expect(Option.flatten(Some(Some(1)))) |> toEqual(Some(1))
  );

  test("eq is true when inner values match", () =>
    expect(Option.eq(Int.eq, Some(1), Some(1))) |> toEqual(true)
  );

  test("eq is false when inner values do not match", () =>
    expect(Option.eq(Int.eq, Some(1), Some(2))) |> toEqual(false)
  );

  test("eq is false when one option is Some and one is None", () =>
    expect(Option.eq(Int.eq, Some(1), None)) |> toEqual(false)
  );

  test("eq is true when both options are None", () =>
    expect(Option.eq(Int.eq, None, None)) |> toEqual(true)
  );

  test("eqM is true when inner values match", () =>
    expect(Option.eqM((module Int.Eq), Some(1), Some(1))) |> toEqual(true)
  );

  test("eqM is false when inner values do not match", () =>
    expect(Option.eqM((module Int.Eq), Some(1), Some(2)))
    |> toEqual(false)
  );

  test("eqM is true when both values are None", () =>
    expect(Option.eqM((module Int.Eq), None, None)) |> toEqual(true)
  );

  test("eqM is false when one value is Some and one is None", () =>
    expect(Option.eqM((module Int.Eq), None, Some(1))) |> toEqual(false)
  );

  /**
   * TODO: test infix functions
   */ ();
});
