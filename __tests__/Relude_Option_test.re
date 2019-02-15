open Jest;
open Expect;

module Int = Relude_Int;
module Option = Relude_Option;

describe("Option", () => {
  test("fold maps value when option is Some", () =>
    expect(Option.fold(_ => "", string_of_int, Some(1))) |> toEqual("1")
  );

  test("fold uses default when option is None", () =>
    expect(Option.fold(_ => "", string_of_int, None)) |> toEqual("")
  );

  test("getOrElse extracts value when option is Some", () =>
    expect(Option.getOrElse(_ => 0, Some(1))) |> toEqual(1)
  );

  test("getOrElse uses default when option is None", () =>
    expect(Option.getOrElse(_ => 0, None)) |> toEqual(0)
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

  test("map", () =>
    expect(Option.map(a => a + 2, Some(1))) |> toEqual(Some(3))
  );

  test("apply", () =>
    expect(Option.apply(Some(a => a + 2), Some(1))) |> toEqual(Some(3))
  );

  test("pure", () =>
    expect(Option.pure(5)) |> toEqual(Some(5))
  );

  test("bind", () =>
    expect(Option.bind(Some(1), a => Some(a + 2))) |> toEqual(Some(3))
  );

  test("map2", () =>
    expect(Option.map2((a, b) => a + b, Some(1), Some(2)))
    |> toEqual(Some(3))
  );

  test("map3", () =>
    expect(
      Option.map3((a, b, c) => a + b + c, Some(1), Some(2), Some(3)),
    )
    |> toEqual(Some(6))
  );

  test("map4", () =>
    expect(
      Option.map4(
        (a, b, c, d) => a + b + c + d,
        Some(1),
        Some(2),
        Some(3),
        Some(4),
      ),
    )
    |> toEqual(Some(10))
  );

  test("map5", () =>
    expect(
      Option.map5(
        (a, b, c, d, e) => a + b + c + d + e,
        Some(1),
        Some(2),
        Some(3),
        Some(4),
        Some(5),
      ),
    )
    |> toEqual(Some(15))
  );

  test("foldLeft", () =>
    expect(Option.foldLeft((acc, v) => [v, ...acc], [10, 20], Some(1)))
    |> toEqual([1, 10, 20])
  );

  test("foldRight", () =>
    expect(Option.foldRight((v, acc) => [v, ...acc], [10, 20], Some(1)))
    |> toEqual([1, 10, 20])
  );

  test("alt both", () =>
    expect(Option.alt(Some(1), Some(2))) |> toEqual(Some(1))
  );

  test("alt left", () =>
    expect(Option.alt(Some(1), None)) |> toEqual(Some(1))
  );

  test("alt right", () =>
    expect(Option.alt(None, Some(1))) |> toEqual(Some(1))
  );

  test("alt neither", () =>
    expect(Option.alt(None, None)) |> toEqual(None)
  );

  test("empty", () =>
    expect(Option.empty) |> toEqual(None)
  );

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

  test("eqBy is true when inner values match", () =>
    expect(Option.eqBy(Int.eq, Some(1), Some(1))) |> toEqual(true)
  );

  test("eqBy is false when inner values do not match", () =>
    expect(Option.eqBy(Int.eq, Some(1), Some(2))) |> toEqual(false)
  );

  test("eqBy is false when one option is Some and one is None", () =>
    expect(Option.eqBy(Int.eq, Some(1), None)) |> toEqual(false)
  );

  test("eqBy is true when both options are None", () =>
    expect(Option.eqBy(Int.eq, None, None)) |> toEqual(true)
  );

  test("eq is true when inner values match", () =>
    expect(Option.eq((module Int.Eq), Some(1), Some(1))) |> toEqual(true)
  );

  test("eq is false when inner values do not match", () =>
    expect(Option.eq((module Int.Eq), Some(1), Some(2))) |> toEqual(false)
  );

  test("eq is true when both values are None", () =>
    expect(Option.eq((module Int.Eq), None, None)) |> toEqual(true)
  );

  test("eq is false when one value is Some and one is None", () =>
    expect(Option.eq((module Int.Eq), None, Some(1))) |> toEqual(false)
  );

  /**
   * TODO: test infix functions
   */ ();
});
