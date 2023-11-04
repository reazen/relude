open Jest;
open Expect;

module Int = Relude.Int;
module Option = Relude.Option;

describe("Option", () => {
  test("some", () =>
    expect(Option.some("foo")) |> toEqual(Some("foo"))
  );

  test("orElse (primary is none)", () =>
    expect(None |> Option.orElse(~fallback=Some(1))) |> toEqual(Some(1))
  );

  test("orElse (primary is some)", () =>
    expect(Some(3) |> Option.orElse(~fallback=Some(1)))
    |> toEqual(Some(3))
  );

  test("tap (none)", () => {
    let x = ref(1);
    let _ = Option.tap(v => x := x^ + v, None);
    expect(x^) |> toEqual(1);
  });

  test("tap (some)", () => {
    let x = ref(1);
    let _ = Option.tap(v => x := x^ + v, Some(3));
    expect(x^) |> toEqual(4);
  });

  test("tapSome (none)", () => {
    let x = ref(1);
    let _ = Option.tapSome(v => x := x^ + v, None);
    expect(x^) |> toEqual(1);
  });

  test("tapSome (some)", () => {
    let x = ref(1);
    let _ = Option.tapSome(v => x := x^ + v, Some(-1));
    expect(x^) |> toEqual(0);
  });

  test("tapNone (some)", () => {
    let x = ref(0);
    let _ = Option.tapNone(() => x := 3, Some(1));
    expect(x^) |> toEqual(0);
  });

  test("tapNone (none)", () => {
    let x = ref(0);
    let _ = Option.tapNone(() => x := 3, None);
    expect(x^) |> toEqual(3);
  });

  test("bitap (some)", () => {
    let x = ref(0);
    let _ = Option.bitap(() => x := 3, v => x := v, Some(1));
    expect(x^) |> toEqual(1);
  });

  test("bitap (none)", () => {
    let x = ref(0);
    let _ = Option.bitap(() => x := 3, v => x := v, None);
    expect(x^) |> toEqual(3);
  });

  test("foldLazy maps value when option is Some", () =>
    expect(Option.foldLazy(_ => "", string_of_int, Some(1)))
    |> toEqual("1")
  );

  test("foldLazy uses default when option is None", () =>
    expect(Option.foldLazy(_ => "", string_of_int, None)) |> toEqual("")
  );

  test("fold maps value when option is Some", () =>
    expect(Option.fold("", string_of_int, Some(1))) |> toEqual("1")
  );

  test("fold uses default when option is None", () =>
    expect(Option.fold("", string_of_int, None)) |> toEqual("")
  );

  test("getOrElseLazy extracts value when option is Some", () =>
    expect(Option.getOrElseLazy(_ => 0, Some(1))) |> toEqual(1)
  );

  test("getOrElseLazy uses default when option is None", () =>
    expect(Option.getOrElseLazy(_ => 0, None)) |> toEqual(0)
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

  test("map", () =>
    expect(Option.map(a => a + 2, Some(1))) |> toEqual(Some(3))
  );

  test("apply", () =>
    expect(Option.apply(Some(a => a + 2), Some(1))) |> toEqual(Some(3))
  );

  testAll(
    "align",
    [
      (Some(42), Some("a"), Some(Relude.Ior_Type.Both(42, "a"))),
      (Some(42), None, Some(Relude.Ior_Type.This(42))),
      (None, Some("a"), Some(Relude.Ior_Type.That("a"))),
      (None, None, None),
    ],
    ((fa, fb, expected)) => {
    expect(Option.align(fa, fb)) |> toEqual(expected)
  });

  testAll(
    "alignWith",
    [
      (Some(42), Some("99"), Some(141)),
      (Some(42), None, Some(42)),
      (None, Some("99"), Some(99)),
      (None, None, None),
    ],
    ((fa, fb, expected)) => {
      let f =
        fun
        | Relude.Ior_Type.This(a) => a
        | Relude.Ior_Type.That(b) => int_of_string(b)
        | Relude.Ior_Type.Both(a, b) => a + int_of_string(b);
      expect(Option.alignWith(f, fa, fb)) |> toEqual(expected);
    },
  );

  test("pure", () =>
    expect(Option.pure(5)) |> toEqual(Some(5))
  );

  test("bind", () =>
    expect(Option.bind(Some(1), a => Some(a + 2))) |> toEqual(Some(3))
  );

  test(">>=", () => {
    let (>>=) = Relude.Option.Infix.(>>=);
    expect(Some(1) >>= (a => Some(a + 2))) |> toEqual(Some(3));
  });

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

  test("tuple2", () =>
    expect(Option.tuple2(Some(1), Some(2))) |> toEqual(Some((1, 2)))
  );

  test("tuple3", () =>
    expect(Option.tuple3(Some(1), Some(2), Some(3)))
    |> toEqual(Some((1, 2, 3)))
  );

  test("tuple4", () =>
    expect(Option.tuple4(Some(1), Some(2), Some(3), Some(4)))
    |> toEqual(Some((1, 2, 3, 4)))
  );

  test("tuple5", () =>
    expect(Option.tuple5(Some(1), Some(2), Some(3), Some(4), Some(5)))
    |> toEqual(Some((1, 2, 3, 4, 5)))
  );

  test("mapTuple2", () =>
    expect((Some(1), Some(2)) |> Option.mapTuple2((a, b) => a + b))
    |> toEqual(Some(3))
  );

  test("mapTuple3", () =>
    expect(
      (Some(1), Some(2), Some(3))
      |> Option.mapTuple3((a, b, c) => a + b + c),
    )
    |> toEqual(Some(6))
  );

  test("mapTuple4", () =>
    expect(
      (Some(1), Some(2), Some(3), Some(4))
      |> Option.mapTuple4((a, b, c, d) => a + b + c + d),
    )
    |> toEqual(Some(10))
  );

  test("mapTuple5", () =>
    expect(
      (Some(1), Some(2), Some(3), Some(4), Some(5))
      |> Option.mapTuple5((a, b, c, d, e) => a + b + c + d + e),
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

  test("filterNot", () =>
    expect(Option.filterNot(x => x mod 2 == 0, Some(1)))
    |> toEqual(Some(1))
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
});

describe("Option Specializations", () => {
  test("String.eq (both none)", () =>
    expect(Option.String.eq(None, None)) |> toEqual(true)
  );

  test("String.eq (one none, one some)", () =>
    expect(Option.String.eq(None, Some("a"))) |> toEqual(false)
  );

  test("String.eq (both some, same value)", () =>
    expect(Option.String.eq(Some("a"), Some("a"))) |> toEqual(true)
  );

  test("String.eq (both some, different value)", () =>
    expect(Option.String.eq(Some("a"), Some("b"))) |> toEqual(false)
  );

  test("String.compare (both none)", () =>
    expect(Option.String.compare(None, None)) |> toEqual(`equal_to)
  );

  test("String.compare (none less than some)", () =>
    expect(Option.String.compare(None, Some("a"))) |> toEqual(`less_than)
  );

  test("String.compare (some is greater than none)", () =>
    expect(Option.String.compare(Some(""), None)) |> toEqual(`greater_than)
  );

  test("String.compare (both some, same value)", () =>
    expect(Option.String.compare(Some("hi"), Some("hi")))
    |> toEqual(`equal_to)
  );

  test("String.compare (both some, different value)", () =>
    expect(Option.String.compare(Some("b"), Some("a")))
    |> toEqual(`greater_than)
  );

  test("Int.eq", () =>
    expect(Option.Int.eq(Some(1), None)) |> toEqual(false)
  );

  test("Float.eq", () =>
    expect(Option.Float.eq(Some(3.14), Some(3.14))) |> toEqual(true)
  );
});
