open Jest;
open Expect;

module Bool = Relude.Bool;

describe("Bool", () => {
  test("ifElse true", () =>
    expect(Bool.ifElse(() => "true", () => "false", true))
    |> toEqual("true")
  );

  test("ifElse false", () =>
    expect(Bool.ifElse(() => "true", () => "false", false))
    |> toEqual("false")
  );

  testAll("inverse", [(true, false), (false, true)], ((input, expected)) =>
    expect(Bool.inverse(input)) |> toEqual(expected)
  );

  testAll(
    "and_",
    [
      (true, true, true),
      (true, false, false),
      (false, true, false),
      (false, false, false),
    ],
    ((a, b, c)) =>
    expect(Bool.and_(a, b)) |> toEqual(c)
  );

  testAll(
    "or_",
    [
      (true, true, true),
      (true, false, true),
      (false, true, true),
      (false, false, false),
    ],
    ((a, b, c)) =>
    expect(Bool.or_(a, b)) |> toEqual(c)
  );

  testAll(
    "nand",
    [
      (true, true, false),
      (true, false, true),
      (false, true, true),
      (false, false, true),
    ],
    ((a, b, c)) =>
    expect(Bool.nand(a, b)) |> toEqual(c)
  );

  testAll(
    "nor_",
    [
      (true, true, false),
      (true, false, false),
      (false, true, false),
      (false, false, true),
    ],
    ((a, b, c)) =>
    expect(Bool.nor(a, b)) |> toEqual(c)
  );

  testAll(
    "xor",
    [
      (true, true, false),
      (true, false, true),
      (false, true, true),
      (false, false, false),
    ],
    ((a, b, c)) =>
    expect(Bool.xor(a, b)) |> toEqual(c)
  );

  testAll(
    "xnor",
    [
      (true, true, true),
      (true, false, false),
      (false, true, false),
      (false, false, true),
    ],
    ((a, b, c)) =>
    expect(Bool.xnor(a, b)) |> toEqual(c)
  );

  testAll(
    "implies",
    [
      (true, true, true),
      (true, false, false),
      (false, true, true),
      (false, false, true),
    ],
    ((a, b, c)) =>
    expect(Bool.implies(a, b)) |> toEqual(c)
  );

  testAll(
    "eq",
    [
      (true, true, true),
      (true, false, false),
      (false, true, false),
      (false, false, true),
    ],
    ((a, b, c)) =>
    expect(Bool.eq(a, b)) |> toEqual(c)
  );

  testAll(
    "compare",
    [
      (true, true, `equal_to),
      (true, false, `greater_than),
      (false, true, `less_than),
      (false, false, `equal_to),
    ],
    ((a, b, c)) =>
    expect(Bool.compare(a, b)) |> toEqual(c)
  );

  testAll("show", [(true, "true"), (false, "false")], ((a, b)) =>
    expect(Bool.show(a)) |> toEqual(b)
  );
});