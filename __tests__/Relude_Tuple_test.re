open Jest;
open Expect;

module Array = Relude.Array;
module List = Relude.List;
module Tuple = Relude.Tuple;

describe("Tuple constructors", () => {
  test("make", () =>
    expect(Tuple.make("A", 0)) |> toEqual(("A", 0))
  );

  test("make3", () =>
    expect(Tuple.make3("A", 0, true)) |> toEqual(("A", 0, true))
  );

  test("make4", () =>
    expect(Tuple.make4("A", 0, true, false))
    |> toEqual(("A", 0, true, false))
  );

  test("make5", () =>
    expect(Tuple.make5("A", 0, true, false, 3.14))
    |> toEqual(("A", 0, true, false, 3.14))
  );
});

describe("Tuple from array", () => {
  test("fromArray (success)", () =>
    expect(Tuple.fromArray([|0, 1|])) |> toEqual(Some((0, 1)))
  );

  test("fromArray (failure on long array)", () =>
    expect(Tuple.fromArray([|0, 1, 2|])) |> toEqual(None)
  );

  test("fromArray3 (success)", () =>
    expect(Tuple.fromArray3([|0, 1, 2|])) |> toEqual(Some((0, 1, 2)))
  );

  test("fromArray3 (failure on short array)", () =>
    expect(Tuple.fromArray3([|0, 1|])) |> toEqual(None)
  );

  test("fromArray4 (success)", () =>
    expect(Tuple.fromArray4([|0, 1, 2, 3|])) |> toEqual(Some((0, 1, 2, 3)))
  );

  test("fromArray4 (failure on long array)", () =>
    expect(Tuple.fromArray4([|0, 1, 2, 3, 4|])) |> toEqual(None)
  );

  test("fromArray5 (success)", () =>
    expect(Tuple.fromArray5([|0, 1, 2, 3, 4|]))
    |> toEqual(Some((0, 1, 2, 3, 4)))
  );

  test("fromArray5 (failure on long array)", () =>
    expect(Tuple.fromArray5([|0, 1, 2, 3, 4, 5|])) |> toEqual(None)
  );

  test("fromArrayAtLeast2", () =>
    expect(Tuple.fromArrayAtLeast2(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A")))
  );

  test("fromArrayAtLeast3", () =>
    expect(Tuple.fromArrayAtLeast3(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A")))
  );

  test("fromArrayAtLeast4", () =>
    expect(Tuple.fromArrayAtLeast4(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A")))
  );

  test("fromArrayAtLeast5", () =>
    expect(Tuple.fromArrayAtLeast5(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A", "A")))
  );
});

describe("Tuple from list", () => {
  test("fromList (success)", () =>
    expect(Tuple.fromList([0, 1])) |> toEqual(Some((0, 1)))
  );

  test("fromList (failure on long array)", () =>
    expect(Tuple.fromList([0, 1, 2])) |> toEqual(None)
  );

  test("fromList3 (success)", () =>
    expect(Tuple.fromList3([0, 1, 2])) |> toEqual(Some((0, 1, 2)))
  );

  test("fromList3 (failure on short array)", () =>
    expect(Tuple.fromList3([0, 1])) |> toEqual(None)
  );

  test("fromList4 (success)", () =>
    expect(Tuple.fromList4([0, 1, 2, 3])) |> toEqual(Some((0, 1, 2, 3)))
  );

  test("fromList4 (failure on long array)", () =>
    expect(Tuple.fromList4([0, 1, 2, 3, 4])) |> toEqual(None)
  );

  test("fromList5 (success)", () =>
    expect(Tuple.fromList5([0, 1, 2, 3, 4]))
    |> toEqual(Some((0, 1, 2, 3, 4)))
  );

  test("fromList4 (failure on long array)", () =>
    expect(Tuple.fromList5([0, 1, 2, 3, 4, 5])) |> toEqual(None)
  );

  test("fromListAtLeast2", () =>
    expect(Tuple.fromListAtLeast2(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A")))
  );

  test("fromListAtLeast3", () =>
    expect(Tuple.fromListAtLeast3(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A")))
  );

  test("fromListAtLeast4", () =>
    expect(Tuple.fromListAtLeast4(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A")))
  );

  test("fromListAtLeast5", () =>
    expect(Tuple.fromListAtLeast5(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A", "A")))
  );
});

describe("Tuple apply helpers", () => {
  test("apply2", () =>
    expect((1, 2) |> Tuple.apply2((a, b) => a + b)) |> toEqual(3)
  );

  test("apply3", () =>
    expect((1, 2, 3) |> Tuple.apply3((a, b, c) => a + b + c)) |> toEqual(6)
  )

  test("apply4", () =>
    expect((1, 2, 3, 4) |> Tuple.apply4((a, b, c, d) => a + b + c + d)) |> toEqual(10)
  )

  test("apply5", () =>
    expect((1, 2, 3, 4, 5) |> Tuple.apply5((a, b, c, d, e) => a + b + c + d + e)) |> toEqual(15)
  )
});