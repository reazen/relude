open Jest;
open Expect;

module IntSet = Relude.Set.WithOrd(Relude.Int.Ord);

describe("Set", () => {
  test("length of empty set", () =>
    expect(IntSet.(empty |> length)) |> toEqual(0)
  );

  test("length of non-empty set", () =>
    expect(IntSet.(singleton(1) |> length)) |> toEqual(1)
  );

  test("isEmpty (empty set)", () =>
    expect(IntSet.(empty |> isEmpty)) |> toEqual(true)
  );

  test("isEmpty (nonempty set)", () =>
    expect(IntSet.(singleton(1) |> isEmpty)) |> toEqual(false)
  );

  test("fromArray (empty)", () =>
    expect(IntSet.fromArray([||])) |> toEqual(IntSet.empty)
  );

  test("fromArray (nonEmpty, unique)", () =>
    expect(IntSet.(fromArray([|1, 2, 3|]) |> length)) |> toEqual(3)
  );

  test("fromArray (nonEmpty, removes duplicates)", () =>
    expect(IntSet.(fromArray([|1, 2, 1|]) |> length)) |> toEqual(2)
  );

  test("fromList (empty)", () =>
    expect(IntSet.fromList([])) |> toEqual(IntSet.empty)
  );

  test("fromList (nonEmpty)", () =>
    expect(IntSet.(fromList([4, 4, 4, 4, 3]) |> length)) |> toEqual(2)
  );

  test("contains (false)", () =>
    expect(IntSet.(fromList([1, 3, 3, 4]) |> contains(5)))
    |> toEqual(false)
  );

  test("contains (true)", () =>
    expect(IntSet.(fromList([1, 3, 3, 4]) |> contains(3))) |> toEqual(true)
  );

  test("add (new unique value)", () =>
    expect(IntSet.(fromList([1, 2]) |> add(3)))
    |> toEqual(IntSet.fromList([1, 2, 3]))
  );

  test("add (duplicate value)", () =>
    expect(IntSet.(fromList([1, 2]) |> add(1)))
    |> toEqual(IntSet.fromList([1, 2]))
  );

  test("mergeMany (empty array to merge)", () =>
    expect(IntSet.(singleton(1) |> mergeMany([||])))
    |> toEqual(IntSet.fromList([1]))
  );

  test("mergeMany (all unique)", () =>
    expect(
      IntSet.(
        singleton(1) |> mergeMany([|2, 3|]) |> eq(fromList([1, 2, 3]))
      ),
    )
    |> toEqual(true)
  );

  test("mergeMany (some duplicates)", () =>
    expect(
      IntSet.(
        fromList([1, 2, 3])
        |> mergeMany([|2, 3, 4|])
        |> eq(fromList([1, 2, 3, 4]))
      ),
    )
    |> toEqual(true)
  );
});
