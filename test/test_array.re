open Jest;
open Expect;

module Array = Relude.Array;

describe("Array", () => {
  test("length empty array", () =>
    [||] |> Array.length |> expect |> toEqual(0)
  );

  test("length non-empty array", () =>
    [|1, 2, 3|] |> Array.length |> expect |> toEqual(3)
  );

  test("isEmpty is true for empty array", () =>
    [||] |> Array.isEmpty |> expect |> toBe(true)
  );

  test("isEmpty is false for non-empty array", () =>
    [|1|] |> Array.isEmpty |> expect |> toBe(false)
  );

  test("isNotEmpty is false for empty array", () =>
    [||] |> Array.isNotEmpty |> expect |> toBe(false)
  );

  test("isNotEmpty is true for non-empty array", () =>
    [|1|] |> Array.isNotEmpty |> expect |> toBe(true)
  );

  test("pure creates a one-item array", () =>
    123 |> Array.pure |> expect |> toEqual([|123|])
  );

  test("repeat creates a array of n items", () =>
    Array.repeat(3, "a") |> expect |> toEqual([|"a", "a", "a"|])
  );

  test("makeWithIndex creates a array of n items using f", () =>
    Array.makeWithIndex(4, i => i + 2) |> expect |> toEqual([|2, 3, 4, 5|])
  );

  test("makeWithIndex with pattern matching fn", () =>
    Array.makeWithIndex(
      3,
      fun
      | 0 => "a"
      | _ => "b",
    )
    |> expect |> toEqual([|"a", "b", "b"|])
  );

  test("makeWithIndex creates an empty array if given a negative count", () =>
    Array.makeWithIndex(-1, i => i + 2) |> expect |> toEqual([||])
  );
});