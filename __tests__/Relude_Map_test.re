open Jest;
open Expect;
open Relude.Globals;

describe("StringMap", () => {
  let empty = StringMap.make();
  let foo1 = StringMap.set("foo", 1, empty);
  let abcdef = StringMap.fromList([("a", "b"), ("c", "d"), ("e", "f")]);
  let zyx = StringMap.fromList([("z", 5), ("y", 3), ("x", 9)]);

  test("make is empty", () =>
    expect(StringMap.isEmpty(empty)) |> toEqual(true)
  );

  test("set (add new key)", () =>
    expect(StringMap.set("a", 0, empty) |> StringMap.get("a"))
    |> toEqual(Some(0))
  );

  test("set (overwrite exising key)", () =>
    expect(StringMap.set("a", "a", abcdef) |> StringMap.get("a"))
    |> toEqual(Some("a"))
  );

  test("set (is immutable)", () => {
    let _ = StringMap.set("a", 1, empty);
    expect(StringMap.length(empty)) |> toEqual(0);
  });

  test("singleton", () =>
    expect(StringMap.(singleton("foo", 3) |> get("foo")))
    |> toEqual(Some(3))
  );

  test("contains (false)", () =>
    expect(StringMap.contains("foo", empty)) |> toEqual(false)
  );

  test("contains (true)", () =>
    expect(StringMap.contains("foo", foo1)) |> toEqual(true)
  );

  test("eqBy (false)", () =>
    expect(StringMap.eqBy(Int.eq, foo1, empty)) |> toEqual(false)
  );

  test("eqBy (true)", () =>
    expect(StringMap.eqBy(Int.eq, foo1, foo1)) |> toEqual(true)
  );

  test("eqBy (empty always equals empty)", () =>
    expect(StringMap.eqBy((_, _) => false, empty, empty)) |> toEqual(true)
  );

  test("find (found)", () =>
    expect(
      StringMap.(
        fromList([("z", 1), ("foo", 2), ("a", (-1))])
        |> find((_k, v) => v > 0)
      ),
    )
    |> toEqual(Some(("foo", 2)))
  );

  test("find (not found)", () =>
    expect(StringMap.find((_k, v) => v > 10, foo1)) |> toEqual(None)
  );

  test("forEach (side effects run)", () => {
    let x = ref("");
    StringMap.forEach((k, v) => x := x^ ++ k ++ v, abcdef);
    expect(x^) |> toEqual("abcdef");
  });

  test("foldLeft (accumulate keys and values)", () =>
    expect(StringMap.foldLeft((acc, k, v) => acc ++ k ++ v, "", abcdef))
    |> toEqual("abcdef")
  );

  test("all (false)", () =>
    expect(
      StringMap.(
        fromArray([|("a", "a"), ("b", "")|])
        |> all((_k, v) => String.length(v) == 1)
      ),
    )
    |> toEqual(false)
  );

  test("all (true)", () =>
    expect(StringMap.all((_, v) => v == 1, foo1)) |> toEqual(true)
  );

  test("all (always true when empty)", () =>
    expect(StringMap.all((_, _) => false, empty)) |> toEqual(true)
  );

  test("any (false)", () =>
    expect(StringMap.any((_, v) => v == 0, foo1)) |> toEqual(false)
  );

  test("any (true)", () =>
    expect(StringMap.any((k, _) => k == "e", abcdef)) |> toEqual(true)
  );

  test("any (always false when empty)", () =>
    expect(StringMap.any((_, _) => true, empty)) |> toEqual(false)
  );

  test("length (empty)", () =>
    expect(StringMap.length(empty)) |> toEqual(0)
  );

  test("length (three keys)", () =>
    expect(StringMap.length(abcdef)) |> toEqual(3)
  );

  test("toArray (empty)", () =>
    expect(StringMap.toArray(empty)) |> toEqual([||])
  );

  test("toArray (from unsorted array)", () =>
    expect(StringMap.toArray(zyx))
    |> toEqual([|("x", 9), ("y", 3), ("z", 5)|])
  );

  test("fromValueArray", () => {
    let map =
      StringMap.fromValueArray(
        fst,
        [|("a", "b"), ("b", "c"), ("a", "d")|],
      );
    expect(StringMap.get("a", map)) |> toEqual(Some(("a", "d")));
  });

  test("fromValueList", () => {
    let map = StringMap.fromValueList(fst, [("a", 0), ("b", 1)]);
    expect(StringMap.get("a", map)) |> toEqual(Some(("a", 0)));
  });

  test("keyArray (empty)", () =>
    expect(StringMap.keyArray(empty)) |> toEqual([||])
  );

  test("keyArray (zyx)", () =>
    expect(StringMap.keyArray(zyx)) |> toEqual([|"x", "y", "z"|])
  );

  test("keys (empty)", () =>
    expect(StringMap.keys(empty)) |> toEqual([])
  );

  test("keys (zyx)", () =>
    expect(StringMap.keys(zyx)) |> toEqual(["x", "y", "z"])
  );

  test("valueArray (empty)", () =>
    expect(StringMap.valueArray(empty)) |> toEqual([||])
  );

  test("valueArray (zyx)", () =>
    expect(StringMap.valueArray(zyx)) |> toEqual([|9, 3, 5|])
  );

  test("values (empty)", () =>
    expect(StringMap.values(empty)) |> toEqual([])
  );

  test("values (zyx)", () =>
    expect(StringMap.values(zyx)) |> toEqual([9, 3, 5])
  );

  test("minKey (empty)", () =>
    expect(StringMap.minKey(empty)) |> toEqual(None)
  );

  test("minKey (smallest of several)", () =>
    expect(StringMap.minKey(zyx)) |> toEqual(Some("x"))
  );

  test("maxKey (empty)", () =>
    expect(StringMap.maxKey(empty)) |> toEqual(None)
  );

  test("maxKey (largest of several)", () =>
    expect(StringMap.maxKey(zyx)) |> toEqual(Some("z"))
  );

  test("min (empty)", () =>
    expect(StringMap.min(empty)) |> toEqual(None)
  );

  test("min (smallest of several)", () =>
    expect(StringMap.min(zyx)) |> toEqual(Some(("x", 9)))
  );

  test("max (empty)", () =>
    expect(StringMap.max(empty)) |> toEqual(None)
  );

  test("max (largest of several)", () =>
    expect(StringMap.max(zyx)) |> toEqual(Some(("z", 5)))
  );

  test("get (not found)", () =>
    expect(StringMap.get("a", zyx)) |> toEqual(None)
  );

  test("get (found)", () =>
    expect(StringMap.get("x", zyx)) |> toEqual(Some(9))
  );

  test("getOrElse (not found, falls back to default)", () =>
    expect(StringMap.getOrElse("a", 0, zyx)) |> toEqual(0)
  );

  test("getOrElse (found, ignores default)", () =>
    expect(StringMap.getOrElse("x", 0, zyx)) |> toEqual(9)
  );

  test("remove (key not found)", () =>
    expect(StringMap.remove("a", zyx)) |> toEqual(zyx)
  );

  test("remove (key found in singleton)", () =>
    expect(StringMap.remove("foo", foo1)) |> toEqual(empty)
  );

  test("remove (key found)", () => {
    let withoutA = StringMap.fromList([("c", "d"), ("e", "f")]);
    expect(StringMap.(remove("a", abcdef) |> eqBy(String.eq, withoutA)))
    |> toEqual(true);
  });

  test("remove (doesn't mutate)", () => {
    let _ = StringMap.remove("a", abcdef);
    expect(StringMap.length(abcdef)) |> toEqual(3);
  });

  test("removeMany", () =>
    expect(StringMap.removeMany([|"a", "b", "x", "y", "z"|], zyx))
    |> toEqual(empty)
  );
});
