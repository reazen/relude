open Jest;
open Expect;

module Array = Relude.Array;
module Int = Relude.Int;

describe("Array", () => {
  test("length empty array", () =>
    expect(Array.length([||])) |> toEqual(0)
  );

  test("length non-empty array", () =>
    expect(Array.length([|1, 2, 3|])) |> toEqual(3)
  );

  test("isEmpty is true for empty array", () =>
    expect(Array.isEmpty([||])) |> toBe(true)
  );

  test("isEmpty is false for non-empty array", () =>
    expect(Array.isEmpty([|1|])) |> toBe(false)
  );

  test("isNotEmpty is false for empty array", () =>
    expect(Array.isNotEmpty([||])) |> toBe(false)
  );

  test("isNotEmpty is true for non-empty array", () =>
    expect(Array.isNotEmpty([|1|])) |> toBe(true)
  );

  test("empty is []", () =>
    expect(Array.empty) |> toEqual([||])
  );

  test("pure creates a one-item array", () =>
    expect(Array.pure(123)) |> toEqual([|123|])
  );

  test("repeat creates a array of n items", () =>
    expect(Array.repeat(3, "a")) |> toEqual([|"a", "a", "a"|])
  );

  test("makeWithIndex creates a array of n items using f", () =>
    expect(Array.makeWithIndex(3, i => i + 2)) |> toEqual([|2, 3, 4|])
  );

  test("makeWithIndex creates an empty array if given a negative count", () =>
    expect(Array.makeWithIndex(-1, i => i + 2)) |> toEqual([||])
  );

  test("concat", () =>
    expect(Array.concat([|1, 2|], [|3, 4|])) |> toEqual([|1, 2, 3, 4|])
  );

  test("cons", () =>
    expect(Array.cons(1, [|2, 3|])) |> toEqual([|1, 2, 3|])
  );

  test("uncons empty array", () =>
    expect(Array.uncons([||])) |> toEqual(None)
  );

  test("uncons single item array", () =>
    expect(Array.uncons([|1|])) |> toEqual(Some((1, [||])))
  );

  test("uncons multi item array", () =>
    expect(Array.uncons([|1, 2, 3|])) |> toEqual(Some((1, [|2, 3|])))
  );

  test("prepend", () =>
    expect(Array.prepend(1, [|2, 3|])) |> toEqual([|1, 2, 3|])
  );

  test("append", () =>
    expect(Array.append(1, [|2, 3|])) |> toEqual([|2, 3, 1|])
  );

  test("foldLeft", () =>
    expect(
      Array.foldLeft(
        (acc, curr) => Array.append(curr, acc),
        [||],
        [|1, 2, 3, 4, 5|],
      ),
    )
    |> toEqual([|1, 2, 3, 4, 5|])
  );

  test("foldLeft2", () =>
    expect(Array.foldLeft((acc, item) => acc - item, 0, [|1, 2, 3|]))
    |> toEqual(-6)
  );

  test("foldRight", () =>
    expect(
      Array.foldRight(
        (curr, acc) => Array.append(curr, acc),
        [||],
        [|1, 2, 3, 4, 5|],
      ),
    )
    |> toEqual([|5, 4, 3, 2, 1|])
  );

  test("scanLeft", () =>
    expect(
      Array.scanLeft(
        (acc, curr) => Array.append(curr, acc),
        [||],
        [|1, 2, 3, 4, 5|],
      ),
    )
    |> toEqual([|
         [|1|],
         [|1, 2|],
         [|1, 2, 3|],
         [|1, 2, 3, 4|],
         [|1, 2, 3, 4, 5|],
       |])
  );

  test("scanRight", () =>
    expect(
      Array.scanRight(
        (curr, acc) => Array.append(curr, acc),
        [||],
        [|1, 2, 3, 4, 5|],
      ),
    )
    |> toEqual([|
         [|5, 4, 3, 2, 1|],
         [|5, 4, 3, 2|],
         [|5, 4, 3|],
         [|5, 4|],
         [|5|],
       |])
  );

  test("at empty array", () =>
    expect(Array.at(0, [||])) |> toEqual(None)
  );

  test("at Some", () =>
    expect(Array.at(2, [|0, 10, 20, 30|])) |> toEqual(Some(20))
  );

  test("at None", () =>
    expect(Array.at(10, [|0, 10, 20, 30|])) |> toEqual(None)
  );

  /* Apparently setAt 0 doesn't work on an empty array... */
  test("setAt empty array", () =>
    expect(Array.setAt(0, "a", [||])) |> toEqual(None)
  );

  test("setAt valid index in non-empty array", () =>
    expect(Array.setAt(1, "a", [|"0", "1", "2"|]))
    |> toEqual(Some([|"0", "a", "2"|]))
  );

  test("setAt invalid index in non-empty array", () =>
    expect(Array.setAt(5, "a", [|"0", "1", "2"|])) |> toEqual(None)
  );

  test("head empty array", () =>
    expect(Array.head([||])) |> toEqual(None)
  );

  test("head single item array", () =>
    expect(Array.head([|1|])) |> toEqual(Some(1))
  );

  test("head multi-item array", () =>
    expect(Array.head([|1, 2, 3|])) |> toEqual(Some(1))
  );

  test("tail empty array", () =>
    expect(Array.tail([||])) |> toEqual(None)
  );

  test("tail single item array", () =>
    expect(Array.tail([|1|])) |> toEqual(Some([||]))
  );

  test("tail multi-item array", () =>
    expect(Array.tail([|1, 2, 3|])) |> toEqual(Some([|2, 3|]))
  );

  test("tailOrEmpty empty array", () =>
    expect(Array.tailOrEmpty([||])) |> toEqual([||])
  );

  test("tailOrEmpty single item array", () =>
    expect(Array.tailOrEmpty([|1|])) |> toEqual([||])
  );

  test("tailOrEmpty multi-item array", () =>
    expect(Array.tailOrEmpty([|1, 2, 3|])) |> toEqual([|2, 3|])
  );

  test("init empty array", () =>
    expect(Array.init([||])) |> toEqual(None)
  );

  test("init single item array", () =>
    expect(Array.init([|1|])) |> toEqual(Some([||]))
  );

  test("init multi item array", () =>
    expect(Array.init([|1, 2, 3, 4|])) |> toEqual(Some([|1, 2, 3|]))
  );

  test("initOrEmpty empty array", () =>
    expect(Array.initOrEmpty([||])) |> toEqual([||])
  );

  test("initOrEmpty single item array", () =>
    expect(Array.initOrEmpty([|1|])) |> toEqual([||])
  );

  test("initOrEmpty multi item array", () =>
    expect(Array.initOrEmpty([|1, 2, 3, 4|])) |> toEqual([|1, 2, 3|])
  );

  test("last empty array", () =>
    expect(Array.last([||])) |> toEqual(None)
  );

  test("last single item array", () =>
    expect(Array.last([|1|])) |> toEqual(Some(1))
  );

  test("last multi item array", () =>
    expect(Array.last([|1, 2, 3, 4|])) |> toEqual(Some(4))
  );

  test("take zero from empty array", () =>
    expect(Array.take(0, [||])) |> toEqual([||])
  );

  test("take non-zero from empty array", () =>
    expect(Array.take(2, [||])) |> toEqual([||])
  );

  test("take non-zero from short array", () =>
    expect(Array.take(2, [|1|])) |> toEqual([|1|])
  );

  test("take non-zero from equal array", () =>
    expect(Array.take(2, [|1, 2|])) |> toEqual([|1, 2|])
  );

  test("take non-zero from long array", () =>
    expect(Array.take(2, [|1, 2, 3|])) |> toEqual([|1, 2|])
  );

  test("takeExactly zero from empty array", () =>
    expect(Array.takeExactly(0, [||])) |> toEqual(Some([||]))
  );

  test("takeExactly non-zero from empty array", () =>
    expect(Array.takeExactly(2, [||])) |> toEqual(None)
  );

  test("takeExactly non-zero from short array", () =>
    expect(Array.takeExactly(2, [|1|])) |> toEqual(None)
  );

  test("takeExactly non-zero from equal array", () =>
    expect(Array.takeExactly(2, [|1, 2|])) |> toEqual(Some([|1, 2|]))
  );

  test("takeExactly non-zero from long array", () =>
    expect(Array.takeExactly(2, [|1, 2, 3|])) |> toEqual(Some([|1, 2|]))
  );

  test("takeWhile empty array", () =>
    expect(Array.takeWhile(a => a < 2, [||])) |> toEqual([||])
  );

  test("takeWhile array", () =>
    expect(Array.takeWhile(a => a < 2, [|0, 1, 2, 3|]))
    |> toEqual([|0, 1|])
  );

  test("takeWhile array condition never true", () =>
    expect(Array.takeWhile(a => a < 0, [|0, 1, 2, 3|])) |> toEqual([||])
  );

  test("takeWhile array condition never false", () =>
    expect(Array.takeWhile(a => a < 10, [|0, 1, 2, 3|]))
    |> toEqual([|0, 1, 2, 3|])
  );

  test("drop zero from empty array", () =>
    expect(Array.drop(0, [||])) |> toEqual([||])
  );

  test("drop some from short array ", () =>
    expect(Array.drop(1, [|1, 2|])) |> toEqual([|2|])
  );

  test("drop some from equal array ", () =>
    expect(Array.drop(2, [|1, 2|])) |> toEqual([||])
  );

  test("drop some from long array ", () =>
    expect(Array.drop(2, [|1, 2, 3, 4|])) |> toEqual([|3, 4|])
  );

  test("drop more from long array ", () =>
    expect(Array.drop(5, [|1, 2, 3, 4|])) |> toEqual([||])
  );

  test("dropExactly zero from empty array", () =>
    expect(Array.dropExactly(0, [||])) |> toEqual(Some([||]))
  );

  test("dropExactly some from short array ", () =>
    expect(Array.dropExactly(1, [|1, 2|])) |> toEqual(Some([|2|]))
  );

  test("dropExactly some from equal array ", () =>
    expect(Array.dropExactly(2, [|1, 2|])) |> toEqual(Some([||]))
  );

  test("dropExactly some from long array ", () =>
    expect(Array.dropExactly(2, [|1, 2, 3, 4|]))
    |> toEqual(Some([|3, 4|]))
  );

  test("dropExactly more from long array ", () =>
    expect(Array.dropExactly(5, [|1, 2, 3, 4|])) |> toEqual(None)
  );

  test("dropWhile empty array", () =>
    expect(Array.dropWhile(a => a < 2, [||])) |> toEqual([||])
  );

  test("dropWhile array", () =>
    expect(Array.dropWhile(a => a < 2, [|0, 1, 2, 1, 3|]))
    |> toEqual([|2, 1, 3|])
  );

  test("dropWhile array condition never true", () =>
    expect(Array.dropWhile(a => a < 0, [|0, 1, 2, 3|]))
    |> toEqual([|0, 1, 2, 3|])
  );

  test("dropWhile array condition never false", () =>
    expect(Array.dropWhile(a => a < 10, [|0, 1, 2, 3|])) |> toEqual([||])
  );

  test("filter", () =>
    expect(
      Array.filter(
        i => i mod 2 == 0,
        [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 10|],
      ),
    )
    |> toEqual([|2, 4, 6, 8, 10, 10|])
  );

  test("filterWithIndex", () =>
    expect(
      Array.filterWithIndex(
        (v, i) => i mod 2 == 0 || v == 3,
        [|1, 3, 3, 4, 5, 6|],
      ),
    )
    |> toEqual([|1, 3, 3, 5|])
  );

  test("filterNot", () =>
    expect(Array.filterNot(x => x mod 2 == 0, [|1, 2, 3, 4|]))
    |> toEqual([|1, 3|])
  );

  test("filterNotWithIndex", () =>
    expect(Array.filterNotWithIndex((_, i) => i > 2, [|1, 2, 3, 4|]))
    |> toEqual([|1, 2, 3|])
  );

  test("find not found", () =>
    expect(Array.find(a => a == 3, [|0, 1, 2|])) |> toEqual(None)
  );

  test("find found", () =>
    expect(Array.find(a => a == 2, [|0, 1, 2, 3, 4|])) |> toEqual(Some(2))
  );

  test("findWithIndex", () =>
    expect(Array.findWithIndex((a, i) => a == 3 || i == 2, [|0, 1, 2, 3|]))
    |> toEqual(Some(2))
  );

  test("partition", () =>
    expect(Array.partition(a => a mod 2 == 0, [|0, 1, 2, 3, 4, 5|]))
    |> toEqual(([|0, 2, 4|], [|1, 3, 5|]))
  );

  test("splitAt", () =>
    expect(Array.splitAt(3, [|0, 1, 2, 3, 4, 5|]))
    |> toEqual(Some(([|0, 1, 2|], [|3, 4, 5|])))
  );

  test("prependToAll", () =>
    expect(Array.prependToAll(0, [|0, 1, 2|]))
    |> toEqual([|0, 0, 0, 1, 0, 2|])
  );

  test("prependToAll (empty array)", () =>
    expect(Array.prependToAll(0, [||])) |> toEqual([||])
  );

  test("intersperse", () =>
    expect(Array.intersperse(",", [|"a", "b", "c"|]))
    |> toEqual([|"a", ",", "b", ",", "c"|])
  );

  test("intersperse (empty array)", () =>
    expect(Array.intersperse(",", [||])) |> toEqual([||])
  );

  test("replicate", () =>
    expect(Array.replicate(3, [|"a", "b", "c"|]))
    |> toEqual([|"a", "b", "c", "a", "b", "c", "a", "b", "c"|])
  );

  test("replicate zero", () =>
    expect(Array.replicate(0, [|"a", "b", "c"|])) |> toEqual([||])
  );

  test("replicate negative", () =>
    expect(Array.replicate(-3, [|"a", "b", "c"|])) |> toEqual([||])
  );

  test("zip same length array", () =>
    expect(Array.zip([|1, 2, 3|], [|"4", "5", "6"|]))
    |> toEqual([|(1, "4"), (2, "5"), (3, "6")|])
  );

  test("zip different length array", () =>
    expect(Array.zip([|1, 2, 3|], [|"4", "5"|]))
    |> toEqual([|(1, "4"), (2, "5")|])
  );

  test("zipWith", () =>
    expect(Array.zipWith((a, b) => a + b, [|1, 2, 3|], [|4, 5, 6|]))
    |> toEqual([|5, 7, 9|])
  );

  test("zipWithIndex", () =>
    expect(Array.zipWithIndex([|"a", "b", "c"|]))
    |> toEqual([|("a", 0), ("b", 1), ("c", 2)|])
  );

  test("unzip", () =>
    expect(Array.unzip([|(1, 2), (3, 4), (5, 6)|]))
    |> toEqual(([|1, 3, 5|], [|2, 4, 6|]))
  );

  test("sortWithInt", () =>
    expect(Array.sortWithInt(Int.compareAsInt, [|2, 0, 1, 3, 5, (-1)|]))
    |> toEqual([|(-1), 0, 1, 2, 3, 5|])
  );

  test("sortBy", () =>
    expect(Array.sortBy(Int.compare, [|2, 0, 1, 3, 5, (-1)|]))
    |> toEqual([|(-1), 0, 1, 2, 3, 5|])
  );

  test("sort", () =>
    expect(Array.sort((module Int.Ord), [|2, 0, 1, 3, 5, (-1)|]))
    |> toEqual([|(-1), 0, 1, 2, 3, 5|])
  );

  test("reverse", () =>
    expect(Array.reverse([|1, 2, 3, 4, 5|])) |> toEqual([|5, 4, 3, 2, 1|])
  );

  test("containsBy false", () =>
    expect(Array.containsBy(Int.eq, 10, [|0, 1, 2, 3, 4|]))
    |> toEqual(false)
  );

  test("indexOfBy success", () =>
    expect(Array.indexOfBy(Int.eq, 30, [|0, 10, 20, 30, 40|]))
    |> toEqual(Some(3))
  );

  test("indexOfBy failure", () =>
    expect(Array.indexOfBy(Int.eq, 500, [|0, 10, 20, 30, 40|]))
    |> toEqual(None)
  );

  test("containsBy true", () =>
    expect(Array.containsBy(Int.eq, 3, [|0, 1, 2, 3, 4|])) |> toEqual(true)
  );

  test("any empty", () =>
    expect(Array.any(a => a > 2, [||])) |> toEqual(false)
  );

  test("any true", () =>
    expect(Array.any(a => a > 2, [|0, 1, 2, 3|])) |> toEqual(true)
  );

  test("any false", () =>
    expect(Array.any(a => a > 10, [|0, 1, 2, 3|])) |> toEqual(false)
  );

  test("all empty", () =>
    expect(Array.all(a => a > 2, [||])) |> toEqual(true)
  );

  test("all true", () =>
    expect(Array.all(a => a > (-1), [|0, 1, 2, 3|])) |> toEqual(true)
  );

  test("all false", () =>
    expect(Array.all(a => a < 3, [|0, 1, 2, 3|])) |> toEqual(false)
  );

  test("distinctBy", () =>
    expect(Array.distinctBy(Int.eq, [|6, 1, 1, 2, 1, 3, 2, 3, 2, 4, 5, 5|]))
    |> toEqual([|6, 1, 2, 3, 4, 5|])
  );

  test("map", () =>
    expect(Array.map(a => a + 2, [|1, 2, 3|])) |> toEqual([|3, 4, 5|])
  );

  test("mapWithIndex", () =>
    expect(
      Array.mapWithIndex(
        (v, i) => v ++ string_of_int(i),
        [|"a", "b", "c"|],
      ),
    )
    |> toEqual([|"a0", "b1", "c2"|])
  );

  test("apply", () =>
    expect(Array.apply([|a => a + 2, a => a * 3|], [|1, 2, 3|]))
    |> toEqual([|3, 4, 5, 3, 6, 9|])
  );

  test("bind", () =>
    expect(Array.bind([|1, 2, 3|], a => [|a, a|]))
    |> toEqual([|1, 1, 2, 2, 3, 3|])
  );

  test("flatten", () =>
    expect(Array.flatten([|[|1, 2, 3|], [|4, 5|]|]))
    |> toEqual([|1, 2, 3, 4, 5|])
  );

  test("fromList", () =>
    expect(Array.fromList([1, 2, 3])) |> toEqual([|1, 2, 3|])
  );

  test("toList", () =>
    expect(Array.toList([|1, 2, 3|])) |> toEqual([1, 2, 3])
  );

  test("eqBy returns true if array items are equal", () =>
    expect(Array.eqBy(Int.eq, [|1, 2, 3|], [|1, 2, 3|])) |> toBe(true)
  );

  test("eqBy returns false if array items are not equal", () =>
    expect(Array.eqBy(Int.eq, [|1, 2, 3|], [|1, 2, 4|])) |> toBe(false)
  );

  test("eqBy returns false if array are of different sizes", () =>
    expect(Array.eqBy(Int.eq, [|1|], [|1, 2|])) |> toBe(false)
  );

  test("eq returns true if array items are equal", () =>
    expect(Array.eq((module Int.Eq), [|1, 2, 3|], [|1, 2, 3|]))
    |> toBe(true)
  );

  test("eq returns false if array items are not equal", () =>
    expect(Array.eq((module Int.Eq), [|1, 2, 3|], [|1, 2, 4|]))
    |> toBe(false)
  );

  test("Array.String.joinWith", () =>
    expect(Array.String.joinWith(", ", [|"a", "b", "c"|]))
    |> toEqual("a, b, c")
  );

  test("showBy", () =>
    expect(Array.showBy(string_of_int, [|1, 2, 3|]))
    |> toEqual("[1, 2, 3]")
  );

  test("alt", () =>
    expect(Array.Alt.alt([|"a", "b"|], [|"c"|]))
    |> toEqual([|"a", "b", "c"|])
  );

  test("<|> is associative", () => {
    let (<|>) = Array.Infix.(<|>);
    expect([|0, 1|] <|> [|2, 3|] <|> [|4, 5|])
    |> toEqual([|0, 1|] <|> ([|2, 3|] <|> [|4, 5|]));
  });

  test("void", () =>
    expect(Array.void([|1, 2, 3|])) |> toEqual([|(), (), ()|])
  );

  test("flap", () =>
    expect(Array.flap([|a => a + 1, a => a + 2, a => a + 3|], 5))
    |> toEqual([|6, 7, 8|])
  );

  test("map2", () =>
    expect(Array.map2((a, b) => a + b, [|1, 2|], [|3, 4|]))
    |> toEqual([|4, 5, 5, 6|])
  );

  test("map3", () =>
    expect(
      Array.map3((a, b, c) => a + b + c, [|1, 2|], [|3, 4|], [|10, 20|]),
    )
    |> toEqual([|14, 24, 15, 25, 15, 25, 16, 26|])
  );

  test("map4", () =>
    expect(
      Array.map4(
        (a, b, c, d) => a + b + c + d,
        [|1, 2|],
        [|3, 4|],
        [|10, 20|],
        [|100, 200|],
      ),
    )
    |> toEqual([|
         114,
         214,
         124,
         224,
         115,
         215,
         125,
         225,
         115,
         215,
         125,
         225,
         116,
         216,
         126,
         226,
       |])
  );
});
