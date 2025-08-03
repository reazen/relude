open Jest;
open Expect;

module Int = Relude.Int;
module List = Relude.List;
module IO = Relude.IO;

describe("List", () => {
  test("length empty list", () =>
    expect(List.length([])) |> toEqual(0)
  );

  test("length non-empty list", () =>
    expect(List.length([1, 2, 3])) |> toEqual(3)
  );

  test("isEmpty is true for empty list", () =>
    expect(List.isEmpty([])) |> toBe(true)
  );

  test("isEmpty is false for non-empty list", () =>
    expect(List.isEmpty([1])) |> toBe(false)
  );

  test("isNotEmpty is false for empty list", () =>
    expect(List.isNotEmpty([])) |> toBe(false)
  );

  test("isNotEmpty is true for non-empty list", () =>
    expect(List.isNotEmpty([1])) |> toBe(true)
  );

  test("empty is []", () =>
    expect(List.empty) |> toEqual([])
  );

  test("pure creates a one-item list", () =>
    expect(List.pure(123)) |> toEqual([123])
  );

  test("repeat creates a list of n items", () =>
    expect(List.repeat(3, "a")) |> toEqual(["a", "a", "a"])
  );

  test("repeat (negative count is empty)", () =>
    expect(List.repeat(-1, "a")) |> toEqual([])
  );

  test("makeWithIndex creates a list of n items using f", () =>
    expect(List.makeWithIndex(3, i => i + 2)) |> toEqual([2, 3, 4])
  );

  test("concat", () =>
    expect(List.concat([1, 2], [3, 4])) |> toEqual([1, 2, 3, 4])
  );

  test("concatNamed", () =>
    expect(List.concatNamed(~prefix=[1, 2], [3, 4]))
    |> toEqual([1, 2, 3, 4])
  );

  test("guard true", () =>
    expect(List.guard(true, [1, 2, 3])) |> toEqual([1, 2, 3])
  );

  test("guard false", () =>
    expect(List.guard(false, [1, 2, 3])) |> toEqual([])
  );

  test("power", () =>
    expect(List.power([1, 2, 3], 3))
    |> toEqual([1, 2, 3, 1, 2, 3, 1, 2, 3])
  );

  test("cons", () =>
    expect(List.cons(1, [2, 3])) |> toEqual([1, 2, 3])
  );

  test("uncons empty list", () =>
    expect(List.uncons([])) |> toEqual(None)
  );

  test("uncons single item list", () =>
    expect(List.uncons([1])) |> toEqual(Some((1, [])))
  );

  test("uncons multi item list", () =>
    expect(List.uncons([1, 2, 3])) |> toEqual(Some((1, [2, 3])))
  );

  test("prepend", () =>
    expect(List.prepend(1, [2, 3])) |> toEqual([1, 2, 3])
  );

  test("append", () =>
    expect(List.append(1, [2, 3])) |> toEqual([2, 3, 1])
  );

  test("appendOption Some appends element", () =>
    expect(List.appendOption(Some(1), [2, 3])) |> toEqual([2, 3, 1])
  );

  test("appendOption None no-ops", () =>
    expect(List.appendOption(None, [2, 3])) |> toEqual([2, 3])
  );

  test("consOption Some conss element", () =>
    expect(List.consOption(Some(1), [2, 3])) |> toEqual([1, 2, 3])
  );

  test("consOption None no-ops", () =>
    expect(List.consOption(None, [2, 3])) |> toEqual([2, 3])
  );

  test("foldLeft", () =>
    expect(
      List.foldLeft(
        (acc, curr) => List.append(curr, acc),
        [],
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("foldRight", () =>
    expect(
      List.foldRight(
        (curr, acc) => List.append(curr, acc),
        [],
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual([5, 4, 3, 2, 1])
  );

  test("unfold", () =>
    expect(
      List.unfold(
        x =>
          if (x > 5) {
            None;
          } else {
            Some((x, x + 1));
          },
        0,
      ),
    )
    |> toEqual([0, 1, 2, 3, 4, 5])
  );

  test("scanLeft", () =>
    expect(
      List.scanLeft(
        (acc, curr) => List.append(curr, acc),
        [],
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual([[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]])
  );

  test("scanRight", () =>
    expect(
      List.scanRight(
        (curr, acc) => List.append(curr, acc),
        [],
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual([[5, 4, 3, 2, 1], [5, 4, 3, 2], [5, 4, 3], [5, 4], [5]])
  );

  test("at empty list", () =>
    expect(List.at(0, [])) |> toEqual(None)
  );

  test("at success", () =>
    expect(List.at(2, [0, 10, 20, 30])) |> toEqual(Some(20))
  );

  test("at failure", () =>
    expect(List.at(10, [0, 10, 20, 30])) |> toEqual(None)
  );

  test("head empty list", () =>
    expect(List.head([])) |> toEqual(None)
  );

  test("head single item list", () =>
    expect(List.head([1])) |> toEqual(Some(1))
  );

  test("head multi-item list", () =>
    expect(List.head([1, 2, 3])) |> toEqual(Some(1))
  );

  test("tail empty list", () =>
    expect(List.tail([])) |> toEqual(None)
  );

  test("tail single item list", () =>
    expect(List.tail([1])) |> toEqual(Some([]))
  );

  test("tail multi-item list", () =>
    expect(List.tail([1, 2, 3])) |> toEqual(Some([2, 3]))
  );

  test("tailOrEmpty empty list", () =>
    expect(List.tailOrEmpty([])) |> toEqual([])
  );

  test("tailOrEmpty single item list", () =>
    expect(List.tailOrEmpty([1])) |> toEqual([])
  );

  test("tailOrEmpty multi-item list", () =>
    expect(List.tailOrEmpty([1, 2, 3])) |> toEqual([2, 3])
  );

  test("init empty list", () =>
    expect(List.init([])) |> toEqual(None)
  );

  test("init single item list", () =>
    expect(List.init([1])) |> toEqual(Some([]))
  );

  test("init multi item list", () =>
    expect(List.init([1, 2, 3, 4])) |> toEqual(Some([1, 2, 3]))
  );

  test("initOrEmpty empty list", () =>
    expect(List.initOrEmpty([])) |> toEqual([])
  );

  test("initOrEmpty single item list", () =>
    expect(List.initOrEmpty([1])) |> toEqual([])
  );

  test("initOrEmpty multi item list", () =>
    expect(List.initOrEmpty([1, 2, 3, 4])) |> toEqual([1, 2, 3])
  );

  test("last empty list", () =>
    expect(List.last([])) |> toEqual(None)
  );

  test("last single item list", () =>
    expect(List.last([1])) |> toEqual(Some(1))
  );

  test("last multi item list", () =>
    expect(List.last([1, 2, 3, 4])) |> toEqual(Some(4))
  );

  test("take zero from empty list", () =>
    expect(List.take(0, [])) |> toEqual([])
  );

  test("take non-zero from empty list", () =>
    expect(List.take(2, [])) |> toEqual([])
  );

  test("take non-zero from short list", () =>
    expect(List.take(2, [1])) |> toEqual([1])
  );

  test("take non-zero from equal list", () =>
    expect(List.take(2, [1, 2])) |> toEqual([1, 2])
  );

  test("take non-zero from long list", () =>
    expect(List.take(2, [1, 2, 3])) |> toEqual([1, 2])
  );

  test("takeExactly negative from list", () =>
    expect(List.takeExactly(-2, [])) |> toEqual(None)
  );

  test("takeExactly zero from empty list", () =>
    expect(List.takeExactly(0, [])) |> toEqual(Some([]))
  );

  test("takeExactly non-zero from empty list", () =>
    expect(List.takeExactly(2, [])) |> toEqual(None)
  );

  test("takeExactly non-zero from short list", () =>
    expect(List.takeExactly(2, [1])) |> toEqual(None)
  );

  test("takeExactly non-zero from equal list", () =>
    expect(List.takeExactly(2, [1, 2])) |> toEqual(Some([1, 2]))
  );

  test("takeExactly non-zero from long list", () =>
    expect(List.takeExactly(2, [1, 2, 3])) |> toEqual(Some([1, 2]))
  );

  test("takeWhile empty list", () =>
    expect(List.takeWhile(a => a < 2, [])) |> toEqual([])
  );

  test("takeWhile list", () =>
    expect(List.takeWhile(a => a < 2, [0, 1, 2, 3])) |> toEqual([0, 1])
  );

  test("takeWhile list condition never true", () =>
    expect(List.takeWhile(a => a < 0, [0, 1, 2, 3])) |> toEqual([])
  );

  test("takeWhile list condition never false", () =>
    expect(List.takeWhile(a => a < 10, [0, 1, 2, 3]))
    |> toEqual([0, 1, 2, 3])
  );

  test("drop negative from empty list", () =>
    expect(List.drop(-2, [])) |> toEqual([])
  );

  test("drop negative from nonempty list", () =>
    expect(List.drop(-2, [1, 2])) |> toEqual([1, 2])
  );

  test("drop zero from empty list", () =>
    expect(List.drop(0, [])) |> toEqual([])
  );

  test("drop zero from nonempty list", () =>
    expect(List.drop(0, [1, 2])) |> toEqual([1, 2])
  );

  test("drop some from short list ", () =>
    expect(List.drop(1, [1, 2])) |> toEqual([2])
  );

  test("drop some from equal list ", () =>
    expect(List.drop(2, [1, 2])) |> toEqual([])
  );

  test("drop some from long list ", () =>
    expect(List.drop(3, [1, 2, 3, 4, 5])) |> toEqual([4, 5])
  );

  test("drop more from long list ", () =>
    expect(List.drop(5, [1, 2, 3, 4])) |> toEqual([])
  );

  test("dropWhile empty list", () =>
    expect(List.dropWhile(a => a < 2, [])) |> toEqual([])
  );

  test("dropExactly zero from empty list", () =>
    expect(List.dropExactly(0, [])) |> toEqual(Some([]))
  );

  test("dropExactly some from short list ", () =>
    expect(List.dropExactly(1, [1, 2])) |> toEqual(Some([2]))
  );

  test("dropExactly some from equal list ", () =>
    expect(List.dropExactly(2, [1, 2])) |> toEqual(Some([]))
  );

  test("dropExactly some from long list ", () =>
    expect(List.dropExactly(2, [1, 2, 3, 4])) |> toEqual(Some([3, 4]))
  );

  test("dropExactly more from long list ", () =>
    expect(List.dropExactly(5, [1, 2, 3, 4])) |> toEqual(None)
  );

  test("dropWhile list", () =>
    expect(List.dropWhile(a => a < 2, [0, 1, 2, 1, 3]))
    |> toEqual([2, 1, 3])
  );

  test("dropWhile list condition never true", () =>
    expect(List.dropWhile(a => a < 0, [0, 1, 2, 3]))
    |> toEqual([0, 1, 2, 3])
  );

  test("dropWhile list condition never false", () =>
    expect(List.dropWhile(a => a < 10, [0, 1, 2, 3])) |> toEqual([])
  );

  test("filter", () =>
    expect(
      List.filter(i => i mod 2 == 0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 10]),
    )
    |> toEqual([2, 4, 6, 8, 10, 10])
  );

  test("filterWithIndex", () =>
    expect(
      List.filterWithIndex(
        (v, i) => i mod 2 == 0 || v == 3,
        [1, 3, 3, 4, 5, 6],
      ),
    )
    |> toEqual([1, 3, 3, 5])
  );

  test("filterNot", () =>
    expect(List.filterNot(x => x mod 2 == 0, [1, 2, 3, 4]))
    |> toEqual([1, 3])
  );

  test("filterNotWithIndex", () =>
    expect(List.filterNotWithIndex((_, idx) => idx > 2, [0, 0, 0, 0]))
    |> toEqual([0, 0, 0])
  );

  test("find not found", () =>
    expect(List.find(a => a == 3, [0, 1, 2])) |> toEqual(None)
  );

  test("find found", () =>
    expect(List.find(a => a == 2, [0, 1, 2, 3, 4])) |> toEqual(Some(2))
  );

  test("findWithIndex", () =>
    expect(List.findWithIndex((a, i) => a == 3 || i == 2, [0, 1, 2, 3]))
    |> toEqual(Some(2))
  );

  test("partition", () =>
    expect(List.partition(a => a mod 2 == 0, [0, 1, 2, 3, 4, 5]))
    |> toEqual(([0, 2, 4], [1, 3, 5]))
  );

  test("splitAt", () =>
    expect(List.splitAt(3, [0, 1, 2, 3, 4, 5]))
    |> toEqual(Some(([0, 1, 2], [3, 4, 5])))
  );

  test("prependToAll", () =>
    expect(List.prependToAll(0, [0, 1, 2])) |> toEqual([0, 0, 0, 1, 0, 2])
  );

  test("intersperse", () =>
    expect(List.intersperse(",", ["a", "b", "c"]))
    |> toEqual(["a", ",", "b", ",", "c"])
  );

  test("intersperse is tail recursive", () =>
    expect(List.(repeat(20000, 0) |> intersperse(1) |> length))
    |> toEqual(39999)
  );

  test("replicate", () =>
    expect(List.replicate(3, ["a", "b", "c"]))
    |> toEqual(["a", "b", "c", "a", "b", "c", "a", "b", "c"])
  );

  test("replicate once", () =>
    expect(List.replicate(1, ["foo"])) |> toEqual(["foo"])
  );

  test("replicate zero", () =>
    expect(List.replicate(0, ["none"])) |> toEqual([])
  );

  test("replicate negative", () =>
    expect(List.replicate(-1, [0])) |> toEqual([])
  );

  test("replicate empty list", () =>
    expect(List.replicate(10, [])) |> toEqual([])
  );

  test("zip same length lists", () =>
    expect(List.zip([1, 2, 3], ["4", "5", "6"]))
    |> toEqual([(1, "4"), (2, "5"), (3, "6")])
  );

  test("zip different length lists", () =>
    expect(List.zip([1, 2, 3], ["4", "5"]))
    |> toEqual([(1, "4"), (2, "5")])
  );

  test("zipWith", () =>
    expect(List.zipWith((a, b) => a + b, [1, 2, 3], [4, 5, 6]))
    |> toEqual([5, 7, 9])
  );

  test("zipWithIndex", () =>
    expect(List.zipWithIndex(["a", "b", "c"]))
    |> toEqual([("a", 0), ("b", 1), ("c", 2)])
  );

  test("unzip", () =>
    expect(List.unzip([(1, "a"), (2, "b"), (3, "c")]))
    |> toEqual(([1, 2, 3], ["a", "b", "c"]))
  );

  test("sortWithInt", () =>
    expect(List.sortWithInt(Int.compareAsInt, [2, 0, 1, 3, 5, (-1)]))
    |> toEqual([(-1), 0, 1, 2, 3, 5])
  );

  test("sortBy", () =>
    expect(List.sortBy(Int.compare, [2, 0, 1, 3, 5, (-1)]))
    |> toEqual([(-1), 0, 1, 2, 3, 5])
  );

  test("sort", () =>
    expect(List.sort((module Int.Ord), [2, 0, 1, 3, 5, (-1)]))
    |> toEqual([(-1), 0, 1, 2, 3, 5])
  );

  test("reverse", () =>
    expect(List.reverse([1, 2, 3, 4, 5])) |> toEqual([5, 4, 3, 2, 1])
  );

  test("contains false", () =>
    expect(List.containsBy(Int.eq, 10, [0, 1, 2, 3, 4])) |> toEqual(false)
  );

  test("contains true", () =>
    expect(List.containsBy(Int.eq, 3, [0, 1, 2, 3, 4])) |> toEqual(true)
  );

  test("indexOfBy failure", () =>
    expect(List.indexOfBy(Int.eq, 500, [0, 10, 20, 30, 40]))
    |> toEqual(None)
  );

  test("indexOfBy success", () =>
    expect(List.indexOfBy(Int.eq, 30, [0, 10, 20, 30, 40]))
    |> toEqual(Some(3))
  );

  test("indexOf success", () =>
    expect(List.indexOf((module Int.Eq), 30, [0, 10, 20, 30, 40]))
    |> toEqual(Some(3))
  );

  test("indexOf failure", () =>
    expect(List.indexOf((module Int.Eq), 500, [0, 10, 20, 30, 40]))
    |> toEqual(None)
  );

  test("minBy empty", () =>
    expect(List.minBy(Int.Ord.compare, [])) |> toEqual(None)
  );

  test("minBy one", () =>
    expect(List.minBy(Int.Ord.compare, [0])) |> toEqual(Some(0))
  );

  test("minBy many", () =>
    expect(List.minBy(Int.Ord.compare, [3, 1, 2])) |> toEqual(Some(1))
  );

  test("min empty", () =>
    expect(List.min((module Relude.String.Ord), [])) |> toEqual(None)
  );

  test("min many", () =>
    expect(List.min((module Relude.String.Ord), ["b", "a", "c"]))
    |> toEqual(Some("a"))
  );

  test("maxBy empty", () =>
    expect(List.maxBy(Int.Ord.compare, [])) |> toEqual(None)
  );

  test("maxBy one", () =>
    expect(List.maxBy(Int.Ord.compare, [0])) |> toEqual(Some(0))
  );

  test("maxBy many", () =>
    expect(List.maxBy(Int.Ord.compare, [3, 1, 2])) |> toEqual(Some(3))
  );

  test("max empty", () =>
    expect(List.max((module Int.Ord), [])) |> toEqual(None)
  );

  test("max many", () =>
    expect(List.max((module Int.Ord), [3, 1, 2])) |> toEqual(Some(3))
  );

  test("any empty", () =>
    expect(List.any(a => a > 2, [])) |> toEqual(false)
  );

  test("any true", () =>
    expect(List.any(a => a > 2, [0, 1, 2, 3])) |> toEqual(true)
  );

  test("any false", () =>
    expect(List.any(a => a > 10, [0, 1, 2, 3])) |> toEqual(false)
  );

  test("all empty", () =>
    expect(List.all(a => a > 2, [])) |> toEqual(true)
  );

  test("all true", () =>
    expect(List.all(a => a > (-1), [0, 1, 2, 3])) |> toEqual(true)
  );

  test("all false", () =>
    expect(List.all(a => a < 3, [0, 1, 2, 3])) |> toEqual(false)
  );

  test("removeFirstBy empty", () =>
    expect(List.removeFirstBy(Int.eq, 0, [])) |> toEqual([])
  );

  test("removeFirstBy single match", () =>
    expect(List.removeFirstBy(Int.eq, 0, [0])) |> toEqual([])
  );

  test("removeFirstBy single not a match", () =>
    expect(List.removeFirstBy(Int.eq, 0, [1])) |> toEqual([1])
  );

  test("removeFirstBy one match at beginning", () =>
    expect(List.removeFirstBy(Int.eq, 0, [0, 1, 2])) |> toEqual([1, 2])
  );

  test("removeFirstBy one match at end", () =>
    expect(List.removeFirstBy(Int.eq, 2, [0, 1, 2])) |> toEqual([0, 1])
  );

  test("removeFirstBy many matches", () =>
    expect(List.removeFirstBy(Int.eq, 0, [1, 0, 2, 0]))
    |> toEqual([1, 2, 0])
  );

  test("removeEachBy empty", () =>
    expect(List.removeEachBy(Int.eq, 0, [])) |> toEqual([])
  );

  test("removeEachBy single match", () =>
    expect(List.removeEachBy(Int.eq, 0, [0])) |> toEqual([])
  );

  test("removeEachBy single not a match", () =>
    expect(List.removeEachBy(Int.eq, 0, [1])) |> toEqual([1])
  );

  test("removeEachBy many matches removed", () =>
    expect(List.removeEachBy(Int.eq, 0, [0, 2, 0, 4, 0]))
    |> toEqual([2, 4])
  );

  test("replaceAt", () =>
    expect(List.replaceAt(2, 100, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 100, 4, 5])
  );

  test("insertAt first", () =>
    expect(List.insertAt(0, 100, [1, 2, 3, 4, 5]))
    |> toEqual([100, 1, 2, 3, 4, 5])
  );

  test("insertAt within range", () =>
    expect(List.insertAt(2, 100, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 100, 3, 4, 5])
  );

  test("insertAt last", () =>
    expect(List.insertAt(5, 100, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 3, 4, 5, 100])
  );

  test("insertAt out of range", () =>
    expect(List.insertAt(6, 100, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("updateAt within range", () =>
    expect(List.updateAt(2, x => x + 1, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 4, 4, 5])
  );

  test("updateAt out of range", () =>
    expect(List.updateAt(6, x => x + 1, [1, 2, 3, 4, 5]))
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("swapAt within range", () =>
    expect(List.swapAt(0, 4, [1, 2, 3, 4, 5])) |> toEqual([5, 2, 3, 4, 1])
  );

  test("swapAt out of range", () =>
    expect(List.swapAt(0, 5, [1, 2, 3, 4, 5])) |> toEqual([1, 2, 3, 4, 5])
  );

  test("removeAt within range", () =>
    expect(List.removeAt(1, [1, 2, 3, 4, 5])) |> toEqual([1, 3, 4, 5])
  );

  test("removeAt out of range", () =>
    expect(List.removeAt(5, [1, 2, 3, 4, 5])) |> toEqual([1, 2, 3, 4, 5])
  );

  test("distinctBy", () =>
    expect(List.distinctBy(Int.eq, [6, 1, 1, 2, 1, 3, 2, 3, 2, 4, 5, 5]))
    |> toEqual([6, 1, 2, 3, 4, 5])
  );

  test("map", () =>
    expect(List.map(a => a + 2, [1, 2, 3])) |> toEqual([3, 4, 5])
  );

  test("apply", () =>
    expect(List.apply([a => a + 2, a => a * 3], [1, 2, 3]))
    |> toEqual([3, 4, 5, 3, 6, 9])
  );

  test("bind", () =>
    expect(List.bind([1, 2, 3], a => [a, a]))
    |> toEqual([1, 1, 2, 2, 3, 3])
  );

  test("flatten", () =>
    expect(List.flatten([[1, 2, 3], [4, 5]])) |> toEqual([1, 2, 3, 4, 5])
  );

  test("fromArray", () =>
    expect(List.fromArray([|1, 2, 3|])) |> toEqual([1, 2, 3])
  );

  test("toArray", () =>
    expect(List.toArray([1, 2, 3])) |> toEqual([|1, 2, 3|])
  );

  test("eqBy returns true if list items are equal", () =>
    expect(List.eqBy(Int.eq, [1, 2, 3], [1, 2, 3])) |> toBe(true)
  );

  test("eqBy returns false if list items are not equal", () =>
    expect(List.eqBy(Int.eq, [1, 2, 3], [1, 2, 4])) |> toBe(false)
  );

  test("eqBy returns false if lists are of different sizes", () =>
    expect(List.eqBy(Int.eq, [1], [1, 2])) |> toBe(false)
  );

  test("eq returns true if list items are equal", () =>
    expect(List.eq((module Int.Eq), [1, 2, 3], [1, 2, 3])) |> toBe(true)
  );

  test("eq returns false if list items are not equal", () =>
    expect(List.eq((module Int.Eq), [1, 2, 3], [1, 2, 4])) |> toBe(false)
  );

  test("mapOption keep all", () =>
    expect(List.mapOption(v => Some(string_of_int(v)), [1, 2, 3]))
    |> toEqual(["1", "2", "3"])
  );

  test("mapOption keep none", () =>
    expect(List.mapOption(_ => None, [1, 2, 3])) |> toEqual([])
  );

  test("mapOption keep int", () =>
    expect(List.mapOption(Relude.String.toInt, ["1", "a", "2", "", "3"]))
    |> toEqual([1, 2, 3])
  );

  test("showBy", () =>
    expect(List.showBy(string_of_int, [1, 2, 3])) |> toEqual("[1, 2, 3]")
  );

  test("void", () =>
    expect(List.void([1, 2, 3])) |> toEqual([(), (), ()])
  );

  test("flap", () =>
    expect(List.flap([a => a + 1, a => a + 2, a => a + 3], 5))
    |> toEqual([6, 7, 8])
  );

  test("map2", () =>
    expect(List.map2((a, b) => a + b, [1, 2], [3, 4]))
    |> toEqual([4, 5, 5, 6])
  );

  test("map3", () =>
    expect(List.map3((a, b, c) => a + b + c, [1, 2], [3, 4], [10, 20]))
    |> toEqual([14, 24, 15, 25, 15, 25, 16, 26])
  );

  test("map4", () =>
    expect(
      List.map4(
        (a, b, c, d) => a + b + c + d,
        [1, 2],
        [3, 4],
        [10, 20],
        [100, 200],
      ),
    )
    |> toEqual([
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
       ])
  );

  test("chunk", () =>
    expect(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
      |> List.chunk(3),
    )
    |> toEqual([
         [1, 2, 3],
         [4, 5, 6],
         [7, 8, 9],
         [10, 11, 12],
         [13, 14, 15],
         [16],
       ])
  );

  test("List.Float.sum empty", () =>
    expect(List.Float.sum([])) |> toEqual(0.)
  );

  test("List.Float.sum one", () =>
    expect(List.Float.sum([1.])) |> toEqual(1.)
  );

  test("List.Float.sum many", () =>
    expect(List.Float.sum([1., 3., 5.])) |> toEqual(9.)
  );

  test("countBy empty", () =>
    expect(List.countBy(_ => true, [])) |> toEqual(0)
  );

  test("countBy", () =>
    expect(List.countBy(v => v < 10, [1, 12, 3, 20, 4])) |> toEqual(3)
  );

  test("String.eq empties true", () =>
    expect(List.String.eq([], [])) |> toEqual(true)
  );

  test("String.eq same values true", () =>
    expect(List.String.eq(["a", "b"], ["a", "b"])) |> toEqual(true)
  );

  test("String.eq different values false", () =>
    expect(List.String.eq(["a", "c"], ["a", "b"])) |> toEqual(false)
  );

  test("String.eq different size false", () =>
    expect(List.String.eq(["a", "c"], ["a"])) |> toEqual(false)
  );

  test("String.min empty", () =>
    expect(List.String.min([])) |> toEqual(None)
  );

  test("String.min non-empty", () =>
    expect(List.String.min(["foo", "bar"])) |> toEqual(Some("bar"))
  );

  test("String.max non-empty", () =>
    expect(List.String.max(["b", "a", "c"])) |> toEqual(Some("c"))
  );

  test("String.contains empty", () =>
    expect(List.String.contains("a", [])) |> toEqual(false)
  );

  test("String.contains false", () =>
    expect(List.String.contains("a", ["b", "c"])) |> toEqual(false)
  );

  test("String.contains true", () =>
    expect(List.String.contains("a", ["b", "c", "a"])) |> toEqual(true)
  );

  test("String.indexOf missing", () =>
    expect(List.String.indexOf("x", ["a", "b", "c", "d", "e"]))
    |> toEqual(None)
  );

  test("String.indexOf found", () =>
    expect(List.String.indexOf("d", ["a", "b", "c", "d", "e"]))
    |> toEqual(Some(3))
  );

  test("String.distinct", () =>
    expect(List.String.distinct(["foo", "bar", "baz", "bar"]))
    |> toEqual(["foo", "bar", "baz"])
  );

  test("String.removeFirst", () =>
    expect(List.String.removeFirst("b", ["b", "a", "b", "c"]))
    |> toEqual(["a", "b", "c"])
  );

  test("String.removeEach", () =>
    expect(List.String.removeEach("b", ["b", "a", "b", "c"]))
    |> toEqual(["a", "c"])
  );

  test("String.sort", () =>
    expect(List.String.sort(["c", "d", "a", "b", "c"]))
    |> toEqual(["a", "b", "c", "c", "d"])
  );

  test("String.join empty", () =>
    expect(List.String.join([])) |> toEqual("")
  );

  test("String.join one", () =>
    expect(List.String.join(["foo"])) |> toEqual("foo")
  );

  test("String.join many", () =>
    expect(List.String.join(["foo", "bar"])) |> toEqual("foobar")
  );

  test("String.joinWith empty", () =>
    expect(List.String.joinWith(", ", [])) |> toEqual("")
  );

  test("String.joinWith", () =>
    expect(List.String.joinWith(", ", ["a", "b", "c"]))
    |> toEqual("a, b, c")
  );

  test("Int.sum empty", () =>
    expect(List.Int.sum([])) |> toEqual(0)
  );

  test("Int.sum one", () =>
    expect(List.Int.sum([1])) |> toEqual(1)
  );

  test("Int.sum many", () =>
    expect(List.Int.sum([1, 3, 5])) |> toEqual(9)
  );

  test("Option.traverse success", () =>
    expect([1, 2, 3, 4] |> List.Option.traverse(a => Some(a)))
    |> toEqual(Some([1, 2, 3, 4]))
  );

  test("Option.traverse failure", () =>
    expect(
      [1, 2, 3, 4]
      |> List.Option.traverse(a => a mod 2 == 0 ? Some(a) : None),
    )
    |> toEqual(None)
  );

  test("Validation.traverse success", () =>
    expect(List.Validation.traverse(a => Ok(a), [1, 2, 3, 4, 5]))
    |> toEqual(Relude.Validation.VOk([1, 2, 3, 4, 5]))
  );

  test("Validation.traverse failure", () =>
    expect(
      List.Validation.traverse(
        a => Error(string_of_int(a) ++ " is bad"),
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual(
         Relude.Validation.VError(
           Relude.NonEmpty.List.make(
             "1 is bad",
             ["2 is bad", "3 is bad", "4 is bad", "5 is bad"],
           ),
         ),
       )
  );

  testAsync("List.IO.sequence", onDone => {
    // Try a bunch of random IOs to seek out problems
    let io1 = IO.pure(1);

    let io2 = IO.suspend(() => 2);

    let io3 = IO.suspendIO(() => IO.pure(3));

    let io4 =
      IO.async(onDone =>
        Js.Global.setTimeout(~f=() => onDone(Ok(4)), 0) |> ignore
      );
    let io5 = io4 |> IO.map(four => four + 1);

    let io6 = io4 |> IO.flatMap(four => IO.pure(four + 2));

    let io7 =
      io4
      |> IO.flatMap(four => IO.suspend(() => four + 2))
      |> IO.flatMap(six => IO.async(onDone => onDone(Ok(six + 1))));

    let io8 =
      io7
      |> IO.flatMap(seven =>
           Relude.Js.Promise.toIOLazy(() => Js.Promise.resolve(seven + 1))
         );

    let io9 = IO.throw(9) |> IO.flip;

    let io10 =
      io7
      |> IO.summonError
      |> IO.unsummonError
      |> IO.flip
      |> IO.flip
      |> IO.withDelay(0)
      |> IO.flatMap(seven => IO.suspend(() => seven + 3));

    let ios = [io1, io2, io3, io4, io5, io6, io7, io8, io9, io10];

    List.IO.sequence(ios)
    |> IO.unsafeRunAsync(
         fun
         | Ok([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) => onDone(pass)
         | _ => onDone(fail("fail")),
       );
  });
});
