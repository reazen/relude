open Jest;
open Expect;

module S = Relude.Sequence;
module LS = S.List;
module AS = S.Array;
module LZ = Relude.ListZipper;
type t('a) = LZ.t('a) = | Zipper(list('a), 'a, list('a));

open LZ.Infix;

module LZFoldMap = LZ.Foldable.Fold_Map(Relude.String.Monoid);
module LZFoldMapAny = LZ.Foldable.Fold_Map_Any(Relude.List.MonoidAny);
module LZTraversable = LZ.Traversable(Relude.Option.Applicative);

describe("ListZipper", () => {
  test("make", () =>
    expect(LZ.make([2, 1], 3, [4, 5]))
    |> toEqual(Zipper([2, 1], 3, [4, 5]))
  );

  test("makeWithLeft", () =>
    expect(LZ.makeWithLeft([2, 1], 3)) |> toEqual(Zipper([2, 1], 3, []))
  );

  test("makeWithRight", () =>
    expect(LZ.makeWithRight(3, [4, 5])) |> toEqual(Zipper([], 3, [4, 5]))
  );

  test("makeWithRightArray", () =>
    expect(LZ.makeWithRightArray(3, [|4, 5|]))
    |> toEqual(Zipper([], 3, [4, 5]))
  );

  test("makeWithRightList", () =>
    expect(LZ.makeWithRightList(3, [4, 5]))
    |> toEqual(Zipper([], 3, [4, 5]))
  );

  test("makeWithFocus", () =>
    expect(LZ.makeWithFocus(3)) |> toEqual(Zipper([], 3, []))
  );

  test("fromSequence list non-empty", () =>
    expect(LZ.fromSequence(LS.fromList([1, 2, 3, 4, 5])))
    |> toEqual(Some(Zipper([], 1, [2, 3, 4, 5])))
  );

  test("fromSequence list empty", () =>
    expect(LZ.fromSequence(LS.fromList([]))) |> toEqual(None)
  );

  test("fromSequence array non-empty", () =>
    expect(LZ.fromSequence(LS.fromArray([|1, 2, 3, 4, 5|])))
    |> toEqual(Some(Zipper([], 1, [2, 3, 4, 5])))
  );

  test("fromSequence array empty", () =>
    expect(LZ.fromSequence(LS.fromArray([||]))) |> toEqual(None)
  );

  test("fromArray non-empty", () =>
    expect(LZ.fromArray([|1, 2, 3, 4, 5|]))
    |> toEqual(Some(Zipper([], 1, [2, 3, 4, 5])))
  );

  test("fromArray empty", () =>
    expect(LZ.fromArray([||])) |> toEqual(None)
  );

  test("fromList non-empty", () =>
    expect(LZ.fromList([1, 2, 3, 4, 5]))
    |> toEqual(Some(Zipper([], 1, [2, 3, 4, 5])))
  );

  test("fromList empty", () =>
    expect(LZ.fromList([])) |> toEqual(None)
  );

  test("fromNonEmptyArray", () =>
    expect(LZ.fromNea(Relude.Nea.make(1, [|2, 3, 4, 5|])))
    |> toEqual(Zipper([], 1, [2, 3, 4, 5]))
  );

  test("fromNonEmptyList", () =>
    expect(LZ.fromNel(Relude.Nel.make(1, [2, 3, 4, 5])))
    |> toEqual(Zipper([], 1, [2, 3, 4, 5]))
  );

  test("map", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) <#> (a => a * 10))
    |> toEqual(LZ.make([20, 10], 30, [40, 50]))
  );

  test("apply", () => {
    let l = [a => a * 10, a => a * 100];
    let f = a => a * 1000;
    let r = [a => a * 10000, a => a * 100000];
    let actual = LZ.make(l, f, r) <*> LZ.make([2, 1], 3, [4, 5]);
    let expected = LZ.make([20, 100], 3000, [40000, 500000]);
    expect(actual) |> toEqual(expected);
  });

  test("pure", () =>
    expect(LZ.pure(42)) |> toEqual(LZ.makeWithFocus(42))
  );

  test("foldLeft", () =>
    expect(
      LZ.make([2, 1], 3, [4, 5])
      |> LZ.foldLeft((acc, a) => acc |> Relude.List.append(a), []),
    )
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("foldRight", () =>
    expect(
      LZ.make([2, 1], 3, [4, 5])
      |> LZ.foldRight((a, acc) => acc |> Relude.List.append(a), []),
    )
    |> toEqual([5, 4, 3, 2, 1])
  );

  test("foldMap", () =>
    expect(LZFoldMap.fold_map(string_of_int, LZ.make([2, 1], 3, [4, 5])))
    |> toEqual("12345")
  );

  test("foldMapAny", () =>
    expect(LZFoldMapAny.fold_map(a => [a], LZ.make([2, 1], 3, [4, 5])))
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("traverse", () =>
    expect(
      LZ.make([2, 1], 3, [4, 5])
      |> LZTraversable.traverse(a => Some(string_of_int(a))),
    )
    |> toEqual(Some(LZ.make(["2", "1"], "3", ["4", "5"])))
  );

  test("sequence", () =>
    expect(
      LZ.make([Some(2), Some(1)], Some(3), [Some(4), Some(5)])
      |> LZTraversable.sequence,
    )
    |> toEqual(Some(LZ.make([2, 1], 3, [4, 5])))
  );

  test("toSequence", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.toSequence)
    |> toEqual(LS.fromList([1, 2, 3, 4, 5]))
  );

  test("toArray", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.toArray)
    |> toEqual([|1, 2, 3, 4, 5|])
  );

  test("toList", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.toList)
    |> toEqual([1, 2, 3, 4, 5])
  );

  test("toNonEmptyArray", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.toNea)
    |> toEqual(Relude.Nea.make(1, [|2, 3, 4, 5|]))
  );

  test("toNonEmptyArray empty left", () =>
    expect(LZ.make([], 3, [4, 5]) |> LZ.toNea)
    |> toEqual(Relude.Nea.make(3, [|4, 5|]))
  );

  test("toNonEmptyList", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.toNel)
    |> toEqual(Relude.Nel.make(1, [2, 3, 4, 5]))
  );

  test("toNonEmptyList empty left", () =>
    expect(LZ.make([], 3, [4, 5]) |> LZ.toNel)
    |> toEqual(Relude.Nel.make(3, [4, 5]))
  );

  test("concatWithKeepLeftFocus", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([20, 10], 30, [40, 50]);
    let z = LZ.concatWithKeepLeftFocus(~prefix=x, y);
    expect((z, z |> LZ.toList))
    |> toEqual((
         LZ.make([2, 1], 3, [4, 5, 10, 20, 30, 40, 50]),
         [1, 2, 3, 4, 5, 10, 20, 30, 40, 50],
       ));
  });

  test("concatWithKeepRightFocus", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([20, 10], 30, [40, 50]);
    let z = LZ.concatWithKeepRightFocus(~prefix=x, y);
    expect((z, z |> LZ.toList))
    |> toEqual((
         LZ.make([20, 10, 5, 4, 3, 2, 1], 30, [40, 50]),
         [1, 2, 3, 4, 5, 10, 20, 30, 40, 50],
       ));
  });

  test("reverse", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.reverse)
    |> toEqual(LZ.make([4, 5], 3, [2, 1]))
  );

  test("zipWith", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([20, 10], 30, [40, 50]);
    let z = LZ.zipWith((a, b) => a + b, x, y);
    expect(z) |> toEqual(LZ.make([22, 11], 33, [44, 55]));
  });

  test("zip", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([20, 10], 30, [40, 50]);
    let z = LZ.zip(x, y);
    expect(z)
    |> toEqual(
         LZ.make([(2, 20), (1, 10)], (3, 30), [(4, 40), (5, 50)]),
       );
  });

  test("zipWithIndex", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.zipWithIndex)
    |> toEqual(LZ.make([(2, 1), (1, 0)], (3, 2), [(4, 3), (5, 4)]))
  );

  test("getFocus", () =>
    expect(LZ.make([2, 1], 3, [4, 5]) |> LZ.getFocus) |> toEqual(3)
  );

  test("showBy", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = x |> LZ.showBy(string_of_int);
    expect(y) |> toEqual("Zipper([2, 1], 3, [4, 5])");
  });

  test("show", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = x |> LZ.show((module Relude.Int.Show));
    expect(y) |> toEqual("Zipper([2, 1], 3, [4, 5])");
  });

  test("eqBy true", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([2, 1], 3, [4, 5]);
    expect(LZ.eqBy(Relude.Int.eq, x, y)) |> toEqual(true);
  });

  test("eqBy false", () => {
    let x = LZ.make([2, 1], 3, [4, 5]);
    let y = LZ.make([2, 1], 3, [44, 5]);
    expect(LZ.eqBy(Relude.Int.eq, x, y)) |> toEqual(false);
  });
});