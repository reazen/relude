open Jest;
open Expect;

describe("List", () => {
  test("length empty list", () => {
    expect(List.length([])) |> toEqual(0);
  });

  test("length non-empty list", () => {
    expect(List.length([1, 2, 3])) |> toEqual(3);
  });

  test("isEmpty is true for empty list", () => {
    expect(List.isEmpty([])) |> toBe(true);
  });

  test("isEmpty is false for non-empty list", () => {
    expect(List.isEmpty([1])) |> toBe(false);
  });

  test("isNotEmpty is false for empty list", () => {
    expect(List.isNotEmpty([])) |> toBe(false);
  });

  test("isNotEmpty is true for non-empty list", () => {
    expect(List.isNotEmpty([1])) |> toBe(true);
  });

  test("empty is []", () => {
    expect(List.empty) |> toEqual([]);
  });

  test("pure creates a one-item list", () => {
    expect(List.pure(123)) |> toEqual([123]);
  });

  test("concat", () => {
    expect(List.concat([1, 2], [3, 4])) |> toEqual([1, 2, 3, 4]);
  });

  test("cons", () => {
    expect(List.cons(1, [2, 3])) |> toEqual([1, 2, 3]);
  });

  test("prepend", () => {
    expect(List.prepend(1, [2, 3])) |> toEqual([1, 2, 3]);
  });

  test("append", () => {
    expect(List.append(1, [2, 3])) |> toEqual([2, 3, 1]);
  });

  test("foldLeft", () => {
    expect(List.foldLeft((acc, curr) => List.append(curr, acc), [], [1, 2, 3, 4, 5])) |> toEqual([1, 2, 3, 4, 5]);
  });

  test("foldRight", () => {
    expect(List.foldRight((curr, acc) => List.append(curr, acc), [], [1, 2, 3, 4, 5])) |> toEqual([5, 4, 3, 2, 1]);
  });

  test("scanLeft", () => {
    expect(List.scanLeft((acc, curr) => List.append(curr, acc), [], [1, 2, 3, 4, 5])) |> toEqual([[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]);
  });

  test("scanRight", () => {
    expect(List.scanRight((curr, acc) => List.append(curr, acc), [], [1, 2, 3, 4, 5])) |> toEqual([[5, 4, 3, 2, 1], [5, 4, 3, 2], [5, 4, 3], [5, 4], [5]]);
  });

  test("head empty list", () => {
    expect(List.head([])) |> toEqual(None);
  });

  test("head non-empty list", () => {
    expect(List.head([1, 2, 3])) |> toEqual(Some(1));
  });

  test("tail empty list", () => {
    expect(List.tail([])) |> toEqual(None);
  });

  test("tail single item list", () => {
    expect(List.tail([1])) |> toEqual(Some([]));
  });

  test("tail multi-item list", () => {
    expect(List.tail([1, 2, 3])) |> toEqual(Some([2, 3]));
  });

  test("tailOrEmpty empty list", () => {
    expect(List.tailOrEmpty([])) |> toEqual([]);
  });

  test("tailOrEmpty single item list", () => {
    expect(List.tailOrEmpty([1])) |> toEqual([]);
  });

  test("tailOrEmpty multi-item list", () => {
    expect(List.tailOrEmpty([1, 2, 3])) |> toEqual([2, 3]);
  });

  test("init empty list", () => {
    expect(List.init([])) |> toEqual(None);
  });

  test("init single item list", () => {
    expect(List.init([1])) |> toEqual(Some([]));
  });

  test("init multi item list", () => {
    expect(List.init([1, 2, 3, 4])) |> toEqual(Some([1, 2, 3]));
  });

  test("last empty list", () => {
    expect(List.last([])) |> toEqual(None);
  });

  test("last single item list", () => {
    expect(List.last([1])) |> toEqual(Some(1));
  });

  test("last multi item list", () => {
    expect(List.last([1, 2, 3, 4])) |> toEqual(Some(4));
  });

  test("take zero from empty list", () => {
    expect(List.take(0, [])) |> toEqual(Some([]));
  });

  test("take non-zero from empty list", () => {
    expect(List.take(2, [])) |> toEqual(None);
  });

  test("take non-zero from short list", () => {
    expect(List.take(2, [1])) |> toEqual(None);
  });

  test("take non-zero from equal list", () => {
    expect(List.take(2, [1, 2])) |> toEqual(Some([1, 2]));
  });

  test("take non-zero from long list", () => {
    expect(List.take(2, [1, 2, 3])) |> toEqual(Some([1, 2]));
  });

  test("takeUpTo zero from empty list", () => {
    expect(List.takeUpTo(0, [])) |> toEqual([]);
  });

  test("takeUpTo non-zero from empty list", () => {
    expect(List.takeUpTo(2, [])) |> toEqual([]);
  });

  test("takeUpTo non-zero from short list", () => {
    expect(List.takeUpTo(2, [1])) |> toEqual([1]);
  });

  test("takeUpTo non-zero from equal list", () => {
    expect(List.takeUpTo(2, [1, 2])) |> toEqual([1, 2]);
  });

  test("takeUpTo non-zero from long list", () => {
    expect(List.takeUpTo(2, [1, 2, 3])) |> toEqual([1, 2]);
  });

  test("takeWhile empty list", () => {
    expect(List.takeWhile(a => a < 2, [])) |> toEqual([]);
  });

  test("takeWhile list", () => {
    expect(List.takeWhile(a => a < 2, [0, 1, 2, 3])) |> toEqual([0, 1]);
  });

  test("zip same length lists", () => {
    expect(List.zip([1, 2, 3], ["4", "5", "6"])) |> toEqual([(1, "4"), (2, "5"), (3, "6")]);
  });

  test("zip different length lists", () => {
    expect(List.zip([1, 2, 3], ["4", "5"])) |> toEqual([(1, "4"), (2, "5")]);
  });

  test("zipWithIndex", () => {
    expect(List.zipWithIndex(["a", "b", "c"])) |> toEqual([("a", 0), ("b", 1), ("c", 2)]);
  });

  test("intersperse", () => {
    expect(List.intersperse(",", ["a", "b", "c"])) |> toEqual(["a", ",", "b", ",", "c"]);
  });

  test("replicate", () => {
    expect(List.replicate(3, ["a", "b", "c"])) |> toEqual(["a", "b", "c", "a", "b", "c", "a", "b", "c"]);
  });

  test("mkString", () => {
    expect(List.mkString(", ", ["a", "b", "c"])) |> toEqual("a, b, c");
  });

  test("show", () => {
    expect(List.show(string_of_int, [1, 2, 3])) |> toEqual("[1, 2, 3]");
  });

  test("eq returns true if list items are equal", () => {
    expect(List.eq([1, 2, 3], [1, 2, 3], Int.eq)) |> toBe(true);
  });

  test("eq returns false if list items are not equal", () => {
    expect(List.eq([1, 2, 3], [1, 2, 4], Int.eq)) |> toBe(false);
  });

  test("eqM returns true if list items are equal", () => {
    expect(List.eqM([1, 2, 3], [1, 2, 3], (module Int.Eq))) |> toBe(true);
  });

  test("eqM returns false if list items are not equal", () => {
    expect(List.eqM([1, 2, 3], [1, 2, 4], (module Int.Eq))) |> toBe(false);
  });
});
