open Jest;
open Expect;

module List = Relude_List;

describe("List submodules", () => {
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
    |> toEqual(Relude_Validation.VOk([1, 2, 3, 4, 5]))
  );

  test("Validation.traverse failure", () =>
    expect(
      List.Validation.traverse(
        a => Error(string_of_int(a) ++ " is bad"),
        [1, 2, 3, 4, 5],
      ),
    )
    |> toEqual(
         Relude_Validation.VError(
           Relude_NonEmpty.List.make(
             "1 is bad",
             ["2 is bad", "3 is bad", "4 is bad", "5 is bad"],
           ),
         ),
       )
  );
});
