open Jest;
open Expect;

module ListF = Relude_List_Submodules;

describe("ListF", () => {
  test("String.eq empties true", () =>
    expect(ListF.String.eq([], [])) |> toEqual(true)
  );

  test("String.eq same values true", () =>
    expect(ListF.String.eq(["a", "b"], ["a", "b"])) |> toEqual(true)
  );

  test("String.eq different values false", () =>
    expect(ListF.String.eq(["a", "c"], ["a", "b"])) |> toEqual(false)
  );

  test("String.eq different size false", () =>
    expect(ListF.String.eq(["a", "c"], ["a"])) |> toEqual(false)
  );

  test("String.contains empty", () =>
    expect(ListF.String.contains("a", [])) |> toEqual(false)
  );

  test("String.contains false", () =>
    expect(ListF.String.contains("a", ["b", "c"])) |> toEqual(false)
  );

  test("String.contains true", () =>
    expect(ListF.String.contains("a", ["b", "c", "a"])) |> toEqual(true)
  );

  test("String.indexOf missing", () =>
    expect(ListF.String.indexOf("x", ["a", "b", "c", "d", "e"]))
    |> toEqual(None)
  );

  test("String.indexOf found", () =>
    expect(ListF.String.indexOf("d", ["a", "b", "c", "d", "e"]))
    |> toEqual(Some(3))
  );

  test("String.distinct", () =>
    expect(ListF.String.distinct(["foo", "bar", "baz", "bar"]))
    |> toEqual(["foo", "bar", "baz"])
  );

  test("String.remove", () =>
    expect(ListF.String.remove("b", ["b", "a", "b", "c"]))
    |> toEqual(["a", "b", "c"])
  );

  test("String.removeEach", () =>
    expect(ListF.String.removeEach("b", ["b", "a", "b", "c"]))
    |> toEqual(["a", "c"])
  );

  test("String.sort", () =>
    expect(ListF.String.sort(["c", "d", "a", "b", "c"]))
    |> toEqual(["a", "b", "c", "c", "d"])
  );

  test("String.join empty", () =>
    expect(ListF.String.join([])) |> toEqual("")
  );

  test("String.join one", () =>
    expect(ListF.String.join(["foo"])) |> toEqual("foo")
  );

  test("String.join many", () =>
    expect(ListF.String.join(["foo", "bar"])) |> toEqual("foobar")
  );

  test("String.joinWith empty", () =>
    expect(ListF.String.joinWith(", ", [])) |> toEqual("")
  );

  test("String.joinWith", () =>
    expect(ListF.String.joinWith(", ", ["a", "b", "c"]))
    |> toEqual("a, b, c")
  );

  test("Int.sum empty", () =>
    expect(ListF.Int.sum([])) |> toEqual(0)
  );

  test("Int.sum one", () =>
    expect(ListF.Int.sum([1])) |> toEqual(1)
  );

  test("Int.sum many", () =>
    expect(ListF.Int.sum([1, 3, 5])) |> toEqual(9)
  );

  test("Option.traverse success", () =>
    expect([1, 2, 3, 4] |> ListF.Option.traverse(a => Some(a)))
    |> toEqual(Some([1, 2, 3, 4]))
  );

  test("Option.traverse failure", () =>
    expect(
      [1, 2, 3, 4]
      |> ListF.Option.traverse(a => a mod 2 == 0 ? Some(a) : None),
    )
    |> toEqual(None)
  );

  test("Validation.traverse success", () =>
    expect(ListF.Validation.traverse(a => Ok(a), [1, 2, 3, 4, 5]))
    |> toEqual(Relude_Validation.VOk([1, 2, 3, 4, 5]))
  );

  test("Validation.traverse failure", () =>
    expect(
      ListF.Validation.traverse(
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
