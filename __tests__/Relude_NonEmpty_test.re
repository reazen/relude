open Jest;
open Expect;

module NonEmpty = Relude_NonEmpty;

describe("NonEmpty.List", () => {
  test("concat with two full NonEmpty.Lists", () => {
    let l1 = NonEmpty.List.make(1, [2, 3]);
    let l2 = NonEmpty.List.make(4, [5]);
    expect(NonEmpty.List.concat(l1, l2))
    |> toEqual(NonEmpty.List.make(1, [2, 3, 4, 5]));
  });

  test("foldLeft", () => {
    let ne = NonEmpty.List.make(1, [2, 3, 4, 5]);
    let result =
      NonEmpty.List.foldLeft(
        (acc, curr) => Belt.List.concat(acc, [curr]),
        [],
        ne,
      );
    expect(result) |> toEqual([1, 2, 3, 4, 5]);
  });

  test("foldRight", () => {
    let ne = NonEmpty.List.make(1, [2, 3, 4, 5]);
    let result =
      NonEmpty.List.foldRight(
        (curr, acc) => Belt.List.concat(acc, [curr]),
        [],
        ne,
      );
    expect(result) |> toEqual([5, 4, 3, 2, 1]);
  });

  test("flatten", () => {
    let ne1 = NonEmpty.List.make(1, [2, 3]);
    let ne2 = NonEmpty.List.make(4, []);
    let ne3 = NonEmpty.List.make(5, [6]);
    let all = NonEmpty.List.make(ne1, [ne2, ne3]);
    let result = NonEmpty.List.flatten(all);
    let expected = NonEmpty.List.make(1, [2, 3, 4, 5, 6]);
    expect(result) |> toEqual(expected);
  });

  test("eq", () => {
    let ne = NonEmpty.List.make(1, [2, 3]);
    expect(NonEmpty.List.eq((module Relude_Int.Eq), ne, ne))
    |> toEqual(true);
  });

  test("show", () => {
    let ne = NonEmpty.List.make(1, [2, 3]);
    expect(NonEmpty.List.show((module Relude_Int.Show), ne))
    |> toEqual("[!1, 2, 3!]");
  });

  module NonEmptyListTraversableOption =
    NonEmpty.List.Traversable(Relude_Option.Applicative);

  test("traverse option", () =>
    expect(
      NonEmptyListTraversableOption.traverse(
        a => Some(a),
        NonEmpty(1, [2, 3]),
      ),
    )
    |> toEqual(Some(NonEmpty.List.NonEmpty(1, [2, 3])))
  );
});

describe("NonEmpty.Array", () => {
  test("concat with two full NonEmpty.Arrays", () => {
    let l1 = NonEmpty.Array.make(1, [|2, 3|]);
    let l2 = NonEmpty.Array.make(4, [|5|]);
    expect(NonEmpty.Array.concat(l1, l2))
    |> toEqual(NonEmpty.Array.make(1, [|2, 3, 4, 5|]));
  });

  module NonEmptyArrayTraversableOption =
    NonEmpty.Array.Traversable(Relude_Option.Applicative);

  test("traverse option", () =>
    expect(
      NonEmptyArrayTraversableOption.traverse(
        a => Some(a),
        NonEmpty(1, [|2, 3|]),
      ),
    )
    |> toEqual(Some(NonEmpty.Array.NonEmpty(1, [|2, 3|])))
  );
});
