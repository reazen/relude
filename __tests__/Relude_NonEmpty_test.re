open Jest;
open Expect;

module NonEmpty = Relude_NonEmpty;

describe("NonEmpty.List", () => {
  test("cons", () =>
    expect(NonEmpty.List.cons(1, NonEmpty.List.one(2)))
    |> toEqual(NonEmpty.List.make(1, [2]))
  );

  test("uncons", () =>
    expect(NonEmpty.List.uncons(NonEmpty.List.make(1, [2, 3])))
    |> toEqual((1, [2, 3]))
  );

  test("concat with two full NonEmpty.Lists", () => {
    let l1 = NonEmpty.List.make(1, [2, 3]);
    let l2 = NonEmpty.List.make(4, [5]);
    expect(NonEmpty.List.concat(l1, l2))
    |> toEqual(NonEmpty.List.make(1, [2, 3, 4, 5]));
  });

  test("concat named", () =>
    expect(
      NonEmpty.List.concatNamed(
        ~prefix=NonEmpty.List.make(1, [2, 3]),
        NonEmpty.List.make(4, [5, 6]),
      ),
    )
    |> toEqual(NonEmpty.List.make(1, [2, 3, 4, 5, 6]))
  );

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

  module NonEmptyListWithOption =
    NonEmpty.List.WithApplicative(Relude_Option.Applicative);

  test("traverse option", () =>
    expect(
      NonEmptyListWithOption.traverse(a => Some(a), NonEmpty(1, [2, 3])),
    )
    |> toEqual(Some(NonEmpty.List.NonEmpty(1, [2, 3])))
  );
});

describe("NonEmpty.Array", () => {
  test("cons", () =>
    expect(NonEmpty.Array.cons(1, NonEmpty.Array.one(2)))
    |> toEqual(NonEmpty.Array.make(1, [|2|]))
  );

  test("uncons", () =>
    expect(NonEmpty.Array.uncons(NonEmpty.Array.make(1, [|2, 3|])))
    |> toEqual((1, [|2, 3|]))
  );

  test("concat with two full NonEmpty.Arrays", () => {
    let l1 = NonEmpty.Array.make(1, [|2, 3|]);
    let l2 = NonEmpty.Array.make(4, [|5|]);
    expect(NonEmpty.Array.concat(l1, l2))
    |> toEqual(NonEmpty.Array.make(1, [|2, 3, 4, 5|]));
  });

  module NonEmptyArrayWithOption =
    NonEmpty.Array.WithApplicative(Relude_Option.Applicative);

  test("traverse option", () =>
    expect(
      NonEmptyArrayWithOption.traverse(
        a => Some(a),
        NonEmpty(1, [|2, 3|]),
      ),
    )
    |> toEqual(Some(NonEmpty.Array.NonEmpty(1, [|2, 3|])))
  );
});