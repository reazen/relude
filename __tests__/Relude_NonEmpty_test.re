open Jest;
open Expect;
open Relude.Globals;

[@coverage exclude_file];
afterAll(Bisect.Runtime.write_coverage_data);

describe("NonEmpty.List", () => {
  test("make", () =>
    expect(NonEmpty.List.make(1, [2, 3]))
    |> toEqual(NonEmpty.List.NonEmpty(1, [2, 3]))
  );

  testAll(
    "fromList",
    [
      ([], None),
      ([1], Some(NonEmpty.List.pure(1))),
      ([1, 2, 3], Some(NonEmpty.List.make(1, [2, 3]))),
    ],
    ((input, expected)) =>
    expect(NonEmpty.List.fromList(input)) |> toEqual(expected)
  );

  testAll(
    "toList",
    [
      (NonEmpty.List.pure(1), [1]),
      (NonEmpty.List.make(1, [2, 3]), [1, 2, 3]),
    ],
    ((input, expected)) =>
    expect(NonEmpty.List.toList(input)) |> toEqual(expected)
  );

  testAll(
    "fromArray",
    [
      ([||], None),
      ([|1|], Some(NonEmpty.List.pure(1))),
      ([|1, 2, 3|], Some(NonEmpty.List.make(1, [2, 3]))),
    ],
    ((input, expected)) =>
    expect(NonEmpty.List.fromArray(input)) |> toEqual(expected)
  );

  testAll(
    "toArray",
    [
      (NonEmpty.List.pure(1), [|1|]),
      (NonEmpty.List.make(1, [2, 3]), [|1, 2, 3|]),
    ],
    ((input, expected)) =>
    expect(NonEmpty.List.toArray(input)) |> toEqual(expected)
  );

  test("pure", () =>
    expect(NonEmpty.List.pure(1)) |> toEqual(NonEmpty.List.make(1, []))
  );

  test("flatMap", () =>
    expect(
      NonEmpty.List.one(1)
      |> NonEmpty.List.flatMap(a => NonEmpty.List.one(a + 1)),
    )
    |> toEqual(NonEmpty.List.make(2, []))
  );

  test("cons", () =>
    expect(NonEmpty.List.cons(1, NonEmpty.List.one(2)))
    |> toEqual(NonEmpty.List.make(1, [2]))
  );

  test("uncons", () =>
    expect(NonEmpty.List.uncons(NonEmpty.List.make(1, [2, 3])))
    |> toEqual((1, [2, 3]))
  );

  test("fromSequence", () =>
    NonEmpty.List.one(1)
    |> NonEmpty.List.toSequence
    |> NonEmpty.List.fromSequence
    |> expect
    |> toEqual(Some(NonEmpty.List.make(1, [])))
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
      NonEmpty.List.foldLeft((acc, curr) => List.append(curr, acc), [], ne);
    expect(result) |> toEqual([1, 2, 3, 4, 5]);
  });

  test("foldRight", () => {
    let ne = NonEmpty.List.make(1, [2, 3, 4, 5]);
    let result =
      NonEmpty.List.foldRight(
        (curr, acc) => List.append(curr, acc),
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

  testAll(
    "reverse",
    [
      (NonEmpty.List.pure(1), NonEmpty.List.pure(1)),
      (NonEmpty.List.make(1, [2]), NonEmpty.List.make(2, [1])),
      (NonEmpty.List.make(1, [2, 3]), NonEmpty.List.make(3, [2, 1])),
    ],
    ((input, expected)) =>
    expect(NonEmpty.List.reverse(input)) |> toEqual(expected)
  );

  test("apply", () => {
    let result =
      NonEmpty.List.apply(
        NonEmpty.List.make(v => v + 10, [v => v * 3]),
        NonEmpty.List.make(1, [2]),
      );
    let expected = NonEmpty.List.make(11, [12, 3, 6]);
    expect(result) |> toEqual(expected);
  });

  test("eq tail", () => {
    let ne = NonEmpty.List.make(1, [2, 3]);
    expect(NonEmpty.List.eq((module Relude_Int.Eq), ne, ne))
    |> toEqual(true);
  });

  test("eq head", () => {
    let ne1 = NonEmpty.List.make(0, [2, 3]);
    let ne2 = NonEmpty.List.make(1, [2, 3]);
    expect(NonEmpty.List.eq((module Relude_Int.Eq), ne1, ne2))
    |> toEqual(false);
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
  test("make", () =>
    expect(NonEmpty.Array.make(1, [|2, 3|]))
    |> toEqual(NonEmpty.Array.NonEmpty(1, [|2, 3|]))
  );

  testAll(
    "fromList",
    [
      ([], None),
      ([1], Some(NonEmpty.Array.pure(1))),
      ([1, 2, 3], Some(NonEmpty.Array.make(1, [|2, 3|]))),
    ],
    ((input, expected)) =>
    expect(NonEmpty.Array.fromList(input)) |> toEqual(expected)
  );

  testAll(
    "toList",
    [
      (NonEmpty.Array.pure(1), [1]),
      (NonEmpty.Array.make(1, [|2, 3|]), [1, 2, 3]),
    ],
    ((input, expected)) =>
    expect(NonEmpty.Array.toList(input)) |> toEqual(expected)
  );

  testAll(
    "fromArray",
    [
      ([||], None),
      ([|1|], Some(NonEmpty.Array.pure(1))),
      ([|1, 2, 3|], Some(NonEmpty.Array.make(1, [|2, 3|]))),
    ],
    ((input, expected)) =>
    expect(NonEmpty.Array.fromArray(input)) |> toEqual(expected)
  );

  testAll(
    "toArray",
    [
      (NonEmpty.Array.pure(1), [|1|]),
      (NonEmpty.Array.make(1, [|2, 3|]), [|1, 2, 3|]),
    ],
    ((input, expected)) =>
    expect(NonEmpty.Array.toArray(input)) |> toEqual(expected)
  );

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

  testAll(
    "reverse",
    [
      (NonEmpty.Array.pure(1), NonEmpty.Array.pure(1)),
      (NonEmpty.Array.make(1, [|2|]), NonEmpty.Array.make(2, [|1|])),
      (
        NonEmpty.Array.make(1, [|2, 3|]),
        NonEmpty.Array.make(3, [|2, 1|]),
      ),
    ],
    ((input, expected)) =>
    expect(NonEmpty.Array.reverse(input)) |> toEqual(expected)
  );

  test("toNonEmptyList", () =>
    NonEmpty.Array.cons(1, NonEmpty.Array.one(2))
    |> NonEmpty.Array.toNonEmptyList
    |> expect
    |> toEqual(NonEmpty.List.make(1, [2]))
  );

  test("fromNonEmptyList", () =>
    NonEmpty.List.make(1, [2])
    |> NonEmpty.Array.fromNonEmptyList
    |> expect
    |> toEqual(NonEmpty.Array.make(1, [|2|]))
  );

  test("show", () => {
    let ne = NonEmpty.Array.make(1, [|2, 3|]);
    expect(NonEmpty.Array.show((module Relude_Int.Show), ne))
    |> toEqual("[!1, 2, 3!]");
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
