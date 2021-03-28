open Jest;
open Expect;
open! Relude.Globals;

type user = {
  name: string,
  age: int,
};

let user1 = {name: "Andy", age: 42};
let user2 = {name: "Andy", age: 99};
let user3 = {name: "Bob", age: 42};

describe("Eq", () => {
  testAll(
    "by",
    [
      (user1, user1, true),
      (user1, user2, true),
      (user1, user3, false),
      (user2, user1, true),
      (user2, user2, true),
      (user2, user3, false),
      (user3, user1, false),
      (user3, user2, false),
      (user3, user3, true),
    ],
    ((user1, user2, expected)) => {
      let userEqByName = String.eq |> Eq.by((user: user) => user.name);
      expect(userEqByName(user1, user2)) |> toEqual(expected);
    },
  );

  testAll(
    "invert",
    [
      ("a", "a", true, false),
      ("a", "b", false, true),
      ("b", "a", false, true),
    ],
    ((a, b, eqExpected, notEqExpected)) => {
      let eq = (a: string, b: string) => a == b;
      let notEq = eq |> Eq.invert;

      expect((eq(a, b), notEq(a, b)))
      |> toEqual((eqExpected, notEqExpected));
    },
  );
});
