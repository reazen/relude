open Jest;
open Expect;
open Relude.Globals;

module User = {
  type t = {
    name: string,
    age: int,
  };

  module EqByName =
    String.EqBy({
      type a = t;
      type b = string;
      let f = user => user.name;
    });

  module EqByAge =
    Int.EqBy({
      type a = t;
      type b = int;
      let f = user => user.age;
    });

  module EqByNameAndAge =
    Tuple2.EqBy(
      String.Eq,
      Int.Eq,
      {
        type a = t;
        type b = (string, int);
        let f = user => (user.name, user.age);
      },
    );
};

open User;
let user1 = {name: "Andy", age: 42};
let user2 = {name: "Andy", age: 99};
let user3 = {name: "Bob", age: 99};

describe("Extensions_Eq", () => {
  testAll("notEq", [(1, 1, false), (1, 2, true)], ((a, b, expected)) => {
    expect(Int.notEq(a, b)) |> toEqual(expected)
  });

  testAll(
    "EqByName",
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
    expect(User.EqByName.eq(user1, user2)) |> toEqual(expected)
  });

  testAll(
    "EqByAge",
    [
      (user1, user1, true),
      (user1, user2, false),
      (user1, user3, false),
      (user2, user1, false),
      (user2, user2, true),
      (user2, user3, true),
      (user3, user1, false),
      (user3, user2, true),
      (user3, user3, true),
    ],
    ((user1, user2, expected)) => {
    expect(User.EqByAge.eq(user1, user2)) |> toEqual(expected)
  });

  testAll(
    "EqByNameAndAge",
    [
      (user1, user1, true),
      (user1, user2, false),
      (user1, user3, false),
      (user2, user1, false),
      (user2, user2, true),
      (user2, user3, false),
      (user3, user1, false),
      (user3, user2, false),
      (user3, user3, true),
    ],
    ((user1, user2, expected)) => {
    expect(User.EqByNameAndAge.eq(user1, user2)) |> toEqual(expected)
  });

  testAll(
    "|=| operator",
    [(1, 1, true), (1, 2, false), (2, 1, false)],
    ((a, b, expected)) => {
    Int.Infix.(expect(a |=| b) |> toEqual(expected))
  });

  testAll(
    "|!=| operator",
    [(1, 1, false), (1, 2, true), (2, 1, true)],
    ((a, b, expected)) => {
    Int.Infix.(expect(a |!=| b) |> toEqual(expected))
  });
});