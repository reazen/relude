open Jest;
open Expect;
open! Relude.Globals;
open BsBastet.Interface;

module User = {
  type t = {
    id: string,
    name: string,
    age: int,
  };

  let getName = user => user.name;

  let getAge = user => user.age;

  module EqByName: EQ with type t = t =
    String.EqBy({
      type a = t;
      type b = string;
      let f = getName;
    });

  module EqByAge: EQ with type t = t =
    Int.EqBy({
      type a = t;
      type b = int;
      let f = getAge;
    });

  module EqByNameAndAge: EQ with type t = t =
    Tuple.Tuple2.EqBy(
      String.Eq,
      Int.Eq,
      {
        type a = t;
        type b = (string, int);
        let f = user => (user.name, user.age);
      },
    );

  module OrdByName: ORD with type t = t =
    String.OrdBy({
      type a = t;
      type b = string;
      let f = getName;
    });

  module OrdByAge: ORD with type t = t =
    Int.OrdBy({
      type a = t;
      type b = int;
      let f = getAge;
    });

  // This version creates the ordering function first, by contramapping the Tuple2 compareBy with a function that converts a user to a tuple 2
  // Once we have the compareBy, we can create the Ord

  let compareByNameThenAge: (t, t) => ordering =
    Ord.by(
      user => (user.name, user.age),
      Tuple.Tuple2.compareBy(String.compare, Int.compare),
    );

  module OrdByNameThenAge1: ORD with type t = t = {
    include EqByNameAndAge;
    let compare = compareByNameThenAge;
  };

  module OrdByNameThenAge2: ORD with type t = t =
    Tuple.Tuple2.OrdBy(
      String.Ord,
      Int.Ord,
      {
        // Would be cool if we could somehow eliminate the need for type a and type b here, but not sure if it can be done
        // with how module functors need to expose hard types sometimes?
        type a = t;
        type b = (string, int);
        let f = user => (user.name, user.age);
      },
    );
};

open User;
let user1 = {id: "1", name: "Andy", age: 100};
let user2 = {id: "2", name: "Andy", age: 100};
let user3 = {id: "3", name: "Andy", age: 99};
let user4 = {id: "4", name: "Bob", age: 100};
let user5 = {id: "5", name: "Bob", age: 100};
let user6 = {id: "6", name: "Bob", age: 99};

let users1 = [user1, user2, user3, user4, user5, user6];
let users2 = [user5, user1, user3, user4, user6, user2];

describe("Extensions_Ord", () => {
  test("compareWithConversion", () => {
    let compare = String.compareWithConversion((user: User.t) => user.name);
    expect(List.sortBy(compare, users2))
    |> toEqual([user1, user3, user2, user5, user4, user6]);
  });

  test("compareReversed", () => {
    let compare = String.compareReversed;
    expect(List.sortBy(compare, ["c", "b", "a", "e", "d"]))
    |> toEqual(["e", "d", "c", "b", "a"]);
  });

  test("OrdByName", () => {
    expect(List.sort((module User.OrdByName), users2))
    |> toEqual([user1, user3, user2, user5, user4, user6])
  });

  test("OrdByAge", () => {
    expect(List.sort((module User.OrdByAge), users2))
    |> toEqual([user3, user6, user5, user1, user4, user2])
  });

  test("OrdByNameThenAge1", () => {
    expect(List.sort((module User.OrdByNameThenAge1), users2))
    |> toEqual([user3, user1, user2, user6, user5, user4])
  });

  test("OrdByNameThenAge2", () => {
    expect(List.sort((module User.OrdByNameThenAge2), users2))
    |> toEqual([user3, user1, user2, user6, user5, user4])
  });
});
