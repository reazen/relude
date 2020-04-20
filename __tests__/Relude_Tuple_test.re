open Jest;
open Expect;
open Relude.Globals;

afterAll(Bisect.Runtime.write_coverage_data);

describe("Tuple constructors", () => {
  test("make", () =>
    expect(Tuple.make("A", 0)) |> toEqual(("A", 0))
  );

  test("make3", () =>
    expect(Tuple.make3("A", 0, true)) |> toEqual(("A", 0, true))
  );

  test("make4", () =>
    expect(Tuple.make4("A", 0, true, false))
    |> toEqual(("A", 0, true, false))
  );

  test("make5", () =>
    expect(Tuple.make5("A", 0, true, false, 3.14))
    |> toEqual(("A", 0, true, false, 3.14))
  );
});

describe("Tuple fromArray", () => {
  test("fromArray (success)", () =>
    expect(Tuple.fromArray([|0, 1|])) |> toEqual(Some((0, 1)))
  );

  test("fromArray (failure on long array)", () =>
    expect(Tuple.fromArray([|0, 1, 2|])) |> toEqual(None)
  );

  test("fromArray3 (success)", () =>
    expect(Tuple.fromArray3([|0, 1, 2|])) |> toEqual(Some((0, 1, 2)))
  );

  test("fromArray3 (failure on short array)", () =>
    expect(Tuple.fromArray3([|0, 1|])) |> toEqual(None)
  );

  test("fromArray4 (success)", () =>
    expect(Tuple.fromArray4([|0, 1, 2, 3|])) |> toEqual(Some((0, 1, 2, 3)))
  );

  test("fromArray4 (failure on long array)", () =>
    expect(Tuple.fromArray4([|0, 1, 2, 3, 4|])) |> toEqual(None)
  );

  test("fromArray5 (success)", () =>
    expect(Tuple.fromArray5([|0, 1, 2, 3, 4|]))
    |> toEqual(Some((0, 1, 2, 3, 4)))
  );

  test("fromArray5 (failure on long array)", () =>
    expect(Tuple.fromArray5([|0, 1, 2, 3, 4, 5|])) |> toEqual(None)
  );
});

describe("Tuple fromArrayAtLeast", () => {
  test("fromArrayAtLeast2", () =>
    expect(Tuple.fromArrayAtLeast2(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A")))
  );

  test("fromArrayAtLeast3", () =>
    expect(Tuple.fromArrayAtLeast3(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A")))
  );

  test("fromArrayAtLeast4", () =>
    expect(Tuple.fromArrayAtLeast4(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A")))
  );

  test("fromArrayAtLeast5", () =>
    expect(Tuple.fromArrayAtLeast5(Array.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A", "A")))
  );
});

describe("Tuple fromList", () => {
  test("fromList (success)", () =>
    expect(Tuple.fromList([0, 1])) |> toEqual(Some((0, 1)))
  );

  test("fromList (failure on long array)", () =>
    expect(Tuple.fromList([0, 1, 2])) |> toEqual(None)
  );

  test("fromList3 (success)", () =>
    expect(Tuple.fromList3([0, 1, 2])) |> toEqual(Some((0, 1, 2)))
  );

  test("fromList3 (failure on short array)", () =>
    expect(Tuple.fromList3([0, 1])) |> toEqual(None)
  );

  test("fromList4 (success)", () =>
    expect(Tuple.fromList4([0, 1, 2, 3])) |> toEqual(Some((0, 1, 2, 3)))
  );

  test("fromList4 (failure on long array)", () =>
    expect(Tuple.fromList4([0, 1, 2, 3, 4])) |> toEqual(None)
  );

  test("fromList4 (failure on long array)", () =>
    expect(Tuple.fromList5([0, 1, 2, 3, 4, 5])) |> toEqual(None)
  );

  test("fromList5 (success)", () =>
    expect(Tuple.fromList5([0, 1, 2, 3, 4]))
    |> toEqual(Some((0, 1, 2, 3, 4)))
  );
});

describe("Tuple fromListAtLeast", () => {
  test("fromListAtLeast2", () =>
    expect(Tuple.fromListAtLeast2(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A")))
  );

  test("fromListAtLeast3", () =>
    expect(Tuple.fromListAtLeast3(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A")))
  );

  test("fromListAtLeast4", () =>
    expect(Tuple.fromListAtLeast4(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A")))
  );

  test("fromListAtLeast5", () =>
    expect(Tuple.fromListAtLeast5(List.repeat(6, "A")))
    |> toEqual(Some(("A", "A", "A", "A", "A")))
  );
});

describe("Tuple apply", () => {
  test("apply2", () =>
    expect((1, 2) |> Tuple.apply2((a, b) => a + b)) |> toEqual(3)
  );

  test("apply3", () =>
    expect((1, 2, 3) |> Tuple.apply3((a, b, c) => a + b + c)) |> toEqual(6)
  );

  test("apply4", () =>
    expect((1, 2, 3, 4) |> Tuple.apply4((a, b, c, d) => a + b + c + d))
    |> toEqual(10)
  );

  test("apply5", () =>
    expect(
      (1, 2, 3, 4, 5) |> Tuple.apply5((a, b, c, d, e) => a + b + c + d + e),
    )
    |> toEqual(15)
  );
});

describe("Tuple showBy", () => {
  test("showBy2", () => {
    expect(Tuple.showBy2(Int.show, String.show, (42, "hi")))
    |> toEqual("(42, hi)")
  });

  test("showBy3", () => {
    expect(
      Tuple.showBy3(Int.show, String.show, Bool.show, (42, "hi", true)),
    )
    |> toEqual("(42, hi, true)")
  });

  test("showBy4", () => {
    expect(
      Tuple.showBy4(
        Int.show,
        String.show,
        Bool.show,
        Unit.show,
        (42, "hi", true, ()),
      ),
    )
    |> toEqual("(42, hi, true, ())")
  });

  test("showBy5", () => {
    expect(
      Tuple.showBy5(
        Int.show,
        String.show,
        Bool.show,
        Unit.show,
        Float.show,
        (42, "hi", true, (), 99.9),
      ),
    )
    |> toEqual("(42, hi, true, (), 99.9)")
  });
});

module Eq2 = Tuple.WithEqs2(Int.Eq, String.Eq);
module Eq3 = Tuple.WithEqs3(Int.Eq, String.Eq, Bool.Eq);
module Eq4 = Tuple.WithEqs4(Int.Eq, String.Eq, Bool.Eq, Unit.Eq);
module Eq5 = Tuple.WithEqs5(Int.Eq, String.Eq, Bool.Eq, Unit.Eq, Float.Eq);

describe("Tuple WithEqs", () => {
  test("WithEqs2", () => {
    expect(Eq2.eq((42, "hi"), (42, "hi"))) |> toEqual(true)
  });

  test("WithEqs3", () => {
    expect(Eq3.eq((42, "hi", true), (42, "hi", true))) |> toEqual(true)
  });

  test("WithEqs4", () => {
    expect(Eq4.eq((42, "hi", true, ()), (42, "hi", true, ())))
    |> toEqual(true)
  });

  test("WithEqs5", () => {
    expect(Eq5.eq((42, "hi", true, (), 99.9), (42, "hi", true, (), 99.9)))
    |> toEqual(true)
  });
});

module User = {
  type t = {
    first: string,
    last: string,
    age: int,
    email: string,
    approved: bool,
  };

  let user1 = {
    first: "Andy",
    last: "White",
    age: 99,
    email: "test@example.com",
    approved: true,
  };

  module EqBy2 =
    Tuple2.EqBy(
      String.Eq,
      String.Eq,
      {
        type a = t;
        type b = (string, string);
        let f = user => (user.first, user.last);
      },
    );

  module EqBy3 =
    Tuple3.EqBy(
      String.Eq,
      String.Eq,
      Int.Eq,
      {
        type a = t;
        type b = (string, string, int);
        let f = user => (user.first, user.last, user.age);
      },
    );

  module EqBy4 =
    Tuple4.EqBy(
      String.Eq,
      String.Eq,
      Int.Eq,
      String.Eq,
      {
        type a = t;
        type b = (string, string, int, string);
        let f = user => (user.first, user.last, user.age, user.email);
      },
    );

  module EqBy5 =
    Tuple5.EqBy(
      String.Eq,
      String.Eq,
      Int.Eq,
      String.Eq,
      Bool.Eq,
      {
        type a = t;
        type b = (string, string, int, string, bool);
        let f = user => (
          user.first,
          user.last,
          user.age,
          user.email,
          user.approved,
        );
      },
    );

  module OrdBy2 =
    Tuple2.OrdBy(
      String.Ord,
      String.Ord,
      {
        type a = t;
        type b = (string, string);
        let f = user => (user.first, user.last);
      },
    );

  module OrdBy3 =
    Tuple3.OrdBy(
      String.Ord,
      String.Ord,
      Int.Ord,
      {
        type a = t;
        type b = (string, string, int);
        let f = user => (user.first, user.last, user.age);
      },
    );

  module OrdBy4 =
    Tuple4.OrdBy(
      String.Ord,
      String.Ord,
      Int.Ord,
      String.Ord,
      {
        type a = t;
        type b = (string, string, int, string);
        let f = user => (user.first, user.last, user.age, user.email);
      },
    );

  module OrdBy5 =
    Tuple5.OrdBy(
      String.Ord,
      String.Ord,
      Int.Ord,
      String.Ord,
      Bool.Ord,
      {
        type a = t;
        type b = (string, string, int, string, bool);
        let f = user => (
          user.first,
          user.last,
          user.age,
          user.email,
          user.approved,
        );
      },
    );
};

describe("Tuple EqBy", () => {
  test("EqBy2", () => {
    expect(User.EqBy2.eq(User.user1, User.user1)) |> toEqual(true)
  });

  test("EqBy3", () => {
    expect(User.EqBy3.eq(User.user1, User.user1)) |> toEqual(true)
  });

  test("EqBy4", () => {
    expect(User.EqBy4.eq(User.user1, User.user1)) |> toEqual(true)
  });

  test("EqBy5", () => {
    expect(User.EqBy5.eq(User.user1, User.user1)) |> toEqual(true)
  });
});

describe("Tuple compareBy", () => {
  test("compareBy2", () => {
    expect(
      Tuple.compareBy2(String.compare, Int.compare, ("a", 42), ("a", 43)),
    )
    |> toEqual(`less_than)
  });

  test("compareBy3", () => {
    expect(
      Tuple.compareBy3(
        String.compare,
        Int.compare,
        Bool.compare,
        ("a", 42, true),
        ("a", 42, false),
      ),
    )
    |> toEqual(`greater_than)
  });

  test("compareBy4", () => {
    expect(
      Tuple.compareBy4(
        String.compare,
        Int.compare,
        Bool.compare,
        Float.compare,
        ("a", 42, true, 98.8),
        ("a", 42, true, 99.9),
      ),
    )
    |> toEqual(`less_than)
  });

  test("compareBy5", () => {
    expect(
      Tuple.compareBy5(
        String.compare,
        Int.compare,
        Bool.compare,
        Unit.compare,
        Float.compare,
        ("a", 42, true, (), 99.9),
        ("a", 42, true, (), 99.7),
      ),
    )
    |> toEqual(`greater_than)
  });
});

module Ord2 = Tuple.WithOrds2(Int.Ord, String.Ord);
module Ord3 = Tuple.WithOrds3(Int.Ord, String.Ord, Bool.Ord);
module Ord4 = Tuple.WithOrds4(Int.Ord, String.Ord, Bool.Ord, Unit.Ord);
module Ord5 =
  Tuple.WithOrds5(Int.Ord, String.Ord, Bool.Ord, Unit.Ord, Float.Ord);

describe("Tuple WithOrds", () => {
  test("WithOrds2", () => {
    expect(Ord2.compare((42, "hi"), (42, "hi"))) |> toEqual(`equal_to)
  });

  test("WithOrds3", () => {
    expect(Ord3.compare((42, "hi", true), (42, "hi", true)))
    |> toEqual(`equal_to)
  });

  test("WithOrds4", () => {
    expect(Ord4.compare((42, "hi", true, ()), (42, "hi", true, ())))
    |> toEqual(`equal_to)
  });

  test("WithOrds5", () => {
    expect(
      Ord5.compare((42, "hi", true, (), 99.9), (42, "hi", true, (), 99.9)),
    )
    |> toEqual(`equal_to)
  });
});

describe("Tuple OrdBy", () => {
  test("OrdBy2", () => {
    expect(User.OrdBy2.compare(User.user1, User.user1))
    |> toEqual(`equal_to)
  });

  test("OrdBy3", () => {
    expect(User.OrdBy3.compare(User.user1, User.user1))
    |> toEqual(`equal_to)
  });

  test("OrdBy4", () => {
    expect(User.OrdBy4.compare(User.user1, User.user1))
    |> toEqual(`equal_to)
  });

  test("OrdBy5", () => {
    expect(User.OrdBy5.compare(User.user1, User.user1))
    |> toEqual(`equal_to)
  });
});