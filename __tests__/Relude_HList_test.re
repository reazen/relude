open Jest;
open Expect;

module HList = Relude.HList;

describe("HList", () => {
  test("empty", () =>
    expect(HList.empty) |> toEqual(HList.HNil)
  );

  test("pure", () =>
    expect(HList.pure(1)) |> toEqual(HList.HCons(1, HNil))
  );

  test("cons with empty", () =>
    expect(HList.cons(1, HNil)) |> toEqual(HList.HCons(1, HNil))
  );

  test("cons with non-empty", () =>
    expect(HList.cons(1, HCons("hi", HNil)))
    |> toEqual(HList.HCons(1, HCons("hi", HNil)))
  );

  test("uncons singleton", () =>
    expect(HList.uncons(HCons(42, HNil))) |> toEqual((42, HList.empty))
  );

  test("uncons non-singleton", () =>
    expect(HList.uncons(HCons(42, HCons("hi", HNil))))
    |> toEqual((42, HList.HCons("hi", HNil)))
  );

  test("head", () =>
    expect(HList.head(HCons(1, HCons("hi", HCons(2.0, HNil)))))
    |> toEqual(1)
  );

  test("tail", () =>
    expect(HList.tail(HCons(1, HCons("hi", HCons(2.0, HNil)))))
    |> toEqual(HList.HCons("hi", HCons(2.0, HNil)))
  );

  test("second", () =>
    expect(HList.second(HCons(1, HCons("hi", HCons(2.0, HNil)))))
    |> toEqual("hi")
  );

  test("third", () =>
    expect(HList.third(HCons(1, HCons("hi", HCons(2.0, HNil)))))
    |> toEqual(2.0)
  );

  test("third longer list", () =>
    expect(
      HList.third(HCons(1, HCons("hi", HCons(2.0, HCons((+), HNil))))),
    )
    |> toEqual(2.0)
  );

  test("fourth", () =>
    expect(
      HList.fourth(HCons(1, HCons("hi", HCons(2.0, HCons(42, HNil))))),
    )
    |> toEqual(42)
  );

  test("fifth", () =>
    expect(
      HList.fifth(
        HCons(1, HCons("hi", HCons(2.0, HCons((+), HCons(42, HNil))))),
      ),
    )
    |> toEqual(42)
  );

  test("fromTuple2", () =>
    expect(HList.fromTuple2((1, "hi")))
    |> toEqual(HList.HCons(1, HCons("hi", HNil)))
  );

  test("fromTuple3", () =>
    expect(HList.fromTuple3((1, "hi", 2.0)))
    |> toEqual(HList.HCons(1, HCons("hi", HCons(2.0, HNil))))
  );

  test("fromTuple4", () =>
    expect(HList.fromTuple4((1, "hi", 2.0, true)))
    |> toEqual(HList.HCons(1, HCons("hi", HCons(2.0, HCons(true, HNil)))))
  );

  test("fromTuple5", () =>
    expect(HList.fromTuple5((1, "hi", 2.0, true, 42)))
    |> toEqual(
         HList.HCons(
           1,
           HCons("hi", HCons(2.0, HCons(true, HCons(42, HNil)))),
         ),
       )
  );

  test("toTuple2", () =>
    expect(HList.toTuple2(HCons(1, HCons("hi", HNil))))
    |> toEqual((1, "hi"))
  );

  test("toTuple3", () =>
    expect(HList.toTuple3(HCons(1, HCons("hi", HCons(2.0, HNil)))))
    |> toEqual((1, "hi", 2.0))
  );

  test("toTuple4", () =>
    expect(
      HList.toTuple4(
        HCons(1, HCons("hi", HCons(2.0, HCons(true, HNil)))),
      ),
    )
    |> toEqual((1, "hi", 2.0, true))
  );

  test("toTuple4", () =>
    expect(
      HList.toTuple5(
        HCons(1, HCons("hi", HCons(2.0, HCons(true, HCons(42, HNil))))),
      ),
    )
    |> toEqual((1, "hi", 2.0, true, 42))
  );
});