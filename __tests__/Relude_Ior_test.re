open Jest;
open Expect;

module NonEmptyList = Relude_NonEmpty.List;
module Ior = Relude_Ior;
open Ior;

module That = {
  type t =
    | Unknown;
};

module IorT = Ior.WithThats(NonEmptyList.SemigroupAny, That);

describe("Ior", () => {
  test("pure", () =>
    expect(Ior.pure(5)) |> toEqual(This(5))
  );

  testAll(
    "bind",
    [
      (Ior.this(1), This(2)),
      (Ior.that("Warning"), That("Warning")),
      (Ior.both(1, "Warning"), This(2)),
    ],
    ((ior, expected)) =>
    expect(Ior.bind(ior, a => Ior.this(a + 1))) |> toEqual(expected)
  );

  testAll(
    "flatMap",
    [
      (Ior.this(1), This(2)),
      (Ior.that("Warning"), That("Warning")),
      (Ior.both(1, "Warning"), This(2)),
    ],
    ((ior, expected)) =>
    expect(Ior.flatMap(a => Ior.this(a + 1), ior)) |> toEqual(expected)
  );

  test("this", () =>
    expect(Ior.this(5)) |> toEqual(This(5))
  );

  test("that", () =>
    expect(Ior.that(5)) |> toEqual(That(5))
  );

  test("both", () =>
    expect(Ior.both(5, "Warning")) |> toEqual(Both(5, "Warning"))
  );

  testAll(
    "isThis",
    [
      (Ior.pure(5), true),
      (Ior.that("Warning"), false),
      (Ior.both(5, "Warning"), false),
    ],
    ((actual, expected)) =>
    expect(Ior.isThis(actual)) |> toEqual(expected)
  );

  testAll(
    "isThat",
    [
      (Ior.pure(5), false),
      (Ior.that("Warning"), true),
      (Ior.both(5, "Warning"), false),
    ],
    ((actual, expected)) =>
    expect(Ior.isThat(actual)) |> toEqual(expected)
  );

  testAll(
    "isBoth",
    [
      (Ior.pure(5), false),
      (Ior.that("Warning"), false),
      (Ior.both(5, "Warning"), true),
    ],
    ((actual, expected)) =>
    expect(Ior.isBoth(actual)) |> toEqual(expected)
  );

  test("map This", () =>
    expect(Ior.map(a => a + 2, This(1))) |> toEqual(This(3))
  );

  test("map That", () =>
    expect(Ior.map(a => a + 2, That("Warning")))
    |> toEqual(That("Warning"))
  );

  test("map Both", () =>
    expect(Ior.map(a => a + 2, Both(5, "Warning")))
    |> toEqual(Both(7, "Warning"))
  );

  test("tap This", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        this => actual := this,
        _that => actual := (-1),
        (_this, _that) => actual := (-2),
        This(1),
      );

    expect(actual^) |> toEqual(1);
  });

  test("tap That", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        _this => actual := (-1),
        that => actual := that,
        (_this, _that) => actual := (-2),
        That(1),
      );

    expect(actual^) |> toEqual(1);
  });

  test("tap Both", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        _this => actual := (-1),
        _that => actual := (-2),
        (this, that) => actual := this + that,
        Both(1, 2),
      );

    expect(actual^) |> toEqual(3);
  });

  testAll(
    "tapThis",
    [(This(1), 1), (That("Warning"), 0), (Both(5, "Warning"), 0)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapThis(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapThat",
    [(This(1), 0), (That(1), 1), (Both(1, 2), 0)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapThat(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapBoth",
    [(This(1), 0), (That(2), 0), (Both(1, 2), 3)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapBoth((a, e) => actual := a + e, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapThisOrBoth",
    [(This(1), 1), (That(2), 0), (Both(1, 2), 1)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapThisOrBoth(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapThatOrBoth",
    [(This(1), 0), (That(2), 2), (Both(1, 2), 2)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapThatOrBoth(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  test("map2 This This", () =>
    expect(Ior.map2((++), (a, b) => a + b, This(1), This(2)))
    |> toEqual(This(3))
  );

  test("map2 This That", () =>
    expect(Ior.map2((++), (a, b) => a + b, This(1), That("W2")))
    |> toEqual(That("W2"))
  );

  test("map2 This Both", () =>
    expect(Ior.map2((++), (a, b) => a + b, This(1), Both(2, "W2")))
    |> toEqual(Both(3, "W2"))
  );

  test("map2 That This", () =>
    expect(Ior.map2((++), (a, b) => a + b, That("W1"), This(1)))
    |> toEqual(That("W1"))
  );

  test("map2 That That", () =>
    expect(Ior.map2((++), (a, b) => a + b, That("W1"), That("W2")))
    |> toEqual(That("W1W2"))
  );

  test("map2 That Both", () =>
    expect(Ior.map2((++), (a, b) => a + b, That("W1"), Both(1, "W2")))
    |> toEqual(That("W1W2"))
  );

  test("map2 Both This", () =>
    expect(Ior.map2((++), (a, b) => a + b, Both(1, "W1"), This(2)))
    |> toEqual(Both(3, "W1"))
  );

  test("map2 Both That", () =>
    expect(Ior.map2((++), (a, b) => a + b, Both(1, "W1"), That("W2")))
    |> toEqual(That("W1W2"))
  );

  test("map2 Both Both", () =>
    expect(Ior.map2((++), (a, b) => a + b, Both(1, "W1"), Both(2, "W2")))
    |> toEqual(Both(3, "W1W2"))
  );

  test("map3 This This This", () =>
    expect(
      Ior.map3((++), (a, b, c) => a + b + c, This(1), This(2), This(3)),
    )
    |> toEqual(This(6))
  );

  test("map4 This This This This", () =>
    expect(
      Ior.map4(
        (++),
        (a, b, c, d) => a + b + c + d,
        This(1),
        This(2),
        This(3),
        This(4),
      ),
    )
    |> toEqual(This(10))
  );

  test("map5 This This This This This", () =>
    expect(
      Ior.map5(
        (++),
        (a, b, c, d, f) => a + b + c + d + f,
        This(1),
        This(2),
        This(3),
        This(4),
        This(5),
      ),
    )
    |> toEqual(This(15))
  );

  describe("WithThats", () => {
    test("map", () =>
      Ior.that(NonEmptyList.pure(That.Unknown))
      |> IorT.map(a => a + 1)
      |> expect
      |> toEqual(That(NonEmptyList.pure(That.Unknown)))
    );

    test("apply", () =>
      Ior.that(NonEmptyList.pure(That.Unknown))
      |> IorT.apply(Ior.this(a => a + 1))
      |> expect
      |> toEqual(That(NonEmptyList.pure(That.Unknown)))
    );
  });
});