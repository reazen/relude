open Jest;
open Expect;

module NonEmptyList = Relude_NonEmpty.List;
module Ior = Relude_Ior;
open Ior;

module Error = {
  type t =
    | Unknown;

  module Type: BsAbstract.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};
module IorE = Ior.WithErrors(NonEmptyList.SemigroupAny, Error.Type);

describe("Ior", () => {
  test("pure", () =>
    expect(Ior.pure(5)) |> toEqual(IOk(5))
  );

  testAll(
    "bind",
    [
      (Ior.ok(1), IOk(2)),
      (Ior.error("Warning"), IError("Warning")),
      (Ior.both(1, "Warning"), IOk(2)),
    ],
    ((ior, expected)) =>
    expect(Ior.bind(ior, a => Ior.ok(a + 1))) |> toEqual(expected)
  );

  testAll(
    "flatMap",
    [
      (Ior.ok(1), IOk(2)),
      (Ior.error("Warning"), IError("Warning")),
      (Ior.both(1, "Warning"), IOk(2)),
    ],
    ((ior, expected)) =>
    expect(Ior.flatMap(a => Ior.ok(a + 1), ior)) |> toEqual(expected)
  );

  test("ok", () =>
    expect(Ior.ok(5)) |> toEqual(IOk(5))
  );

  test("error", () =>
    expect(Ior.error(5)) |> toEqual(IError(5))
  );

  test("both", () =>
    expect(Ior.both(5, "Warning")) |> toEqual(IBoth(5, "Warning"))
  );

  testAll(
    "isOk",
    [
      (Ior.pure(5), true),
      (Ior.error("Warning"), false),
      (Ior.both(5, "Warning"), false),
    ],
    ((actual, expected)) =>
    expect(Ior.isOk(actual)) |> toEqual(expected)
  );

  testAll(
    "isError",
    [
      (Ior.pure(5), false),
      (Ior.error("Warning"), true),
      (Ior.both(5, "Warning"), false),
    ],
    ((actual, expected)) =>
    expect(Ior.isError(actual)) |> toEqual(expected)
  );

  testAll(
    "isBoth",
    [
      (Ior.pure(5), false),
      (Ior.error("Warning"), false),
      (Ior.both(5, "Warning"), true),
    ],
    ((actual, expected)) =>
    expect(Ior.isBoth(actual)) |> toEqual(expected)
  );

  test("map IOk", () =>
    expect(Ior.map(a => a + 2, IOk(1))) |> toEqual(IOk(3))
  );

  test("map IError", () =>
    expect(Ior.map(a => a + 2, IError("Warning")))
    |> toEqual(IError("Warning"))
  );

  test("map IBoth", () =>
    expect(Ior.map(a => a + 2, IBoth(5, "Warning")))
    |> toEqual(IBoth(7, "Warning"))
  );

  test("tap IOk", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        ok => actual := ok,
        _error => actual := (-1),
        (_ok, _error) => actual := (-2),
        IOk(1),
      );

    expect(actual^) |> toEqual(1);
  });

  test("tap IError", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        _ok => actual := (-1),
        error => actual := error,
        (_ok, _error) => actual := (-2),
        IError(1),
      );

    expect(actual^) |> toEqual(1);
  });

  test("tap IBoth", () => {
    let actual = ref(0);
    let _ =
      Ior.tap(
        _ok => actual := (-1),
        _error => actual := (-2),
        (ok, error) => actual := ok + error,
        IBoth(1, 2),
      );

    expect(actual^) |> toEqual(3);
  });

  testAll(
    "tapOk",
    [(IOk(1), 1), (IError("Warning"), 0), (IBoth(5, "Warning"), 0)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapOk(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapError",
    [(IOk(1), 0), (IError(1), 1), (IBoth(1, 2), 0)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapError(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapBoth",
    [(IOk(1), 0), (IError(2), 0), (IBoth(1, 2), 3)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapBoth((a, e) => actual := a + e, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapOkOrBothOk",
    [(IOk(1), 1), (IError(2), 0), (IBoth(1, 2), 1)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapOkOrBothOk(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  testAll(
    "tapErrorOrBothError",
    [(IOk(1), 0), (IError(2), 2), (IBoth(1, 2), 2)],
    ((ior, expected)) => {
      let actual = ref(0);
      let _ = Ior.tapErrorOrBothError(a => actual := a, ior);
      expect(actual^) |> toEqual(expected);
    },
  );

  test("map2 IOk IOk", () =>
    expect(Ior.map2((++), (a, b) => a + b, IOk(1), IOk(2)))
    |> toEqual(IOk(3))
  );

  test("map2 IOk IError", () =>
    expect(Ior.map2((++), (a, b) => a + b, IOk(1), IError("W2")))
    |> toEqual(IError("W2"))
  );

  test("map2 IOk IBoth", () =>
    expect(Ior.map2((++), (a, b) => a + b, IOk(1), IBoth(2, "W2")))
    |> toEqual(IBoth(3, "W2"))
  );

  test("map2 IError IOk", () =>
    expect(Ior.map2((++), (a, b) => a + b, IError("W1"), IOk(1)))
    |> toEqual(IError("W1"))
  );

  test("map2 IError IError", () =>
    expect(Ior.map2((++), (a, b) => a + b, IError("W1"), IError("W2")))
    |> toEqual(IError("W1W2"))
  );

  test("map2 IError IBoth", () =>
    expect(Ior.map2((++), (a, b) => a + b, IError("W1"), IBoth(1, "W2")))
    |> toEqual(IError("W1W2"))
  );

  test("map2 IBoth IOk", () =>
    expect(Ior.map2((++), (a, b) => a + b, IBoth(1, "W1"), IOk(2)))
    |> toEqual(IBoth(3, "W1"))
  );

  test("map2 IBoth IError", () =>
    expect(Ior.map2((++), (a, b) => a + b, IBoth(1, "W1"), IError("W2")))
    |> toEqual(IError("W1W2"))
  );

  test("map2 IBoth IBoth", () =>
    expect(
      Ior.map2((++), (a, b) => a + b, IBoth(1, "W1"), IBoth(2, "W2")),
    )
    |> toEqual(IBoth(3, "W1W2"))
  );

  test("map3 IOk IOk IOk", () =>
    expect(
      Ior.map3((++), (a, b, c) => a + b + c, IOk(1), IOk(2), IOk(3)),
    )
    |> toEqual(IOk(6))
  );

  test("map4 IOk IOk IOk IOk", () =>
    expect(
      Ior.map4(
        (++),
        (a, b, c, d) => a + b + c + d,
        IOk(1),
        IOk(2),
        IOk(3),
        IOk(4),
      ),
    )
    |> toEqual(IOk(10))
  );

  test("map5 IOk IOk IOk IOk IOk", () =>
    expect(
      Ior.map5(
        (++),
        (a, b, c, d, f) => a + b + c + d + f,
        IOk(1),
        IOk(2),
        IOk(3),
        IOk(4),
        IOk(5),
      ),
    )
    |> toEqual(IOk(15))
  );

  describe("WithErrors", () => {
    test("map", () =>
      Ior.error(NonEmptyList.pure(Error.Unknown))
      |> IorE.map(a => a + 1)
      |> expect
      |> toEqual(IError(NonEmptyList.pure(Error.Unknown)))
    );

    test("apply", () =>
      Ior.error(NonEmptyList.pure(Error.Unknown))
      |> IorE.apply(Ior.ok(a => a + 1))
      |> expect
      |> toEqual(IError(NonEmptyList.pure(Error.Unknown)))
    );
  });
});