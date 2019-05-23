open Jest;
open Expect;

module Ior = Relude_Ior;
open Ior;

describe("Ior", () => {
  test("pure", () =>
    expect(Ior.pure(5)) |> toEqual(IOk(5))
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
});