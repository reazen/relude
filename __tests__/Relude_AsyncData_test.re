open Jest;
open Expect;

module AsyncData = Relude_AsyncData;

describe("AsyncData state checks", () => {
  test("isInit when Init", () =>
    expect(AsyncData.isInit(Init)) |> toEqual(true)
  );

  test("isInit when Complete", () =>
    expect(AsyncData.isInit(Complete(3))) |> toEqual(false)
  );

  test("isBusy when Init", () =>
    expect(AsyncData.isBusy(Init)) |> toEqual(false)
  );

  test("isBusy when Loading", () =>
    expect(AsyncData.isBusy(Loading)) |> toEqual(true)
  );

  test("isBusy when Reloading", () =>
    expect(AsyncData.isBusy(Reloading(10))) |> toEqual(true)
  );

  test("isComplete when Loading", () =>
    expect(AsyncData.isComplete(Loading)) |> toEqual(false)
  );

  test("isComplete when Complete", () =>
    expect(AsyncData.isComplete(Complete(""))) |> toEqual(true)
  );

  test("getComplete when Init", () =>
    expect(AsyncData.getComplete(Init)) |> toEqual(None)
  );

  test("getComplete when Reloading", () =>
    expect(AsyncData.getComplete(Reloading(1))) |> toEqual(None)
  );

  test("getComplete when Complete", () =>
    expect(AsyncData.getComplete(Complete(1))) |> toEqual(Some(1))
  );

  test("getValue when Complete", () =>
    expect(AsyncData.getValue(Complete(1))) |> toEqual(Some(1))
  );

  test("getValue when Reloading", () =>
    expect(AsyncData.getValue(Reloading(1))) |> toEqual(Some(1))
  );

  test("getValue when Loading", () =>
    expect(AsyncData.getValue(Loading)) |> toEqual(None)
  );
});

describe("AsyncData apply", () => {
  test("map2 Init Init", () =>
    expect(AsyncData.map2((_, _) => (), Init, Init))
    |> toEqual(AsyncData.Init)
  );

  test("map2 Init Loading", () =>
    expect(AsyncData.map2((_, _) => (), Init, Loading))
    |> toEqual(AsyncData.Loading)
  );

  test("map2 Init Reloading", () =>
    expect(AsyncData.map2((_, _) => (), Init, Reloading(123)))
    |> toEqual(AsyncData.Init)
  );

  test("map2 Reloading Reloading", () =>
    expect(AsyncData.map2((+), Reloading(10), Reloading(20)))
    |> toEqual(AsyncData.Reloading(30))
  );

  test("map2 Reloading Complete", () =>
    expect(AsyncData.map2((+), Reloading(10), Complete(20)))
    |> toEqual(AsyncData.Reloading(30))
  );
});
