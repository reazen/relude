open Jest;
open Expect;

module AsyncData = Relude_AsyncData;

describe("AsyncData", () => {
  test("map2 Init Init", () => {
    expect(AsyncData.map2((_, _) => (), Init, Init)) |> toEqual(AsyncData.Init);
  });

  test("map2 Init Loading", () => {
    expect(AsyncData.map2((_, _) => (), Init, Loading)) |> toEqual(AsyncData.Loading);
  });

  test("map2 Init Reloading", () => {
    expect(AsyncData.map2((_, _) => (), Init, Reloading(123))) |> toEqual(AsyncData.Init);
  });

  test("map2 Reloading Reloading", () => {
    expect(AsyncData.map2((+), Reloading(10), Reloading(20))) |> toEqual(AsyncData.Reloading(30));
  });

  test("map2 Reloading Complete", () => {
    expect(AsyncData.map2((+), Reloading(10), Complete(20))) |> toEqual(AsyncData.Reloading(30));
  });
});
