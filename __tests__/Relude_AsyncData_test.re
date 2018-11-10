open Jest;
open Expect;

module AsyncData = Relude_AsyncData;

describe("AsyncData", () => {
  test("map2 Idle Idle", () => {
    expect(AsyncData.map2((_, _) => (), Idle, Idle)) |> toEqual(AsyncData.Idle);
  });

  test("map2 Idle Loading", () => {
    expect(AsyncData.map2((_, _) => (), Idle, Loading)) |> toEqual(AsyncData.Loading);
  });

  test("map2 Idle Loaded", () => {
    expect(AsyncData.map2((_, _) => (), Idle, Loaded(123))) |> toEqual(AsyncData.Idle);
  });

  test("map2 Loaded Loaded", () => {
    expect(AsyncData.map2((+), Loaded(10), Loaded(20))) |> toEqual(AsyncData.Loaded(30));
  });

  test("map2 Loaded Refreshing", () => {
    expect(AsyncData.map2((+), Loaded(10), Refreshing(20))) |> toEqual(AsyncData.Refreshing(30));
  });
});
