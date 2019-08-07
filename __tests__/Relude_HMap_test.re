open Jest;
open Expect;

module HMap = Relude.HMap;

describe("HMap", () => {
  test("empty", () =>
    expect(HMap.empty) |> toEqual(HMap.empty)
  );

  test("hasKey non-empty", () => {
    let intKey = HMap.Key.create();
    let stringKey = HMap.Key.create();
    let map =
      HMap.empty |> HMap.add(intKey, 5) |> HMap.add(stringKey, "hey");
    expect(HMap.hasKey(intKey, map) && HMap.hasKey(stringKey, map))
    |> toEqual(true);
  });

  test("find success", () => {
    let intKey = HMap.Key.create();
    let map = HMap.empty |> HMap.add(intKey, 5);
    expect(HMap.find(intKey, map)) |> toEqual(Some(5));
  });

  test("find failure", () => {
    let intKey = HMap.Key.create();
    let stringKey = HMap.Key.create();
    let map = HMap.empty |> HMap.add(intKey, 5);
    expect(HMap.find(stringKey, map)) |> toEqual(None);
  });
});