open Jest;
open Expect;

module Z = Relude.ListZipper;
type t('a) = Z.t('a) = | SZ(list('a), 'a, list('a));

describe("ListZipper", () => {
  test("make", () =>
    expect(Z.make([2, 1], 3, [4, 5])) |> toEqual(SZ([2, 1], 3, [4, 5]))
  );

  test("makeWithLeft", () =>
    expect(Z.makeWithLeft([2, 1], 3)) |> toEqual(SZ([2, 1], 3, []))
  );

  test("makeWithRight", () =>
    expect(Z.makeWithRight(3, [4, 5])) |> toEqual(SZ([], 3, [4, 5]))
  );
});