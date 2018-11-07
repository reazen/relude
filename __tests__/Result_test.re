open Jest;
open Expect;

describe("Result", () => {
  test("pure", () =>
    expect(Result.pure(1)) |> toEqual(Belt.Result.Ok(1))
  );

  test("map", () =>
    expect(Result.map(a => a + 2, Belt.Result.Ok(1)))
    |> toEqual(Belt.Result.Ok(3))
  );
});
