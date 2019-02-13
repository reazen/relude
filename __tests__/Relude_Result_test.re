open Jest;
open Expect;

module Result = Relude_Result;

describe("Result", () => {
  test("pure", () =>
    expect(Result.pure(1)) |> toEqual(Belt.Result.Ok(1))
  );

  test("map", () =>
    expect(Result.map(a => a + 2, Belt.Result.Ok(1)))
    |> toEqual(Belt.Result.Ok(3))
  );

  test("fold Ok", () =>
    expect(Result.fold(_ => "error", _ => "ok", Belt.Result.Ok(1)))
    |> toEqual("ok")
  );

  test("fold Error", () =>
    expect(Result.fold(_ => "error", _ => "ok", Belt.Result.Error(1)))
    |> toEqual("error")
  );
});