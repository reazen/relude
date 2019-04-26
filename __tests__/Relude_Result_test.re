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

  test("flatMap", () =>
    expect(Result.flatMap(a => Belt.Result.Ok(a + 2), Belt.Result.Ok(1)))
    |> toEqual(Belt.Result.Ok(3))
  );

  test("bind", () =>
    expect(Result.bind(Belt.Result.Ok(1), a => Belt.Result.Ok(a + 2)))
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

  test("merge Error", () => {
    expect(Result.merge(Belt.Result.Error(1))) |> toEqual(1)
  });

  test("merge Ok", () => {
    expect(Result.merge(Belt.Result.Ok(1))) |> toEqual(1)
  });

  test("isOk when Ok", () =>
    expect(Result.isOk(Belt.Result.Ok(1))) |> toEqual(true)
  );

  test("isOk when Error", () =>
    expect(Result.isOk(Belt.Result.Error(1))) |> toEqual(false)
  );

  test("isError when Error", () =>
    expect(Result.isError(Belt.Result.Error(1))) |> toEqual(true)
  );

  test("isError when Ok", () =>
    expect(Result.isError(Belt.Result.Ok(1))) |> toEqual(false)
  );

  test("getOk when Ok", () =>
    expect(Result.getOk(Belt.Result.Ok(1))) |> toEqual(Some(1))
  );

  test("getOk when Error", () =>
    expect(Result.getOk(Belt.Result.Error(1))) |> toEqual(None)
  );

  test("getError when Error", () =>
    expect(Result.getError(Belt.Result.Error(1))) |> toEqual(Some(1))
  );

  test("getError when Ok", () =>
    expect(Result.getError(Belt.Result.Ok(1))) |> toEqual(None)
  );
});