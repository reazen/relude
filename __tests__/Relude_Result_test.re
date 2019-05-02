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

  test("merge Error", () =>
    expect(Result.merge(Belt.Result.Error(1))) |> toEqual(1)
  );

  test("merge Ok", () =>
    expect(Result.merge(Belt.Result.Ok(1))) |> toEqual(1)
  );

  test("flip Ok", () =>
    expect(Result.flip(Belt.Result.Ok(1)))
    |> toEqual(Belt.Result.Error(1))
  );

  test("flip Error", () =>
    expect(Result.flip(Belt.Result.Error("my error")))
    |> toEqual(Belt.Result.Ok("my error"))
  );

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

  test("eqBy when eq, both Ok", () =>
    expect(Result.eqBy(Relude_Int.eq, Relude_String.eq, Ok("a"), Ok("a")))
    |> toEqual(true)
  );

  test("eqBy when not eq, both Ok", () =>
    expect(Result.eqBy((_, _) => true, Relude_Int.eq, Ok(1), Ok(2)))
    |> toEqual(false)
  );

  test("eqBy when first Ok, second Error", () =>
    expect(Result.eqBy((_, _) => true, (_, _) => true, Ok(1), Error(1)))
    |> toEqual(false)
  );

  test("eqBy when first Error, second Ok", () =>
    expect(Result.eqBy((_, _) => true, (_, _) => true, Error(1), Ok(1)))
    |> toEqual(false)
  );

  test("eqBy when eq, both Error", () =>
    expect(Result.eqBy(Relude_Int.eq, (_, _) => true, Error(1), Error(1)))
    |> toEqual(true)
  );

  test("eqBy when not eq, both Error", () =>
    expect(
      Result.eqBy((_, _) => false, (_, _) => true, Error(1), Error(1)),
    )
    |> toEqual(false)
  );
});
