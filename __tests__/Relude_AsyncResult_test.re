open Jest;
open Expect;
module AsyncData = Relude_AsyncData;
module AsyncResult = Relude_AsyncResult;

describe("AsyncResult constructors", () => {
  test("Init", () =>
    expect(AsyncResult.init) |> toEqual(AsyncData.init)
  );

  test("Loading", () =>
    expect(AsyncResult.loading) |> toEqual(AsyncData.loading)
  );

  test("Complete Ok", () =>
    expect(AsyncResult.ok(1))
    |> toEqual(AsyncData.complete(Belt.Result.Ok(1)))
  );

  test("Complete Error", () =>
    expect(AsyncResult.error("Fail"))
    |> toEqual(AsyncData.complete(Belt.Result.Error("Fail")))
  );
});

describe("AsyncResult state checks", () => {
  test("isOk when Loading", () =>
    expect(AsyncResult.(loading |> isOk)) |> toEqual(false)
  );

  test("isOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isOk)) |> toEqual(true)
  );

  test("isOk when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> isOk)) |> toEqual(true)
  );

  test("isOk when Complete Error", () =>
    expect(AsyncResult.(error("Fail") |> isOk)) |> toEqual(false)
  );

  test("isOk when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("Fail") |> isOk)) |> toEqual(false)
  );

  test("isError when Complete Error", () =>
    expect(AsyncResult.(error("Fail") |> isError)) |> toEqual(true)
  );

  test("isError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("Fail") |> isError)) |> toEqual(true)
  );

  test("isError when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isError)) |> toEqual(false)
  );

  test("isError when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> isError)) |> toEqual(false)
  );

  test("isCompleteOk when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> isCompleteOk)) |> toEqual(true)
  );

  test("isCompleteOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isCompleteOk)) |> toEqual(false)
  );

  test("isCompleteOk when Complete Error", () =>
    expect(AsyncResult.(error("fail") |> isCompleteOk)) |> toEqual(false)
  );

  test("getOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> getOk)) |> toEqual(Some(1))
  );

  test("getOk when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> getOk)) |> toEqual(Some(1))
  );

  test("getOk when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> getOk)) |> toEqual(None)
  );

  test("getOk when Complete Error", () =>
    expect(AsyncResult.(error("fail") |> getOk)) |> toEqual(None)
  );

  test("getCompleteOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> getCompleteOk)) |> toEqual(None)
  );

  test("getCompleteOk when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> getCompleteOk)) |> toEqual(Some(1))
  );

  test("getError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> getError))
    |> toEqual(Some("fail"))
  );

  test("getError when Complete Error", () =>
    expect(AsyncResult.(error("fail") |> getError))
    |> toEqual(Some("fail"))
  );

  test("getError when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> getError)) |> toEqual(None)
  );

  test("getError when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> getError)) |> toEqual(None)
  );

  test("getCompleteError when Complete Error", () =>
    expect(AsyncResult.(error("fail") |> getCompleteError))
    |> toEqual(Some("fail"))
  );

  test("getCompleteError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> getCompleteError))
    |> toEqual(None)
  );

  test("eqBy different constructors", () =>
    expect(
      AsyncResult.eqBy(
        (_, _) => true,
        (_, _) => true,
        AsyncResult.init,
        AsyncResult.loading,
      ),
    )
    |> toEqual(false)
  );

  test("eqBy same success value", () =>
    expect(
      AsyncResult.eqBy(
        Relude_String.eq,
        Relude_Int.eq,
        AsyncResult.ok(1),
        AsyncResult.ok(1),
      ),
    )
    |> toEqual(true)
  );
});
