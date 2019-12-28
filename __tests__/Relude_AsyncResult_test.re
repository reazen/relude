open Jest;
open Expect;

module AsyncData = Relude.AsyncData;
module AsyncResult = Relude.AsyncResult;
module Option = Relude.Option;
module Result = Relude.Result;

describe("AsyncResult", () => {
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

  test("isReloadingOk when Complete Ok", () =>
    expect(AsyncResult.(completeOk(1) |> isReloadingOk)) |> toEqual(false)
  );

  test("isReloadingOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isReloadingOk)) |> toEqual(true)
  );

  test("isReloadingOk when Complete Error", () =>
    expect(AsyncResult.(completeError("fail") |> isReloadingOk))
    |> toEqual(false)
  );

  test("isReloadingError when Complete Ok", () =>
    expect(AsyncResult.(completeOk(1) |> isReloadingError))
    |> toEqual(false)
  );

  test("isReloadingError when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isReloadingError))
    |> toEqual(false)
  );

  test("isReloadingError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> isReloadingError))
    |> toEqual(true)
  );

  test("isCompleteOk when Complete Ok", () =>
    expect(AsyncResult.(completeOk(1) |> isCompleteOk)) |> toEqual(true)
  );

  test("isCompleteOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isCompleteOk)) |> toEqual(false)
  );

  test("isCompleteOk when Complete Error", () =>
    expect(AsyncResult.(completeError("fail") |> isCompleteOk))
    |> toEqual(false)
  );

  test("isCompleteError when Complete Ok", () =>
    expect(AsyncResult.(completeOk(1) |> isCompleteError)) |> toEqual(false)
  );

  test("isCompleteError when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> isCompleteError))
    |> toEqual(false)
  );

  test("isCompleteError when Complete Error", () =>
    expect(AsyncResult.(completeError("fail") |> isCompleteError))
    |> toEqual(true)
  );

  test("getOk when Init", () =>
    expect(AsyncResult.(init |> getOk)) |> toEqual(None)
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

  test("getError when Init", () =>
    expect(AsyncResult.(init |> getError)) |> toEqual(None)
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

  test("getCompleteOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> getCompleteOk)) |> toEqual(None)
  );

  test("getCompleteOk when Complete Ok", () =>
    expect(AsyncResult.(ok(1) |> getCompleteOk)) |> toEqual(Some(1))
  );

  test("getCompleteError when Complete Error", () =>
    expect(AsyncResult.(error("fail") |> getCompleteError))
    |> toEqual(Some("fail"))
  );

  test("getCompleteError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> getCompleteError))
    |> toEqual(None)
  );

  test("getReloadingOk when Reloading Ok", () =>
    expect(AsyncResult.(reloadingOk(1) |> getReloadingOk))
    |> toEqual(Some(1))
  );

  test("getReloadingOk when Complete Ok", () =>
    expect(AsyncResult.(completeOk(1) |> getReloadingOk)) |> toEqual(None)
  );

  test("getReloadingError when Complete Error", () =>
    expect(AsyncResult.(completeError("fail") |> getReloadingError))
    |> toEqual(None)
  );

  test("getReloadingError when Reloading Error", () =>
    expect(AsyncResult.(reloadingError("fail") |> getReloadingError))
    |> toEqual(Some("fail"))
  );

  test("map Init", () =>
    expect(AsyncResult.init |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.init)
  );

  test("map Loading", () =>
    expect(AsyncResult.loading |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.loading)
  );

  test("map Reloading Ok", () =>
    expect(AsyncResult.reloadingOk(42) |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.reloadingOk(43))
  );

  test("map Reloading Error", () =>
    expect(AsyncResult.reloadingError(42) |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.reloadingError(42))
  );

  test("map Complete Ok", () =>
    expect(AsyncResult.completeOk(42) |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.completeOk(43))
  );

  test("map Complete Error", () =>
    expect(AsyncResult.completeError(42) |> AsyncResult.map(i => i + 1))
    |> toEqual(AsyncResult.completeError(42))
  );

  test("mapError Init", () =>
    expect(AsyncResult.init |> AsyncResult.mapError(i => i + 1))
    |> toEqual(AsyncResult.init)
  );

  test("mapError Loading", () =>
    expect(AsyncResult.loading |> AsyncResult.mapError(i => i + 1))
    |> toEqual(AsyncResult.loading)
  );

  test("mapError Reloading Ok", () =>
    expect(AsyncResult.reloadingOk(42) |> AsyncResult.mapError(i => i + 1))
    |> toEqual(AsyncResult.reloadingOk(42))
  );

  test("mapError Reloading Error", () =>
    expect(
      AsyncResult.reloadingError(42) |> AsyncResult.mapError(i => i + 1),
    )
    |> toEqual(AsyncResult.reloadingError(43))
  );

  test("mapError Complete Ok", () =>
    expect(AsyncResult.completeOk(42) |> AsyncResult.mapError(i => i + 1))
    |> toEqual(AsyncResult.completeOk(42))
  );

  test("mapError Complete Error", () =>
    expect(AsyncResult.completeError(42) |> AsyncResult.mapError(i => i + 1))
    |> toEqual(AsyncResult.completeError(43))
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
        Relude.String.eq,
        Relude.Int.eq,
        AsyncResult.ok(1),
        AsyncResult.ok(1),
      ),
    )
    |> toEqual(true)
  );

  test("tapByValue init", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncResult.init |> AsyncResult.tapByValue(f, _ => ()) |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tapByValue loading", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncResult.loading |> AsyncResult.tapByValue(f, _ => ()) |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tapByValue reloadingOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + (a |> Result.getOk |> Option.getOrElse(-1)) + 1;
    };
    AsyncResult.reloadingOk(10)
    |> AsyncResult.tapByValue(() => (), f)
    |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapByValue reloadingError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + (a |> Result.getError |> Option.getOrElse(-1)) + 1;
    };
    AsyncResult.reloadingError(10)
    |> AsyncResult.tapByValue(() => (), f)
    |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapByValue completeOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + (a |> Result.getOk |> Option.getOrElse(-1)) + 1;
    };
    AsyncResult.completeOk(10)
    |> AsyncResult.tapByValue(() => (), f)
    |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapByValue completeError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + (a |> Result.getError |> Option.getOrElse(-1)) + 1;
    };
    AsyncResult.completeError(10)
    |> AsyncResult.tapByValue(() => (), f)
    |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapOk init", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.init |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapOk loading", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.loading |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapOk reloadingOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.reloadingOk(10) |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapOk reloadingError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.reloadingError(10) |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapOk completeOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.completeOk(10) |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapOk completeError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.completeError(10) |> AsyncResult.tapOk(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapError init", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.init |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapError loading", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.loading |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapError reloadingOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.reloadingOk(10) |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapError reloadingError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.reloadingError(10) |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(11);
  });

  test("tapError completeOk", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.completeOk(10) |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(0);
  });

  test("tapError completeError", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncResult.completeError(10) |> AsyncResult.tapError(f) |> ignore;
    expect(count^) |> toEqual(11);
  });
});
