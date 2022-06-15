open Jest;
open Expect;

module AsyncData = Relude.AsyncData;
module AsyncResult = Relude.AsyncResult;
module Option = Relude.Option;
module Result = Relude.Result;

module AsyncResultS =
  AsyncResult.WithError({
    type t = string;
  });

describe("AsyncResult", () => {
  test("Init", () =>
    expect(AsyncResult.init) |> toEqual(AsyncData.init)
  );

  test("Loading", () =>
    expect(AsyncResult.loading) |> toEqual(AsyncData.loading)
  );

  test("Complete Ok", () =>
    expect(AsyncResult.ok(1)) |> toEqual(AsyncData.complete(Ok(1)))
  );

  test("Complete Error", () =>
    expect(AsyncResult.error("Fail"))
    |> toEqual(AsyncData.complete(Error("Fail")))
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

  test("apply Init & Init = Init", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Init & Loading = Loading", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.loading)
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("apply Init & Reloading Ok = Init", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.reloadingOk("ok"))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Init & Reloading Error = Reloading Error", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.reloadingError("error"))
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Init & Complete Ok = Init", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.completeOk("ok"))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Init & Complete Error = Complete Error", () =>
    AsyncResult.apply(AsyncResult.init, AsyncResult.completeError("error"))
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Loading & Init = Loading", () =>
    AsyncResult.apply(AsyncResult.loading, AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("apply Loading & Loading = Loading", () =>
    AsyncResult.apply(AsyncResult.loading, AsyncResult.loading)
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("apply Loading & Reloading Ok = Init", () =>
    AsyncResult.apply(AsyncResult.loading, AsyncResult.reloadingOk("ok"))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Loading & Reloading Error = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.loading,
      AsyncResult.reloadingError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Loading & Complete Ok = Init", () =>
    AsyncResult.apply(AsyncResult.loading, AsyncResult.completeOk("ok"))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Loading & Complete Error = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.loading,
      AsyncResult.completeError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Reloading Ok & Init = Init", () =>
    AsyncResult.apply(AsyncResult.reloadingOk(() => "ok"), AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Reloading Error & Init = Init", () =>
    AsyncResult.apply(AsyncResult.reloadingError("error"), AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Ok & Loading = Loading", () =>
    AsyncResult.apply(
      AsyncResult.reloadingOk(() => "ok"),
      AsyncResult.loading,
    )
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("apply Reloading Error & Loading = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingError("error"),
      AsyncResult.loading,
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Ok & Reloading Ok = Reloading Ok", () =>
    AsyncResult.apply(
      AsyncResult.reloadingOk(r => r ++ "ok"),
      AsyncResult.reloadingOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingOk("okok"))
  );

  test("apply Reloading Ok & Reloading Error = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingOk(() => "ok"),
      AsyncResult.reloadingError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Error & Reloading Ok = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingError("error"),
      AsyncResult.reloadingOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Error & Reloading Error = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingError("error"),
      AsyncResult.reloadingError("error_2"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Ok & Complete Ok = Reloading Ok", () =>
    AsyncResult.apply(
      AsyncResult.reloadingOk(r => r ++ "ok"),
      AsyncResult.completeOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingOk("okok"))
  );

  test("apply Reloading Ok & Complete Error = Complete Erro", () =>
    AsyncResult.apply(
      AsyncResult.reloadingOk(r => r ++ "ok"),
      AsyncResult.completeError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Reloading Error & Complete Ok = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingError("error"),
      AsyncResult.completeOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Reloading Error & Complete Error = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.reloadingError("error"),
      AsyncResult.completeError("error_2"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Complete Ok & Init = Init", () =>
    AsyncResult.apply(AsyncResult.completeOk(() => "ok"), AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("apply Complete Error & Init = Complete Error", () =>
    AsyncResult.apply(AsyncResult.completeError("error"), AsyncResult.init)
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Ok & Loading = Loading", () =>
    AsyncResult.apply(AsyncResult.completeOk(() => "ok"), AsyncResult.loading)
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("apply Complete Error & Loading = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeError("error"),
      AsyncResult.loading,
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Ok & Reloading Ok = Reloading Ok", () =>
    AsyncResult.apply(
      AsyncResult.completeOk(r => r ++ "ok"),
      AsyncResult.reloadingOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingOk("okok"))
  );

  test("apply Complete Ok & Reloading Error = Reloading Error", () =>
    AsyncResult.apply(
      AsyncResult.completeOk(r => r ++ "ok"),
      AsyncResult.reloadingError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError("error"))
  );

  test("apply Complete Error & Reloading Ok = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeError("error"),
      AsyncResult.reloadingOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Error & Reloading Error = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeError("error"),
      AsyncResult.reloadingError("error_2"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Ok & Complete Ok = Complete Ok", () =>
    AsyncResult.apply(
      AsyncResult.completeOk(r => r ++ "ok"),
      AsyncResult.completeOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.completeOk("okok"))
  );

  test("apply Complete Ok & Complete Error = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeOk(r => r ++ "ok"),
      AsyncResult.completeError("error"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Error & Complete Ok = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeError("error"),
      AsyncResult.completeOk("ok"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("apply Complete Error & Complete Error = Complete Error", () =>
    AsyncResult.apply(
      AsyncResult.completeError("error"),
      AsyncResult.completeError("error_2"),
    )
    |> expect
    |> toEqual(AsyncResult.completeError("error"))
  );

  test("bind Init", () =>
    AsyncResult.bind(AsyncResult.init, a => AsyncResult.reloadingOk(a))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("bind Loading", () =>
    AsyncResult.bind(AsyncResult.loading, a => AsyncResult.reloadingOk(a))
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("bind Reloading Ok", () =>
    AsyncResult.bind(AsyncResult.reloadingOk(1), a =>
      AsyncResult.reloadingError(a + 1)
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError(2))
  );

  test("bind Reloading Error", () =>
    AsyncResult.bind(AsyncResult.reloadingError(1), a =>
      AsyncResult.reloadingOk(a + 1)
    )
    |> expect
    |> toEqual(AsyncResult.reloadingError(1))
  );

  test("bind Complete Ok", () =>
    AsyncResult.bind(AsyncResult.completeOk(1), a =>
      AsyncResult.completeError(a + 1)
    )
    |> expect
    |> toEqual(AsyncResult.completeError(2))
  );

  test("bind Complete Error", () =>
    AsyncResult.bind(AsyncResult.completeError(1), a =>
      AsyncResult.completeOk(a + 1)
    )
    |> expect
    |> toEqual(AsyncResult.completeError(1))
  );

  test("flatMap Init", () =>
    AsyncResult.init
    |> AsyncResult.flatMap(a => AsyncResult.reloadingOk(a))
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("flatMap Loading", () =>
    AsyncResult.loading
    |> AsyncResult.flatMap(a => AsyncResult.reloadingOk(a))
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("flatMap Reloading Ok", () =>
    AsyncResult.reloadingOk(1)
    |> AsyncResult.flatMap(a => AsyncResult.reloadingError(a + 1))
    |> expect
    |> toEqual(AsyncResult.reloadingError(2))
  );

  test("flatMap Reloading Error", () =>
    AsyncResult.reloadingError(1)
    |> AsyncResult.flatMap(a => AsyncResult.reloadingOk(a + 1))
    |> expect
    |> toEqual(AsyncResult.reloadingError(1))
  );

  test("flatMap Complete Ok", () =>
    AsyncResult.completeOk(1)
    |> AsyncResult.flatMap(a => AsyncResult.completeError(a + 1))
    |> expect
    |> toEqual(AsyncResult.completeError(2))
  );

  test("flatMap Complete Error", () =>
    AsyncResult.completeError(1)
    |> AsyncResult.flatMap(a => AsyncResult.completeOk(a + 1))
    |> expect
    |> toEqual(AsyncResult.completeError(1))
  );

  test("flatten Init", () =>
    AsyncResult.init
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("flatten Loading", () =>
    AsyncResult.loading
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("flatten Reloading Ok", () =>
    AsyncResult.reloadingOk(AsyncResult.reloadingOk(2))
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.reloadingOk(2))
  );

  test("flatten Reloading Error", () =>
    AsyncResult.reloadingError(AsyncResult.reloadingOk(2))
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.reloadingError(AsyncResult.reloadingOk(2)))
  );

  test("flatten Complete Ok", () =>
    AsyncResult.completeOk(AsyncResult.completeOk(2))
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.completeOk(2))
  );

  test("flatten Complete Error", () =>
    AsyncResult.completeError(AsyncResult.completeOk(2))
    |> AsyncResult.flatten
    |> expect
    |> toEqual(AsyncResult.completeError(AsyncResult.completeOk(2)))
  );

  test("fold Init", () =>
    AsyncResult.init
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(1)
  );

  test("fold Loading", () =>
    AsyncResult.loading
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(2)
  );

  test("fold Reloading Ok", () =>
    AsyncResult.reloadingOk(10)
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(13)
  );

  test("fold Reloading Error", () =>
    AsyncResult.reloadingError(10)
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(3)
  );

  test("fold Complete Ok", () =>
    AsyncResult.completeOk(10)
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(14)
  );

  test("fold Complete Error", () =>
    AsyncResult.completeError(10)
    |> AsyncResult.fold(
         1,
         2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(4)
  );

  test("foldLazy Init", () =>
    AsyncResult.init
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(1)
  );

  test("foldLazy Loading", () =>
    AsyncResult.loading
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(2)
  );

  test("foldLazy Reloading Ok", () =>
    AsyncResult.reloadingOk(10)
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(13)
  );

  test("foldLazy Reloading Error", () =>
    AsyncResult.reloadingError(10)
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(3)
  );

  test("foldLazy Complete Ok", () =>
    AsyncResult.completeOk(10)
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(14)
  );

  test("foldLazy Complete Error", () =>
    AsyncResult.completeError(10)
    |> AsyncResult.foldLazy(
         () => 1,
         () => 2,
         r => (r |> Result.getOrElse(0)) + 3,
         r => (r |> Result.getOrElse(0)) + 4,
       )
    |> expect
    |> toEqual(4)
  );

  test("foldByValue Init", () =>
    AsyncResult.init
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(1)
  );

  test("foldByValue Loading", () =>
    AsyncResult.loading
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(1)
  );

  test("foldByValue Reloading Ok", () =>
    AsyncResult.reloadingOk(10)
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(12)
  );

  test("foldByValue Reloading Error", () =>
    AsyncResult.reloadingError(10)
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(13)
  );

  test("foldByValue Complete Ok", () =>
    AsyncResult.completeOk(10)
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(12)
  );

  test("foldByValue Complete Error", () =>
    AsyncResult.completeError(10)
    |> AsyncResult.foldByValue(1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(13)
  );

  test("foldByValueLazy Init", () =>
    AsyncResult.init
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(1)
  );

  test("foldByValueLazy Loading", () =>
    AsyncResult.loading
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(1)
  );

  test("foldByValueLazy Reloading Ok", () =>
    AsyncResult.reloadingOk(10)
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(12)
  );

  test("foldByValueLazy Reloading Error", () =>
    AsyncResult.reloadingError(10)
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(13)
  );

  test("foldByValueLazy Complete Ok", () =>
    AsyncResult.completeOk(10)
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(12)
  );

  test("foldByValueLazy Complete Error", () =>
    AsyncResult.completeError(10)
    |> AsyncResult.foldByValueLazy(() => 1, ok => ok + 2, error => error + 3)
    |> expect
    |> toEqual(13)
  );

  test("alt (Init, Init)", () =>
    AsyncResult.(alt(Init, Init) |> expect |> toEqual(init))
  );

  test("alt (Init, Loading)", () =>
    AsyncResult.(alt(Init, Loading) |> expect |> toEqual(loading))
  );

  test("alt (Init, Reloading(Error))", () =>
    AsyncResult.(alt(Init, Reloading(Error(0))) |> expect |> toEqual(init))
  );

  test("alt (Init, Reloading(Ok))", () =>
    AsyncResult.(
      alt(Init, Reloading(Ok(0))) |> expect |> toEqual(reloadingOk(0))
    )
  );

  test("alt (Init, Complete(Error))", () =>
    AsyncResult.(alt(Init, Complete(Error(0))) |> expect |> toEqual(init))
  );

  test("alt (Init, Complete(Ok))", () =>
    AsyncResult.(
      alt(Init, Complete(Ok(0))) |> expect |> toEqual(completeOk(0))
    )
  );

  test("alt (Loading, Init)", () =>
    AsyncResult.(alt(Loading, Init) |> expect |> toEqual(loading))
  );

  test("alt (Reloading(Error), Init)", () =>
    AsyncResult.(alt(Reloading(Error(1)), Init) |> expect |> toEqual(init))
  );

  test("alt (Reloading(Error), Loading)", () =>
    AsyncResult.(
      alt(Reloading(Error(1)), Loading) |> expect |> toEqual(loading)
    )
  );

  test("alt (Reloading(Error), Reloading(Error))", () =>
    AsyncResult.(
      alt(Reloading(Error(1)), Reloading(Error(2)))
      |> expect
      |> toEqual(reloadingError(1))
    )
  );

  test("alt (Reloading(Error), Complete(Error))", () =>
    AsyncResult.(
      alt(Reloading(Error(1)), Complete(Error(2)))
      |> expect
      |> toEqual(completeError(2))
    )
  );

  test("alt (Complete(Error), Init)", () =>
    AsyncResult.(alt(Complete(Error(1)), Init) |> expect |> toEqual(init))
  );

  test("alt (Complete(Error), Loading)", () =>
    AsyncResult.(
      alt(Complete(Error(1)), Loading) |> expect |> toEqual(loading)
    )
  );

  test("alt (Complete(Error), Reloading(Error))", () =>
    AsyncResult.(
      alt(Complete(Error(1)), Reloading(Error(2)))
      |> expect
      |> toEqual(completeError(1))
    )
  );

  test("alt (Complete(Error), Reloading(Ok))", () =>
    AsyncResult.(
      alt(Complete(Error(1)), Reloading(Ok(2)))
      |> expect
      |> toEqual(reloadingOk(2))
    )
  );

  test("alt (Complete(Error), Complete(Error))", () =>
    AsyncResult.(
      alt(Complete(Error(1)), Complete(Error(2)))
      |> expect
      |> toEqual(completeError(1))
    )
  );

  test("fromAsyncData Init", () =>
    AsyncData.init
    |> AsyncResult.fromAsyncData
    |> expect
    |> toEqual(AsyncResult.init)
  );

  test("fromAsyncData Loading", () =>
    AsyncData.loading
    |> AsyncResult.fromAsyncData
    |> expect
    |> toEqual(AsyncResult.loading)
  );

  test("fromAsyncData Reloading", () =>
    AsyncData.reloading(10)
    |> AsyncResult.fromAsyncData
    |> expect
    |> toEqual(AsyncResult.reloadingOk(10))
  );

  test("fromAsyncData Complete", () =>
    AsyncData.complete(10)
    |> AsyncResult.fromAsyncData
    |> expect
    |> toEqual(AsyncResult.completeOk(10))
  );

  test("toAsyncData Init", () =>
    AsyncResult.init
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.init)
  );

  test("toAsyncData Loading", () =>
    AsyncResult.loading
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.loading)
  );

  test("toAsyncData Reloading Ok", () =>
    AsyncResult.reloadingOk(10)
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.reloading(10))
  );

  test("toAsyncData Reloading Error", () =>
    AsyncResult.reloadingError(10)
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.reloading(10))
  );

  test("toAsyncData Complete Ok", () =>
    AsyncResult.completeOk(10)
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.complete(10))
  );

  test("toAsyncData Complete Error", () =>
    AsyncResult.completeError(10)
    |> AsyncResult.toAsyncData
    |> expect
    |> toEqual(AsyncData.complete(10))
  );

  test("WithError Init", () => {
    open AsyncResultS.Infix;
    let f = a => a + 1;
    let ok = AsyncResult.init;
    let actual = f <$> ok;
    expect(actual) |> toEqual(AsyncResult.init);
  });

  test("WithError Loading", () => {
    open AsyncResultS.Infix;
    let f = a => a + 1;
    let ok = AsyncResult.loading;
    let actual = f <$> ok;
    expect(actual) |> toEqual(AsyncResult.loading);
  });

  test("WithError Reloading Ok", () => {
    open AsyncResultS.Infix;
    let f = a => a + 1;
    let ok = AsyncResult.reloadingOk(10);
    let actual = f <$> ok;
    expect(actual) |> toEqual(AsyncResult.reloadingOk(11));
  });

  test("WithError Reloading Error", () => {
    open AsyncResultS.Infix;
    let f = a => a + 1;
    let ok = AsyncResult.reloadingError("error");
    let actual = f <$> ok;
    expect(actual) |> toEqual(AsyncResult.reloadingError("error"));
  });
});
