open Jest;
open Expect;

module AsyncData = Relude.AsyncData;
module Int = Relude.Int;
module String = Relude.String;

afterAll(Bisect.Runtime.write_coverage_data);

describe("AsyncData", () => {
  test("isEmpty init", () =>
    expect(AsyncData.init |> AsyncData.isEmpty) |> toEqual(true)
  );

  test("isEmpty loading", () =>
    expect(AsyncData.loading |> AsyncData.isEmpty) |> toEqual(true)
  );

  test("isEmpty reloading", () =>
    expect(AsyncData.reloading(42) |> AsyncData.isEmpty) |> toEqual(false)
  );

  test("isEmpty complete", () =>
    expect(AsyncData.complete(42) |> AsyncData.isEmpty) |> toEqual(false)
  );

  test("isNotEmpty init", () =>
    expect(AsyncData.init |> AsyncData.isNotEmpty) |> toEqual(false)
  );

  test("isNotEmpty loading", () =>
    expect(AsyncData.loading |> AsyncData.isNotEmpty) |> toEqual(false)
  );

  test("isNotEmpty reloading", () =>
    expect(AsyncData.reloading(42) |> AsyncData.isNotEmpty) |> toEqual(true)
  );

  test("isNotEmpty complete", () =>
    expect(AsyncData.complete(42) |> AsyncData.isNotEmpty) |> toEqual(true)
  );

  test("isInit when Init", () =>
    expect(AsyncData.isInit(Init)) |> toEqual(true)
  );

  test("isInit when Loading", () =>
    expect(AsyncData.isInit(Loading)) |> toEqual(false)
  );

  test("isInit when Reloading", () =>
    expect(AsyncData.isInit(Reloading(""))) |> toEqual(false)
  );

  test("isInit when Complete", () =>
    expect(AsyncData.isInit(Complete(""))) |> toEqual(false)
  );

  test("isLoading when Init", () =>
    expect(AsyncData.isLoading(Init)) |> toEqual(false)
  );

  test("isLoading when Loading", () =>
    expect(AsyncData.isLoading(Loading)) |> toEqual(true)
  );

  test("isLoading when Reloading", () =>
    expect(AsyncData.isLoading(Reloading(""))) |> toEqual(false)
  );

  test("isLoading when Complete", () =>
    expect(AsyncData.isLoading(Complete(""))) |> toEqual(false)
  );

  test("isReloading when Init", () =>
    expect(AsyncData.isReloading(Init)) |> toEqual(false)
  );

  test("isReloading when Loading", () =>
    expect(AsyncData.isReloading(Loading)) |> toEqual(false)
  );

  test("isReloading when Reloading", () =>
    expect(AsyncData.isReloading(Reloading(""))) |> toEqual(true)
  );

  test("isReloading when Complete", () =>
    expect(AsyncData.isReloading(Complete(""))) |> toEqual(false)
  );

  test("isComplete when Init", () =>
    expect(AsyncData.isComplete(Init)) |> toEqual(false)
  );

  test("isComplete when Loading", () =>
    expect(AsyncData.isComplete(Loading)) |> toEqual(false)
  );

  test("isComplete when Reloading", () =>
    expect(AsyncData.isComplete(Reloading(""))) |> toEqual(false)
  );

  test("isComplete when Complete", () =>
    expect(AsyncData.isComplete(Complete(""))) |> toEqual(true)
  );

  test("isBusy when Init", () =>
    expect(AsyncData.isBusy(Init)) |> toEqual(false)
  );

  test("isBusy when Loading", () =>
    expect(AsyncData.isBusy(Loading)) |> toEqual(true)
  );

  test("isBusy when Reloading", () =>
    expect(AsyncData.isBusy(Reloading(10))) |> toEqual(true)
  );

  test("isBusy when Complete", () =>
    expect(AsyncData.isBusy(Complete(10))) |> toEqual(false)
  );

  test("isIdle when Init", () =>
    expect(AsyncData.isIdle(Init)) |> toEqual(true)
  );

  test("isIdle when Loading", () =>
    expect(AsyncData.isIdle(Loading)) |> toEqual(false)
  );

  test("isIdle when Reloading", () =>
    expect(AsyncData.isIdle(Reloading(10))) |> toEqual(false)
  );

  test("isIdle when Complete", () =>
    expect(AsyncData.isIdle(Complete(10))) |> toEqual(true)
  );

  test("toBusy when Init", () =>
    expect(AsyncData.toBusy(Init)) |> toEqual(AsyncData.Loading)
  );

  test("toBusy when Loading", () =>
    expect(AsyncData.toBusy(Loading)) |> toEqual(AsyncData.Loading)
  );

  test("toBusy when Reloading", () =>
    expect(AsyncData.toBusy(Reloading(1)))
    |> toEqual(AsyncData.Reloading(1))
  );

  test("toBusy when Complete", () =>
    expect(AsyncData.toBusy(Complete(1)))
    |> toEqual(AsyncData.Reloading(1))
  );

  test("toIdle when Init", () =>
    expect(AsyncData.toIdle(Init)) |> toEqual(AsyncData.Init)
  );

  test("toIdle when Loading", () =>
    expect(AsyncData.toIdle(Loading)) |> toEqual(AsyncData.Init)
  );

  test("toIdle when Reloading", () =>
    expect(AsyncData.toIdle(Reloading(1)))
    |> toEqual(AsyncData.Complete(1))
  );

  test("toIdle when Complete", () =>
    expect(AsyncData.toIdle(Complete(1)))
    |> toEqual(AsyncData.Complete(1))
  );

  test("getReloading when Init", () =>
    expect(AsyncData.getReloading(Init)) |> toEqual(None)
  );

  test("getReloading when Loading", () =>
    expect(AsyncData.getReloading(Loading)) |> toEqual(None)
  );

  test("getReloading when Reloading", () =>
    expect(AsyncData.getReloading(Reloading(1))) |> toEqual(Some(1))
  );

  test("getReloading when Complete", () =>
    expect(AsyncData.getReloading(Complete(1))) |> toEqual(None)
  );

  test("getComplete when Init", () =>
    expect(AsyncData.getComplete(Init)) |> toEqual(None)
  );

  test("getComplete when Loading", () =>
    expect(AsyncData.getComplete(Loading)) |> toEqual(None)
  );

  test("getComplete when Reloading", () =>
    expect(AsyncData.getComplete(Reloading(1))) |> toEqual(None)
  );

  test("getComplete when Complete", () =>
    expect(AsyncData.getComplete(Complete(1))) |> toEqual(Some(1))
  );

  test("getValue when Init", () =>
    expect(AsyncData.getValue(Init)) |> toEqual(None)
  );

  test("getValue when Loading", () =>
    expect(AsyncData.getValue(Loading)) |> toEqual(None)
  );

  test("getValue when Complete", () =>
    expect(AsyncData.getValue(Complete(1))) |> toEqual(Some(1))
  );

  test("getValue when Reloading", () =>
    expect(AsyncData.getValue(Reloading(1))) |> toEqual(Some(1))
  );

  test("map Init", () =>
    expect(AsyncData.init |> AsyncData.map(i => i + 1))
    |> toEqual(AsyncData.init)
  );

  test("map Loading", () =>
    expect(AsyncData.loading |> AsyncData.map(i => i + 1))
    |> toEqual(AsyncData.loading)
  );

  test("map Reloading", () =>
    expect(AsyncData.reloading(42) |> AsyncData.map(i => i + 1))
    |> toEqual(AsyncData.reloading(43))
  );

  test("map Complete", () =>
    expect(AsyncData.complete(42) |> AsyncData.map(i => i + 1))
    |> toEqual(AsyncData.complete(43))
  );

  test("tap Init", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncData.init |> AsyncData.tap(f, () => (), _ => (), _ => ()) |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tap Loading", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncData.loading
    |> AsyncData.tap(() => (), f, _ => (), _ => ())
    |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tap Reloading", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncData.reloading(42)
    |> AsyncData.tap(() => (), () => (), f, _ => ())
    |> ignore;
    expect(count^) |> toEqual(43);
  });

  test("tap Complete", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncData.complete(42)
    |> AsyncData.tap(() => (), () => (), _ => (), f)
    |> ignore;
    expect(count^) |> toEqual(43);
  });

  test("tapInit", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncData.init |> AsyncData.tapInit(f) |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tapLoading", () => {
    let count = ref(0);
    let f = () => {
      count := count^ + 1;
    };
    AsyncData.loading |> AsyncData.tapLoading(f) |> ignore;
    expect(count^) |> toEqual(1);
  });

  test("tapReloading", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncData.reloading(42) |> AsyncData.tapReloading(f) |> ignore;
    expect(count^) |> toEqual(43);
  });

  test("tapComplete", () => {
    let count = ref(0);
    let f = a => {
      count := count^ + a + 1;
    };
    AsyncData.complete(42) |> AsyncData.tapComplete(f) |> ignore;
    expect(count^) |> toEqual(43);
  });

  test("fold when Init", () =>
    expect(AsyncData.fold(1, 2, a => a + 3, a => a + 4, Init)) |> toEqual(1)
  );

  test("fold when Loading", () =>
    expect(AsyncData.fold(1, 2, a => a + 3, a => a + 4, Loading))
    |> toEqual(2)
  );

  test("fold when Reloading", () =>
    expect(AsyncData.fold(1, 2, a => a + 3, a => a + 4, Reloading(10)))
    |> toEqual(13)
  );

  test("fold when Complete", () =>
    expect(AsyncData.fold(1, 2, a => a + 3, a => a + 4, Complete(10)))
    |> toEqual(14)
  );

  test("foldLazy when Init", () =>
    expect(
      AsyncData.foldLazy(() => 1, () => 2, a => a + 3, a => a + 4, Init),
    )
    |> toEqual(1)
  );

  test("foldLazy when Loading", () =>
    expect(
      AsyncData.foldLazy(() => 1, () => 2, a => a + 3, a => a + 4, Loading),
    )
    |> toEqual(2)
  );

  test("foldLazy when Reloading", () =>
    expect(
      AsyncData.foldLazy(
        () => 1,
        () => 2,
        a => a + 3,
        a => a + 4,
        Reloading(10),
      ),
    )
    |> toEqual(13)
  );

  test("foldLazy when Complete", () =>
    expect(
      AsyncData.foldLazy(
        () => 1,
        () => 2,
        a => a + 3,
        a => a + 4,
        Complete(10),
      ),
    )
    |> toEqual(14)
  );

  test("foldByValue when Init", () =>
    expect(AsyncData.foldByValue(1, a => a + 2, Init)) |> toEqual(1)
  );

  test("foldByValue when Loading", () =>
    expect(AsyncData.foldByValue(1, a => a + 2, Loading)) |> toEqual(1)
  );

  test("foldByValue when Reloading", () =>
    expect(AsyncData.foldByValue(1, a => a + 2, Reloading(10)))
    |> toEqual(12)
  );

  test("foldByValue when Complete", () =>
    expect(AsyncData.foldByValue(1, a => a + 2, Complete(10)))
    |> toEqual(12)
  );

  test("foldByValueLazy when Init", () =>
    expect(AsyncData.foldByValueLazy(() => 1, a => a + 2, Init))
    |> toEqual(1)
  );

  test("foldByValueLazy when Loading", () =>
    expect(AsyncData.foldByValueLazy(() => 1, a => a + 2, Loading))
    |> toEqual(1)
  );

  test("foldByValueLazy when Reloading", () =>
    expect(AsyncData.foldByValueLazy(() => 1, a => a + 2, Reloading(10)))
    |> toEqual(12)
  );

  test("foldByValueLazy when Complete", () =>
    expect(AsyncData.foldByValueLazy(() => 1, a => a + 2, Complete(10)))
    |> toEqual(12)
  );

  test("eqBy false for Init vs Loading", () =>
    expect(AsyncData.eqBy((_, _) => true, Init, Loading)) |> toEqual(false)
  );

  test("eqBy false for Reloading and Complete", () =>
    expect(AsyncData.eqBy((_, _) => true, Reloading(1), Complete(1)))
    |> toEqual(false)
  );

  test("eqBy true for both Complete", () =>
    expect(AsyncData.eqBy(String.eq, Complete("a"), Complete("a")))
    |> toEqual(true)
  );

  test("eqBy false for both Complete, different values", () =>
    expect(AsyncData.eqBy(Int.eq, Complete(0), Complete(1)))
    |> toEqual(false)
  );

  test("pure", () =>
    expect(AsyncData.pure(42)) |> toEqual(AsyncData.complete(42))
  );

  test("map2 Init Init", () =>
    expect(AsyncData.map2((_, _) => (), Init, Init))
    |> toEqual(AsyncData.Init)
  );

  test("map2 Init Loading", () =>
    expect(AsyncData.map2((_, _) => (), Init, Loading))
    |> toEqual(AsyncData.Loading)
  );

  test("map2 Init Reloading", () =>
    expect(AsyncData.map2((_, _) => (), Init, Reloading(123)))
    |> toEqual(AsyncData.Init)
  );

  test("map2 Reloading Reloading", () =>
    expect(AsyncData.map2((+), Reloading(10), Reloading(20)))
    |> toEqual(AsyncData.Reloading(30))
  );

  test("map2 Reloading Complete", () =>
    expect(AsyncData.map2((+), Reloading(10), Complete(20)))
    |> toEqual(AsyncData.Reloading(30))
  );

  test("bind Init", () =>
    expect(AsyncData.init->AsyncData.bind(x => AsyncData.complete(x)))
    |> toEqual(AsyncData.init)
  );

  test("bind Loading", () =>
    expect(AsyncData.loading->AsyncData.bind(x => AsyncData.complete(x)))
    |> toEqual(AsyncData.loading)
  );

  test("bind Reloading", () =>
    expect(
      AsyncData.reloading(42)
      ->AsyncData.bind(x => AsyncData.complete(x * 2)),
    )
    |> toEqual(AsyncData.complete(84))
  );

  test("bind Complete", () =>
    expect(
      AsyncData.complete(42)
      ->AsyncData.bind(x => AsyncData.reloading(x * 2)),
    )
    |> toEqual(AsyncData.reloading(84))
  );

  test("alt Loading Init", () =>
    expect(AsyncData.Infix.(AsyncData.loading <|> AsyncData.init))
    |> toEqual(AsyncData.loading)
  );

  test("alt Init Loading", () =>
    expect(AsyncData.Infix.(AsyncData.init <|> AsyncData.loading))
    |> toEqual(AsyncData.loading)
  );

  test("alt Loading Reloading", () =>
    expect(AsyncData.Infix.(AsyncData.loading <|> AsyncData.reloading(42)))
    |> toEqual(AsyncData.reloading(42))
  );

  test("alt Reloading Loading", () =>
    expect(AsyncData.Infix.(AsyncData.reloading(42) <|> AsyncData.loading))
    |> toEqual(AsyncData.reloading(42))
  );

  test("alt Reloading Complete", () =>
    expect(
      AsyncData.Infix.(AsyncData.reloading(42) <|> AsyncData.complete(43)),
    )
    |> toEqual(AsyncData.complete(43))
  );

  test("alt Complete Reloading", () =>
    expect(
      AsyncData.Infix.(AsyncData.complete(43) <|> AsyncData.reloading(42)),
    )
    |> toEqual(AsyncData.complete(43))
  );
});