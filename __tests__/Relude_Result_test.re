open Jest;
open Expect;

module Result = Relude_Result;
module ResultS =
  Result.WithError({
    type t = string;
  });

describe("Result", () => {
  test("pure", () =>
    expect(Result.pure(1)) |> toEqual(Belt.Result.Ok(1))
  );

  test("map Ok", () =>
    expect(Result.map(a => a + 2, Belt.Result.Ok(1)))
    |> toEqual(Belt.Result.Ok(3))
  );

  test("map Error", () =>
    expect(Result.map(a => a + 2, Belt.Result.Error("error")))
    |> toEqual(Belt.Result.Error("error"))
  );

  test("mapError Ok", () =>
    expect(Result.ok(42) |> Result.mapError(x => x ++ x))
    |> toEqual(Result.ok(42))
  );

  test("mapError Error", () =>
    expect(Result.error("hi") |> Result.mapError(x => x ++ x))
    |> toEqual(Result.error("hihi"))
  );

  test("bimap Ok", () =>
    expect(Result.ok(42) |> Result.bimap(a => a + 10, e => e ++ e))
    |> toEqual(Result.ok(52))
  );

  test("bimap Error", () =>
    expect(Result.error("hi") |> Result.bimap(a => a + 10, e => e ++ e))
    |> toEqual(Result.error("hihi"))
  );

  test("tap Ok", () => {
    let x = ref(0);
    Result.ok(42) |> Result.tap(i => x := i) |> ignore;
    expect(x^) |> toEqual(42);
  });

  test("tap Error", () => {
    let x = ref(0);
    Result.error(42) |> Result.tap(i => x := i) |> ignore;
    expect(x^) |> toEqual(0);
  });

  test("tapError Ok", () => {
    let x = ref(0);
    Result.ok(42) |> Result.tapError(i => x := i) |> ignore;
    expect(x^) |> toEqual(0);
  });

  test("tapError Error", () => {
    let x = ref(0);
    Result.error(42) |> Result.tapError(i => x := i) |> ignore;
    expect(x^) |> toEqual(42);
  });

  test("apply Ok Ok", () =>
    expect(Result.apply(Result.ok(a => a + 10), Result.ok(42)))
    |> toEqual(Result.ok(52))
  );

  test("map2", () =>
    expect(Result.map2((a, b) => a + b, Result.ok(5), Result.ok(10)))
    |> toEqual(Result.ok(15))
  );

  test("map3", () =>
    expect(
      Result.map3(
        (a, b, c) => a + b + c,
        Result.ok(5),
        Result.ok(10),
        Result.ok(100),
      ),
    )
    |> toEqual(Result.ok(115))
  );

  test("map4", () =>
    expect(
      Result.map4(
        (a, b, c, d) => a + b + c + d,
        Result.ok(5),
        Result.ok(10),
        Result.ok(100),
        Result.ok(1000),
      ),
    )
    |> toEqual(Result.ok(1115))
  );

  test("map5", () =>
    expect(
      Result.map5(
        (a, b, c, d, e) => a + b + c + d + e,
        Result.ok(5),
        Result.ok(10),
        Result.ok(100),
        Result.ok(1000),
        Result.ok(10000),
      ),
    )
    |> toEqual(Result.ok(11115))
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

  test("getOrElse Ok", () =>
    expect(Result.ok(42) |> Result.getOrElse(5)) |> toEqual(42)
  );

  test("getOrElse Error", () =>
    expect(Result.error("abc") |> Result.getOrElse(5)) |> toEqual(5)
  );

  test("getOrElseLazy Ok", () =>
    expect(Result.ok(42) |> Result.getOrElseLazy(_ => 5)) |> toEqual(42)
  );

  test("getOrElseLazy Error", () =>
    expect(Result.error("abc") |> Result.getOrElseLazy(_ => 5))
    |> toEqual(5)
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

  test("catchError success", () =>
    expect(
      Result.pure(42)
      |> Result.catchError((e: string) => Result.error(e ++ e)),
    )
    |> toEqual(Belt.Result.Ok(42))
  );

  test("catchError failure", () =>
    expect(
      Result.error("42")
      |> Result.catchError(e => {
           let intValue =
             Relude.Int.fromString(e) |> Relude.Option.getOrElse(0);
           Result.error(intValue * 2);
         }),
    )
    |> toEqual(Belt.Result.Error(84))
  );

  test("handleError success", () =>
    expect(Result.pure(42) |> Result.handleError((_e: string) => 84))
    |> toEqual(Belt.Result.Ok(42))
  );

  test("handleError failure", () =>
    expect(
      Result.error("42") |> Result.handleError(e => int_of_string(e) * 2),
    )
    |> toEqual(Belt.Result.Ok(84))
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

  test("<<$>> Ok", () => {
    open ResultS.Infix;
    let f = a => a * 2;
    let g = err => err ++ err;
    let ok = Belt.Result.Ok(42);
    let actual = (f <<$>> g)(ok);
    expect(actual) |> toEqual(Belt.Result.Ok(84));
  });

  test("<<$>> Error", () => {
    open ResultS.Infix;
    let f = a => a * 2;
    let g = err => err ++ err;
    let error = Belt.Result.Error("hi");
    let actual = (f <<$>> g)(error);
    expect(actual) |> toEqual(Belt.Result.Error("hihi"));
  });
});