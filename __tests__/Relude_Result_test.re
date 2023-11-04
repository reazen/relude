open Jest;
open Expect;
open! Relude.Globals;

module ResultS =
  Result.WithError({
    type t = string;
  });

describe("Result", () => {
  test("pure", () =>
    expect(Result.pure(1)) |> toEqual(Ok(1))
  );

  test("map Ok", () =>
    expect(Result.map(a => a + 2, Ok(1))) |> toEqual(Ok(3))
  );

  test("map Error", () =>
    expect(Result.map(a => a + 2, Error("error")))
    |> toEqual(Error("error"))
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

  test("align Ok Ok", () =>
    expect(Result.align(Result.ok(42), Result.ok("a")))
    |> toEqual(Result.ok(Ior.both(42, "a")))
  );

  test("align Ok Error", () =>
    expect(Result.align(Result.ok(42), Result.error("a")))
    |> toEqual(Result.ok(Ior.this(42)))
  );

  test("align Error Ok", () =>
    expect(Result.align(Result.error(42), Result.ok("a")))
    |> toEqual(Result.ok(Ior.that("a")))
  );

  test("align Error Error", () =>
    expect(Result.align(Result.error(42), Result.error(99)))
    |> toEqual(Result.error(42))
  );

  test("alignWith Ok Ok", () => {
    let f =
      fun
      | Relude.Ior.This(a) => a
      | That(b) => int_of_string(b)
      | Both(a, b) => a + int_of_string(b);
    expect(Result.alignWith(f, Result.ok(42), Result.ok("99")))
    |> toEqual(Result.ok(141));
  });

  test("alignWith Ok Error", () => {
    let f =
      fun
      | Relude.Ior.This(a) => a
      | That(b) => int_of_string(b)
      | Both(a, b) => a + int_of_string(b);
    expect(Result.alignWith(f, Result.ok(42), Result.error("99")))
    |> toEqual(Result.ok(42));
  });

  test("alignWith Error Ok", () => {
    let f =
      fun
      | Relude.Ior.This(a) => a
      | That(b) => int_of_string(b)
      | Both(a, b) => a + int_of_string(b);
    expect(Result.alignWith(f, Result.error(42), Result.ok("99")))
    |> toEqual(Result.ok(99));
  });

  test("alignWith Error Ok", () => {
    let f =
      fun
      | Relude.Ior.This(a) => a
      | That(b) => int_of_string(b)
      | Both(a, b) => a + int_of_string(b);
    expect(Result.alignWith(f, Result.error("a"), Result.error("b")))
    |> toEqual(Result.error("a"));
  });

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
    expect(Result.flatMap(a => Ok(a + 2), Ok(1))) |> toEqual(Ok(3))
  );

  test("bind", () =>
    expect(Result.bind(Ok(1), a => Ok(a + 2))) |> toEqual(Ok(3))
  );

  test("fold Ok", () =>
    expect(Result.fold(_ => "error", _ => "ok", Ok(1))) |> toEqual("ok")
  );

  test("fold Error", () =>
    expect(Result.fold(_ => "error", _ => "ok", Error(1)))
    |> toEqual("error")
  );

  test("getOrElseBy Ok", () =>
    expect(Result.ok(42) |> Result.getOrElseBy(_ => 5)) |> toEqual(42)
  );

  test("getOrElseBy Error", () =>
    expect(Result.error(42) |> Result.getOrElseBy(x => x / 7)) |> toEqual(6)
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

  test("getErrorOrElse Ok", () =>
    expect(Result.ok(42) |> Result.getErrorOrElse(5)) |> toEqual(5)
  );

  test("getErrorOrElse Error", () =>
    expect(Result.error(42) |> Result.getErrorOrElse(5)) |> toEqual(42)
  );

  test("getErrorOrElseBy Ok", () =>
    expect(Result.ok(42) |> Result.getErrorOrElseBy(x => x / 7))
    |> toEqual(6)
  );

  test("getErrorOrElseBy Error", () =>
    expect(Result.error(42) |> Result.getErrorOrElseBy(x => x / 7))
    |> toEqual(42)
  );

  test("merge Error", () =>
    expect(Result.merge(Error(1))) |> toEqual(1)
  );

  test("merge Ok", () =>
    expect(Result.merge(Ok(1))) |> toEqual(1)
  );

  test("flip Ok", () =>
    expect(Result.flip(Ok(1))) |> toEqual(Error(1))
  );

  test("flip Error", () =>
    expect(Result.flip(Error("my error"))) |> toEqual(Ok("my error"))
  );

  test("isOk when Ok", () =>
    expect(Result.isOk(Ok(1))) |> toEqual(true)
  );

  test("isOk when Error", () =>
    expect(Result.isOk(Error(1))) |> toEqual(false)
  );

  test("isError when Error", () =>
    expect(Result.isError(Error(1))) |> toEqual(true)
  );

  test("isError when Ok", () =>
    expect(Result.isError(Ok(1))) |> toEqual(false)
  );

  test("getOk when Ok", () =>
    expect(Result.getOk(Ok(1))) |> toEqual(Some(1))
  );

  test("getOk when Error", () =>
    expect(Result.getOk(Error(1))) |> toEqual(None)
  );

  test("getError when Error", () =>
    expect(Result.getError(Error(1))) |> toEqual(Some(1))
  );

  test("getError when Ok", () =>
    expect(Result.getError(Ok(1))) |> toEqual(None)
  );

  test("catchError success", () =>
    expect(
      Result.pure(42)
      |> Result.catchError((e: string) => Result.error(e ++ e)),
    )
    |> toEqual(Ok(42))
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
    |> toEqual(Error(84))
  );

  test("handleError success", () =>
    expect(Result.pure(42) |> Result.handleError((_e: string) => 84))
    |> toEqual(Ok(42))
  );

  test("handleError failure", () =>
    expect(
      Result.error("42") |> Result.handleError(e => int_of_string(e) * 2),
    )
    |> toEqual(Ok(84))
  );

  test("mapHandleError success", () =>
    expect(
      Result.pure(42) |> Result.mapHandleError(a => a * 2, int_of_string),
    )
    |> toEqual(Ok(84))
  );

  test("mapHandleError failure", () =>
    expect(
      Result.error("42") |> Result.mapHandleError(a => a * 2, int_of_string),
    )
    |> toEqual(Ok(42))
  );

  test("eqBy when eq, both Ok", () =>
    expect(Result.eqBy(Relude.Int.eq, Relude.String.eq, Ok("a"), Ok("a")))
    |> toEqual(true)
  );

  test("eqBy when not eq, both Ok", () =>
    expect(Result.eqBy((_, _) => true, Relude.Int.eq, Ok(1), Ok(2)))
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
    expect(Result.eqBy(Relude.Int.eq, (_, _) => true, Error(1), Error(1)))
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
    let ok = Ok(42);
    let actual = (f <<$>> g)(ok);
    expect(actual) |> toEqual(Ok(84));
  });

  test("<<$>> Error", () => {
    open ResultS.Infix;
    let f = a => a * 2;
    let g = err => err ++ err;
    let error = Error("hi");
    let actual = (f <<$>> g)(error);
    expect(actual) |> toEqual(Error("hihi"));
  });

  test("bitraverse", () => {
    module ResultE =
      Result.WithError({
        type t = string;
      });
    module ResultA = ResultE.WithApplicative(Option.Applicative);
    let success: option(result(int, string)) =
      Ok(42)
      |> ResultA.bitraverse(i => Some(i + 3), err => Some(err ++ err));

    let failure: option(result(int, string)) =
      Error("fail")
      |> ResultA.bitraverse(i => Some(i + 3), err => Some(err ++ err));

    expect((success, failure))
    |> toEqual((Some(Ok(45)), Some(Error("failfail"))));
  });
});
