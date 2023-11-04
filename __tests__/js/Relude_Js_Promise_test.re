open Jest;
open Expect;

module IO = Relude.IO;
module RPromise = Relude.Js_Promise;

describe("Js.Promise", () => {
  testAsync("toIO success", onDone =>
    Js.Promise.resolve(42)
    |> Relude.Js_Promise.toIO
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("failed")),
       )
  );

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIO error", onDone =>
    Js.Promise.reject(Relude.Unsafe.coerce("my error toIO"))
    |> Relude.Js_Promise.toIO
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("failed"))
         | Error(e) => {
             let str: string = Relude.Unsafe.coerce(e);
             onDone(expect(str) |> toEqual("my error toIO"));
           },
       )
  );

  testAsync("toIOLazy success", onDone =>
    (() => Js.Promise.resolve(42))
    |> Relude.Js_Promise.toIOLazy
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("failed")),
       )
  );

  // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIOLazy error", onDone =>
    (() => Js.Promise.reject(Relude.Unsafe.coerce("my error toIOLazy")))
    |> Relude.Js_Promise.toIOLazy
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("failed"))
         | Error(e) => {
             let str: string = Relude.Unsafe.coerce(e);
             onDone(expect(str) |> toEqual("my error toIOLazy"));
           },
       )
  );

  testPromise("fromIOWithResult success", () =>
    Relude.IO.pure(42)
    |> Relude.Js_Promise.fromIOWithResult
    |> Js.Promise.then_(actual =>
         actual |> expect |> toEqual(Ok(42)) |> Js.Promise.resolve
       )
  );

  testPromise("fromIOWithResult error", () =>
    Relude.IO.throw(42)
    |> Relude.Js_Promise.fromIOWithResult
    |> Js.Promise.then_(actual =>
         actual |> expect |> toEqual(Error(42)) |> Js.Promise.resolve
       )
  );

  testPromise("fromIO success", () =>
    Relude.IO.pure(42)
    |> Relude.Js_Promise.fromIO
    |> Js.Promise.then_(actual =>
         actual |> expect |> toEqual(42) |> Js.Promise.resolve
       )
  );

  testPromise("fromIO error", () =>
    Relude.IO.throw(42)
    |> Relude.Js_Promise.fromIO
    |> Js.Promise.then_(_ => fail("fail") |> Js.Promise.resolve)
    |> Js.Promise.catch(error =>
         error
         |> Relude.Unsafe.coerce
         |> expect
         |> toEqual(42)
         |> Js.Promise.resolve
       )
  );

  testPromise("fromIOExn success", () =>
    Relude.IO.pure(42)
    |> Relude.Js_Promise.fromIOExn
    |> Js.Promise.then_(actual =>
         actual |> expect |> toEqual(42) |> Js.Promise.resolve
       )
  );

  testPromise("fromIOExn error", () =>
    Relude.IO.suspendThrow(() =>
      Relude.Js_Exn.make("exn") |> Relude.Js_Exn.unsafeToExn
    )
    |> Relude.Js_Promise.fromIOExn
    |> Js.Promise.then_(_ => fail("fail") |> Js.Promise.resolve)
    |> Js.Promise.catch(error =>
         error
         |> Relude.Unsafe.coerce
         |> expect
         |> toEqual(Relude.Js_Exn.make("exn") |> Relude.Js_Exn.unsafeToExn)
         |> Js.Promise.resolve
       )
  );

  testPromise("fromIOJsExn success", () =>
    Relude.IO.pure(42)
    |> Relude.Js_Promise.fromIOJsExn
    |> Js.Promise.then_(actual =>
         actual |> expect |> toEqual(42) |> Js.Promise.resolve
       )
  );

  testPromise("fromIOJsExn error", () =>
    Relude.IO.suspendThrow(() => Relude.Js_Exn.make("js_exn"))
    |> Relude.Js_Promise.fromIOJsExn
    |> Js.Promise.then_(_ => fail("fail") |> Js.Promise.resolve)
    |> Js.Promise.catch(error =>
         error
         |> Relude.Unsafe.coerce
         |> expect
         |> toEqual(Relude.Js_Exn.make("js_exn"))
         |> Js.Promise.resolve
       )
  );
});
