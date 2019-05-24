open Jest;
open Expect;

module IO = Relude_IO;
module RPromise = Relude_Js_Promise;

describe("Js.Promise", () => {
  testAsync("toIO success", onDone =>
    Js.Promise.resolve(42)
    |> Relude_Js_Promise.toIO
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("failed")),
       )
  );

 // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIO error", onDone =>
    Js.Promise.reject(Relude_Unsafe.coerce("my error toIO"))
    |> Relude_Js_Promise.toIO
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("failed"))
         | Error(e) => {
           let str: string = Relude_Unsafe.coerce(e);
           onDone(expect(str) |> toEqual("my error toIO"));
         }
       )
  );

  testAsync("toIOLazy success", onDone =>
    (() => Js.Promise.resolve(42))
    |> Relude_Js_Promise.toIOLazy
    |> IO.unsafeRunAsync(
         fun
         | Ok(value) => onDone(expect(value) |> toEqual(42))
         | Error(_) => onDone(fail("failed")),
       )
  );

 // Unsafe.coerces here b/c I can't figure how to make this compile with the actual types
  testAsync("toIOLazy error", onDone =>
    (() => Js.Promise.reject(Relude_Unsafe.coerce("my error toIOLazy")))
    |> Relude_Js_Promise.toIOLazy
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(fail("failed"))
         | Error(e) => {
           let str: string = Relude_Unsafe.coerce(e);
           onDone(expect(str) |> toEqual("my error toIOLazy"));
         }
       )
  );

});