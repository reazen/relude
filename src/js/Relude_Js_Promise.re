/**
# Relude.Js.Promise

Utilities for interoperating with `Js.Promise`
*/

/**
Lifts a `Js.Promise` into a `Relude.IO`

Note: prefer `toIOLazy` over this function if possible.  `Js.Promise`s are eagerly executed,
so using `toIO` with an already-constructed and running `Js.Promise` will not suspend the side effects.
 */
let toIO: 'a. Js.Promise.t('a) => Relude_IO.t('a, Js.Promise.error) =
  promise =>
    Relude_IO.async(onDone =>
      promise
      |> Js.Promise.then_(v =>
           Js.Promise.resolve(onDone(Belt.Result.Ok(v)))
         )
      |> Js.Promise.catch(e =>
           Js.Promise.resolve(onDone(Belt.Result.Error(e)))
         )
      |> ignore
    );

/**
Lifts a lazily-executed Js.Promise into a Relude.IO
 */
let toIOLazy:
  'a.
  (unit => Js.Promise.t('a)) => Relude_IO.t('a, Js.Promise.error)
 =
  runPromise =>
    Relude_IO.async(onDone => {
      let promise = runPromise();
      promise
      |> Js.Promise.then_(v =>
           Js.Promise.resolve(onDone(Belt.Result.Ok(v)))
         )
      |> Js.Promise.catch(e =>
           Js.Promise.resolve(onDone(Belt.Result.Error(e)))
         )
      |> ignore;
    });

// TODO: not sure how best to handle exn/Js.Exn.t/Js.Promise.error below... open to suggestions/ideas

/**
Converts a `Relude.IO` into a Js.Promise.t.

This function will cause the IO effects to be run.

The error channel is unsafely coerced into the promise error type, which is
probably fine, because the Js.Promise error type is opaque.
 */
let fromIO: 'a 'e. Relude_IO.t('a, 'e) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Belt.Result.Ok(v) => resolve(. v)
           | Belt.Result.Error(e) => reject(. Relude_Unsafe.coerce(e)) /* TODO: not sure if this is wise/good */
           }
         )
    );

/**
Converts a `Relude.IO` with an extensible OCaml exn as the error type into a Js.Promise.t.

This function will cause the IO effects to be run.
 */
let fromIOExn: 'a. Relude_IO.t('a, exn) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Belt.Result.Ok(v) => resolve(. v)
           | Belt.Result.Error(e) => reject(. e)
           }
         )
    );

/**
Converts a `Relude.IO` with a `Js.Exn.t` as the error type into a Js.Promise.t.

This function will cause the IO effects to be run.
 */
let fromIOJsExn: 'a. Relude_IO.t('a, Js.Exn.t) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Belt.Result.Ok(v) => resolve(. v)
           | Belt.Result.Error(e) => reject(. Relude_Js_Exn.unsafeToExn(e))
           }
         )
    );