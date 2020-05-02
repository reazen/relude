[@ocaml.doc
  {|
[Relude.Js.Promise] contains utilities for interoperating with [Js.Promise].
Many of these functions will help you convert to and from [Relude.IO].
|}
]

/**
Lifts a [Js.Promise] into a [Relude.IO]

Note: prefer {!val:toIOLazy} over this function if possible. A [Js.Promise] is
eagerly executed, so using [toIO] with an already-constructed and running
[Js.Promise] will not suspend the side effects.
*/
let toIO: 'a. Js.Promise.t('a) => Relude_IO.t('a, Js.Promise.error) =
  promise =>
    Relude_IO.async(onDone =>
      promise
      |> Js.Promise.then_(v => Js.Promise.resolve(onDone(Ok(v))))
      |> Js.Promise.catch(e => Js.Promise.resolve(onDone(Error(e))))
      |> ignore
    );

/**
Lifts a lazily-executed [Js.Promise] into a [Relude.IO].
*/
let toIOLazy:
  'a.
  (unit => Js.Promise.t('a)) => Relude_IO.t('a, Js.Promise.error)
 =
  runPromise =>
    Relude_IO.async(onDone => {
      let promise = runPromise();
      promise
      |> Js.Promise.then_(v => Js.Promise.resolve(onDone(Ok(v))))
      |> Js.Promise.catch(e => Js.Promise.resolve(onDone(Error(e))))
      |> ignore;
    });

// TODO: not sure how best to handle exn/Js.Exn.t/Js.Promise.error below...
// open to suggestions/ideas

/**
Converts a [Relude.IO] into a [Js.Promise.t]. This function will cause the IO
effects to be run.

The promise that is returned will not reject, it will instead have a [result] as
its resolution.
*/
let fromIOWithResult:
  'a 'e.
  Relude_IO.t('a, 'e) => Js.Promise.t(result('a, 'e))
 =
  io =>
    Js.Promise.make((~resolve, ~reject as _) =>
      io |> Relude_IO.unsafeRunAsync(result => resolve(. result))
    );
/**
Converts a [Relude.IO] into a [Js.Promise.t]. This function will cause the IO
effects to be run.

The error channel is unsafely coerced into the promise error type, which is
probably fine, because the [Js.Promise] error type is opaque.
*/
let fromIO: 'a 'e. Relude_IO.t('a, 'e) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Ok(v) => resolve(. v)
           | Error(e) => reject(. Relude_Unsafe.coerce(e)) /* TODO: not sure if this is wise/good */
           }
         )
    );

/**
Converts a [Relude.IO] with an extensible OCaml [exn] as the error type into a
[Js.Promise.t]. This function will cause the IO effects to be run.
*/
let fromIOExn: 'a. Relude_IO.t('a, exn) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Ok(v) => resolve(. v)
           | Error(e) => reject(. e)
           }
         )
    );

/**
Converts a [Relude.IO] with a [Js.Exn.t] as the error type into a
[Js.Promise.t]. This function will cause the IO effects to be run.
*/
let fromIOJsExn: 'a. Relude_IO.t('a, Js.Exn.t) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Ok(v) => resolve(. v)
           | Error(e) => reject(. Relude_Js_Exn.unsafeToExn(e))
           }
         )
    );
