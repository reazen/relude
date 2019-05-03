let toIO: 'a. Js.Promise.t('a) => Relude_IO.t('a, Js.Promise.error) =
  promise =>
    Relude_IO.async(onDone =>
      promise
      |> Js.Promise.then_(v => {
           onDone(Belt.Result.Ok(v));
           promise;
         })
      |> Js.Promise.catch(e => {
           onDone(Belt.Result.Error(e));
           promise;
         })
      |> ignore
    );

/* TODO: not sure how best to handle exn/Js.Exn.t/Js.Promise.error below... open to suggestions/ideas */

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

let fromIOJsExn: 'a. Relude_IO.t('a, Js.Exn.t) => Js.Promise.t('a) =
  io =>
    Js.Promise.make((~resolve, ~reject) =>
      io
      |> Relude_IO.unsafeRunAsync(result =>
           switch (result) {
           | Belt.Result.Ok(v) => resolve(. v)
           | Belt.Result.Error(e) => reject(. Relude_JsExn.unsafeToExn(e))
           }
         )
    );
