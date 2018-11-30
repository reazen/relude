let toIO: Js.Promise.t('a) => Relude_IO.t('a, Js.Promise.error) =
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

let fromIO: Relude_IO.t('a, 'e) => Js.Promise.t('a) =
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
