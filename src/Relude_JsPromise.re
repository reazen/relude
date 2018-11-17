let toAff: Js.Promise.t('a) => Relude_Aff.t('a, Js.Promise.error) =
  (promise, onDone, ()) =>
    promise
    |> Js.Promise.then_(v => {
         onDone(Belt.Result.Ok(v), ());
         promise;
       })
    |> Js.Promise.catch(e => {
         onDone(Belt.Result.Error(e), ());
         promise;
       })
    |> ignore;

let fromAff: Relude_Aff.t('a, 'e) => Js.Promise.t('a) =
  onDone => {
    Js.Promise.make((~resolve, ~reject) =>
      onDone(result =>
        switch (result) {
        | Belt.Result.Ok(v) => {
            () => resolve(. v);
          }
        | Belt.Result.Error(e) => {
            () => reject(. e);
          }
        }
      ) |> Relude_Eff.run
    );
  }
