/*
 AsyncResult is a specialization of AsyncData that uses a Belt.Result.t as the value type.

 This type also implements map/apply/flatMap/etc. to operate on the innermost a value (inside the Result.Ok)
 */

type t('a, 'e) = Relude_AsyncData.t(Belt.Result.t('a, 'e));

let init: t('a, 'e) = Relude_AsyncData.init;

let loading: t('a, 'e) = Relude_AsyncData.loading;

let reloadingOk: 'a => t('a, 'e) =
  a => Relude_AsyncData.reloading(Belt.Result.Ok(a));

let reloadingError: 'e => t('a, 'e) =
  e => Relude_AsyncData.reloading(Belt.Result.Error(e));

let completeOk: 'a => t('a, 'e) =
  a => Relude_AsyncData.complete(Belt.Result.Ok(a));

let completeError: 'e => t('a, 'e) =
  e => Relude_AsyncData.complete(Belt.Result.Error(e));

/* Shortcuts */

let ok = completeOk;

let error = completeError;

let pure = ok;

let map: ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Belt.Result.Ok(a)) => reloadingOk(f(a))
    | Reloading(Belt.Result.Error(_)) as r => r
    | Complete(Belt.Result.Ok(a)) => completeOk(f(a))
    | Complete(Belt.Result.Error(_)) as r => r
    };

let mapError: ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Belt.Result.Ok(_)) as r => r
    | Reloading(Belt.Result.Error(e)) => reloadingError(f(e))
    | Complete(Belt.Result.Ok(_)) as c => c
    | Complete(Belt.Result.Error(e)) => completeError(f(e))
    };

let apply: (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (ff, fa) =>
    switch (ff, fa) {
    | (Init, Init) => Init
    | (Init, Loading) => Loading
    | (Init, Reloading(Ok(_))) => Init
    | (Init, Reloading(Error(_)) as r) => r
    | (Init, Complete(Ok(_))) => Init
    | (Init, Complete(Error(_)) as r) => r

    | (Loading, Init) => Loading
    | (Loading, Loading) => Loading
    | (Loading, Reloading(Ok(_))) => Init
    | (Loading, Reloading(Error(_)) as r) => r
    | (Loading, Complete(Ok(_))) => Init
    | (Loading, Complete(Error(_)) as r) => r

    | (Reloading(Ok(_)), Init) => Init
    | (Reloading(Error(_)) as r, Init) => r
    | (Reloading(Ok(_)), Loading) => Loading
    | (Reloading(Error(_)) as r, Loading) => r
    | (Reloading(Ok(f)), Reloading(Ok(a))) => reloadingOk(f(a))
    | (Reloading(Ok(_)), Reloading(Error(_)) as r) => r
    | (Reloading(Error(_)) as r, Reloading(Ok(_))) => r
    | (Reloading(Error(_)) as r, Reloading(Error(_))) => r
    | (Reloading(Ok(f)), Complete(Ok(a))) => reloadingOk(f(a))
    | (Reloading(Ok(_)), Complete(Error(_)) as r) => r
    | (Reloading(Error(_)) as r, Complete(Ok(_))) => r
    | (Reloading(Error(_)) as r, Complete(Error(_))) => r

    | (Complete(Ok(_)), Init) => Init
    | (Complete(Error(_)) as r, Init) => r
    | (Complete(Ok(_)), Loading) => Loading
    | (Complete(Error(_)) as r, Loading) => r
    | (Complete(Ok(f)), Reloading(Ok(a))) => reloadingOk(f(a))
    | (Complete(Ok(_)), Reloading(Error(_)) as r) => r
    | (Complete(Error(_)) as r, Reloading(Ok(_))) => r
    | (Complete(Error(_)) as r, Reloading(Error(_))) => r
    | (Complete(Ok(f)), Complete(Ok(a))) => completeOk(f(a))
    | (Complete(Ok(_)), Complete(Error(_)) as r) => r
    | (Complete(Error(_)) as r, Complete(Ok(_))) => r
    | (Complete(Error(_)) as r, Complete(Error(_))) => r
    };

let flatMap: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (fa, f) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Ok(a)) => f(a)
    | Reloading(Error(_)) as r => r
    | Complete(Ok(a)) => f(a)
    | Complete(Error(_)) as r => r
    };

module type FUNCTOR_F = (E: BsAbstract.Interface.TYPE) => BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t);

module Functor: FUNCTOR_F = (E: BsAbstract.Interface.TYPE) => {
  type nonrec t('a) = t('a, E.t);
  let map = map;
}

module type APPLY_F = (E: BsAbstract.Interface.TYPE) => BsAbstract.Interface.APPLY with type t('a) = t('a, E.t);

module Apply: APPLY_F = (E: BsAbstract.Interface.TYPE) => {
  include Functor(E);
  let apply = apply;
}

module ApplyFunctions = (E: BsAbstract.Interface.TYPE) => BsAbstract.Functions.Apply(Apply(E));

module type APPLICATIVE_F = (E: BsAbstract.Interface.TYPE) => BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, E.t);

module Applicative: APPLICATIVE_F = (E: BsAbstract.Interface.TYPE) => {
  include Apply(E);
  let pure = pure;
}

module type MONAD_F = (E: BsAbstract.Interface.TYPE) => BsAbstract.Interface.MONAD with type t('a) = t('a, E.t);

module Monad: MONAD_F = (E: BsAbstract.Interface.TYPE) => {
  include Applicative(E);
  let flat_map = flatMap;
}

module Infix = (E: BsAbstract.Interface.TYPE) => {
  module Functor = BsAbstract.Infix.Functor(Functor(E));
  module Apply = BsAbstract.Infix.Apply(Apply(E));
  module Monad = BsAbstract.Infix.Monad(Monad(E));
}
