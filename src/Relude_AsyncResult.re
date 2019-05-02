/*
 AsyncResult is a specialization of AsyncData that uses a Belt.Result.t as the value type.

 This type also implements map/apply/flatMap/etc. to operate on the innermost a value (inside the Result.Ok)
 */

/*******************************************************************************
 * Type definition and constructors
 ******************************************************************************/

type t('a, 'e) = Relude_AsyncData.t(Belt.Result.t('a, 'e));

let init: 'a 'e. t('a, 'e) = Relude_AsyncData.init;

let loading: 'a 'e. t('a, 'e) = Relude_AsyncData.loading;

let reloadingOk: 'a 'e. 'a => t('a, 'e) =
  a => Relude_AsyncData.reloading(Belt.Result.Ok(a));

let reloadingError: 'a 'e. 'e => t('a, 'e) =
  e => Relude_AsyncData.reloading(Belt.Result.Error(e));

let completeOk: 'a 'e. 'a => t('a, 'e) =
  a => Relude_AsyncData.complete(Belt.Result.Ok(a));

let completeError: 'a 'e. 'e => t('a, 'e) =
  e => Relude_AsyncData.complete(Belt.Result.Error(e));

/* Shortcuts */
let ok = completeOk;

let error = completeError;

/*******************************************************************************
 * Needed by typeclasses
 ******************************************************************************/

let pure: 'a 'e. 'a => t('a, 'e) = completeOk;

let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Belt.Result.Ok(a)) => reloadingOk(f(a))
    | Reloading(Belt.Result.Error(_)) as r => r
    | Complete(Belt.Result.Ok(a)) => completeOk(f(a))
    | Complete(Belt.Result.Error(_)) as r => r
    };

let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Belt.Result.Ok(_)) as r => r
    | Reloading(Belt.Result.Error(e)) => reloadingError(f(e))
    | Complete(Belt.Result.Ok(_)) as c => c
    | Complete(Belt.Result.Error(e)) => completeError(f(e))
    };

let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
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

let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (fa, f) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(Ok(a)) => f(a)
    | Reloading(Error(_)) as r => r
    | Complete(Ok(a)) => f(a)
    | Complete(Error(_)) as r => r
    };

let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (f, fa) => bind(fa, f);

let eqBy:
  'a 'e.
  (('e, 'e) => bool, ('a, 'a) => bool, t('a, 'e), t('a, 'e)) => bool
 =
  (errEq, okEq) => Relude_AsyncData.eqBy(Relude_Result.eqBy(errEq, okEq));

/*******************************************************************************
 * Utilities specific to this type
 ******************************************************************************/

let isInit = Relude_AsyncData.isInit;

let isLoading = Relude_AsyncData.isLoading;

let isReloading = Relude_AsyncData.isReloading;

let isComplete = Relude_AsyncData.isComplete;

let isBusy = Relude_AsyncData.isBusy;

let isIdle = Relude_AsyncData.isIdle;

let isOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => true
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => true;

let isError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => true
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => true
  | Complete(Ok(_)) => false;

let isReloadingOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => true
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => false;

let isReloadingError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => true
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => false;

let isCompleteOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => true;

let isCompleteError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => true
  | Complete(Ok(_)) => false;

let getOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(v)) => Some(v)
  | Complete(Error(_)) => None
  | Complete(Ok(v)) => Some(v);

let getError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Complete(Error(x)) => Some(x)
  | Complete(Ok(_)) => None
  | Reloading(Error(x)) => Some(x)
  | Reloading(Ok(_)) => None;

let getReloadingOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(v)) => Some(v)
  | Complete(Error(_)) => None
  | Complete(Ok(_)) => None;

let getReloadingError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(x)) => Some(x)
  | Reloading(Ok(_)) => None
  | Complete(Error(_)) => None
  | Complete(Ok(_)) => None;

let getCompleteOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(_)) => None
  | Complete(Error(_)) => None
  | Complete(Ok(v)) => Some(v);

let getCompleteError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(_)) => None
  | Complete(Error(x)) => Some(x)
  | Complete(Ok(_)) => None;

let toBusy = Relude_AsyncData.toBusy;

let toIdle = Relude_AsyncData.toBusy;

let fold:
  'a 'e 'b.
  (
    'b,
    'b,
    Belt.Result.t('a, 'e) => 'b,
    Belt.Result.t('a, 'e) => 'b,
    t('a, 'e)
  ) =>
  'b
 =
  (initValue, loadingValue, onReloading, onComplete, fa) =>
    switch (fa) {
    | Init => initValue
    | Loading => loadingValue
    | Reloading(result) => onReloading(result)
    | Complete(result) => onComplete(result)
    };

let foldLazy:
  'a 'e 'b.
  (
    unit => 'b,
    unit => 'b,
    Belt.Result.t('a, 'e) => 'b,
    Belt.Result.t('a, 'e) => 'b,
    t('a, 'e)
  ) =>
  'b
 =
  (onInit, onLoading, onReloading, onComplete, fa) =>
    switch (fa) {
    | Init => onInit()
    | Loading => onLoading()
    | Reloading(result) => onReloading(result)
    | Complete(result) => onComplete(result)
    };

let foldByValue: 'a 'e 'b. ('b, 'a => 'b, 'e => 'b, t('a, 'e)) => 'b =
  (defaultValue, onOk, onError, fa) =>
    switch (fa) {
    | Init => defaultValue
    | Loading => defaultValue
    | Reloading(Ok(a)) => onOk(a)
    | Reloading(Error(e)) => onError(e)
    | Complete(Ok(a)) => onOk(a)
    | Complete(Error(e)) => onError(e)
    };

let foldByValueLazy:
  'a 'e 'b.
  (unit => 'b, 'a => 'b, 'e => 'b, t('a, 'e)) => 'b
 =
  (onNoValue, onOk, onError, fa) =>
    switch (fa) {
    | Init => onNoValue()
    | Loading => onNoValue()
    | Reloading(Ok(a)) => onOk(a)
    | Reloading(Error(e)) => onError(e)
    | Complete(Ok(a)) => onOk(a)
    | Complete(Error(e)) => onError(e)
    };

/**
 * Converts an AsyncData to an AsyncResult.
 */
let fromAsyncData: 'a 'e. Relude_AsyncData.t('a) => t('a, 'e) =
  fun
  | Init => Init
  | Loading => Loading
  | Reloading(a) => Reloading(Ok(a))
  | Complete(a) => Complete(Ok(a));

/**
 * Converts an AsyncResult to an AsyncData (requires the value and error types to be the same).
 */
let toAsyncData: 'a. t('a, 'a) => Relude_AsyncData.t('a) =
  fun
  | Init => Init
  | Loading => Loading
  | Reloading(Error(a)) => Reloading(a)
  | Reloading(Ok(a)) => Reloading(a)
  | Complete(Error(a)) => Complete(a)
  | Complete(Ok(a)) => Complete(a);

/*******************************************************************************
 * Typeclass implementations
 ******************************************************************************/

module type FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t);

module Functor: FUNCTOR_F =
  (E: BsAbstract.Interface.TYPE) => {
    type nonrec t('a) = t('a, E.t);
    let map = map;
  };

module type APPLY_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLY with type t('a) = t('a, E.t);

module Apply: APPLY_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Functor(E);
    let apply = apply;
  };

module ApplyFunctions = (E: BsAbstract.Interface.TYPE) =>
  BsAbstract.Functions.Apply((Apply(E)));

module type APPLICATIVE_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, E.t);

module Applicative: APPLICATIVE_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Apply(E);
    let pure = pure;
  };

module type MONAD_F =
  (E: BsAbstract.Interface.TYPE) =>
   BsAbstract.Interface.MONAD with type t('a) = t('a, E.t);

module Monad: MONAD_F =
  (E: BsAbstract.Interface.TYPE) => {
    include Applicative(E);
    let flat_map = bind;
  };

module Infix = (E: BsAbstract.Interface.TYPE) => {
  module Functor = BsAbstract.Infix.Functor((Functor(E)));
  module Apply = BsAbstract.Infix.Apply((Apply(E)));
  module Monad = BsAbstract.Infix.Monad((Monad(E)));
};
