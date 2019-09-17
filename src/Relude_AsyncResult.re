/*
 AsyncResult is a specialization of AsyncData that uses a Belt.Result.t as the value type.

 This type also implements map/apply/flatMap/etc. to operate on the innermost a value (inside the Result.Ok)
 */
type t('a, 'e) = Relude_AsyncData.t(Belt.Result.t('a, 'e));

/**
 * Constructs an Init value
 */
let init: 'a 'e. t('a, 'e) = Relude_AsyncData.init;

/**
 * Constructs a Loading value
 */
let loading: 'a 'e. t('a, 'e) = Relude_AsyncData.loading;

/**
 * Constructs a Reloading(Ok(_)) value
 */
let reloadingOk: 'a 'e. 'a => t('a, 'e) =
  a => Relude_AsyncData.reloading(Belt.Result.Ok(a));

/**
 * Constructs a Reloading(Error(_)) value
 */
let reloadingError: 'a 'e. 'e => t('a, 'e) =
  e => Relude_AsyncData.reloading(Belt.Result.Error(e));

/**
 * Constructs a Complete(Ok(_)) value
 */
let completeOk: 'a 'e. 'a => t('a, 'e) =
  a => Relude_AsyncData.complete(Belt.Result.Ok(a));

/**
 * Constructs a Complete(Error(_)) value
 */
let completeError: 'a 'e. 'e => t('a, 'e) =
  e => Relude_AsyncData.complete(Belt.Result.Error(e));

/**
 * Constructs a Complete(Ok(_)) value
 *
 * Alias for `completeOk`
 */
let ok: 'a 'e. 'a => t('a, 'e) = completeOk;

/**
 * Constructs a Complete(Error(_)) value
 *
 * Alias for `completeError`
 */
let error: 'a 'e. 'e => t('a, 'e) = completeError;

/**
 * Indicates if the AsyncResult is in the Init state
 */
let isInit = Relude_AsyncData.isInit;

/**
 * Indicates if the AsyncResult is in the Loading state
 */
let isLoading = Relude_AsyncData.isLoading;

/**
 * Indicates if the AsyncResult is in the Reloading state with any value
 */
let isReloading = Relude_AsyncData.isReloading;

/**
 * Indicates if the AsyncResult is in the Complete state with any value
 */
let isComplete = Relude_AsyncData.isComplete;

/**
 * Indicates if the AsyncResult is in a working state (Loading or Reloading)
 */
let isBusy = Relude_AsyncData.isBusy;

/**
 * Indicates if the AsyncResult is in a non-working state (Init or Complete)
 */
let isIdle = Relude_AsyncData.isIdle;

/**
 * Indicates if the AsyncResult is Init or Loading
 */
let isEmpty = Relude_AsyncData.isEmpty;

/**
 * Indicates if the AsyncResult is Reloading or Complete
 */
let isNotEmpty = Relude_AsyncData.isNotEmpty;

/**
 * Indicates if the contained Result is in an Ok state for Reloading or Complete
 */
let isOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => true
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => true;

/**
 * Indicates if the contained Result is in an Error state for Reloading or Complete
 */
let isError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => true
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => true
  | Complete(Ok(_)) => false;

/**
 * Indicates if AsyncResult is in a Reloading(Ok(_)) state
 */
let isReloadingOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => true
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => false;

/**
 * Indicates if AsyncResult is in a Reloading(Error(_)) state
 */
let isReloadingError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => true
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => false;

/**
 * Indicates if AsyncResult is in a Complete(Ok(_)) state
 */
let isCompleteOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => false
  | Complete(Ok(_)) => true;

/**
 * Indicates if AsyncResult is in a Complete(Error(_)) state
 */
let isCompleteError: 'a 'e. t('a, 'e) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(Error(_)) => false
  | Reloading(Ok(_)) => false
  | Complete(Error(_)) => true
  | Complete(Ok(_)) => false;

/**
 * Gets the value from a Reloading(Ok(_)) or Complete(Ok(_)) value, as an option
 */
let getOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(v)) => Some(v)
  | Complete(Error(_)) => None
  | Complete(Ok(v)) => Some(v);

/**
 * Gets the value from a Reloading(Error(_)) or Complete(Error(_)) value, as an option
 */
let getError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Complete(Error(x)) => Some(x)
  | Complete(Ok(_)) => None
  | Reloading(Error(x)) => Some(x)
  | Reloading(Ok(_)) => None;

/**
 * Gets the value from a Reloading(Ok(_)) value, as an option
 */
let getReloadingOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(v)) => Some(v)
  | Complete(Error(_)) => None
  | Complete(Ok(_)) => None;

/**
 * Gets the value from a Reloading(Error(_)) value, as an option
 */
let getReloadingError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(x)) => Some(x)
  | Reloading(Ok(_)) => None
  | Complete(Error(_)) => None
  | Complete(Ok(_)) => None;

/**
 * Gets the value from a Complete(Ok(_)) value, as an option
 */
let getCompleteOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(_)) => None
  | Complete(Error(_)) => None
  | Complete(Ok(v)) => Some(v);

/**
 * Gets the value from a Complete(Error(_)) value, as an option
 */
let getCompleteError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Init => None
  | Loading => None
  | Reloading(Error(_)) => None
  | Reloading(Ok(_)) => None
  | Complete(Error(x)) => Some(x)
  | Complete(Ok(_)) => None;

/**
 * Indicates if the AsyncResult is in a working state (Loading or Reloading)
 */
let toBusy = Relude_AsyncData.toBusy;

/**
 * Indicates if the AsyncResult is in a non-working state (Init or Complete)
 */
let toIdle = Relude_AsyncData.toIdle;

/**
 * Maps a pure function over the value in a Reloading(Ok(_)) or Complete(Ok(_)) value
 */
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

/**
 * Maps a pure function over the value in a Reloading(Error(_)) or Complete(Error(_)) value
 */
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

/**
 * Applies a side effect function for each case of the AsyncResult
 */
let tap:
  'a 'e.
  (
    unit => unit,
    unit => unit,
    Belt.Result.t('a, 'e) => unit,
    Belt.Result.t('a, 'e) => unit,
    t('a, 'e)
  ) =>
  t('a, 'e)
 = Relude_AsyncData.tap;

/**
 * Applies a side-effect function if the value is Init
 */
let tapInit: 'a 'e. (unit => unit, t('a, 'e)) => t('a, 'e) = Relude_AsyncData.tapInit;

/**
 * Applies a side-effect function if the value is Loading
 */
let tapLoading: 'a 'e. (unit => unit, t('a, 'e)) => t('a, 'e) = Relude_AsyncData.tapLoading;

/**
 * Applies a side-effect function if the value is Reloading
 */
let tapReloading:
  'a 'e.
  (Belt.Result.t('a, 'e) => unit, t('a, 'e)) => t('a, 'e)
 = Relude_AsyncData.tapReloading;

/**
 * Applies a side-effect function if the value is Complete
 */
let tapComplete:
  'a 'e.
  (Belt.Result.t('a, 'e) => unit, t('a, 'e)) => t('a, 'e)
 = Relude_AsyncData.tapComplete;

/**
 * Applies a side-effect function if the value is Init or Loading
 */
let tapEmpty: 'a 'e. (unit => unit, t('a, 'e)) => t('a, 'e) = Relude_AsyncData.tapEmpty;

/**
 * Applies a side-effect function if the value is Reloading or Complete
 */
let tapNotEmpty:
  'a 'e.
  (Belt.Result.t('a, 'e) => unit, t('a, 'e)) => t('a, 'e)
 = Relude_AsyncData.tapNotEmpty;

/**
 * Applies a side effect function if the value is empty or not-empty
 */
let tapByValue:
  'a 'e.
  (unit => unit, Belt.Result.t('a, 'e) => unit, t('a, 'e)) => t('a, 'e)
 =
  (ifEmpty, ifNotEmpty, fa) =>
    switch (fa) {
    | Init
    | Loading =>
      ifEmpty();
      fa;
    | Reloading(a)
    | Complete(a) =>
      ifNotEmpty(a);
      fa;
    };

/**
 * Applies a side effect function if the value is Reloading or Complete Ok
 */
let tapOk: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (ifOk, fa) =>
    switch (fa) {
    | Init
    | Loading
    | Reloading(Error(_))
    | Complete(Error(_)) => fa
    | Reloading(Ok(a))
    | Complete(Ok(a)) =>
      ifOk(a);
      fa;
    };

/**
 * Applies a side effect function if the value is Reloading or Complete Error
 */
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (ifError, fa) =>
    switch (fa) {
    | Init
    | Loading
    | Reloading(Ok(_))
    | Complete(Ok(_)) => fa
    | Reloading(Error(e))
    | Complete(Error(e)) =>
      ifError(e);
      fa;
    };

/**
 * Applies a wrapped function to a value in a Reloading(Ok(_)) or Complete(Ok(_)) value
 */
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

/**
 * Lifts a pure value into a Complete(Ok(_)) context
 */
let pure: 'a 'e. 'a => t('a, 'e) = completeOk;

/**
 * Applies a monadic function to the value in a Reloading(Ok(_)) or Complete(Ok(_)) value
 */
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

/**
 * Applies a monadic function to the value in a Reloading(Ok(_)) or Complete(Ok(_)) value
 */
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (f, fa) => bind(fa, f);

/**
 * Flattens a nested AsyncResult value one time
 */
let flatten: 'a 'e. t(t('a, 'e), 'e) => t('a, 'e) =
  mma => flatMap(a => a, mma);

/**
 * Folds an AsyncResult value into a value of a new type, by applying the appropriate function
 * for each of the possible states.
 */
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

/**
 * Folds an AsyncResult value into a value of a new type, by applying the appropriate function
 * for each of the possible states.
 */
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

/**
 * Folds an AsyncResult value into a value of a new type, by applying the appropriate
 * function.  The non-value Init/Loading constructors use the same function, the
 * Reloading(Ok(_)) and Complete(Ok(_)) values use the same function and same for Reloading(Error(_))
 * and Complete(Error(_)).
 */
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

/**
 * Folds an AsyncResult value into a value of a new type, by applying the appropriate
 * function.  The non-value Init/Loading constructors use the same function, the
 * Reloading(Ok(_)) and Complete(Ok(_)) values use the same function and same for Reloading(Error(_))
 * and Complete(Error(_)).
 */
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

/**
 * Indicates if two AsyncResult values are in the same state, and have the same contained value.
 */
let eqBy:
  'a 'e.
  (('e, 'e) => bool, ('a, 'a) => bool, t('a, 'e), t('a, 'e)) => bool
 =
  (errEq, okEq) => Relude_AsyncData.eqBy(Relude_Result.eqBy(errEq, okEq));

/**
 * Create a Result module with the given Error Type, specified as a TYPE module.
 *
 * This is useful so that we can provide typeclass instances for typeclasses that
 * have a single type hole, like Functor, Apply, Monad, etc.
 */
module WithError = (E: BsAbstract.Interface.TYPE) => {
  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a, E.t) = {
    type nonrec t('a) = t('a, E.t);
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a, E.t) = {
    include Functor;
    let apply = apply;
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a, E.t) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a, E.t) = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
  };
};