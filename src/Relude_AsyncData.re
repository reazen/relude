/**
 * AsyncData represents the state of data that is being loaded asynchronously.
 * While Promise and IO represent the effect of loading that data, `AsyncData`
 * represents what that data looks like at one particular snapshot in time. This
 * is particularly useful when storing application state (e.g. in a React
 * component).
 *
 * By default, this type does not represent failures. If you want to represent
 * the possibility for an async value to fail, you can use a `result` in the
 * `'a` type (or see `AsyncResult`, which does this for you).
 *
 * The reason for this is that not all async data loading mechanisms will
 * necessarily fail.
 *
 * The other interesting bit is that `Reloading` can be used if you already have
 * data (e.g. an Ok or Error Result), but you need to reload the data to get a
 * new result.
*/
type t('a) =
  | Init
  | Loading
  | Reloading('a)
  | Complete('a);

/**
 * Constructs an Init value
 */
let init: t('a) = Init;

/**
 * Constructs a Loading value
 */
let loading: t('a) = Loading;

/**
 * Constructs a Reloading value containing the given value
 */
let reloading: 'a => t('a) = a => Reloading(a);

/**
 * Constructs a Reloading value containing the given value
 */
let complete: 'a => t('a) = a => Complete(a);

/**
 * Checks if this AsyncData value is Init
 */
let isInit: 'a. t('a) => bool =
  fun
  | Init => true
  | Loading => false
  | Reloading(_) => false
  | Complete(_) => false;

/**
 * Checks if this AsyncData value is Loading
 */
let isLoading: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => true
  | Reloading(_) => false
  | Complete(_) => false;

/**
 * Checks if this AsyncData value is Reloading with any value
 */
let isReloading: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(_) => true
  | Complete(_) => false;

/**
 * Checks if this AsyncData value is Complete with any value
 */
let isComplete: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(_) => false
  | Complete(_) => true;

/**
 * Checks if this AsyncData value is working (Loading or Reloading)
 */
let isBusy: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => true
  | Reloading(_) => true
  | Complete(_) => false;

/**
 * Checks if this AsyncData value is not working (Init or Complete)
 */
let isIdle: 'a. t('a) => bool = fa => !isBusy(fa);

/**
 * Checks if this AsyncData value is Init or Loading
 */
let isEmpty: 'a. t('a) => bool =
  fun
  | Init => true
  | Loading => true
  | Reloading(_) => false
  | Complete(_) => false;

/**
 * Checks if this AsyncData value is Reloading or Complete
 */
let isNotEmpty: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(_) => true
  | Complete(_) => true;

/**
 * Creates a new `AsyncData` by transitioning the given `AsyncData` into a busy state (`Loading` or `Reloading`), and carrying over the internal data if available.
 */
let toBusy: 'a. t('a) => t('a) =
  fun
  | Init => Loading
  | Loading as a => a
  | Reloading(_) as a => a
  | Complete(a) => Reloading(a);

/**
 * Creates a new `AsyncData` by transitioning the given `AsyncData` into an idle state (`Init` or `Complete`), and carrying over the internal data if available.
 */
let toIdle: 'a. t('a) => t('a) =
  fun
  | Init as a => a
  | Loading => Init
  | Reloading(a) => Complete(a)
  | Complete(_) as a => a;

/**
 * Get a value of type `'a`, using the `Complete` value if the AsyncData is
 * complete, or the last known complete value in `Reloading`.
 */
let getValue: 'a. t('a) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(v) => Some(v)
  | Complete(v) => Some(v);

/**
 * Get `Some` value of type `'a` only from the `Reloading` state, and `None` in
 * all other cases, including `Complete`
 */
let getReloading: 'a. t('a) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(a) => Some(a)
  | Complete(_) => None;

/**
 * Get `Some` value of type `'a` only from the `Complete` state, and `None` in
 * all other cases, including `Reloading`.
 */
let getComplete: 'a. t('a) => option('a) =
  fun
  | Init => None
  | Loading => None
  | Reloading(_) => None
  | Complete(a) => Some(a);

/**
 * Fold the `AsyncData` into a new type by providing a strict value or function to handle each case.
 */
let fold: 'a 'b. ('b, 'b, 'a => 'b, 'a => 'b, t('a)) => 'b =
  (initValue, loadingValue, onReloading, onComplete, fa) =>
    switch (fa) {
    | Init => initValue
    | Loading => loadingValue
    | Reloading(a) => onReloading(a)
    | Complete(a) => onComplete(a)
    };

/**
 * Fold the `AsyncData` into a new value by providing a function to handle each case.
 */
let foldLazy: 'a 'b. (unit => 'b, unit => 'b, 'a => 'b, 'a => 'b, t('a)) => 'b =
  (onInit, onLoading, onReloading, onComplete, fa) =>
    switch (fa) {
    | Init => onInit()
    | Loading => onLoading()
    | Reloading(a) => onReloading(a)
    | Complete(a) => onComplete(a)
    };

/**
 * Fold the `AsyncData` into a new value by providing a strict value to use when there is no data, or function to handle when there is data.
 */
let foldByValue: 'a 'b. ('b, 'a => 'b, t('a)) => 'b =
  (defaultValue, onValue, fa) =>
    fold(defaultValue, defaultValue, onValue, onValue, fa);

/**
 * Fold the `AsyncData` into a new value by providing a lazy value to use when there is no data, or function to handle when there is data.
 */
let foldByValueLazy: 'a 'b. (unit => 'b, 'a => 'b, t('a)) => 'b =
  (onNoValue, onValue, fa) =>
    foldLazy(onNoValue, onNoValue, onValue, onValue, fa);

/**
 * Maps a pure function over the value contained by Reloading or Complete
 */
let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => Reloading(f(a))
    | Complete(a) => Complete(f(a))
    };

module Functor: BsBastet.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
 * Applies a side effect function for each case of the variant (Init, Loading, Reloading, Complete)
 */
let tap:
  'a.
  (unit => unit, unit => unit, 'a => unit, 'a => unit, t('a)) => t('a)
 =
  (ifInit, ifLoading, ifReloading, ifComplete, fa) =>
    switch (fa) {
    | Init =>
      ifInit();
      fa;
    | Loading =>
      ifLoading();
      fa;
    | Reloading(a) =>
      ifReloading(a);
      fa;
    | Complete(a) =>
      ifComplete(a);
      fa;
    };

/**
 * Applies a side effect function if the value is Init
 */
let tapInit: 'a. (unit => unit, t('a)) => t('a) =
  (ifInit, fa) =>
    switch (fa) {
    | Init =>
      ifInit();
      fa;
    | Loading => fa
    | Reloading(_) => fa
    | Complete(_) => fa
    };

/**
 * Applies a side effect function if the value is Loading
 */
let tapLoading: 'a. (unit => unit, t('a)) => t('a) =
  (ifLoading, fa) =>
    switch (fa) {
    | Init => fa
    | Loading =>
      ifLoading();
      fa;
    | Reloading(_) => fa
    | Complete(_) => fa
    };

/**
 * Applies a side effect function if the value is Reloading
 */
let tapReloading: 'a. ('a => unit, t('a)) => t('a) =
  (ifReloading, fa) =>
    switch (fa) {
    | Init => fa
    | Loading => fa
    | Reloading(a) =>
      ifReloading(a);
      fa;
    | Complete(_) => fa
    };

/**
 * Applies a side effect function if the value is Complete
 */
let tapComplete: 'a. ('a => unit, t('a)) => t('a) =
  (ifComplete, fa) =>
    switch (fa) {
    | Init => fa
    | Loading => fa
    | Reloading(_) => fa
    | Complete(a) =>
      ifComplete(a);
      fa;
    };

/**
 * Applies a side effect function if the value is Init or Loading
 */
let tapEmpty: 'a. (unit => unit, t('a)) => t('a) =
  (ifEmpty, fa) =>
    switch (fa) {
    | Init
    | Loading =>
      ifEmpty();
      fa;
    | Reloading(_)
    | Complete(_) => fa
    };

/**
 * Applies a side effect function if the value is Reloading or Complete
 */
let tapNotEmpty: 'a. ('a => unit, t('a)) => t('a) =
  (ifNotEmpty, fa) =>
    switch (fa) {
    | Init
    | Loading => fa
    | Reloading(a)
    | Complete(a) =>
      ifNotEmpty(a);
      fa;
    };

/**
 * Applies a side effect function for the empty case and the not-empty case
 */
let tapByValue: 'a. (unit => unit, 'a => unit, t('a)) => t('a) =
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
 * Applies a wrapped function to the value contained by Reloading or Complete
 */
let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
  (ff, fa) =>
    switch (ff, fa) {
    | (Init, Init) => Init
    | (Init, Loading) => Loading
    | (Init, Reloading(_)) => Init
    | (Init, Complete(_)) => Init

    | (Loading, Init) => Loading
    | (Loading, Loading) => Loading
    | (Loading, Reloading(_)) => Loading
    | (Loading, Complete(_)) => Loading

    | (Reloading(_), Init) => Init
    | (Reloading(_), Loading) => Loading
    | (Reloading(f), Reloading(a)) => Reloading(f(a))
    | (Reloading(f), Complete(a)) => Reloading(f(a))

    | (Complete(_), Init) => Init
    | (Complete(_), Loading) => Loading
    | (Complete(f), Reloading(a)) => Reloading(f(a))
    | (Complete(f), Complete(a)) => Complete(f(a))
    };

module Apply: BsBastet.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
 * Lifts a pure value into the context of an AsyncData, in the Complete state
 */
let pure: 'a. 'a => t('a) = a => Complete(a);

module Applicative: BsBastet.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
 * Applies a monadic function to the value contained by Reloading or Complete
 */
let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
  (fa, f) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => f(a)
    | Complete(a) => f(a)
    };

module Monad: BsBastet.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

/**
 * alt for AsyncData tries to find the most advanced state between
 * two AsyncData values in terms of completeness.
 */
let alt: 'a. (t('a), t('a)) => t('a) =
  (fa, fb) =>
    switch (fa, fb) {
    | (Init, Init) => Init
    | (Init, Loading) => Loading
    | (Init, Reloading(_) as r) => r
    | (Init, Complete(_) as c) => c

    | (Loading, Init) => Loading
    | (Loading, Loading) => Loading
    | (Loading, Reloading(_) as r) => r
    | (Loading, Complete(_) as c) => c

    | (Reloading(_) as r, Init) => r
    | (Reloading(_) as r, Loading) => r
    | (Reloading(_) as r, Reloading(_)) => r
    | (Reloading(_), Complete(_) as c) => c

    | (Complete(_) as c, Init) => c
    | (Complete(_) as c, Loading) => c
    | (Complete(_) as c, Reloading(_)) => c
    | (Complete(_) as c, Complete(_)) => c
    };

module Alt: BsBastet.Interface.ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

/**
 * Indicates if two AsyncData values are in the same state, and that the
 * contained values are equal.
 */
let eqBy: 'a. (('a, 'a) => bool, t('a), t('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | (Init, Init)
    | (Loading, Loading) => true
    | (Reloading(innerA), Reloading(innerB)) => innerEq(innerA, innerB)
    | (Complete(innerA), Complete(innerB)) => innerEq(innerA, innerB)
    | (Init, _)
    | (Loading, _)
    | (Reloading(_), _)
    | (Complete(_), _) => false
    };

module Eq = (E: BsBastet.Interface.EQ) : BsBastet.Interface.EQ => {
  type nonrec t = t(E.t);
  let eq = eqBy(E.eq);
};

/**
 * Converts an AsyncData value to a string, using the given function to
 * convert the contained value to a string.
 */
let showBy: 'a. ('a => string, t('a)) => string =
  (showA, fa) =>
    switch (fa) {
    | Init => "Init"
    | Loading => "Loading"
    | Reloading(a) => "Reloading(" ++ showA(a) ++ ")"
    | Complete(a) => "Complete(" ++ showA(a) ++ ")"
    };

module Show = (S: BsBastet.Interface.SHOW) : BsBastet.Interface.SHOW => {
  type nonrec t = t(S.t);
  let show = showBy(S.show);
};

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Alt.AltInfix(Alt);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);
};
