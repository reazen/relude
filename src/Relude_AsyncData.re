/**
AsyncData represents the state of data that is being loaded asynchronously.

This type does not represent failures by default, but it can by using Belt.Result.t as your 'a type.

The reason for this is that not all async data loading mechanisms will necessarily fail.

The other interesting bit is that `Reloading` can be used if you already have data (e.g. an Ok or Error Result),
but you need to reload the data to get a new Result.
*/
type t('a) =
  | Init
  | Loading
  | Reloading('a)
  | Complete('a);

let init: t('a) = Init;

let loading: t('a) = Loading;

let reloading: 'a => t('a) = a => Reloading(a);

let complete: 'a => t('a) = a => Complete(a);

let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => Reloading(f(a))
    | Complete(a) => Complete(f(a))
    };

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

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

module Alt: BsAbstract.Interface.ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

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

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

let pure: 'a. 'a => t('a) = a => Complete(a);

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
  (fa, f) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => f(a)
    | Complete(a) => f(a)
    };

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

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

module Eq = (E: BsAbstract.Interface.EQ) : BsAbstract.Interface.EQ => {
  type nonrec t = t(E.t);
  let eq = eqBy(E.eq);
};

let showBy: 'a. ('a => string, t('a)) => string =
  (showA, fa) =>
    switch (fa) {
    | Init => "Init"
    | Loading => "Loading"
    | Reloading(a) => "Reloading(" ++ showA(a) ++ ")"
    | Complete(a) => "Complete(" ++ showA(a) ++ ")"
    };

module Show = (S: BsAbstract.Interface.SHOW) : BsAbstract.Interface.SHOW => {
  type nonrec t = t(S.t);
  let show = showBy(S.show);
};

let isInit: 'a. t('a) => bool =
  fun
  | Init => true
  | Loading => false
  | Reloading(_) => false
  | Complete(_) => false;

let isLoading: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => true
  | Reloading(_) => false
  | Complete(_) => false;

let isReloading: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(_) => true
  | Complete(_) => false;

let isComplete: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => false
  | Reloading(_) => false
  | Complete(_) => true;

let isBusy: 'a. t('a) => bool =
  fun
  | Init => false
  | Loading => true
  | Reloading(_) => true
  | Complete(_) => false;

let isIdle: 'a. t('a) => bool = fa => !isBusy(fa);

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

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Alt.AltInfix(Alt);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);
};

/* This stuff comes in via extensions now
   module ApplyFunctions = BsAbstract.Functions.Apply(Apply);

   /* These can be derived from BsAbstract.Functions.Apply, but because we have an error type, it becomes a module functor with an error type */
   let map2: 'a 'b 'c. (('a, 'b) => 'c, t('a), t('b)) => t('c) = ApplyFunctions.lift2;

   let map3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, t('a), t('b), t('c)) => t('d) = ApplyFunctions.lift3;

   let map4:
     'a 'b 'c 'd 'e.
     (('a, 'b, 'c, 'd) => 'e, t('a), t('b), t('c), t('d)) => t('e)
    = ApplyFunctions.lift4;

   let map5:
     'a 'b 'c 'd 'e 'f.
     (('a, 'b, 'c, 'd, 'e) => 'f, t('a), t('b), t('c), t('d), t('e)) =>
     t('f)
    = ApplyFunctions.lift5;
    */