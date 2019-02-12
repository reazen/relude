/*
 AsyncData represents the state of data that is being loaded asynchronously.

 This type does not represent failures by default, but it can by using Belt.Result.t as your 'a type.

 The reason for this is that not all async data loading mechanisms will necessarily fail.

 The other interesting bit is that `Reloading` can be used if you already have data (e.g. an Ok or Error Result),
 but you need to reload the data to get a new Result.
 */

/*******************************************************************************
 * Type definition and constructors
 ******************************************************************************/
type t('a) =
  | Init
  | Loading
  | Reloading('a)
  | Complete('a);

let init: t('a) = Init;
let loading: t('a) = Loading;
let reloading: 'a => t('a) = a => Reloading(a);
let complete: 'a => t('a) = a => Complete(a);

/*******************************************************************************
 * Needed by typeclasses
 ******************************************************************************/

let pure: 'a => t('a) = a => Complete(a);

let map: ('a => 'b, t('a)) => t('b) =
  (f, fa) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => Reloading(f(a))
    | Complete(a) => Complete(f(a))
    };

/* TODO: not sure about this */
let alt: (t('a), t('a)) => t('a) =
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

let apply: (t('a => 'b), t('a)) => t('b) =
  (ff, fa) =>
    switch (ff, fa) {
    | (Init, Init) => Init
    | (Init, Loading) => Loading /* prefer Loading over Init */
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

let bind: (t('a), 'a => t('b)) => t('b) =
  (fa, f) =>
    switch (fa) {
    | Init => Init
    | Loading => Loading
    | Reloading(a) => f(a)
    | Complete(a) => f(a)
    };

let flatMap: ('a => t('b), t('a)) => t('b) = (f, fa) => bind(fa, f);

/*******************************************************************************
 * Utilities specific to this type
 ******************************************************************************/

let isInit: t('a) => bool =
  fun
  | Init => true
  | _ => false;

let isBusy: t('a) => bool =
  fun
  | Loading
  | Reloading(_) => true
  | _ => false;

let isLoading: t('a) => bool =
  fun
  | Loading => true
  | _ => false;

let isReloading: t('a) => bool =
  fun
  | Reloading(_) => true
  | _ => false;

let isComplete: t('a) => bool =
  fun
  | Complete(_) => true
  | _ => false;

/**
 * Get a value of type `'a`, using the `Complete` value if the AsyncData is
 * complete, or the last known complete value in `Reloading`.
 */
let getValue: t('a) => option('a) =
  fun
  | Complete(v)
  | Reloading(v) => Some(v)
  | _ => None;

/**
 * Get `Some` value of type `'a` only from the `Complete` state, and `None` in
 * all other cases, including `Reloading`.
 */
let getComplete: t('a) => option('a) =
  fun
  | Complete(v) => Some(v)
  | _ => None;

/*******************************************************************************
 * Typeclass implementations
 ******************************************************************************/

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};

module Alt: BsAbstract.Interface.ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};

module Infix = {
  include BsAbstract.Infix.Functor(Functor);
  include BsAbstract.Infix.Apply(Apply);
  include BsAbstract.Infix.Monad(Monad);
};

/*******************************************************************************
 * Free utilities from typeclasses
 ******************************************************************************/

module ApplyFunctions = BsAbstract.Functions.Apply(Apply);

/* These can be derived from BsAbstract.Functions.Apply, but because we have an error type, it becomes a module functor with an error type */
let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c) = ApplyFunctions.lift2;

let map3: (('a, 'b, 'c) => 'd, t('a), t('b), t('c)) => t('d) = ApplyFunctions.lift3;

let map4: (('a, 'b, 'c, 'd) => 'e, t('a), t('b), t('c), t('d)) => t('e) = ApplyFunctions.lift4;

let map5:
  (('a, 'b, 'c, 'd, 'e) => 'f, t('a), t('b), t('c), t('d), t('e)) =>
  t('f) = ApplyFunctions.lift5;
