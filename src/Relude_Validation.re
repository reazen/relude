/**
 * Similar to Belt.Result, but has an Applicative instance that collects the errors using a semigroup, rather than fail-fast
 * semantics.
 */
type t('a, 'e) =
  | VOk('a)
  | VError('e);

let pure = a => VOk(a);

let ok = pure;

let error = e => VError(e);

let isOk: t('a, 'b) => bool =
  fun
  | VOk(_) => true
  | VError(_) => false;

let isError: t('a, 'b) => bool = a => !isOk(a);

let map = (f, v) =>
  switch (v) {
  | VOk(a) => VOk(f(a))
  | VError(e) => VError(e)
  };

let mapError = (f, v) =>
  switch (v) {
  | VOk(a) => VOk(a)
  | VError(e) => VError(f(e))
  };

let apply = (ff, fv, appendErrors) =>
  switch (ff, fv) {
  | (VOk(f), VOk(a)) => VOk(f(a))
  | (VOk(_), VError(e)) => VError(e)
  | (VError(e), VOk(_)) => VError(e)
  | (VError(e1), VError(e2)) => VError(appendErrors(e1, e2))
  };

/**
 * This function performs a flatMap-like operation, but if the `f` fails, all previous errors are discarded.
 * Validation is not a traditional Monad, because the point of it is to preserve the errors via a Semigroup.
 */
let flatMapV: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (v, f) =>
    switch (v) {
    | VOk(a) => f(a)
    | VError(e) => VError(e)
    };

let fromResult: Belt.Result.t('a, 'b) => t('a, 'b) =
  result =>
    switch (result) {
    | Ok(a) => VOk(a)
    | Error(e) => VError(e)
    };

let toResult: t('a, 'b) => Belt.Result.t('a, 'b) =
  validation =>
    switch (validation) {
    | VOk(a) => Ok(a)
    | VError(a) => Error(a)
    };

/* Modules */

module type FUNCTOR_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) =>
   BsAbstract.Interface.FUNCTOR with type t('a) = t('a, Errors.t(Error.t));

module Functor: FUNCTOR_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) => {
    type nonrec t('a) = t('a, Errors.t(Error.t));
    let map = map;
  };

module type APPLY_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) =>
   BsAbstract.Interface.APPLY with type t('a) = t('a, Errors.t(Error.t));

module Apply: APPLY_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) => {
    include Functor(Errors, Error);
    let apply = (f, v) => apply(f, v, Errors.append);
  };

module type APPLICATIVE_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) =>

    BsAbstract.Interface.APPLICATIVE with
      type t('a) = t('a, Errors.t(Error.t));

module Applicative: APPLICATIVE_F =
  (
    Errors: BsAbstract.Interface.SEMIGROUP_ANY,
    Error: BsAbstract.Interface.TYPE,
  ) => {
    include Apply(Errors, Error);
    let pure = pure;
  };
