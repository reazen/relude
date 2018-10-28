open BsAbstract.Interface;
open Belt.Result;

/**
 * Similar to Belt.Result, but has an Applicative instance that collects the errors using a semigroup, rather than fail-fast
 * semantics.
 */
type t('a, 'b) =
| VOk('a)
| VError('b)
;

let isOk: t('a, 'b) => bool = a => switch(a) {
| VOk(_) => true
| VError(_) => false
};

let isError: t('a, 'b) => bool = a => !isOk(a);

let map = (f, v) => switch (v) {
| VOk(a) => VOk(f(a));
| VError(e) => VError(e);
};

let apply = (f, v, append) => switch (f, v) {
| (VOk(f), VOk(a)) => VOk(f(a));
| (VOk(_), VError(b)) => VError(b);
| (VError(b), VOk(_)) => VError(b);
| (VError(b1), VError(b2)) => VError(append(b1, b2));
};

let pure = a => VOk(a);

/**
 * This function performs a flatMap-like operation, but if the `f` fails, all previous errors are discarded.
 * Validation is not a traditional Monad, because the point of it is to preserve the errors via a Semigroup.
 */
let flatMapV: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) = (v, f) => switch v {
| VOk(a) => f(a);
| VError(e) => VError(e);
};

let fromResult: Belt.Result.t('a, 'b) => t('a, 'b) = result => switch (result) {
| Ok(a) => VOk(a);
| Error(e) => VError(e);
};

let toResult: t('a, 'b) => Belt.Result.t('a, 'b) = validation => switch (validation) {
| VOk(a) => Ok(a);
| VError(a) => Error(a);
};

module type FUNCTOR_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => FUNCTOR with type t('a) = t('a, Errors.t(Error.t));
module Functor: FUNCTOR_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => {
  type t_('a, 'b) = t('a, 'b); /* TODO: Compiler seems to dislike trying to use the `t` from above in the line below */
  type t('a) = t_('a, Errors.t(Error.t));
  let map = map;
};

module type APPLY_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => APPLY with type t('a) = t('a, Errors.t(Error.t));
module Apply: APPLY_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => {
  include Functor(Errors, Error);
  let apply = (f, v) => apply(f, v, Errors.append);
};

module type APPLICATIVE_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => APPLICATIVE with type t('a) = t('a, Errors.t(Error.t));
module Applicative: APPLICATIVE_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => {
  include Apply(Errors, Error)
  let pure = pure;
};
