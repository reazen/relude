/**
Lifts a pure value into an option
*/
let some: 'a. 'a => option('a) = a => Some(a);

/**
Returns a `None` value
*/
let none: 'a. option('a) = None;

/**
Alias for `none`
 */
let empty: 'a. option('a) = None;

/**
  `isSome(opt)` returns `true` if `opt` is of the form `Some(v)`;
  `false` otherwise.
*/
let isSome: 'a. option('a) => bool =
  fun
  | Some(_) => true
  | None => false;

/**
  `isNone(opt)` returns `true` if `opt` is `None`;
  `false` otherwise.
*/
let isNone: 'a. option('a) => bool =
  fun
  | Some(_) => false
  | None => true;

/**
  `fold(default, f, opt)` returns `f(v)` when `opt` is `Some(v)`.
  If `opt` is `None`, `fold()` returns `default`, which must be of
  the same type that `f()` returns.

  This is not a lazy function, as the `default` value is always
  evaluated, no matter what `opt`’s value is.

  ### Example
  ```re
  fold(0.0, (x) => {1.0 /. x}, Some(2.0)) == 0.5;
  fold(0.0, (x) => {1.0 /. x}, None) == 0.0;
  ```
*/
let fold: 'a 'b. ('b, 'a => 'b, option('a)) => 'b =
  (default, f) =>
    fun
    | Some(v) => f(v)
    | None => default;

/**
  `foldLazy(defaultFcn, f, opt)` returns `f(v)` when `opt` is
  `Some(v)`. If `opt` is `None`, `foldLazy()` returns `default()`.

  The `default()` function must have no parameters and must return
  a value of the same type that `f()` returns.

  This is a *lazy* function because the default value is not evaluated
  unless `opt` is `None`.

  ### Example
  ```re
  let zero = () => 0.0;
  foldLazy(zero, (x) => {1.0 /. x}, Some(2.0)) == 0.5;
  foldLazy(zero, (x) => {1.0 /. x}, None) == 0.0;
  ```
*/
let foldLazy: 'a 'b. (unit => 'b, 'a => 'b, option('a)) => 'b =
  (getDefault, f) =>
    fun
    | Some(v) => f(v)
    | None => getDefault();

/**
  `getOrElse(default, opt)` returns `v` when `opt` is
  `Some(v)`. If `opt` is `None`, `getOrelse()` returns `default`.

  If `opt` is `None`, `getOrElse()` returns `default`, which must be of
  the same type that as `v`.

  This is not a lazy function, as the `default` value is always
  evaluated, no matter what `opt`’s value is.

  ### Example
  ```re
  getOrElse(0, Some(3)) == 3;
  getOrElse(0, None) == 0;
  ```
*/
let getOrElse: 'a. ('a, option('a)) => 'a =
  default =>
    fun
    | Some(a) => a
    | None => default;

/**
  `getOrElseLazy(defaultFcn, opt)` returns `v` when `opt` is
  `Some(v)`. If `opt` is `None`, `getOrElseLazy()` returns `defaultFcn()`.

  The `defaultFcn()` function must have no parameters and must return
  a value of the same type that `f()` returns.

  This is a *lazy* function because the default value is not evaluated
  unless `opt` is `None`.

  ### Example
  ```re
  let zero = () => {0};
  getOrElseLazy(zero, Some(3)) == 3;
  getOrElseLazy(zero, None) == 0;
  ```
*/
let getOrElseLazy: 'a. (unit => 'a, option('a)) => 'a =
  getDefault =>
    fun
    | Some(a) => a
    | None => getDefault();

/**
 * `getOrThrow(opt)` returns the value of the option or throws an exception.
 *
 * This should only be used if you are absolutely sure there is a value in the option.
 */
let getOrThrow: 'a. option('a) => 'a = Belt.Option.getExn;

/**
 * Similar to alt, but with the arguments reversed and labelled for use with `|>`
 */
let orElse: 'a. (~fallback: option('a), option('a)) => option('a) =
  (~fallback) =>
    fun
    | Some(_) as fa => fa
    | None => fallback;

/**
 * Similar to alt, but with the arguments reversed and labelled for use with `|>`.
 * The fallback value is also lazy for expensive constructions.
 */
let orElseLazy:
  'a.
  (~fallback: unit => option('a), option('a)) => option('a)
 =
  (~fallback) =>
    fun
    | Some(_) as fa => fa
    | None => fallback();

/**
 * `tap(f, opt)` applies a side-effect function to the value in a `Some`, and returns
 * the original option value untouched.
 */
let tap: 'a. ('a => unit, option('a)) => option('a) =
  (ifSome, fa) =>
    switch (fa) {
    | Some(a) =>
      ifSome(a);
      fa;
    | None => fa
    };

/**
 * `tapSome` is an alias for `tap`
 */
let tapSome: 'a. ('a => unit, option('a)) => option('a) = tap;

/**
 * `tap(f, opt)` applies a side-effect function if the value of the option is None, and returns
 * the original option value untouched.
 */
let tapNone: 'a. (unit => unit, option('a)) => option('a) =
  (ifNone, fa) =>
    switch (fa) {
    | Some(_) => fa
    | None =>
      ifNone();
      fa;
    };

/**
 * `bitap(ifNone, ifSome, opt)` applies a side effect function for each of the cases of the option, and
 * returns the original option untouched.
 */
let bitap: 'a. (unit => unit, 'a => unit, option('a)) => option('a) =
  (ifNone, ifSome, fa) =>
    switch (fa) {
    | Some(a) =>
      ifSome(a);
      fa;
    | None =>
      ifNone();
      fa;
    };

/**
  `filter(f, opt)` works as follows:

  - If `opt` is `Some(v)` and `f(v)` is `true`, the result is `Some(v)`.
  - If `opt` is `Some(v)` and `f(v)` is `false`, the result is `None`.
  - If `opt` is `None`, the result is `None`.

  ### Example
  ```re
  let isEven = x => x mod 2 == 0;
  filter(isEven, Some(2)) == Some(2);
  filter(isEven, Some(3)) == None;
  filter(isEven, None) == None;
  ```
*/
let filter: 'a. ('a => bool, option('a)) => option('a) =
  fn =>
    Relude_Option_Instances.foldLeft(
      (default, v) => fn(v) ? Some(v) : default,
      empty,
    );

/**
  `filterNot` is the inverse of `filter`, meaning `Some` values are preserved
  if the provided predicate function returns false.

  ### Example
  ```re
  let isEven = x => x mod 2 == 0;
  filterNot(isEven, Some(1)) == Some(1);
  filterNot(isEven, Some(2)) == None;
  ```
*/
let filterNot: 'a. ('a => bool, option('a)) => option('a) =
  f => filter(a => !f(a));
