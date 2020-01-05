/**
 * Similar to Belt.Result, but has an Applicative instance that collects the errors using a semigroup, rather than fail-fast
 * semantics.
 */
type t('a, 'e) =
  | VOk('a)
  | VError('e);

/**
 `ok()` wraps a value in `VOk`
*/
let ok: 'a 'e. 'a => t('a, 'e) = a => VOk(a);

/**
  `error(val)` wraps the value in a `VError()`.

  ### Example
  ```re
  error("Not even") == VError("Not even");
  ```
*/
let error: 'a 'e. 'e => t('a, 'e) = e => VError(e);

/**
  `isOk(v)` returns `true` if `v` is of the form `VOk(val)`;
  `false` otherwise.
*/
let isOk: 'a 'e. t('a, 'e) => bool =
  fun
  | VOk(_) => true
  | VError(_) => false;

/**
  `isError(x)` returns `true` if `x` is of the form `VError(val)`;
  `false` otherwise.
*/
let isError: 'a 'e. t('a, 'e) => bool = a => !isOk(a);

/**
  `map(f, x)` returns `VOk(f(x))` if `x` is of the form `VOk(v)`.
  It returns `VError(e)` if `x` is of the form `VError(e)`.

  ### Example
  ```re
  map((x) => {sqrt(float_of_int(x))}, VOk(4)) == VOk(2.0);
  map((x) => {sqrt(float_of_int(x))}, VError("bad")) == VError("bad");
  ```
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, v) =>
    switch (v) {
    | VOk(a) => VOk(f(a))
    | VError(e) => VError(e)
    };

/**
  In `tap(f, x)`, function `f()` returns `unit`. Thus, `f()`
  is used only for its side effects. If `x` is of the
  form `VOk(v)`, `tap()` calls `f(v)`. The `tap()` function returns
  the argument `x`.

  ### Example
  ```re
  tap((x) => {Js.log(x)}, VOk(4)) == VOk(4); // prints 4
  tap((x) => {Js.log(x)}, VError("bad")) == VError("bad"); // prints nothing
  ```
*/
let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, fa) =>
    fa
    |> map(a => {
         f(a);
         a;
       });

/**
  `mapError(f, x)` returns `VOk(v)` if `x` is of the form `VOk(v)`.
  It returns `VError(f(e))` if `x` is of the form `VError(e)`.

  ### Example
  ```re
  mapError((x) => {"Error: " ++ x}, VOk(4)) == VOk(4);
  mapError((x) => {"Error: " ++ x}, VError("bad")) == VError("Error: bad");
  ```
*/
let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, fa) =>
    switch (fa) {
    | VOk(a) => VOk(a)
    | VError(e) => VError(f(e))
    };

/**
  `mapErrorsAsNea()` applies a function to each error in a `NonEmpty.Array` of errors in the
  error channel of the `Validation`.
*/
let mapErrorsNea:
  'a 'e1 'e2.
  ('e1 => 'e2, t('a, Relude_NonEmpty.Array.t('e1))) =>
  t('a, Relude_NonEmpty.Array.t('e2))
 =
  (f, fa) =>
    switch (fa) {
    | VOk(_) as ok => ok
    | VError(nea) => VError(nea |> Relude_NonEmpty.Array.map(f))
    };

/**
  `mapErrorsAsNel()` applies a function to each error in a `NonEmpty.List` of errors in the error
  channel of the `Validation`.
*/
let mapErrorsNel:
  'a 'e1 'e2.
  ('e1 => 'e2, t('a, Relude_NonEmpty.List.t('e1))) =>
  t('a, Relude_NonEmpty.List.t('e2))
 =
  (f, fa) =>
    switch (fa) {
    | VOk(_) as ok => ok
    | VError(nea) => VError(nea |> Relude_NonEmpty.List.map(f))
    };

/**
  In `tapError(f, x)`, function `f()` returns `unit`. Thus, `f()`
  is used only for its side effects. If `x` is of the
  form `VError(v)`, `tap()` calls `f(v)`. The `tap()` function returns
  the argument `x`.

  ### Example
  ```re
  tapError((x) => {Js.log(x)}, VOk(4)) == VOk(4); // prints nothing
  tapError((x) => {Js.log(x)}, VError("bad")) == VError("bad"); // prints "bad"
  ```
*/
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, fa) =>
    fa
    |> mapError(e => {
         f(e);
         e;
       });

/**
  `bimap(f, g, x)` returns `VOk(f(v))` if `x` is of the form `VOk(v)`;
  it returns `VError(g(e))` if `x` is of the form `VError(e)`.

  ### Example
  ```re
  let cube = (x) => {x * x * x};
  let label = (x) => {"Error: " ++ x};
  bimap(cube, label, VOk(12)) == VOk(1728);
  bimap(cube, label, VError("bad")) == VError("Error: bad");
  ```
*/
let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (f, g, fa) =>
    switch (fa) {
    | VOk(a) => VOk(f(a))
    | VError(e) => VError(g(e))
    };

/**
  `bitap(f, g, x)` the functions `f()` and `g()` both return
  `unit`, so they are used only for their side effects. If `x`
  is of the form `VOk(v)`, `bitap()` calls `f(v)`; if `x is
  of the form `VError(e), `bitap()` calls `g(e)`. In either case,
  `bitap()` returns `x`.

  ### Example
  ```re
  let printCube = (x) => {Js.log(x * x * x)};
  let printLabel = (x) => {Js.log("Error: " ++ x)};
  bitap(printCube, printLabel, VOk(12)) == VOk(12); // prints 1728
  bitap(printCube, printLabel, VError("bad")) == VError("bad"); // prints "Error: bad"
  ```
*/
let bitap: 'a 'e. ('a => unit, 'e => unit, t('a, 'e)) => t('a, 'e) =
  (f, g, fa) =>
    fa
    |> bimap(
         a => {
           f(a);
           a;
         },
         e => {
           g(e);
           e;
         },
       );

/**
  `apply(valFcn, x, appErrFcn)` provides a way of creating a
  chain of validation functions, accumulating errors along the way.

  If `valFcn` is of the form `VOk(f)`, function `f` is applied to
  `x`. If `x` is `VOk(v)`, the result is `VOk(f(v))`. If `x` is
  `VError(err)`, the error is passed onwards.

  If `valFcn` is itself of the form `VError(err)` and `x` is
  `VOk(v)`, `VError(err)` is passed on.

  Finally, if both `valFcn` and `x` are `VError(e1)` and `VError(e2)`,
  the result is `VError(appendErrFcn(e1, e2))`.

  Using `apply()` properly is somewhat complex. See the example
  in the `__tests__/Relude_Validation_test.re` file for more details.

*/
let applyWithAppendErrors:
  'a 'b 'e.
  (('e, 'e) => 'e, t('a => 'b, 'e), t('a, 'e)) => t('b, 'e)
 =
  (appendErrors, ff, fv) =>
    switch (ff, fv) {
    | (VOk(f), VOk(a)) => VOk(f(a))
    | (VOk(_), VError(e)) => VError(e)
    | (VError(e), VOk(_)) => VError(e)
    | (VError(e1), VError(e2)) => VError(appendErrors(e1, e2))
    };

/**
  `pure(val)` wraps its argument in a `VOk()`.

  ### Example
  ```re
  pure(3) == VOk(3);
  ```
*/
let pure: 'a 'e. 'a => t('a, 'e) = a => VOk(a);

/**
  `flatMapV(x, f)` returns `f(v)` when `x` is of the form `VOk(v)`,
  and returns `x` unchanged when it is of the form `VError(v)`.

  Note: `Validation` is not a traditional monad in that it's purpose is to collect errors
  during applicative validation.  Using `flatMap` will cause all previous errors to be
  discarded.

  ### Example
  ```re
  let mustBeEven = (x) => {
    (x mod 2 == 0) ? VOk(x) : VError("not even")
  };
  flatMapV(VOk(12), mustBeEven) == VOk(12);
  flatMapV(VOk(3), mustBeEven) == VError("not even");
  flatMapV(VError("not an int"), mustBeEven) == VError("not an int");
  ```
 */
let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (fa, f) =>
    switch (fa) {
    | VOk(a) => f(a)
    | VError(e) => VError(e)
    };

/**
  `bind` is the same as `flatMap` with the arguments flipped.
 */
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (f, fa) => bind(fa, f);

/**
  `fromResult` converts a variable of type `Belt.Result.t` to
  `Validation.t`. `fromResult(Ok(x))` returns `VOk(x)`;
  `fromResult(Error(err))` returns `VError(err)`.
*/
let fromResult: Belt.Result.t('a, 'b) => t('a, 'b) =
  result =>
    switch (result) {
    | Ok(a) => VOk(a)
    | Error(e) => VError(e)
    };

/**
  `toResult` converts a variable of type `Validation.t` to
  `Belt.Result.t`. `toResult(VOk(x))` returns `Ok(x)`;
  `toResult(VError(err))` returns `Error(err)`.
*/
let toResult: t('a, 'b) => Belt.Result.t('a, 'b) =
  validation =>
    switch (validation) {
    | VOk(a) => Ok(a)
    | VError(a) => Error(a)
    };

let fromOption: 'a 'e. ('e, option('a)) => t('a, 'e) =
  (defaultError, opt) =>
    switch (opt) {
    | Some(a) => ok(a)
    | None => error(defaultError)
    };

let fromOptionLazy: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getDefaultError, opt) =>
    switch (opt) {
    | Some(a) => ok(a)
    | None => error(getDefaultError())
    };

/**
  `fold(errFcn, okFcn, x)` returns `okFcn(v)` when
  `x` is of the form `VOk(v)`; it returns `errFcn(e)` when
  `x` is of the form `VError(e)`.

  ### Example
  ```re
  let errToInt = (_) => {-1};
  let cube = (x) => {x * x * x};
  fold(errToInt, cube, VOk(12)) == 1728;
  fold(errToInt, cube, VError("bad")) == -1;
  ```
*/
let fold: 'a 'e 'c. ('e => 'c, 'a => 'c, t('a, 'e)) => 'c =
  (ec, ac, r) =>
    switch (r) {
    | VOk(a) => ac(a)
    | VError(e) => ec(e)
    };

/**
  `flip()` Flips the values between the success and error channels.

  ### Example
  ```re
  flip(VOk(12)) == VError(12);
  flip(VError(-1)) == VOk(-1);
  ```
*/
let flip: 'a 'e. t('a, 'e) => t('e, 'a) =
  fa => fa |> fold(e => VOk(e), a => VError(a));

let map2:
  (('x, 'x) => 'x, ('a, 'b) => 'c, t('a, 'x), t('b, 'x)) => t('c, 'x) =
  (appendErrors, f, fa, fb) =>
    applyWithAppendErrors(appendErrors, map(f, fa), fb);

let map3:
  (('x, 'x) => 'x, ('a, 'b, 'c) => 'd, t('a, 'x), t('b, 'x), t('c, 'x)) =>
  t('d, 'x) =
  (appendErrors, f, fa, fb, fc) =>
    applyWithAppendErrors(appendErrors, map2(appendErrors, f, fa, fb), fc);

let map4:
  (
    ('x, 'x) => 'x,
    ('a, 'b, 'c, 'd) => 'e,
    t('a, 'x),
    t('b, 'x),
    t('c, 'x),
    t('d, 'x)
  ) =>
  t('e, 'x) =
  (appendErrors, f, fa, fb, fc, fd) =>
    applyWithAppendErrors(
      appendErrors,
      map3(appendErrors, f, fa, fb, fc),
      fd,
    );

let map5:
  (
    ('x, 'x) => 'x,
    ('a, 'b, 'c, 'd, 'e) => 'f,
    t('a, 'x),
    t('b, 'x),
    t('c, 'x),
    t('d, 'x),
    t('e, 'x)
  ) =>
  t('f, 'x) =
  (appendErrors, f, fa, fb, fc, fd, fe) =>
    applyWithAppendErrors(
      appendErrors,
      map4(appendErrors, f, fa, fb, fc, fd),
      fe,
    );

module WithErrors =
       (
         Errors: BsAbstract.Interface.SEMIGROUP_ANY,
         Error: BsAbstract.Interface.TYPE,
       ) => {
  module Functor:
    BsAbstract.Interface.FUNCTOR with type t('a) = t('a, Errors.t(Error.t)) = {
    type nonrec t('a) = t('a, Errors.t(Error.t));
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Apply:
    BsAbstract.Interface.APPLY with type t('a) = t('a, Errors.t(Error.t)) = {
    include Functor;
    let apply = (ff, fa) => applyWithAppendErrors(Errors.append, ff, fa);
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with
      type t('a) = t('a, Errors.t(Error.t)) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Monad:
    BsAbstract.Interface.MONAD with type t('a) = t('a, Errors.t(Error.t)) = {
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