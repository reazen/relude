open BsBastet.Interface;

/**
Similar to result, but has an Applicative instance that collects the errors
using a semigroup, rather than fail-fast semantics.
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

{[
  error("Not even") == VError("Not even");
]}
*/
let error: 'a 'e. 'e => t('a, 'e) = e => VError(e);

/**
Puts an error into a NonEmptyList on the error side of the validation
 */
let errorNel: 'a 'e. 'e => t('a, Relude_NonEmpty.List.t('e)) =
  e => VError(Relude_NonEmpty.List.pure(e));

/**
Puts an error into a NonEmptyArray on the error side of the validation
*/
let errorNea: 'a 'e. 'e => t('a, Relude_NonEmpty.Array.t('e)) =
  e => VError(Relude_NonEmpty.Array.pure(e));

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

{[
  map((x) => {sqrt(float_of_int(x))}, VOk(4)) == VOk(2.0);
  map((x) => {sqrt(float_of_int(x))}, VError("bad")) == VError("bad");
]}
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  f =>
    fun
    | VOk(a) => VOk(f(a))
    | VError(_) as e => e;

/**
In `tap(f, x)`, function `f()` returns `unit`. Thus, `f()`
is used only for its side effects. If `x` is of the
form `VOk(v)`, `tap()` calls `f(v)`. The `tap()` function returns
the argument `x`.

{[
  tap(x => Js.log(x), VOk(4)) == VOk(4); // prints 4
  tap(x => Js.log(x), VError("bad")) == VError("bad"); // prints nothing
]}
*/
let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  f =>
    fun
    | VError(_) as e => e
    | VOk(a) as ok => {
        f(a);
        ok;
      };

/**
`mapError(f, x)` returns `VOk(v)` if `x` is of the form `VOk(v)`.
It returns `VError(f(e))` if `x` is of the form `VError(e)`.

{[
  mapError(x => "Error: " ++ x, VOk(4)) == VOk(4);
  mapError(x => "Error: " ++ x, VError("bad")) == VError("Error: bad");
]}
*/
let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  f =>
    fun
    | VOk(_) as ok => ok
    | VError(e) => VError(f(e));

/**
`mapErrorsAsNea()` applies a function to each error in a `NonEmpty.Array` of errors in the
error channel of the `Validation`.
*/
let mapErrorsNea:
  'a 'e1 'e2.
  ('e1 => 'e2, t('a, Relude_NonEmpty.Array.t('e1))) =>
  t('a, Relude_NonEmpty.Array.t('e2))
 =
  f =>
    fun
    | VOk(_) as ok => ok
    | VError(nea) => VError(nea |> Relude_NonEmpty.Array.map(f));

/**
`mapErrorsAsNel()` applies a function to each error in a `NonEmpty.List` of errors in the error
channel of the `Validation`.
*/
let mapErrorsNel:
  'a 'e1 'e2.
  ('e1 => 'e2, t('a, Relude_NonEmpty.List.t('e1))) =>
  t('a, Relude_NonEmpty.List.t('e2))
 =
  f =>
    fun
    | VOk(_) as ok => ok
    | VError(nea) => VError(nea |> Relude_NonEmpty.List.map(f));

/**
In `tapError(f, x)`, function `f()` returns `unit`. Thus, `f()`
is used only for its side effects. If `x` is of the
form `VError(v)`, `tap()` calls `f(v)`. The `tap()` function returns
the argument `x`.

{[
  tapError(x => Js.log(x), VOk(4)) == VOk(4); // prints nothing
  tapError(x => Js.log(x), VError("bad")) == VError("bad"); // prints "bad"
]}
*/
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  f =>
    fun
    | VOk(_) as ok => ok
    | VError(e) as err => {
        f(e);
        err;
      };

/**
`bimap(f, g, x)` returns `VOk(f(v))` if `x` is of the form `VOk(v)`;
it returns `VError(g(e))` if `x` is of the form `VError(e)`.

{[
  let cube = (x) => {x * x * x};
  let label = (x) => {"Error: " ++ x};
  bimap(cube, label, VOk(12)) == VOk(1728);
  bimap(cube, label, VError("bad")) == VError("Error: bad");
]}
*/
let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (f, g) =>
    fun
    | VOk(a) => VOk(f(a))
    | VError(e) => VError(g(e));

/**
`bitap(f, g, x)` the functions `f()` and `g()` both return `unit`, so they are
used only for their side effects. If `x` is of the form `VOk(v)`, `bitap()`
calls `f(v)`; if `x is of the form `VError(e), `bitap()` calls `g(e)`. In either
case, `bitap()` returns `x`.

{[
  let printCube = x => Js.log(x * x * x);
  let printLabel = x => Js.log("Error: " ++ x);
  bitap(printCube, printLabel, VOk(12)) == VOk(12); // prints 1728
  bitap(printCube, printLabel, VError("bad")) == VError("bad"); // prints "Error: bad"
]}
*/
let bitap: 'a 'e. ('a => unit, 'e => unit, t('a, 'e)) => t('a, 'e) =
  (f, g) =>
    fun
    | VOk(a) as ok => {
        f(a);
        ok;
      }
    | VError(x) as err => {
        g(x);
        err;
      };

/**
`apply(valFcn, x, appErrFcn)` provides a way of creating a chain of validation
functions, accumulating errors along the way.

If `valFcn` is of the form `VOk(f)`, function `f` is applied to `x`. If `x` is
`VOk(v)`, the result is `VOk(f(v))`. If `x` is `VError(err)`, the error is
passed onwards.

If `valFcn` is itself of the form `VError(err)` and `x` is `VOk(v)`,
`VError(err)` is passed on.

Finally, if both `valFcn` and `x` are `VError(e1)` and `VError(e2)`, the result
is `VError(appendErrFcn(e1, e2))`.

Using `apply()` properly is somewhat complex. See the example in the
`__tests__/Relude_Validation_test.re` file for more details.
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

let alignWithAppendErrors:
  'a 'b 'e.
  (('e, 'e) => 'e, t('a, 'e), t('b, 'e)) =>
  t(Relude_Ior_Type.t('a, 'b), 'e)
 =
  (appendErrors, fa, fb) =>
    switch (fa, fb) {
    | (VOk(a), VOk(b)) => VOk(Both(a, b))
    | (VOk(a), VError(_)) => VOk(This(a))
    | (VError(_), VOk(b)) => VOk(That(b))
    | (VError(e1), VError(e2)) => VError(appendErrors(e1, e2))
    };

let alignWithWithAppendErrors:
  'a 'b 'c 'e.
  (('e, 'e) => 'e, Relude_Ior_Type.t('a, 'b) => 'c, t('a, 'e), t('b, 'e)) =>
  t('c, 'e)
 =
  (appendErrors, f, fa, fb) =>
    alignWithAppendErrors(appendErrors, fa, fb) |> map(f);

/**
`pure(val)` wraps its argument in a `VOk()`.

{[
  pure(3) == VOk(3);
]}
*/
let pure: 'a 'e. 'a => t('a, 'e) = a => VOk(a);

/**
`flatMapV(x, f)` returns `f(v)` when `x` is of the form `VOk(v)`, and returns
`x` unchanged when it is of the form `VError(v)`.

Note: `Validation` is not a traditional monad in that it's purpose is to collect
errors during applicative validation. Using `flatMap` will cause all previous
errors to be discarded.

{[
  let mustBeEven = x =>
    (x mod 2 == 0) ? VOk(x) : VError("not even");

  flatMapV(VOk(12), mustBeEven) == VOk(12);
  flatMapV(VOk(3), mustBeEven) == VError("not even");
  flatMapV(VError("not an int"), mustBeEven) == VError("not an int");
]}
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
`fromResult` converts a value of type `result` to a `Validation.t`, returning
`VOk(x)` if the result was `Ok(x)` and `VError(x)` if the result was `Error(x)`.
 */
let fromResult: result('a, 'b) => t('a, 'b) =
  fun
  | Ok(a) => VOk(a)
  | Error(e) => VError(e);

/**
`toResult` converts a value of type `Validation.t` to `result`.
*/
let toResult: t('a, 'b) => result('a, 'b) =
  fun
  | VOk(a) => Ok(a)
  | VError(a) => Error(a);

/**
`fromOption` converts an `option` into a `Validation.t`, returning `VOk(x)`
if the option was `Some(x)` and `VError` (constructed with the provided
error) if the option was `None`.
*/
let fromOption: 'a 'e. ('e, option('a)) => t('a, 'e) =
  defaultError =>
    fun
    | Some(a) => ok(a)
    | None => error(defaultError);

/**
`fromOptionLazy` converts an `option` into a `Validation.t`, similar to
`fromOption`, except that provided error is constructed lazily by calling a
function. This is useful if the error is expensive to construct, especially
since it may not be needed.
*/
let fromOptionLazy: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  getDefaultError =>
    fun
    | Some(a) => ok(a)
    | None => error(getDefaultError());

/**
`fold(errFn, okFn, x)` returns `okFn(v)` when the provided Validation (`x`)
is `VOk(v)`; it returns `errFn(e)` when the Validation is `VError(e)`. This
is effectively a function that allows you to "handle" both possible states of
the Validation.

{[
  let positiveInt = str =>
    Validate.(
      Int.fromtString(str)
      |> fromOption(`InvalidInput)
      |> flatMapV(x => x < 0 ? error(`NotPositive) : pure(x))
    );

  let showError =
    fun
    | `InvalidInput => "The provided string was not an int"
    | `NotPositive => "The provided int was negative";

  let errMsg = x => "Something went wrong: " ++ showError(x);
  let succMsg = x => "Found valid positive int: " ++ Int.toString(x);

  // "Something went wrong: The provided string was not an int"
  fold(errMsg, succMsg, positiveInt("a"));

  // "Something went wrong: The provided int was negative"
  fold(errMsg, succMsg, positiveInt("-3"));

  // "Found valid positive int: 123"
  fold(errMsg, succMsg, positiveInt("123"));
]}
*/
let fold: 'a 'e 'c. ('e => 'c, 'a => 'c, t('a, 'e)) => 'c =
  (ec, ac) =>
    fun
    | VOk(a) => ac(a)
    | VError(e) => ec(e);

/**
`flip` Flips the values between the success and error channels.

{[
  flip(VOk(12)) == VError(12);
  flip(VError(-1)) == VOk(-1);
]}
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

module WithErrors = (Errors: SEMIGROUP_ANY, Error: TYPE) => {
  type nonrec t('a) = t('a, Errors.t(Error.t));

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = (ff, fa) => applyWithAppendErrors(Errors.append, ff, fa);
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative: APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Semialign: Relude_Interface.SEMIALIGN with type t('a) = t('a) = {
    include Functor;
    let align = (fa, fb) => alignWithAppendErrors(Errors.append, fa, fb);
    let alignWith = (f, fa, fb) =>
      alignWithWithAppendErrors(Errors.append, f, fa, fb);
  };
  include Relude_Extensions_Semialign.SemialignExtensions(Semialign);

  module Monad: MONAD with type t('a) = t('a) = {
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
