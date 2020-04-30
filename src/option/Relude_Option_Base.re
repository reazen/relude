/**
[Option.some] constructs an [option] from a value.
*/
let some: 'a. 'a => option('a) = a => Some(a);

/**
[Option.none] is the empty value for [option].
*/
let none: 'a. option('a) = None;

/**
[Option.empty] is an alias for [Option.none].
*/
let empty: 'a. option('a) = None;

/**
[Option.isSome] returns [true] if the provided option is not `None`.
*/
let isSome: 'a. option('a) => bool =
  fun
  | Some(_) => true
  | None => false;

/**
[Option.isNone] returns [true] if the provided option does not contain [Some]
value.
*/
let isNone: 'a. option('a) => bool =
  fun
  | Some(_) => false
  | None => true;

/**
[Option.fold] applies the provided function to the inner value when the given
option is [Some], or returns the provided default value if the option is [None].

This is not a lazy function, as the [default] value is always constructed,
whether or not that value is needed. See [foldLazy] for an alternative.

{[
  Option.fold(0.0, x => 1.0 /. x, Some(2.0)) == 0.5;
  Option.fold(0.0, x => 1.0 /. x, None) == 0.0;
]}
*/
let fold: 'a 'b. ('b, 'a => 'b, option('a)) => 'b =
  (default, f) =>
    fun
    | Some(v) => f(v)
    | None => default;

/**
[Option.foldLazy] applies the provided mapping function to the inner value in
the option if the option is [Some], otherwise a default value is computed and
returned by calling the provided fallback function.

This is a {e lazy} function because the default value is not evaluated unless
the option is [None]. This is useful when the fallback value is expensive to
compute.

{[
  Option.foldLazy(() => "empty", Int.toString, Some(2)) == "2";
  Option.foldLazy(() => "empty", Int.toString, None) == "empty";
]}
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


{[
  getOrElse(0, Some(3)) == 3;
  getOrElse(0, None) == 0;
]}
*/
let getOrElse: 'a. ('a, option('a)) => 'a =
  default =>
    fun
    | Some(a) => a
    | None => default;

/**
[Option.getOrElseLazy] returns the inner value when the provided option is
[Some]. If the option is [None], the fallback function is called, and its value
is returned. This fallback takes no parameters and returns a value of the same
type as the inner option.

This is a {e lazy} function, meaning the default value is not computed unless
the option is [None]. This is useful in cases where the default is expensive to
construct.

{[
  Option.getOrElseLazy(() => 0, Some(3)) == 3;
  Option.getOrElseLazy(() => 0, None) == 0;
]}
*/
let getOrElseLazy: 'a. (unit => 'a, option('a)) => 'a =
  getDefault =>
    fun
    | Some(a) => a
    | None => getDefault();

/**
`getOrThrow(opt)` returns the value of the option or throws an exception.

This should only be used if you are absolutely sure there is a value in the option.
*/
let getOrThrow: 'a. option('a) => 'a = Belt.Option.getExn;

/**
Similar to alt, but with the arguments reversed and labelled for use with `|>`
*/
let orElse: 'a. (~fallback: option('a), option('a)) => option('a) =
  (~fallback) =>
    fun
    | Some(_) as fa => fa
    | None => fallback;

/**
Similar to alt, but with the arguments reversed and labelled for use with `|>`.
The fallback value is also lazy for expensive constructions.
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
`tap(f, opt)` applies a side-effect function to the value in a `Some`, and returns
the original option value untouched.
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
`tapSome` is an alias for `tap`
*/
let tapSome: 'a. ('a => unit, option('a)) => option('a) = tap;

/**
`tap(f, opt)` applies a side-effect function if the value of the option is None, and returns
the original option value untouched.
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
`bitap(ifNone, ifSome, opt)` applies a side effect function for each of the cases of the option, and
returns the original option untouched.
 */
let bitap: 'a. (unit => unit, 'a => unit, option('a)) => option('a) =
  (ifNone, ifSome) =>
    fun
    | Some(a) as fa => {
        ifSome(a);
        fa;
      }
    | None => {
        ifNone();
        None;
      };

/**
`filter(f, opt)` works as follows:

- If `opt` is `Some(v)` and `f(v)` is `true`, the result is `Some(v)`.
- If `opt` is `Some(v)` and `f(v)` is `false`, the result is `None`.
- If `opt` is `None`, the result is `None`.


{[
  let isEven = x => x mod 2 == 0;
  filter(isEven, Some(2)) == Some(2);
  filter(isEven, Some(3)) == None;
  filter(isEven, None) == None;
]}
*/
let filter: 'a. ('a => bool, option('a)) => option('a) =
  fn =>
    Relude_Option_Instances.foldLeft(
      (default, v) => fn(v) ? Some(v) : default,
      empty,
    );

/**
Alias of filter
*/
let keep: 'a. ('a => bool, option('a)) => option('a) = filter;

/**
`filterNot` is the inverse of `filter`, meaning `Some` values are preserved
if the provided predicate function returns false.

{[
  let isEven = x => x mod 2 == 0;
  filterNot(isEven, Some(1)) == Some(1);
  filterNot(isEven, Some(2)) == None;
]}
*/
let filterNot: 'a. ('a => bool, option('a)) => option('a) =
  f => filter(a => !f(a));

/**
Alias of filterNot
*/
let reject: 'a. ('a => bool, option('a)) => option('a) = filterNot;
