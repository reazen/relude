open BsBastet.Interface;
open Relude_Function.Infix;

type t('a, 'e) = result('a, 'e) = | Ok('a) | Error('e);

/**
[Result.ok] constructs an [Ok] result from the provided value.
*/
let ok: 'a 'e. 'a => t('a, 'e) = a => Ok(a);

/**
[Result.error] constructs an [Error] result from the provided value.

{[
  Result.error("Not even") == Error("Not even");
]}
*/
let error: 'a 'e. 'e => t('a, 'e) = e => Error(e);

/**
[Result.unit] is an [Ok] value that holds [()].

{[
  Result.unit == Ok();
]}
*/
let unit: 'e. t(unit, 'e) = Ok();

/**
[Result.getOk] converts a [result] to an [option], returning [Some] when the
result was [Ok] and [None] if the result was an [Error].

{[
  Result.getOk(Ok(1066)) == Some(1066);
  Result.getOk(Error("bad value")) == None;
]}
*/
let getOk: 'a 'e. t('a, 'e) => option('a) =
  fun
  | Ok(a) => Some(a)
  | Error(_) => None;

/**
[Result.toOption] is an alias for {!val:getOk}.
*/
let toOption: 'a 'e. t('a, 'e) => option('a) = getOk;

/**
[Result.getError] converts a [result] to an [option], turning a result [Error]
into [Some] (constructed with the error payload), or returning [None] if the
provided result was [Ok].

{[
  Result.getError(Ok(1066)) == None;
  Result.getError(Error("bad value")) == Some("bad value");
]}
*/
let getError: 'a 'e. t('a, 'e) => option('e) =
  fun
  | Ok(_) => None
  | Error(e) => Some(e);

/**
[Result.isOK] returns [true] when the provided result holds an [Ok] value and
returns [false] otherwise.
*/
let isOk: 'a 'e. t('a, 'e) => bool =
  fun
  | Ok(_) => true
  | Error(_) => false;

/**
[Result.isError] returns [true] when the provided result holds an [Error] value
and returns [false] otherwise.
*/
let isError: 'a 'e. t('a, 'e) => bool =
  fun
  | Ok(_) => false
  | Error(_) => true;

/**
[Result.fold] "handles" both possible cases in a result. The first function maps
the inner error to a new type, and the second function maps the ok value to that
same type. This effectively lets you get {e out} of the [result] context.

A good example of this is a React reducer, where some operation could either
fail or succeed, but either way, you want to convert that into an [action] to
update your application state.

{[
  // imagine this tries to load data from some synchronous store like
  // localstorage, but it could fail to find or decode the data
  let nextAction =
    loadTasks()
    |> Result.fold(
      _err => SetDataFailure("Data could not be loaded"),
      data => UpdateData(data)
    );

  // now, regardless of whether our result was successful, we have an action
  dispatch(nextAction);
]}
*/
let fold: 'a 'e 'c. ('e => 'c, 'a => 'c, t('a, 'e)) => 'c =
  (ec, ac, r) =>
    switch (r) {
    | Ok(a) => ac(a)
    | Error(e) => ec(e)
    };

/**
[Result.getOrElse] attempts to get the [Ok] value out of a [result]. If the
result is an [Error], the provided default value is returned instead.

{[
  // Imagine a function to calculate an average from a array of floats, but it
  // safely prevents division by zero by returning a result
  let safeAvg = (values: array(float)) =>
    switch (Array.length(values)) {
    | 0 => Error("Cannot calculate average")
    | len =>
      let total = Array.Float.sum(values);
      Ok(total /. Int.toFloat(len))
    };

  Result.getOrElse(0.0, safeAvg([|12.3, 9.6, 4.7, 10.8|])) == 9.35;
  Result.getOrElse(0.0, safeAvg([||])) == 0.0;
]}
*/
let getOrElse: 'a 'e. ('a, t('a, 'e)) => 'a =
  (default, fa) =>
    switch (fa) {
    | Ok(a) => a
    | Error(_) => default
    };


/**
[Result.getOrElseBy] attempts to get the [Ok] value out of a [result]. If the
result is an [Error], the provided function is applied to the [Error] and
the result of that is returned.

{[
  // Imagine an authorization endpoint, which calls into a function that 
  // authorizes a user with the following return type;
  type fnError = 
   | ParseError(string) // Could not parse the {email,  password}
   | UserBlocked // Users account has been blocked
   | WrongPassword // Account exists, but password is wrong
   | EmailNotFound // Account doesn't exist

  // It's nice to have such a verbose error state, but not everything should
  // be forwarded to the user. Also, we might want to map these back to a 
  // string depending on the error.
  let mapErrorForOutput = fun
   | ParseError(x) => "We could not parse this request: " ++ x
   | UserBlocked => "This action is not allowed"
   | WrongPassword 
   | EmailNotFound => "Incorrect email or pasword"

   Ok("Login Successfull")
   |> Result.getOrElseBy(fnErrorToHttp) 
    == "Login Successfull"

  Error(ParseError("Invalid Email")) 
   |> Result.getOrElseBy(fnErrorToHttp) 
    == "We could not parse this request: Invalid Email"

  Error(WrongPassword) 
   |> Result.getOrElseBy(fnErrorToHttp) 
    == "Incorrect email or pasword"
]}
*/
let getOrElseBy: 'a 'e. ('e => 'a, t('a, 'e)) => 'a =
  (fn, fa) =>
    switch (fa) {
    | Ok(a) => a
    | Error(e) => fn(e)
    };
/**
[Result.getOrElseLazy] attempts to get the [Ok] value out of a [result]. If the
result is an [Error], a default value is returned by calling the provided
function instead.

This function is similar to {!val:getOrElse}, but in this case the default only
gets constructed if it's actually needed. This can be useful if the fallback
value is expensive to compute.
*/
let getOrElseLazy: 'a 'e. (unit => 'a, t('a, 'e)) => 'a =
  (getDefault, fa) =>
    switch (fa) {
    | Ok(a) => a
    | Error(_) => getDefault()
    };

/**
[Result.getErrorOrElse] attempts to get the [Error] value out of a [result]. If
the result is an [Ok], the provided default value is returned instead. It's 
similar to {!val:getOrElse}, but better suited when conceptually, it's the 
error flow you want to handle.

{[
  // Imagine a part of your application that is only interested in error logging
  // It receives an error and logs it if it is one. If not, it logs 
  // that it's handled.
  type response('a) = Result(String, {id: string, payload: 'a})

  Result.getErrorOrElse("Handled", Err("Not Found")) == "Not Found";
  Result.getErrorOrElse("Handled", Ok({id, payload})) == "Handled";
]}
*/
let getErrorOrElse: 'e 'e. ('e, t('a, 'e)) => 'e =
  (default, fa) =>
    switch (fa) {
    | Ok(_) => default 
    | Error(e) => e
    };

/**
[Result.getErrorOrElseBy] attempts to get the [Error] value out of a [result]. 
If the result is an [Ok], the provided default fn is apply to the error, and 
the result is returned instead. It's similar to {!val:getOrElseBy}, but better 
suited when conceptually, it's the error flow you want to handle.

{[
  // Imagine a part of your application that is only interested in error 
  // logging. It receives a [Result] and logs it the error. If it isn't an 
  // error, it logs that it's handled, but include the request id.

  type response('a) = Result(String, {id: string, payload: 'a})
  let handleResErr = res => "Handled: " ++ id;

  Result.getErrorOrElse(handleResErr, Err("Not Found")) 
    == "Not Found";

  Result.getErrorOrElse(handleResErr, Ok({id: 1, payload: "foo"})) 
    == "Handled: 1";
]}
*/
let getErrorOrElseBy: 'e 'e. ('a => 'e, t('a, 'e)) => 'e =
  (fn, fa) =>
    switch (fa) {
    | Ok(a) => fn(a) 
    | Error(e) => e
    };

/**
[Result.merge] returns the inner value from the [result]. This requires both the
[Ok] and [Error] channels to hold the same type of value.

This is especially useful in cases where you've "handled" the error half using
something like {!val:mapError}, and you now want to get the value out. In many
cases, though, {!val:fold} is probably the simpler alternative.

{[
  let userGreeting =
    decodeUser(data) // imagine this returns a result
    |> Result.map(user => "Hello " ++ user.firstName ++ "!")
    |> Result.mapError(_ => "Hello unknown user!")
    |> Result.merge;

  // if the decode failed...
  userGreeting == "Hello unknown user!";

  // if it succeeded...
  userGreeting == "Hello Alice!"
]}
*/
let merge: 'a. t('a, 'a) => 'a =
  fun
  | Ok(a) => a
  | Error(a) => a;

/**
[Result.flip] swaps the values between the [Ok] and [Error] constructors.

{[
  flip(Ok(3)) == Error(3);
  flip(Error("hello")) == Ok("hello");
]}
*/
let flip: 'a 'e. t('a, 'e) => t('e, 'a) =
  fun
  | Ok(a) => Error(a)
  | Error(e) => Ok(e);

let compose:
  'a 'b 'c 'e.
  (t('b => 'c, 'e), t('a => 'b, 'e)) => t('a => 'c, 'e)
 =
  (resultBToC, resultAToB) =>
    switch (resultAToB, resultBToC) {
    | (Ok(aToB), Ok(bToC)) => Ok(aToB >> bToC)
    | (Error(e), Ok(_)) => Error(e)
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Error(_)) => Error(e)
    };

let andThen:
  'a 'b 'c 'e.
  (t('a => 'b, 'e), t('b => 'c, 'e)) => t('a => 'c, 'e)
 =
  (resultAToB, resultBToC) => compose(resultBToC, resultAToB);

/**
[map(f, x)] returns [Ok(f(x))] if [x] is of the form [OK(v)].
It returns [Error(e)] if [x] is of the form [Error(e)].

{[
  map(x => sqrt(float_of_int(x)), Ok(4)) == Ok(2.0);
  map(x => sqrt(float_of_int(x)), Error("bad")) == Error("bad");
]}

One place you might use [map()] is in validating input to an “ordinary” function.
Consider the (highly artificial) example where you have a function that
will cube a number, but you only want to do so if the number is even.
Here are the functions:

{[
  type intResult = Relude.Result.t(int, string);
  let cube = (x) => {x * x * x};
  let testEven = (n): intResult => {
    (n mod 2 == 0) ? Ok(n) :
      Error(string_of_int(n) ++ " is not even.")
  };
]}

We can now make calls like this:

{[
  map(cube, testEven(12)) == Ok(1728);
  map(cube, testEven(5)) == Error("5 is not even.");
]}

This is something we could have done with a simple [if] statement, but we will
see [map()] become useful when we have several things to validate. (See
[apply()] and [map2()], [map3()], etc.)
*/
let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, fa) =>
    switch (fa) {
    | Ok(a) => Ok(f(a))
    | Error(_) as e => e
    };

/**
[mapOk] is an alias for {!val:map}.
*/
let mapOk = map;

/**
[mapError(f, x)] returns [Ok(v)] if [x] is of the form [OK(v)]. It returns
[Error(f(e))] if [x] is of the form [Error(e)].

{[
  Result.mapError(x => "Err: " ++ x, Ok(4)) == Ok(4);
  Result.mapError(x => "Err: " ++ x, Error("bad")) == Error("Err: bad");
]}
*/
let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, ra) =>
    switch (ra) {
    | Ok(_) as res => res
    | Error(e) => Error(f(e))
    };

/**
[bimap(f, g, x)] returns [Ok(f(v))] if [x] is of the form [Ok(v)];
it returns [Error(g(e))] if [x] is of the form [Error(e)].

{[
  let cube = x => x * x * x;
  let label = x => "Err: " ++ x;

  Result.bimap(cube, label, Ok(12)) == Ok(1728);
  Result.bimap(cube, label, Error("bad")) == Error("Err: bad");
]}
*/
let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (mapA, mapE, result) =>
    switch (result) {
    | Ok(a) => Ok(mapA(a))
    | Error(e1) => Error(mapE(e1))
    };

/**
In [tap(f, x)], function [f()] returns [unit]. Thus, [f()] is used only for its
side effects. If [x] is of the form [Ok(v)], [tap()] calls [f(v)]. The [tap()]
function returns the argument [x].

{[
  Result.tap(x => Js.log(x), Ok(4)) == Ok(4); // prints 4
  Result.tap(x => Js.log(x), Error("bad")) == Error("bad"); // prints nothing
]}
*/
let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, ra) => {
    switch (ra) {
    | Ok(a) => f(a)
    | Error(_) => ()
    };
    ra;
  };

/**
[Result.tapOk] is an alias for {!val:tap}.
*/
let tapOk: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) = tap;

/**
In [tapError(f, x)], function [f()] returns [unit]. Thus, [f()]
is used only for its side effects. If [x] is of the
form [Error(v)], [tap()] calls [f(v)]. The [tap()] function returns
the argument [x].

{[
  tapError(x => Js.log(x), Ok(4)) == Ok(4); // prints nothing
  tapError(x => Js.log(x), Error("bad")) == Error("bad"); // prints "bad"
]}
*/
let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, ra) => {
    switch (ra) {
    | Ok(_) => ()
    | Error(e) => f(e)
    };
    ra;
  };

/**
[apply(fcn, x)] provides a way of creating a chain of validation functions.

If [fcn] is of the form [Ok(f)], function [f] is applied to [x]. If [x] is
[Ok(v)], the result is [Ok(f(v))]. If [x] is [Error(err)], the error is passed
onwards.

If [fcn] is itself of the form [Error(err)] and [x] is [Ok(v)], [Error(err)] is
passed on.

Finally, if both [fcn] and [x] are [Error(e1)] and [Error(e2)], the result is
[Error(e2)].

Using [apply()] properly is somewhat complex. See the example in the
Validation tests for more details. (It uses [VOk] and [VError], but the logic is
identical.)
*/
let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (rf, ra) =>
    switch (rf, ra) {
    | (Ok(f), Ok(a)) => Ok(f(a))
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Ok(_)) => Error(e)
    | (Error(_), Error(e)) => Error(e)
    };

/**
[map2(f, x, y)] has as its first argument a function that takes two values. If
both of [x] and [y] are of the form [Ok(xv)] and [Ok(yv)], [map2()] returns
[Ok(f(xv, yv))]. Otherwise, it returns the last value of type [Error(e)] that it
encounters.

Here is another artificial example that concatenates a string and integer, but
only if the string is non-empty and the number is odd:

{[
  let combine = (str, n) => str ++ " " ++ string_of_int(n);

  type strResult = Relude.Result.t(string, string);
  let testStr = (s): strResult => (s == "") ? Error("empty string"): Ok(s);
  let testOdd = (n): intResult => (n mod 2 == 0) ? Error("not odd") : Ok(n);

  map2(combine, testStr("cloud"), testOdd(9)) == Ok("cloud 9");
  map2(combine, testStr("cloud"), testOdd(10)) == Error("not odd");
  map2(combine, testStr(""), testOdd(9)) == Error("empty string");
  map2(combine, testStr(""), testOdd(10)) == Error("not odd");
]}
*/
let map2: 'a 'b 'c 'x. (('a, 'b) => 'c, t('a, 'x), t('b, 'x)) => t('c, 'x) =
  (f, fa, fb) => apply(map(f, fa), fb);

/**
[map3(f, x, y, z)] has as its first argument a function that takes three values.
If all of [x], [y], and [z] are of the form [Ok(xv)], [Ok(yv)], and [Ok(zv)],
[map3()] returns [Ok(f(xv, yv, zv))]. Otherwise, it returns the last value of
type [Error(e)] that it encounters.

The following example builds on the example from [map2()] and does not show all
the possible combinations.

{[
  let combine = (str, n1, n2) => str ++ " " ++ string_of_int(n1 * n2);
  let testLimit = (n): intResult => {(n < 100) ? Ok(n) : Error("too big")};

  map3(combine, testStr("cloud"), testOdd(3), testLimit(3)) == Ok("cloud 9");
  map3(combine, testStr("cloud"), testOdd(2), testLimit(3)) == Error("not odd");
  map3(combine, testStr(""), testOdd(3), testLimit(3)) == Error("empty string");
  map3(combine, testStr(""), testOdd(10), testLimit(100)) == Error("too big");
]}
*/
let map3:
  'a 'b 'c 'd 'x.
  (('a, 'b, 'c) => 'd, t('a, 'x), t('b, 'x), t('c, 'x)) => t('d, 'x)
 =
  (f, fa, fb, fc) => apply(map2(f, fa, fb), fc);

/**
[map4(f, x, y, z, w)] has as its first argument a function that takes four
values. If all of [x], [y], [z], and [w] are of the form [Ok(xv)], [Ok(yv)],
[Ok(zv)], and [Ok(wv)], [map4()] returns [Ok(f(xv, yv, zv, wv))]. Otherwise,
it returns the last value of type [Error(e)] that it encounters.

This example uses validation functions defined in the example from [map3()].

{[
  let combine = (s1, n2, n3, n4) => {s1 ++ " " ++ string_of_int(n2 + n3 + n4)};
  let testPositive = (n): intResult => {(n > 0) ? Ok(n) : Error("not positive")};

  map4(combine, testStr("car"), testOdd(49), testPositive(2), testLimit(3)) == Ok("car 54");
  map4(combine, testStr("car"), testOdd(50), testPositive(2), testLimit(200)) == Error("too big");
  map4(combine, testStr(""), testOdd(49), testPositive(-5), testLimit(0)) == Error("not positive");
  map4(combine, testStr(""), testOdd(48), testPositive(-9), testLimit(200))
    == Error("too big"); // all failures
]}
*/
let map4:
  'a 'b 'c 'd 'e 'x.
  (('a, 'b, 'c, 'd) => 'e, t('a, 'x), t('b, 'x), t('c, 'x), t('d, 'x)) =>
  t('e, 'x)
 =
  (f, fa, fb, fc, fd) => apply(map3(f, fa, fb, fc), fd);

/**
[map5(f, x, y, z, w, q)] has as its first argument a function that takes five
values. If all of [x], [y], [z], [w], and [q] are of the form [Ok(xv)],
[Ok(yv)], [Ok(zv)], [Ok(wv)], and [Ok(qv)], [map5()] returns
[Ok(f(xv, yv, zv, wv, qv))]. Otherwise, it returns the last value of type
[Error(e)] that it encounters.

The following examples do not show all the possible combinations.

{[
  let combine = (s1, n2, n3, n4, n5) => {s1 ++ " " ++ string_of_int(n2 + n3 + n4 + n5)};
  let testNegative = (n): intResult => {(n < 0) ? Ok(n) : Error("not negative")};

  map5(combine, testStr("square"), testOdd(5), testPositive(2),
    testLimit(3), testNegative(-9)) == Ok("square 1");
  map5(combine, testStr("square"), testOdd(2), testPositive(5), testLimit(200),
    testNegative(-3)) == Error("too big");
  map5(combine, testStr(""), testOdd(3), testPositive(5), testLimit(7),
    testNegative(-9)) == Error("empty string");
  map5(combine, testStr(""), testOdd(2), testPositive(-2), testLimit(200),
    testNegative(42)) == Error("not negative"); // all failures
]}
*/
let map5:
  'a 'b 'c 'd 'e 'f 'x.
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    t('a, 'x),
    t('b, 'x),
    t('c, 'x),
    t('d, 'x),
    t('e, 'x)
  ) =>
  t('f, 'x)
 =
  (f, fa, fb, fc, fd, fe) => apply(map4(f, fa, fb, fc, fd), fe);

/**
[Result.pure] wraps its argument in the [Ok] constructor.

{[
  Result.pure(3) == Ok(3);
]}
*/
let pure: 'a 'e. 'a => t('a, 'e) = a => Ok(a);

/**
In [bind(r, f)], [f()] is a function that takes a non-[Result] argument and
returns a [Result] value. If [r] is of the form [Ok(val)], then [bind()]
returns [f(val)]. Otherwise, it returns [r], which will be an [Error(err)].

Note: [bind()] is the same as [flatMap()], except with the arguments in reverse
order.

{[
  let safeSqrt = (x): Relude.Result.t(float, string) => {
    (x >= 0.0) ? Ok(sqrt(x)) : Error("cannot be negative")
  };
  bind(Ok(4.0), safeSqrt) == Ok(2.0);
  bind(Error("invalid float"), safeSqrt) == Error("invalid float");
]}

*/
let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (fa, f) =>
    switch (fa) {
    | Ok(a) => f(a)
    | Error(_) as fa => fa
    };

/**
In [flatMap(f, r)], [f()] is a function that takes a non-[Result] argument and
returns a [Result] value. If [r] is of the form [Ok(val)], then [flatMap()]
returns [f(val)]. Otherwise, it returns [r], which will be an [Error(err)].

Note: [flatMap()] is the same as [bind()], except with the arguments in reverse
order.

{[
  let safeSqrt = x =>
    (x >= 0.0) ? Ok(sqrt(x)) : Error("cannot be negative");

  flatMap(safeSqrt, Ok(4.0)) == Ok(2.0);
  flatMap(safeSqrt, Error("invalid float")) == Error("invalid float");
]}
*/
let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (f, fa) => bind(fa, f);

/**
[Result.flatten] turns a nested result (an outer result that carries a separate
result in its [Ok] channel) into one less layer of result.

Note that this only works if the inner and outer result agree about the type of
the [Error].
*/
let flatten: 'a 'e. t(t('a, 'e), 'e) => t('a, 'e) =
  mma => bind(mma, a => a);

/**
[alt(r1, r2)] takes two [Result] arguments. If both are [Ok(..)], the first one
is returned. If only one is [Ok(..)], it is returned. If both are [Error(..)],
the last one is returned.

{[
  alt(Ok(2), Ok(3)) == Ok(2);
  alt(Error("bad"), Ok(3)) == Ok(3);
  alt(Ok(2), Error("worse")) == Ok(2);
  alt(Error("bad"), Error("worse")) == Error("worse");
]}
*/
let alt: 'a 'e. (t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (fa1, fa2) =>
    switch (fa1) {
    | Ok(_) => fa1
    | Error(_) => fa2
    };

/**
Converts two results into a result where either side or both sides have
succeded, or a result that fails if both sides failed.
*/
let align:
  'a 'b 'e.
  (t('a, 'e), t('b, 'e)) => t(Relude_Ior_Type.t('a, 'b), 'e)
 =
  (fa, fb) =>
    switch (fa, fb) {
    | (Ok(a), Ok(b)) => Ok(Both(a, b))
    | (Ok(a), Error(_)) => Ok(This(a))
    | (Error(_), Ok(b)) => Ok(That(b))
    | (Error(e), Error(_)) => Error(e) // Not sure if this should have append semantics, or fail fast
    };

/**
Similar to map2, but captures successful values from either or both sides
*/
let alignWith:
  'a 'b 'c 'e.
  (Relude_Ior_Type.t('a, 'b) => 'c, t('a, 'e), t('b, 'e)) => t('c, 'e)
 =
  (f, fa, fb) => {
    align(fa, fb) |> map(f);
  };

/**
[catchError(f, r)] returns [f(e)] when [r] is of the form
[Error(e)]; otherwise it returns [r] (an [Ok] value) unchanged.

{[
  let labelMessage = s => "Attn: " ++ s;

  catchError(labelMessage, Ok(2)) == Ok(2);
  catchError(labelMessage, Error("not even")) == Error("Attn: not even");
]}
*/
let catchError: 'a 'e1 'e2. ('e1 => t('a, 'e2), t('a, 'e1)) => t('a, 'e2) =
  (f, fa) =>
    switch (fa) {
    | Ok(_) as res => res
    | Error(e) => f(e)
    };

/**
[handleError(f, r)] converts errors into successful values, and returns a
Result where the error channel is voided, to indicate that the error has
been handled.
*/
let handleError: 'a 'e. ('e => 'a, t('a, 'e)) => t('a, Relude_Void.t) =
  (eToA, fa) => {
    switch (fa) {
    | Ok(_) as res => res
    | Error(e) => Ok(eToA(e))
    };
  };

/**
Maps the success channel and handles an error on the error channel to end up
with a Result of a new type with a voided error channel.
*/
let mapHandleError:
  'a 'e 'b.
  ('a => 'b, 'e => 'b, t('a, 'e)) => t('b, Relude_Void.t)
 =
  (aToB, eToB, ioAE) => ioAE |> map(aToB) |> handleError(eToB);

/**
[Result.recover] ensures that the returned result is [Ok] by returning the
provided result if it's already [Ok], or by falling back to the default value
(which will be wrapped in the [Ok] constructor) if the provided result is an
[Error].

Note that the type returned by this function is still a [result]. In most cases,
you'll probably want to get out of the result context by using {!val:getOrElse},
or you'll want to indicate to the compiler that the error channel is definitely
empty by using {!val:handleError}.

{[
  // imagine a function to calculate an average from a array of floats, but it
  // safely prevents division by zero by returning a result
  let safeAvg = (values: array(float)) =>
    switch (Array.length(values)) {
    | 0 => Error("Cannot calculate average")
    | len =>
      let total = Array.Float.sum(values);
      Ok(total /. Int.toFloat(len))
    };

  Result.recover(0.0, safeAvg([|12.3, 9.6, 4.7, 10.8|])) == Ok(9.35);
  Result.recover(0.0, safeAvg([||])) == Ok(0.0);
]}
*/
let recover: 'a 'e. ('a, t('a, 'e)) => t('a, 'e) =
  (a, fa) => fa |> catchError(_ => Ok(a));

/**
[fromOption(defaultError, opt)] converts a value of the form [Some(v)] to
[Ok(v)], and converts [None] to [Error(defaultError)].

{[
  Result.fromOption("bad value", Some(3)) == Ok(3);
  Result.fromOption("bad value", None) == Error("bad value");
]}
*/
let fromOption: 'a 'e. ('e, option('a)) => t('a, 'e) =
  (defaultError, opt) =>
    switch (opt) {
    | Some(a) => Ok(a)
    | None => Error(defaultError)
    };

/**
[Result.fromOptionLazy] turns an option into a result by converting a [Some]
value from the option to an [Ok] result, or by calling the provided function
to get an error value, which will be wrapped in the [Error] constructor.

Note that unlike {!val:fromOption}, this function won't construct the error
value unless it's needed, which is more efficient if the error is expensive to
construct.

{[
  let getError = () => "bad value";
  Result.fromOptionLazy(getError, Some(3)) == Ok(3);
  Result.fromOptionLazy(getError, None) == Error("bad value");
]}
*/
let fromOptionLazy: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, opt) =>
    switch (opt) {
    | Some(a) => Ok(a)
    | None => Error(getError())
    };

/**
[eqBy(errorEq, okEq, a, b)] compares [a] and  [b] for equality as follows:

If both are of the form [Ok(..)] and [Ok(..)], [eqBy] calls [okEq()] with their
values and returns a boolean depending on whether they are equal or not.

If both are of the form [Error(..)], [eqBy] calls [errorEq()] with their values
and returns a boolean depending on whether they are equal or not.

In all other cases, [eqBy()] returns [false].

{[
  let clockEqual = (c1, c2) => c1 mod 12 == c2 mod 12;
  let strEqual = (c1, c2) => c1 == c2;

  eqBy(strEqual, clockEqual, Ok(14), Ok(2)) == true;
  eqBy(strEqual, clockEqual, Ok(14), Ok(3)) == false;
  eqBy(strEqual, clockEqual, Error("not an integer"), Error("not an integer")) == true;
  eqBy(strEqual, clockEqual, Error("not an integer"), Error("not positive")) == false;
  eqBy(strEqual, clockEqual, Ok(14), Error("not positive")) == false;
  eqBy(strEqual, clockEqual, Error("not an integer"), Ok(2)) == false;
]}
*/
let eqBy:
  'a 'e.
  (('e, 'e) => bool, ('a, 'a) => bool, t('a, 'e), t('a, 'e)) => bool
 =
  (errorEq, okEq, a, b) =>
    switch (a, b) {
    | (Ok(innerA), Ok(innerB)) => okEq(innerA, innerB)
    | (Error(innerA), Error(innerB)) => errorEq(innerA, innerB)
    | (Ok(_), Error(_))
    | (Error(_), Ok(_)) => false
    };

/**
[Result.tries] calls the provided unsafe function (which may throw an
exception), and returns its value safely wrapped in a [result]. If the provided
function succeeds, its return value is wrapped in the [Ok] constructor. If the
function throws, the exception is caught and wrapped in the [Error] constructor.

{[
  tries(() => int_of_string("37")) == Ok(37);
  tries(() => int_of_string("four")); // returns an exn
]}
*/
let tries: 'a. (unit => 'a) => t('a, exn) =
  fn =>
    try(Ok(fn())) {
    | exn => Error(exn)
    };

/**
[Result.triesAsString] handles potentially-unsafe functions similar to
{!val:tries}, but additionally converts the exception into a [string] using
JavaScript's magic ability to construct a [string] from anything.

{[
  Result.triesAsString(() => int_of_string("37")) == Ok(37);
  Result.triesAsString(() => int_of_string("four")) ==
    Error("Failure,-2,int_of_string");
]}
*/
let triesAsString: 'a. (unit => 'a) => t('a, string) =
  fn => tries(fn) |> mapError(Js.String.make);

/**
[toValidation(result)] converts [Ok(val)] to [VOk(val)] and [Error(err)] to
[VError(err)].
*/
let toValidation = Relude_Validation.fromResult;

/**
[fromValidation(vResult)] converts [VOk(val)] to [Ok(val)] and [VError(err)] to
[Error(err)].
*/
let fromValidation = Relude_Validation.toResult;

/**
[toValidationNel(vResult)] converts [Ok(val)] to [VOk(val)] and [Error(err)] to
[VError([err])], where the list is of type [Relude.NonEmpty.List].

You use this function when you have a [Result] type that you wish to use with
[Validation] in order to accumulate a list of errors.

{[
  toValidationNel(Ok(1066)) == Relude.Validation.VOk(1066);
  toValidationNel(Error("not odd")) == Relude.Validation.VError(
    Relude.NonEmpty.List.pure("not odd"));
]}
*/
let toValidationNel:
  t('a, 'e) => Relude_Validation.t('a, Relude_NonEmpty.List.t('e)) =
  fun
  | Ok(value) => Relude_Validation.VOk(value)
  | Error(error) =>
    Relude_Validation.VError(Relude_NonEmpty.List.pure(error));

/**
[toValidationNea(vResult)] converts [Ok(val)] to [VOk(val)] and [Error(err)] to
[VError([|err|])], where the array is of type [Relude.NonEmpty.Array].

You use this function when you have a [Result] type that you wish to use with
[Validation] in order to accumulate an array of errors.

{[
  toValidationNea(Ok(1066)) == Relude.Validation.VOk(1066);
  toValidationNea(Error("not odd")) == Relude.Validation.VError(
    Relude.NonEmpty.Array.pure("not odd"));
  toValidationNea(Error("not odd"));
]}
*/
let toValidationNea:
  t('a, 'e) => Relude_Validation.t('a, Relude_NonEmpty.Array.t('e)) =
  fun
  | Ok(value) => Relude_Validation.VOk(value)
  | Error(error) =>
    Relude_Validation.VError(Relude_NonEmpty.Array.pure(error));

module Bifunctor: BIFUNCTOR with type t('a, 'e) = t('a, 'e) = {
  type nonrec t('a, 'e) = t('a, 'e);
  let bimap = bimap;
};
let bimap = Bifunctor.bimap;
include Relude_Extensions_Bifunctor.BifunctorExtensions(Bifunctor);

module Bifoldable: BIFOLDABLE with type t('a, 'e) = t('a, 'e) = {
  include BsBastet.Result.Bifoldable;
};
let bifoldLeft = Bifoldable.bifold_left;
let bifoldRight = Bifoldable.bifold_right;
include Relude_Extensions_Bifoldable.BifoldableExtensions(Bifoldable);

/**
Because Result is a bi-functor, we need to capture the error type in order
to implement many of the single-type-parameter typeclasses. Doing it like
this allows us to unlock a bunch of stuff at once using a single module
functor.
*/
module WithError = (E: TYPE) => {
  type nonrec t('a) = t('a, E.t);

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Alt: ALT with type t('a) = t('a) = {
    include Functor;
    let alt = alt;
  };
  let alt = Alt.alt;
  include Relude_Extensions_Alt.AltExtensions(Alt);

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
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
    let align = align;
    let alignWith = alignWith;
  };
  include Relude_Extensions_Semialign.SemialignExtensions(Semialign);

  module Monad: MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module MonadThrow:
    Relude_Interface.MONAD_THROW with type t('a) = t('a) and type e = E.t = {
    include Monad;
    type e = E.t;
    let throwError = error;
  };
  let throwError = MonadThrow.throwError;
  include Relude_Extensions_MonadThrow.MonadThrowExtensions(MonadThrow);

  module MonadError:
    Relude_Interface.MONAD_ERROR with type t('a) = t('a) and type e = E.t = {
    include MonadThrow;
    let catchError = catchError;
  };
  let catchError = MonadError.catchError;
  include Relude_Extensions_MonadError.MonadErrorExtensions(MonadError);

  module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = t('a => 'b) = {
    type nonrec t('a, 'b) = t('a => 'b);
    let compose = compose;
  };
  let compose = compose;
  include Relude_Extensions_Semigroupoid.SemigroupoidExtensions(Semigroupoid);

  module Foldable: FOLDABLE with type t('a) = t('a) = {
    include BsBastet.Result.Foldable(E);
  };
  let foldLeft = Foldable.fold_left;
  let foldRight = Foldable.fold_right;
  include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

  module WithApplicative = (A: APPLICATIVE) => {
    module Traversable:
      TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a) = {
      include BsBastet.Result.Traversable(E, A);
    };
    let traverse = Traversable.traverse;
    let sequence = Traversable.sequence;
    include Relude_Extensions_Traversable.TraversableExtensions(Traversable);

    module Bitraversable:
      BITRAVERSABLE with
        type t('a, 'b) = result('a, 'b) and
        type applicative_t('a) = A.t('a) = {
      include BsBastet.Result.Bitraversable(A);
    };
    let bitraverse = Bitraversable.bitraverse;
    let bisequence = Bitraversable.bisequence;
    include Relude_Extensions_Bitraversable.BitraversableExtensions(
              Bitraversable,
            );
  };

  module Eq = BsBastet.Result.Eq;

  module Ord = BsBastet.Result.Ord;

  module Show = BsBastet.Result.Show;

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Bifunctor.BifunctorInfix(Bifunctor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
    include Relude_Extensions_Semigroupoid.SemigroupoidInfix(Semigroupoid);
  };
};
