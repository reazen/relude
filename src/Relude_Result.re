type t('a, 'e) = Belt.Result.t('a, 'e);

let pure: 'a 'e. 'a => t('a, 'e) = a => Ok(a);
let ok: 'a 'e. 'a => t('a, 'e) = pure;
let error: 'a 'e. 'e => t('a, 'e) = e => Error(e);
let unit: 'e. t(unit, 'e) = pure();

let map: 'a 'b 'e. ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (f, ra) => Belt.Result.map(ra, f);

let mapOk = map;

let mapError: 'a 'e1 'e2. ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2) =
  (f, ra) =>
    switch (ra) {
    | Ok(_) as res => res
    | Error(e) => Error(f(e))
    };

let bimap: 'a 'b 'e1 'e2. ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2) =
  (mapA, mapE, result) =>
    switch (result) {
    | Ok(a) => Ok(mapA(a))
    | Error(e1) => Error(mapE(e1))
    };

let tap: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) =
  (f, ra) => {
    switch (ra) {
    | Ok(a) => f(a)
    | Error(_) => ()
    };
    ra;
  };

let tapOk: 'a 'e. ('a => unit, t('a, 'e)) => t('a, 'e) = tap;

let tapError: 'a 'e. ('e => unit, t('a, 'e)) => t('a, 'e) =
  (f, ra) => {
    switch (ra) {
    | Ok(_) => ()
    | Error(e) => f(e)
    };
    ra;
  };

let apply: 'a 'b 'e. (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e) =
  (rf, ra) =>
    switch (rf, ra) {
    | (Ok(f), Ok(a)) => Ok(f(a))
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Ok(_)) => Error(e)
    | (Error(_), Error(e)) => Error(e)
    };

let map2: 'a 'b 'c 'x. (('a, 'b) => 'c, t('a, 'x), t('b, 'x)) => t('c, 'x) =
  (f, fa, fb) => apply(map(f, fa), fb);

let map3:
  'a 'b 'c 'd 'x.
  (('a, 'b, 'c) => 'd, t('a, 'x), t('b, 'x), t('c, 'x)) => t('d, 'x)
 =
  (f, fa, fb, fc) => apply(map2(f, fa, fb), fc);

let map4:
  'a 'b 'c 'd 'e 'x.
  (('a, 'b, 'c, 'd) => 'e, t('a, 'x), t('b, 'x), t('c, 'x), t('d, 'x)) =>
  t('e, 'x)
 =
  (f, fa, fb, fc, fd) => apply(map3(f, fa, fb, fc), fd);

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

let bind: 'a 'b 'e. (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e) =
  (ra, aToRB) =>
    switch (ra) {
    | Ok(a) => aToRB(a)
    | Error(_) as err => err
    };

let flatMap: 'a 'b 'e. ('a => t('b, 'e), t('a, 'e)) => t('b, 'e) =
  (f, fa) => bind(fa, f);

let fold: 'a 'e 'c. ('a => 'c, 'e => 'c, t('a, 'e)) => 'c = BsAbstract.Result.result;

let alt: 'a 'e. (t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (fa1, fa2) => fold(pure, _ => fa2, fa1);

let catchError: 'a 'e. ('e => t('a, 'e), t('a, 'e)) => t('a, 'e) =
  (f, fa) =>
    switch (fa) {
    | Ok(_) as res => res
    | Error(e) => f(e)
    };

let recover: 'a 'e. ('a, t('a, 'e)) => t('a, 'e) =
  (a, fa) => fa |> catchError(_ => Ok(a));

let getOrElse: 'a 'e. ('a, t('a, 'e)) => 'a =
  (a, fa) => fold(v => v, _ => a, fa);

let fromOption: 'a 'e. (unit => 'e, option('a)) => t('a, 'e) =
  (getError, opt) =>
    switch (opt) {
    | Some(a) => Ok(a)
    | None => Error(getError())
    };

let getOk: 'a 'e. t('a, 'e) => option('a) =
  fa =>
    switch (fa) {
    | Ok(a) => Some(a)
    | Error(_) => None
    };

let getError: 'a 'e. t('a, 'e) => option('e) =
  fa =>
    switch (fa) {
    | Ok(_) => None
    | Error(e) => Some(e)
    };

let tries: 'a. (unit => 'a) => t('a, exn) =
  fn =>
    try (Ok(fn())) {
    | exn => Error(exn)
    };

let triesAsString: 'a. (unit => 'a) => t('a, string) =
  fn => tries(fn) |> mapError(Js.String.make);

let toValidation = Relude_Validation.fromResult;
let fromValidation = Relude_Validation.toResult;

let toValidationNel:
  t('a, 'e) => Relude_Validation.t('a, Relude_NonEmpty.List.t('e)) =
  fun
  | Ok(value) => Relude_Validation.VOk(value)
  | Error(error) =>
    Relude_Validation.VError(Relude_NonEmpty.List.pure(error));

module Functor = BsAbstract.Result.Functor;

module Alt = BsAbstract.Result.Alt;

module Apply = BsAbstract.Result.Apply;

module Applicative = BsAbstract.Result.Applicative;

module Monad = BsAbstract.Result.Monad;

module MonadThrow: Relude_MonadError.MONAD_THROW =
  (E: BsAbstract.Interface.TYPE) => {
    include Monad(E);
    let throwError = error;
  };

module MonadError: Relude_MonadError.MONAD_ERROR =
  (E: BsAbstract.Interface.TYPE) => {
    include Monad(E);
    let throwError = error;
    let catchError = catchError;
  };

module Foldable = BsAbstract.Result.Foldable;

module Traversable = BsAbstract.Result.Traversable;

module Eq = BsAbstract.Result.Eq;

module Show = BsAbstract.Result.Show;

module Ord = BsAbstract.Result.Ord;

module Infix = BsAbstract.Result.Infix;
