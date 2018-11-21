type t('a, 'e) = Belt.Result.t('a, 'e);

let pure: 'a 'e. 'a => Belt.Result.t('a, 'e) = a => Ok(a);

let ok: 'a 'e. 'a => Belt.Result.t('a, 'e) = pure;

let error: 'a 'e. 'e => Belt.Result.t('a, 'e) = e => Error(e);

let unit: 'e. Belt.Result.t(unit, 'e) = ok();

let map: 'a 'b 'e. ('a => 'b, Belt.Result.t('a, 'e)) => Belt.Result.t('b, 'e) =
  (f, ra) => Belt.Result.map(ra, f);

let mapError:
  'a 'e1 'e2.
  ('e1 => 'e2, Belt.Result.t('a, 'e1)) => Belt.Result.t('a, 'e2)
 =
  (f, ra) =>
    switch (ra) {
    | Ok(_) as res => res
    | Error(e) => Error(f(e))
    };

let bimap:
  'a 'b 'e1 'e2.
  ('a => 'b, 'e1 => 'e2, Belt.Result.t('a, 'e1)) => Belt.Result.t('b, 'e2)
 =
  (mapA, mapE, result) =>
    switch (result) {
    | Ok(a) => Ok(mapA(a))
    | Error(e1) => Error(mapE(e1))
    };

let tap: 'a 'e. ('a => unit, Belt.Result.t('a, 'e)) => unit =
  (f, ra) =>
    switch (ra) {
    | Ok(a) => f(a)
    | Error(_) => ()
    };

let apply:
  'a 'b 'e.
  (Belt.Result.t('a => 'b, 'e), Belt.Result.t('a, 'e)) =>
  Belt.Result.t('b, 'e)
 =
  (rf, ra) =>
    switch (rf, ra) {
    | (Ok(f), Ok(a)) => Ok(f(a))
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Ok(_)) => Error(e)
    | (Error(_), Error(e)) => Error(e)
    };

let map2:
  'a 'b 'c 'x.
  (('a, 'b) => 'c, Belt.Result.t('a, 'x), Belt.Result.t('b, 'x)) =>
  Belt.Result.t('c, 'x)
 =
  (f, fa, fb) => apply(map(f, fa), fb);

let map3:
  'a 'b 'c 'd 'x.
  (
    ('a, 'b, 'c) => 'd,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x)
  ) =>
  Belt.Result.t('d, 'x)
 =
  (f, fa, fb, fc) => apply(map2(f, fa, fb), fc);

let map4:
  'a 'b 'c 'd 'e 'x.
  (
    ('a, 'b, 'c, 'd) => 'e,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x),
    Belt.Result.t('d, 'x)
  ) =>
  Belt.Result.t('e, 'x)
 =
  (f, fa, fb, fc, fd) => apply(map3(f, fa, fb, fc), fd);

let map5:
  'a 'b 'c 'd 'e 'f 'x.
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x),
    Belt.Result.t('d, 'x),
    Belt.Result.t('e, 'x)
  ) =>
  Belt.Result.t('f, 'x)
 =
  (f, fa, fb, fc, fd, fe) => apply(map4(f, fa, fb, fc, fd), fe);

let bind:
  'a 'b 'e.
  (Belt.Result.t('a, 'e), 'a => Belt.Result.t('b, 'e)) =>
  Belt.Result.t('b, 'e)
 =
  (ra, aToRB) =>
    switch (ra) {
    | Ok(a) => aToRB(a)
    | Error(_) as err => err
    };

let flatMap:
  'a 'b 'e.
  ('a => Belt.Result.t('b, 'e), Belt.Result.t('a, 'e)) =>
  Belt.Result.t('b, 'e)
 =
  (f, fa) => bind(fa, f);

let fold: 'a 'e 'c. ('a => 'c, 'e => 'c, Belt.Result.t('a, 'e)) => 'c = BsAbstract.Result.result;

let alt:
  'a 'e.
  (Belt.Result.t('a, 'e), Belt.Result.t('a, 'e)) => Belt.Result.t('a, 'e)
 =
  (fa1, fa2) => fold(ok, _ => fa2, fa1);

let catchError:
  'a 'e.
  ('e => Belt.Result.t('a, 'e), Belt.Result.t('a, 'e)) =>
  Belt.Result.t('a, 'e)
 =
  (f, fa) =>
    switch (fa) {
    | Ok(_) as ok => ok
    | Error(e) => f(e)
    };

let recover: 'a 'e. ('a, Belt.Result.t('a, 'e)) => Belt.Result.t('a, 'e) =
  (a, fa) => fa |> catchError(_ => Ok(a));

let fromOption: 'a 'e. (unit => 'e, option('a)) => Belt.Result.t('a, 'e) =
  (getError, opt) =>
    switch (opt) {
    | Some(a) => Ok(a)
    | None => Error(getError())
    };

let toOption: 'a 'e. Belt.Result.t('a, 'e) => option('a) =
  fa =>
    switch (fa) {
    | Ok(a) => Some(a)
    | Error(_) => None
    };

let toValidation = Relude_Validation.fromResult;

let fromValidation = Relude_Validation.toResult;

let toValidationNel:
  Belt.Result.t('a, 'e) =>
  Relude_Validation.t('a, Relude_NonEmpty.List.t('e)) =
  fun
  | Belt.Result.Ok(value) => Relude_Validation.VOk(value)
  | Belt.Result.Error(error) =>
    Relude_Validation.VError(Relude_NonEmpty.List.pure(error));

module Functor = BsAbstract.Result.Functor;

module Alt = BsAbstract.Result.Alt;

module Apply = BsAbstract.Result.Apply;

module Applicative = BsAbstract.Result.Applicative;

module Monad = BsAbstract.Result.Monad;

module MonadThrow: Relude_MonadError.MONAD_THROW = (E: BsAbstract.Interface.TYPE) => {
  include Monad(E);
  let throwError = error;
}

module MonadError: Relude_MonadError.MONAD_ERROR = (E: BsAbstract.Interface.TYPE) => {
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
