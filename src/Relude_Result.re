module NonEmpty = Relude_NonEmpty;
module Validation = Relude_Validation;

let pure: 'a => Belt.Result.t('a, 'e) = a => Ok(a);

let ok: 'a => Belt.Result.t('a, 'e) = pure;

let error: 'e => Belt.Result.t('a, 'e) = e => Error(e);

let map: ('a => 'b, Belt.Result.t('a, 'e)) => Belt.Result.t('b, 'e) =
  (f, ra) => Belt.Result.map(ra, f);

let mapError: ('e1 => 'e2, Belt.Result.t('a, 'e1)) => Belt.Result.t('a, 'e2) =
  (f, ra) =>
    switch (ra) {
    | Ok(_) as res => res
    | Error(e) => Error(f(e))
    };

let tap: ('a => unit, Belt.Result.t('a, 'e)) => unit =
  (f, ra) =>
    switch (ra) {
    | Ok(a) => f(a)
    | Error(_) => ()
    };

let apply:
  (Belt.Result.t('a => 'b, 'e), Belt.Result.t('a, 'e)) =>
  Belt.Result.t('b, 'e) =
  (rf, ra) =>
    switch (rf, ra) {
    | (Ok(f), Ok(a)) => Ok(f(a))
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Ok(_)) => Error(e)
    | (Error(_), Error(e)) => Error(e)
    };

let map2:
  (('a, 'b) => 'c, Belt.Result.t('a, 'x), Belt.Result.t('b, 'x)) =>
  Belt.Result.t('c, 'x) =
  (f, fa, fb) => apply(map(f, fa), fb);

let map3:
  (
    ('a, 'b, 'c) => 'd,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x)
  ) =>
  Belt.Result.t('d, 'x) =
  (f, fa, fb, fc) => apply(map2(f, fa, fb), fc);

let map4:
  (
    ('a, 'b, 'c, 'd) => 'e,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x),
    Belt.Result.t('d, 'x)
  ) =>
  Belt.Result.t('e, 'x) =
  (f, fa, fb, fc, fd) => apply(map3(f, fa, fb, fc), fd);

let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    Belt.Result.t('a, 'x),
    Belt.Result.t('b, 'x),
    Belt.Result.t('c, 'x),
    Belt.Result.t('d, 'x),
    Belt.Result.t('e, 'x)
  ) =>
  Belt.Result.t('f, 'x) =
  (f, fa, fb, fc, fd, fe) => apply(map4(f, fa, fb, fc, fd), fe);

let flatMap:
  (Belt.Result.t('a, 'e), 'a => Belt.Result.t('b, 'e)) =>
  Belt.Result.t('b, 'e) =
  (ra, aToRB) =>
    switch (ra) {
    | Ok(a) => aToRB(a)
    | Error(_) as err => err
    };

let fold: ('a => 'c, 'e => 'c, Belt.Result.t('a, 'e)) => 'c = BsAbstract.Result.result;

let alt:
  (Belt.Result.t('a, 'e), Belt.Result.t('a, 'e)) => Belt.Result.t('a, 'e) =
  (fa1, fa2) => fold(ok, _ => fa2, fa1);

let recover: ('a, Belt.Result.t('a, 'e)) => Belt.Result.t('a, 'e) =
  (a, fa) =>
    switch (fa) {
    | Ok(_) as ok => ok
    | Error(_) => Ok(a)
    };

let fromOption: ('e, option('a)) => Belt.Result.t('a, 'e) =
  (err, opt) =>
    switch (opt) {
    | Some(a) => Ok(a)
    | None => Error(err)
    };

let toOption: Belt.Result.t('a, 'e) => option('a) =
  fa =>
    switch (fa) {
    | Ok(a) => Some(a)
    | Error(_) => None
    };

let toValidation = Validation.fromResult;

let fromValidation = Validation.toResult;

let toValidationNel:
  Belt.Result.t('a, 'e) => Validation.t('a, NonEmpty.List.t('e)) =
  fun
  | Belt.Result.Ok(value) => Validation.VOk(value)
  | Belt.Result.Error(error) =>
    Validation.VError(NonEmpty.List.pure(error));
