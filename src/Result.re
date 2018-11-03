module BResult = Belt.Result;

let pure: 'a => BResult.t('a, 'e) = a => Ok(a);

let map: ('a => 'b, BResult.t('a, 'e)) => BResult.t('b, 'e) =
  (f, ra) => BResult.map(ra, f);

let mapOk = map;

let mapError: ('e1 => 'e2, BResult.t('a, 'e1)) => BResult.t('a, 'e2) =
  (f, ra) =>
    switch (ra) {
    | Ok(_) as res => res
    | Error(e) => Error(f(e))
    };

let apply: (BResult.t('a => 'b, 'e), BResult.t('a, 'e)) => BResult.t('b, 'e) =
  (rf, ra) =>
    switch (rf, ra) {
    | (Ok(f), Ok(a)) => Ok(f(a))
    | (Ok(_), Error(e)) => Error(e)
    | (Error(e), Ok(_)) => Error(e)
    | (Error(_), Error(e)) => Error(e)
    };

let flatMap: (BResult.t('a, 'e), 'a => BResult.t('b, 'e)) => BResult.t('b, 'e) =
  (ra, aToRB) =>
    switch (ra) {
    | Ok(a) => aToRB(a)
    | Error(_) as err => err
    };

let toValidation = Validation.fromResult;

let fromValidation = Validation.toResult;

let toValidationNel:
  Belt.Result.t('a, 'e) => Validation.t('a, NonEmpty.List.t('e)) =
  fun
  | Belt.Result.Ok(value) => Validation.VOk(value)
  | Belt.Result.Error(error) =>
    Validation.VError(NonEmpty.List.pure(error));
