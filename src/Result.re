let toValidation = Validation.fromResult;

let fromValidation = Validation.toResult;

let toValidationNel:
  Belt.Result.t('a, 'e) => Validation.t('a, NonEmpty.List.t('e)) =
  fun
  | Belt.Result.Ok(value) => Validation.VOk(value)
  | Belt.Result.Error(error) => Validation.VError(NonEmpty.List.one(error));
