open Jest;
open Expect;

module Cont = Relude.Cont;

let readIntSuccess =
    (_fileName: string, onSuccess: int => unit, _onFailure: string => unit)
    : unit =>
  onSuccess(42);

let readIntFailure =
    (_fileName: string, _onSuccess: int => unit, onFailure: string => unit)
    : unit =>
  onFailure("readIntFailure");

let writeIntSuccess =
    (
      _fileName: string,
      _i: int,
      onSuccess: unit => unit,
      _onFailure: string => unit,
    )
    : unit =>
  onSuccess();

let writeIntFailure =
    (
      _fileName: string,
      _i: int,
      _onSuccess: int => unit,
      onFailure: string => unit,
    )
    : unit =>
  onFailure("writeIntFailure");

let readIntSuccessCont: string => Cont.t(unit, Belt.Result.t(int, string)) =
  fileName =>
    Cont.ContT(
      onDone =>
        readIntSuccess(
          fileName,
          value => onDone(Belt.Result.Ok(value)),
          error => onDone(Belt.Result.Error(error)),
        ),
    );

let readIntFailureCont = fileName =>
  Cont.ContT(
    onDone =>
      readIntFailure(
        fileName,
        value => onDone(Belt.Result.Ok(value)),
        error => onDone(Belt.Result.Error(error)),
      ),
  );

describe("ContT", () => {
  test("a", () =>
    expect(true) |> toEqual(true)
  );

  test("b", () =>
    expect(true) |> toEqual(true)
  );
});