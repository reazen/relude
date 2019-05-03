open Jest;
open Expect;

module Json = Relude_Js_Json;
module List = Relude_List;
module NonEmptyArray = Relude_NonEmpty.Array;
module Option = Relude_Option;
module Result = Relude_Result;
module Validation = Relude_Validation;

module MyType = {
  type t = {
    a: string,
    b: int,
    c: bool,
  };

  let make = (a, b, c) => {a, b, c};
};

describe("Json", () => {
  test("isNull", () =>
    expect(Json.null |> Json.isNull) |> toEqual(true)
  );

  test("decode array using applicative validation", () => {
    open Json.DSL;

    let decodeString = (_index, json) =>
      json |> Json.toString |> Result.fromOption(_ => "not a string");

    let result =
      arr([|
        null,
        bool(true),
        str("hi"),
        int(42),
        num(42.1),
        arr([|null, null|]),
        list([null, null]),
        dict(Js.Dict.fromList([("a", null)])),
      |])
      |> Json.decodeArray(decodeString);

    expect(result)
    |> toEqual(
         Validation.VError(
           NonEmptyArray.make(
             (0, "not a string"),
             [|
               (1, "not a string"),
               (3, "not a string"),
               (4, "not a string"),
               (5, "not a string"),
               (6, "not a string"),
               (7, "not a string"),
             |],
           ),
         ),
       );
  });

  test("decode object using applicative validation", () => {
    open Json.DSL;

    let dict =
      Js.Dict.fromList([
        ("a", Json.fromString("hi")),
        ("b", Json.fromInt(42)),
        ("c", Json.fromBool(true)),
      ]);

    let json = Json.fromDict(dict);

    let result =
      MyType.make
      <$> (
        Json.field("a", json)
        |> Option.flatMap(Json.toString)
        |> Result.fromOption(_ => "not a string")
        |> Result.toValidationNea
      )
      <*> (
        Json.field("b", json)
        |> Option.flatMap(Json.toInt)
        |> Result.fromOption(_ => "not an int")
        |> Result.toValidationNea
      )
      <*> (
        Json.field("c", json)
        |> Option.flatMap(Json.toBool)
        |> Result.fromOption(_ => "not an bool")
        |> Result.toValidationNea
      );

    let expected: MyType.t = {a: "hi", b: 42, c: true};
    expect(result) |> toEqual(Validation.VOk(expected));
  });
});