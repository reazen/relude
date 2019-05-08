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
    d: array(string),
  };

  let make = (a, b, c, d) => {a, b, c, d};
};

open Json.DSL;

describe("Json", () => {
  test("isNull", () =>
    expect(Json.null |> Json.isNull) |> toEqual(true)
  );

  test("decode array using applicative validation (success)", () => {
    let actual: Validation.t(array(string), NonEmptyArray.t(string)) =
      E.array([|E.string("hi"), E.string("bye")|])
      |> Json.validateArrayOfJson((_, json) => D.string(json));

    let expected = Validation.ok([|"hi", "bye"|]);

    expect(actual) |> toEqual(expected);
  });

  test("decode array using applicative validation (error)", () => {
    let actual: Validation.t(array(string), NonEmptyArray.t(string)) =
      E.array([|
        E.null,
        E.bool(true),
        E.string("hi"),
        E.int(42),
        E.float(42.1),
        E.array([|E.null, E.null|]),
        E.list([E.null, E.null]),
        E.dict(Js.Dict.fromList([("a", E.null)])),
      |])
      |> Json.validateArrayOfJson((_, json) => D.string(json));

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "0: JSON value is not a string: null",
          [|
            "1: JSON value is not a string: true",
            "3: JSON value is not a string: 42",
            "4: JSON value is not a string: 42.1",
            "5: JSON value is not a string: [\n  null,\n  null\n]",
            "6: JSON value is not a string: [\n  null,\n  null\n]",
            "7: JSON value is not a string: {\n  \"a\": null\n}",
          |],
        ),
      );

    expect(actual) |> toEqual(expected);
  });

  test("decode object using applicative validation (success)", () => {
    let json: Js.Json.t =
      Json.fromListOfKeyValueTuples([
        ("a", Json.fromString("hi")),
        ("b", Json.fromInt(42)),
        ("c", Json.fromBool(true)),
        ("d", Json.fromListOfJsonBy(Json.fromString, ["one", "two"])),
      ]);

    let actual: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> D.stringFor("a", json)
      <*> D.intFor("b", json)
      <*> D.boolFor("c", json)
      <*> D.arrayFor("d", (_index, json) => D.string(json), json);

    let expectedData: MyType.t = {
      a: "hi",
      b: 42,
      c: true,
      d: [|"one", "two"|],
    };

    let expected = Validation.ok(expectedData);

    expect(actual) |> toEqual(expected);
  });

  test("decode object using applicative validation (error)", () => {
    let json: Js.Json.t =
      Json.fromListOfKeyValueTuples([
        ("a", Json.fromFloat(42.1)),
        ("b", Json.fromInt(42)),
        ("c", Json.fromString("invalid")),
        ("d", Json.fromListOfJsonBy(Json.fromString, ["one", "two"])),
      ]);

    let result: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> D.stringFor("a", json)
      <*> D.intFor("b", json)
      <*> D.boolFor("c", json)
      <*> D.arrayFor("d", (_index, json) => D.string(json), json);

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "a: JSON value is not a string: 42.1",
          [|"c: JSON value is not a bool: \"invalid\""|],
        ),
      );

    expect(result) |> toEqual(expected);
  });

  test("decode an array into an object (success)", () => {
    let json =
      Json.fromArrayOfJson([|
        E.string("hi"),
        E.int(42),
        E.bool(true),
        E.list([E.string("one"), E.string("two")]),
      |]);

    let actual =
      MyType.make
      <$> (D.stringAt(0, json) >>= (a => VOk(a ++ a)))  // bind (flatMap) a value here just for fun
      <*> (D.intAt(1, json) <#> (a => a * 2))  // map a value here - <#> is flipMap - we need to flip it b/c the Validation comes first here
      <*> D.boolAt(2, json)
      <*> D.arrayAt(3, (_index, json) => D.string(json), json);

    let expectedData: MyType.t = {
      a: "hihi",
      b: 84, // see map operation above
      c: true,
      d: [|"one", "two"|],
    };

    let expected = Validation.ok(expectedData);

    expect(actual) |> toEqual(expected);
  });
});