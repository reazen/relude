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
      JE.array([|JE.string("hi"), JE.string("bye")|])
      |> JD.array((_, json) => JD.string(json));

    let expected = Validation.ok([|"hi", "bye"|]);

    expect(actual) |> toEqual(expected);
  });

  test("decode array using applicative validation (error)", () => {
    let actual: Validation.t(array(string), NonEmptyArray.t(string)) =
      JE.array([|
        JE.null,
        JE.bool(true),
        JE.string("hi"),
        JE.int(42),
        JE.float(42.1),
        JE.array([|JE.null, JE.null|]),
        JE.list([JE.null, JE.null]),
        JE.dict(Js.Dict.fromList([("a", JE.null)])),
      |])
      |> JD.array((_, json) => JD.string(json));

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
      JE.listOfTuples([
        ("a", JE.string("hi")),
        ("b", JE.int(42)),
        ("c", JE.bool(true)),
        ("d", JE.listBy(JE.string, ["one", "two"])),
      ]);

    let actual: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> (JD.stringFor("a", json) >>= (a => VOk(a ++ a))) // flatMap a value
      <*> (JD.intFor("b", json) <#> (a => a * 2)) // map a value
      <*> JD.boolFor("c", json)
      <*> JD.arrayFor("d", (_index, json) => JD.string(json), json);

    let expectedData: MyType.t = {
      a: "hihi",
      b: 84,
      c: true,
      d: [|"one", "two"|],
    };

    let expected = Validation.ok(expectedData);

    expect(actual) |> toEqual(expected);
  });

  test("decode object using applicative validation (error)", () => {
    let json: Js.Json.t =
      JE.listOfTuples([
        ("a", JE.float(42.1)),
        ("b", JE.int(42)),
        ("c", JE.string("invalid")),
        ("d", JE.listBy(JE.string, ["one", "two"])),
      ]);

    let result: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> JD.stringFor("a", json)
      <*> JD.intFor("b", json)
      <*> JD.boolFor("c", json)
      <*> JD.arrayFor("d", (_index, json) => JD.string(json), json);

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
      JE.array([|
        JE.string("hi"),
        JE.int(42),
        JE.bool(true),
        JE.list([JE.string("one"), JE.string("two")]),
      |]);

    let actual =
      MyType.make
      <$> (JD.stringAt(0, json) >>= (a => VOk(a ++ a)))  // bind (flatMap) a value here just for fun
      <*> (JD.intAt(1, json) <#> (a => a * 2))  // map a value here - <#> is flipMap - we need to flip it b/c the Validation comes first here
      <*> JD.boolAt(2, json)
      <*> JD.arrayAt(3, (_index, json) => JD.string(json), json);

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