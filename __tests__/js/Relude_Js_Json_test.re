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
    e: float,
    f: unit,
    g: list(string),
  };

  let make = (a, b, c, d, e, f, g) => {a, b, c, d, e, f, g};
};

open Json.DSL;

afterAll(Bisect.Runtime.write_coverage_data);

describe("Json", () => {
  test("show", () =>
    [("a", JE.bool(true)), ("b", JE.bool(false))]
    |> Js.Dict.fromList
    |> Json.fromDictOfJson
    |> Json.show(~indentSpaces=4)
    |> expect
    |> toEqual({|{
    "a": true,
    "b": false
}|})
  );

  test("isNull", () =>
    expect(JE.null |> Json.isNull) |> toEqual(true)
  );

  test("isBool", () =>
    expect(JE.bool(true) |> Json.isBool) |> toEqual(true)
  );

  test("isString", () =>
    expect(JE.string("hi") |> Json.isString) |> toEqual(true)
  );

  test("isNumber", () =>
    expect(JE.float(1.23) |> Json.isNumber) |> toEqual(true)
  );

  test("isObject", () =>
    expect(
      JE.listOfTuples([("hi", Json.fromFloat(1.23))]) |> Json.isObject,
    )
    |> toEqual(true)
  );

  test("isArray", () =>
    expect(
      JE.list([Json.fromInt(42), Json.fromString("hi")]) |> Json.isArray,
    )
    |> toEqual(true)
  );

  test("arrayOfDict", () =>
    expect(
      [|Js.Dict.fromList([("a", JE.bool(true))])|]
      |> JE.arrayOfDict
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("listOfDict", () =>
    expect(
      [Js.Dict.fromList([("a", JE.bool(true))])]
      |> JE.listOfDict
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toNull Some", () =>
    expect(JE.null |> Json.toNull) |> toEqual(Some())
  );

  test("toNull None", () =>
    expect(JE.bool(true) |> Json.toNull) |> toEqual(None)
  );

  test("toArrayOfJsonOrElse ok", () =>
    expect(
      [|Js.Dict.fromList([("a", JE.bool(true))])|]
      |> JE.arrayOfDict
      |> Json.toArrayOfJsonOrElse([|JE.bool(false)|])
      |> JE.array
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toArrayOfJsonOrElse error", () =>
    expect(
      [%raw {|""|}]
      |> JE.arrayOfDict
      |> Json.toArrayOfJsonOrElse([|JE.bool(false)|])
      |> JE.array
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[false]|})
  );

  test("toArrayOfJsonOrEmpty ok", () =>
    expect(
      [|Js.Dict.fromList([("a", JE.bool(true))])|]
      |> JE.arrayOfDict
      |> Json.toArrayOfJsonOrEmpty
      |> JE.array
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toArrayOfJsonOrEmpty error", () =>
    expect(
      [%raw {|""|}]
      |> JE.arrayOfDict
      |> Json.toArrayOfJsonOrEmpty
      |> JE.array
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[]|})
  );

  test("toListOfJson some", () =>
    expect(
      [Js.Dict.fromList([("a", JE.bool(true))])]
      |> JE.listOfDict
      |> Json.toListOfJson
      |> Option.getOrThrow
      |> JE.list
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toListOfJson none", () =>
    expect([%raw {|""|}] |> Json.toListOfJson) |> toEqual(None)
  );

  test("toListOfJsonOrElse some", () =>
    expect(
      [Js.Dict.fromList([("a", JE.bool(true))])]
      |> JE.listOfDict
      |> Json.toListOfJsonOrElse([JE.bool(false)])
      |> JE.list
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toListOfJsonOrElse none", () =>
    expect(
      [%raw {|""|}]
      |> Json.toListOfJsonOrElse([JE.bool(false)])
      |> JE.list
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual("[false]")
  );

  test("toDictOfJsonOrElse some", () =>
    expect(
      Js.Dict.fromList([("a", JE.bool(true))])
      |> JE.dict
      |> Json.toDictOfJsonOrElse(Js.Dict.empty()),
    )
    |> toEqual(Js.Dict.fromList([("a", JE.bool(true))]))
  );

  test("toDictOfJsonOrElse none", () =>
    expect(
      [%raw {|""|}] |> JE.dict |> Json.toDictOfJsonOrElse(Js.Dict.empty()),
    )
    |> toEqual(Js.Dict.empty())
  );

  test("toDictOfJsonOrEmpty some", () =>
    expect(
      Js.Dict.fromList([("a", JE.bool(true))])
      |> JE.dict
      |> Json.toDictOfJsonOrEmpty,
    )
    |> toEqual(Js.Dict.fromList([("a", JE.bool(true))]))
  );

  test("toDictOfJsonOrEmpty none", () =>
    expect([%raw {|""|}] |> JE.dict |> Json.toDictOfJsonOrEmpty)
    |> toEqual(Js.Dict.empty())
  );

  test("toListOrEmpty ok", () =>
    expect(
      [Js.Dict.fromList([("a", JE.bool(true))])]
      |> JE.listOfDict
      |> Json.toListOrEmpty
      |> JE.list
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[{"a":true}]|})
  );

  test("toListOrEmpty error", () =>
    expect(
      [%raw {|""|}]
      |> JE.listOfDict
      |> Json.toListOrEmpty
      |> JE.list
      |> Json.show(~indentSpaces=0),
    )
    |> toEqual({|[]|})
  );

  test("encode opt Some", () =>
    expect(JE.opt(JE.string, Some("hi"))) |> toEqual(JE.string("hi"))
  );

  test("encode opt None", () =>
    expect(JE.opt(JE.string, None)) |> toEqual(JE.null)
  );

  test("validateOptional null value is None", () => {
    expect(JD.opt(~errorAsNone=false, JD.string, JE.null))
    |> toEqual(Validation.VOk(None))
    |> ignore;

    expect(JD.opt(JD.string, JE.null))
    |> toEqual(Validation.VOk(None))
    |> ignore;

    expect(Json.validateOptional(JD.string, JE.null))
    |> toEqual(Validation.VOk(None));
  });

  test("validateOptional good value validates", () =>
    expect(JD.opt(~errorAsNone=false, JD.string, JE.string("hi")))
    |> toEqual(Validation.VOk(Some("hi")))
  );

  test("validateOptional bad value is an error when errorAsNone=false", () =>
    expect(JD.opt(~errorAsNone=false, JD.string, JE.float(42.2)))
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure("JSON value is not a string: 42.2"),
         ),
       )
  );

  test("validateOptional bad value is None when errorAsNone=true", () =>
    expect(JD.opt(~errorAsNone=true, JD.string, JE.float(42.2)))
    |> toEqual(Validation.VOk(None))
  );

  test("validateOptionalAtIndex good value passes validation", () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        1,
        JD.string,
        JE.list([JE.int(42), JE.string("abc"), JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.VOk(Some("abc")))
  );

  test(
    "validateOptionalAtIndex missing value is an error when missingAsNone=false",
    () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        42,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure(
             "No value was found at index 42 for JSON: [\n  42,\n  null,\n  \"hi\"\n]",
           ),
         ),
       )
  );

  test(
    "validateOptionalAtIndex missing value is a None when missingAsNone=true",
    () =>
    expect(
      JD.optAt(
        ~missingAsNone=true,
        ~nullAsNone=false,
        ~errorAsNone=false,
        42,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.VOk(None))
  );

  test(
    "validateOptionalAtIndex null value is an error when nullAsNone=false", () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        1,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure("1 had a null value in JSON: null"),
         ),
       )
  );

  test("validateOptionalAtIndex null value is a None when nullAsNone=true", () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=true,
        ~errorAsNone=false,
        1,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.VOk(None))
  );

  test(
    "validateOptionalAtIndex bad value is an error when errorAsNone is false",
    () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        1,
        JD.bool,
        JE.list([JE.int(42), JE.string("abc"), JE.string("hi")]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure("JSON value is not a bool: \"abc\""),
         ),
       )
  );

  test("validateOptionalAtIndex bad value is a None when errorAsNone=true", () =>
    expect(
      JD.optAt(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=true,
        1,
        JD.bool,
        JE.list([JE.int(42), JE.string("abc"), JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.pure(None))
  );

  test("validateOptionalAtIndex missing value is a None", () => {
    expect(
      JD.optAt(
        42,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.pure(None))
    |> ignore;

    expect(
      Json.validateOptionalAtIndex(
        42,
        JD.string,
        JE.list([JE.int(42), JE.null, JE.string("hi")]),
      ),
    )
    |> toEqual(Validation.pure(None));
  });

  test("validateOptionalForKey good value passes validation", () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        "b",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.VOk(Some("abc")))
  );

  test(
    "validateOptionalForKey missing value is an error when missingAsNone=false",
    () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        "d",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure(
             "d was not found in JSON: {\n  \"a\": 42,\n  \"b\": \"abc\",\n  \"c\": \"hi\"\n}",
           ),
         ),
       )
  );

  test(
    "validateOptionalForKey missing value is a None when missingAsNone=true",
    () =>
    expect(
      JD.optFor(
        ~missingAsNone=true,
        ~nullAsNone=false,
        ~errorAsNone=false,
        "d",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.VOk(None))
  );

  test(
    "validateOptionalForKey null value is an error when nullAsNone=false", () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        "b",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.null),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure("b contained a null value in JSON: null"),
         ),
       )
  );

  test("validateOptionalForKey null value is a None when nullAsNone=true", () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=true,
        ~errorAsNone=false,
        "b",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.null),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.VOk(None))
  );

  test(
    "validateOptionalForKey bad value is an error when errorAsNone is false",
    () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=false,
        "b",
        JD.bool,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(
         Validation.VError(
           Relude.Nea.pure("JSON value is not a bool: \"abc\""),
         ),
       )
  );

  test("validateOptionalForKey bad value is a None when errorAsNone=true", () =>
    expect(
      JD.optFor(
        ~missingAsNone=false,
        ~nullAsNone=false,
        ~errorAsNone=true,
        "b",
        JD.bool,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("hi")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.pure(None))
  );

  test("validateOptionalForKey missing value is a None", () => {
    expect(
      JD.optFor(
        "d",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.VOk(None))
    |> ignore;

    expect(
      Json.validateOptionalForKey(
        "d",
        JD.string,
        JE.listOfTuples([
          ("a", JE.int(42)),
          ("b", JE.string("abc")),
          ("c", JE.string("hi")),
        ]),
      ),
    )
    |> toEqual(Validation.VOk(None));
  });

  test("nullAt (success)", () => {
    [|JE.string("hi"), JE.null|]
    |> JE.array
    |> JD.nullAt(1)
    |> expect
    |> toEqual(Validation.ok())
  });

  test("nullAt (error)", () => {
    [|JE.string("hi"), JE.string("bye")|]
    |> JE.array
    |> JD.nullAt(1)
    |> expect
    |> toEqual(
         Validation.error(
           NonEmptyArray.make("1: JSON value is not a null: \"bye\"", [||]),
         ),
       )
  });

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
        ("e", JE.float(3.14)),
        ("f", JE.null),
        ("g", JE.listBy(JE.string, ["three", "four"])),
      ]);

    let actual: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> (JD.stringFor("a", json) >>= (a => VOk(a ++ a)))  // flatMap a value
      <*> (JD.intFor("b", json) <#> (a => a * 2))  // map a value
      <*> JD.boolFor("c", json)
      <*> JD.arrayFor("d", (_index, json) => JD.string(json), json)
      <*> JD.floatFor("e", json)
      <*> JD.nullFor("f", json)
      <*> JD.listFor("g", (_index, json) => JD.string(json), json);

    let expectedData: MyType.t = {
      a: "hihi",
      b: 84,
      c: true,
      d: [|"one", "two"|],
      e: 3.14,
      f: (),
      g: ["three", "four"],
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
        ("e", JE.float(3.14)),
        ("f", JE.string("invalid")),
        ("g", JE.null),
      ]);

    let result: Validation.t(MyType.t, NonEmptyArray.t(string)) =
      MyType.make
      <$> JD.stringFor("a", json)
      <*> JD.intFor("b", json)
      <*> JD.boolFor("c", json)
      <*> JD.arrayFor("d", (_index, json) => JD.string(json), json)
      <*> JD.floatFor("e", json)
      <*> JD.nullFor("f", json)
      <*> JD.listFor("g", (_index, json) => JD.string(json), json);

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "a: JSON value is not a string: 42.1",
          [|
            "c: JSON value is not a bool: \"invalid\"",
            "f: JSON value is not a null: \"invalid\"",
            "JSON value is not an array: null",
          |],
        ),
      );

    expect(result) |> toEqual(expected);
  });

  test("decode object using applicative validation (invalid type error)", () => {
    let json: Js.Json.t =
      JE.listOfTuples([
        ("a", JE.null),
        ("b", JE.null),
        ("c", JE.list([JE.int(1)])),
      ]);
    let make = (a, b, c) => (a, b, c);
    let actual =
      make
      <$> JD.intFor("a", json)
      <*> JD.floatFor("b", json)
      <*> JD.listFor("c", (_index, json) => JD.string(json), json);
    let expected =
      Validation.error(
        NonEmptyArray.make(
          "a: JSON value is not an int: null",
          [|
            "b: JSON value is not a float: null",
            "0: JSON value is not a string: 1",
          |],
        ),
      );

    expect(actual) |> toEqual(expected);
  });

  test("decode object using applicative validation (not found error)", () => {
    let json: Js.Json.t = JE.array([||]);
    let make = (a, b, c, d, e, f, g) => (a, b, c, d, e, f, g);

    let actual =
      make
      <$> JD.stringFor("a", json)
      <*> JD.intFor("b", json)
      <*> JD.boolFor("c", json)
      <*> JD.arrayFor("d", (_index, json) => JD.string(json), json)
      <*> JD.floatFor("e", json)
      <*> JD.nullFor("f", json)
      <*> JD.listFor("g", (_index, json) => JD.string(json), json);

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "a was not found in JSON: []",
          [|
            "b was not found in JSON: []",
            "c was not found in JSON: []",
            "d was not found in JSON: []",
            "e was not found in JSON: []",
            "f was not found in JSON: []",
            "g was not found in JSON: []",
          |],
        ),
      );

    expect(actual) |> toEqual(expected);
  });

  test("decode an array into an object (success)", () => {
    let json =
      JE.array([|
        JE.string("hi"),
        JE.int(42),
        JE.bool(true),
        JE.list([JE.string("one"), JE.string("two")]),
        JE.float(3.14),
        JE.null,
        JE.list([JE.string("three"), JE.string("four")]),
      |]);

    let actual =
      MyType.make
      <$> (JD.stringAt(0, json) >>= (a => VOk(a ++ a)))  // bind (flatMap) a value here just for fun
      <*> (JD.intAt(1, json) <#> (a => a * 2))  // map a value here - <#> is flipMap - we need to flip it b/c the Validation comes first here
      <*> JD.boolAt(2, json)
      <*> JD.arrayAt(3, (_index, json) => JD.string(json), json)
      <*> JD.floatAt(4, json)
      <*> JD.nullAt(5, json)
      <*> JD.jsonAt(6, JD.list((_index, json) => JD.string(json)), json);

    let expectedData: MyType.t = {
      a: "hihi",
      b: 84, // see map operation above
      c: true,
      d: [|"one", "two"|],
      e: 3.14,
      f: (),
      g: ["three", "four"],
    };

    let expected = Validation.ok(expectedData);

    expect(actual) |> toEqual(expected);
  });

  test("decode an array into a tuple (not found error)", () => {
    let json: Js.Json.t = JE.array([||]);
    let make:
      (string, int, float, bool, array(string)) =>
      (string, int, float, bool, array(string)) =
      (a, b, c, d, e) => (a, b, c, d, e);

    let actual =
      make
      <$> JD.stringAt(0, json)
      <*> JD.intAt(1, json)
      <*> JD.floatAt(2, json)
      <*> JD.boolAt(3, json)
      <*> JD.arrayAt(4, (_index, json) => JD.string(json), json);

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "0 was not found in JSON: []",
          [|
            "1 was not found in JSON: []",
            "2 was not found in JSON: []",
            "3 was not found in JSON: []",
            "4 was not found in JSON: []",
          |],
        ),
      );

    expect(actual) |> toEqual(expected);
  });

  test("decode an array into a tuple (invalid type error)", () => {
    let json = JE.array([|JE.null, JE.null, JE.null, JE.null, JE.null|]);
    let make = (a, b, c, d, f) => (a, b, c, d, f);

    let actual =
      make
      <$> JD.stringAt(0, json)
      <*> JD.intAt(1, json)
      <*> JD.floatAt(2, json)
      <*> JD.boolAt(3, json)
      <*> JD.arrayAt(4, (_index, json) => JD.string(json), json);

    let expected =
      Validation.error(
        NonEmptyArray.make(
          "0: JSON value is not a string: null",
          [|
            "1: JSON value is not an int: null",
            "2: JSON value is not a float: null",
            "3: JSON value is not a bool: null",
            "JSON value is not an array: null",
          |],
        ),
      );

    expect(actual) |> toEqual(expected);
  });
});