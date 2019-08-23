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

  test("encode opt Some", () =>
    expect(JE.opt(JE.string, Some("hi"))) |> toEqual(JE.string("hi"))
  );

  test("encode opt None", () =>
    expect(JE.opt(JE.string, None)) |> toEqual(JE.null)
  );

  test("validateOptional null value is None", () =>
    expect(JD.opt(~errorAsNone=false, JD.string, JE.null))
    |> toEqual(Validation.VOk(None))
  );

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
      <$> (JD.stringFor("a", json) >>= (a => VOk(a ++ a)))  // flatMap a value
      <*> (JD.intFor("b", json) <#> (a => a * 2))  // map a value
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