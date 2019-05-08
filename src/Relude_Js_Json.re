module Array = Relude_Array;
module List = Relude_List;
module NonEmptyList = Relude_NonEmpty.List;
module NonEmptyArray = Relude_NonEmpty.Array;
module Option = Relude_Option;
module Result = Relude_Result;
module Validation = Relude_Validation;
let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);

/**
Type alias for Js.Json.t.
*/
type json = Js.Json.t;

/**
Type alias for Js.Dict.t(Js.Json.t)
*/
type dict = Js.Dict.t(json);

/**
Shows a JSON value with optional space indentation
 */
let show = (~indentSpaces: int=2, json: json): string =>
  Js.Json.stringifyWithSpace(json, indentSpaces);

////////////////////////////////////////////////////////////////////////////////
// Test functions
////////////////////////////////////////////////////////////////////////////////

/**
Checks if the Js.Json.t value is a null
*/
let isNull: json => bool = json => Js.Json.test(json, Js.Json.Null);

/**
Checks if the Js.Json.t value is a bool
*/
let isBool: json => bool = json => Js.Json.test(json, Js.Json.Boolean);

/**
Checks if the Js.Json.t value is a string
*/
let isString: json => bool = json => Js.Json.test(json, Js.Json.String);

/**
Checks if the Js.Json.t value is a number
*/
let isNumber: json => bool = json => Js.Json.test(json, Js.Json.Number);

/**
Checks if the Js.Json.t value is a JSON object
*/
let isObject: json => bool = json => Js.Json.test(json, Js.Json.Object);

/**
Checks if the Js.Json.t value is a JSON array
*/
let isArray: json => bool = json => Js.Json.test(json, Js.Json.Array);

////////////////////////////////////////////////////////////////////////////////
// Constructor functions (encode values as Json)
////////////////////////////////////////////////////////////////////////////////

/**
Creates a Js.Json.t null value
*/
let null: json = Js.Json.null;

/**
Creates a Js.Json.t boolean value from a boolean
*/
let fromBool: bool => json = Js.Json.boolean;

/**
Creates a Js.Json.t string value from a string
*/
let fromString: string => json = Js.Json.string;

/**
Creates a Js.Json.t number value from an int
*/
let fromInt: int => json = float_of_int >> Js.Json.number;

/**
Creates a Js.Json.t number value from a float
*/
let fromFloat: float => json = Js.Json.number;

/**
Creates a Js.Json.t array value from an array of Js.Json.t values
*/
let fromArrayOfJson: array(json) => json = Js.Json.array;

/**
Creates a Js.Json.t array value from an array of values that can be converted to Js.Json.t values with the given function
*/
let fromArrayOfJsonBy: 'a. ('a => json, array('a)) => json =
  (f, items) => Array.map(f, items) |> fromArrayOfJson;

/**
Creates a Js.Json.t array value from an list of Js.Json.t values
*/
let fromListOfJson: list(json) => json = List.toArray >> fromArrayOfJson;

/**
Creates a Js.Json.t array value from an list of values that can be converted to Js.Json.t values with the given function
*/
let fromListOfJsonBy: 'a. ('a => json, list('a)) => json =
  (f, items) => List.map(f, items) |> fromListOfJson;

/**
Creates a Js.Json.t object value from a Js.Dict containing Js.Json.t values.
*/
let fromDictOfJson: dict => json = Js.Json.object_;

/**
Creates a Js.Json.t value from an array of Js.Dict.t(Js.Json.t) values.
*/
let fromArrayOfDictOfJson: array(dict) => json = Js.Json.objectArray;

/**
Creates a Js.Json.t value from an list of Js.Dict.t(Js.Json.t) values.
*/
let fromListOfDictOfJson: list(dict) => json =
  List.toArray >> Js.Json.objectArray;

/**
Creates a Js.Json.t value from an array of key/value (string/Js.Json.t) tuples.
 */
let fromArrayOfKeyValueTuples: array((Js.Dict.key, json)) => json =
  tuples => fromDictOfJson(Js.Dict.fromArray(tuples));

/**
Creates a Js.Json.t value from an list of key/value (string/Js.Json.t) tuples.
 */
let fromListOfKeyValueTuples: list((Js.Dict.key, json)) => json =
  tuples => fromDictOfJson(Js.Dict.fromList(tuples));

////////////////////////////////////////////////////////////////////////////////
// Basic conversions from the top-level Js.Json.t type to the specific Json
// subtypes.  These functions do not unpack nested json objects - i.e. asDict
// will attempt to convert a `Js.Json.t` value to an `option(Js.Dict.t(Js.Json.t))`
// but does not attempt to decode the resulting dictionary.
//
// Note: there are intentially very basic. For more advanced decoding, try a
// library like https://github.com/mlms13/bs-decode
////////////////////////////////////////////////////////////////////////////////

/**
Attempts to decode the given Js.Json.t value as a null.  Returns `Some(())` if it is a `null`, otherwise `None`.
*/
let toNull: json => option(unit) =
  json => json |> Js.Json.decodeNull |> Option.void;

/**
Attempts to decode the given `Js.Json.t` value as a `boolean`
*/
let toBool: json => option(bool) = Js.Json.decodeBoolean;

/**
Attempts to decode the given `Js.Json.t` value as a `string`
*/
let toString: json => option(string) = Js.Json.decodeString;

/**
Attempts to decode the given `Js.Json.t` value as an `int`
*/
let toInt: json => option(int) =
  json => json |> Js.Json.decodeNumber |> Option.map(int_of_float);

/**
Attempts to decode the given `Js.Json.t` value as a `float`
*/
let toFloat: json => option(float) = Js.Json.decodeNumber;

/**
Attempts to decode the given `Js.Json.t` value as an array of `Js.Json.t` values.
*/
let toArrayOfJson: json => option(array(json)) = Js.Json.decodeArray;

/**
Attempts to decode the given `Js.Json.t` value as an array of `Js.Json.t` values, with a fallback.
*/
let toArrayOfJsonOrElse: (array(json), json) => array(json) =
  (default, json) => json |> toArrayOfJson |> Option.getOrElse(default);

/**
Attempts to decode the given `Js.Json.t` value as an array of `Js.Json.t` values, with a fallback of an empty array.
*/
let toArrayOfJsonOrEmpty = toArrayOfJsonOrElse([||]);

/**
Attempts to decode the given `Js.Json.t` value as a list of `Js.Json.t` values.
*/
let toListOfJson: json => option(list(json)) =
  json => json |> toArrayOfJson |> Option.map(Array.toList);

/**
Attempts to decode the given `Js.Json.t` value as an list of `Js.Json.t` values, with a fallback.
*/
let toListOfJsonOrElse = (default, json) =>
  json |> toListOfJson |> Option.getOrElse(default);

/**
Attempts to decode the given `Js.Json.t` value as a list of `Js.Json.t` values, with a fallback of an empty list.
*/
let toListOrEmpty = toListOfJsonOrElse([]);

/**
Attempts to decode the given `Js.Json.t` value as a `Js.Dict.t(Js.Json.t)`
 */
let toDictOfJson: json => option(dict) = Js.Json.decodeObject;

/**
Attempts to decode the given `Js.Json.t` value as a `Js.Dict.t(Js.Json.t)` with a fallback.
 */
let toDictOfJsonOrElse = (default, json) =>
  json |> toDictOfJson |> Option.getOrElse(default);

/**
Attempts to decode the given `Js.Json.t` value as a `Js.Dict.t(Js.Json.t)` with a fallback of an empty `Js.Dict`.
 */
let toDictOfJsonOrEmpty: json => dict = toDictOfJsonOrElse(Js.Dict.empty());

////////////////////////////////////////////////////////////////////////////////
// Basic applicative-validation style decoding of arrays and objects. The
// applicative behavior of the Validation, when used in a `traverse`, causes the
// errors to be collected. However, it is not possible to gather deeply-nested
// contextual error information with this style of decoding.
//
// These are intended to be simple and easy to use - for more advanced decoding,
// see more dedicated libraries like bs-decode.
//
// Note: this is using string as the error type, because allowing an arbitrary
// error makes this more complicated in terms of constructing errors, etc. It is
// possible to use an arbitrary error type, but with applicative style
// validation, the errors tend to be more informational, rather than things you
// might want to act on.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Typeclass setup for validation code
////////////////////////////////////////////////////////////////////////////////

module Error = {
  type t = string;

  module Type: BsAbstract.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};

module Errors = {
  // I'm standardizing on NonEmptyArray as the error collector type... this was an arbitrary decision between List and Array
  type t = NonEmptyArray.t(Error.t);

  let pure = NonEmptyArray.pure;
  let make = NonEmptyArray.make;
  let map = Validation.mapErrorsNea;

  module SemigroupAny = NonEmptyArray.SemigroupAny;
};

module Apply = Validation.Apply(Errors.SemigroupAny, Error.Type);

module Traversable =
  Array.Validation.Traversable(Errors.SemigroupAny, Error.Type);

module Infix = Validation.Infix(Errors.SemigroupAny, Error.Type);

////////////////////////////////////////////////////////////////////////////////
// Basic value validation
////////////////////////////////////////////////////////////////////////////////

let validateNull: json => Validation.t(unit, Errors.t) =
  json =>
    toNull(json)
    |> Validation.fromOptionLazy(_ =>
         Errors.pure("JSON value is not a null: " ++ show(json))
       );

let validateBool: json => Validation.t(bool, Errors.t) =
  json =>
    toBool(json)
    |> Validation.fromOptionLazy(_ =>
         Errors.pure("JSON value is not a bool: " ++ show(json))
       );

let validateString: json => Validation.t(string, Errors.t) =
  json =>
    toString(json)
    |> Validation.fromOptionLazy(_ =>
         Errors.pure("JSON value is not a string: " ++ show(json))
       );

let validateInt: json => Validation.t(int, Errors.t) =
  json =>
    toInt(json)
    |> Validation.fromOptionLazy(_ =>
         Errors.pure("JSON value is not an int: " ++ show(json))
       );

let validateFloat: json => Validation.t(float, Errors.t) =
  json =>
    toFloat(json)
    |> Validation.fromOptionLazy(_ =>
         Errors.pure("JSON value is not a float: " ++ show(json))
       );

////////////////////////////////////////////////////////////////////////////////
// Array validation (at index and whole array)
////////////////////////////////////////////////////////////////////////////////

let getJsonAtIndex: (int, json) => option(json) =
  (index, json) => {
    toArrayOfJson(json) |> Option.flatMap(Array.at(index));
  };

let validateJsonAtIndex:
  (int, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t) =
  (index, validateItem, json) => {
    getJsonAtIndex(index, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             Errors.pure(
               string_of_int(index)
               ++ " was not found in JSON: "
               ++ show(json),
             ),
           ),
         json => validateItem(json),
       );
  };

let validateNullAtIndex: (int, json) => Validation.t(unit, Errors.t) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateNull(json)
        |> Validation.mapErrorsNea(error =>
             string_of_int(index) ++ ": " ++ error
           ),
      json,
    );

let validateBoolAtIndex: (int, json) => Validation.t(bool, Errors.t) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateBool(json)
        |> Validation.mapErrorsNea(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateIntAtIndex: (int, json) => Validation.t(int, Errors.t) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateInt(json)
        |> Validation.mapErrorsNea(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateFloatAtIndex: (int, json) => Validation.t(float, Errors.t) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateFloat(json)
        |> Validation.mapErrorsNea(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateStringAtIndex: (int, json) => Validation.t(string, Errors.t) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateString(json)
        |> Validation.mapErrorsNea(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateArrayOfJson:
  'a 'e.
  ((int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t)
 =
  (validateItem, json) => {
    json
    |> toArrayOfJson
    |> Option.foldLazy(
         () =>
           Validation.error(
             Errors.pure("JSON value is not an array: " ++ show(json)),
           ),
         jsonValues =>
           jsonValues
           |> Array.zipWithIndex
           |> Traversable.traverse(((json, index)) =>
                validateItem(index, json)
                |> Validation.mapErrorsNea(e =>
                     string_of_int(index) ++ ": " ++ e
                   )
              ),
       );
  };

let validateArrayOfJsonAsList:
  'a 'e.
  ((int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(list('a), Errors.t)
 =
  (validateItem, json) => {
    json
    |> toArrayOfJson
    |> Option.foldLazy(
         () =>
           Validation.error(
             Errors.pure("JSON value is not an array: " ++ show(json)),
           ),
         arrayOfJson =>
           arrayOfJson
           |> Array.zipWithIndex
           |> Traversable.traverse(((json, index)) =>
                validateItem(index, json)
                |> Validation.mapErrorsNea(e =>
                     string_of_int(index) ++ ": " ++ e
                   )
              ),
       )
    |> Validation.map(Array.toList);
  };

let validateArrayAtIndex:
  'a.
  (int, (int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t)
 =
  (index, validateItem, json) =>
    getJsonAtIndex(index, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             Errors.pure(
               string_of_int(index)
               ++ " was not found in JSON: "
               ++ show(json),
             ),
           ),
         json => validateArrayOfJson(validateItem, json),
       );

let validateObjectAtIndex:
  'a.
  (int, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t)
 = validateJsonAtIndex;

////////////////////////////////////////////////////////////////////////////////
// Object validation for key and whole object
////////////////////////////////////////////////////////////////////////////////

let getJsonForKey: (string, json) => option(json) =
  (key, json) => {
    toDictOfJson(json) |> Option.flatMap(dict => Js.Dict.get(dict, key));
  };

let validateJsonForKey:
  (string, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t) =
  (key, validateItem, json) => {
    getJsonForKey(key, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             Errors.pure(key ++ " was not found in JSON: " ++ show(json)),
           ),
         json => validateItem(json),
       );
  };

let validateNullForKey: (string, json) => Validation.t(unit, Errors.t) =
  (key, json) =>
    validateJsonForKey(
      key,
      json =>
        validateNull(json) |> Validation.mapErrorsNea(e => key ++ ": " ++ e),
      json,
    );

let validateBoolForKey: (string, json) => Validation.t(bool, Errors.t) =
  (key, json) =>
    validateJsonForKey(
      key,
      json =>
        validateBool(json) |> Validation.mapErrorsNea(e => key ++ ": " ++ e),
      json,
    );

let validateIntForKey: (string, json) => Validation.t(int, Errors.t) =
  (key, json) =>
    validateJsonForKey(
      key,
      json =>
        validateInt(json) |> Validation.mapErrorsNea(e => key ++ ": " ++ e),
      json,
    );

let validateFloatForKey: (string, json) => Validation.t(float, Errors.t) =
  (key, json) =>
    validateJsonForKey(
      key,
      json =>
        validateFloat(json) |> Validation.mapErrorsNea(e => key ++ ": " ++ e),
      json,
    );

let validateStringForKey: (string, json) => Validation.t(string, Errors.t) =
  (key, json) =>
    validateJsonForKey(
      key,
      json =>
        validateString(json) |> Validation.mapErrorsNea(e => key ++ ": " ++ e),
      json,
    );

let validateArrayForKey:
  'a.
  (string, (int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t)
 =
  (key, validateItem, json) =>
    getJsonForKey(key, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             Errors.pure(key ++ " was not found in JSON: " ++ show(json)),
           ),
         json => validateArrayOfJson(validateItem, json),
       );

let validateObjectForKey:
  'a.
  (string, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t)
 = validateJsonForKey;

////////////////////////////////////////////////////////////////////////////////
// JSON DSL
////////////////////////////////////////////////////////////////////////////////

/**
Exposes a set of focused/abbreviated operations for use as a global or local open.

The map/apply operators are provided for applicative style validation
for decoding objects.
*/
module DSL = {
  // Note: decoding an object can be done using the map-ap-ap-ap-... approach
  // with the <$> (map) and <*> (apply) operators for validation.

  // Bring some infix operators into scope
  let (<$>) = Infix.Functor.(<$>); // map
  let (<#>) = Infix.Functor.(<#>); // flipMap - useful for mapping values after decoding them
  let (<*>) = Infix.Apply.(<*>); // apply
  let (>>=) = Infix.Monad.(>>=); // bind

  ////////////////////////////////////////////////////////////////////////////////
  // Encoding utilities for constructing Js.Json.t values
  ////////////////////////////////////////////////////////////////////////////////

  module E = {
    let null = null;
    let bool = fromBool;
    let int = fromInt;
    let float = fromFloat;
    let num = fromFloat;
    let string = fromString;
    let array = fromArrayOfJson;
    let arrayBy = fromArrayOfJsonBy;
    let arrayOfDict = fromArrayOfDictOfJson;
    let arrayOfTuples = fromArrayOfKeyValueTuples;
    let list = fromListOfJson;
    let listBy = fromListOfJsonBy;
    let listOfDict = fromListOfDictOfJson;
    let listOfTuples = fromListOfKeyValueTuples;
    let dict = fromDictOfJson;
  };

  ////////////////////////////////////////////////////////////////////////////////
  // Decoding (validation) utilities
  ////////////////////////////////////////////////////////////////////////////////

  module D = {
    ////////////////////////////////////////////////////////////////////////////////
    // Plain value validation (decoding)
    ////////////////////////////////////////////////////////////////////////////////

    let null = validateNull;
    let bool = validateBool;
    let int = validateInt;
    let float = validateFloat;
    let string = validateString;

    ////////////////////////////////////////////////////////////////////////////////
    // Array validation - validating items by array index (or whole array)
    ////////////////////////////////////////////////////////////////////////////////

    let getAt = getJsonAtIndex;
    let jsonAt = validateJsonAtIndex;
    let nullAt = validateNullAtIndex;
    let boolAt = validateBoolAtIndex;
    let stringAt = validateStringAtIndex;
    let intAt = validateIntAtIndex;
    let floatAt = validateFloatAtIndex;
    let arrayAt = validateArrayAtIndex;
    let objectAt = validateObjectAtIndex;
    let array = validateArrayOfJson;
    let list = validateArrayOfJsonAsList;

    ////////////////////////////////////////////////////////////////////////////////
    // Object validation - validating items by object key
    ////////////////////////////////////////////////////////////////////////////////

    let getFor = getJsonForKey;
    let jsonFor = validateJsonForKey;
    let nullFor = validateNullForKey;
    let boolFor = validateBoolForKey;
    let stringFor = validateStringForKey;
    let intFor = validateIntForKey;
    let floatFor = validateFloatForKey;
    let arrayFor = validateArrayForKey;
    let objectFor = validateObjectForKey;
  };
};