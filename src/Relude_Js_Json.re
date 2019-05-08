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
// Basic value validation
////////////////////////////////////////////////////////////////////////////////

let validateNull: json => Result.t(unit, string) =
  json =>
    toNull(json)
    |> Result.fromOptionLazy(_ => "JSON value is not a null: " ++ show(json));

let validateBool: json => Result.t(bool, string) =
  json =>
    toBool(json)
    |> Result.fromOptionLazy(_ => "JSON value is not a bool: " ++ show(json));

let validateString: json => Result.t(string, string) =
  json =>
    toString(json)
    |> Result.fromOptionLazy(_ =>
         "JSON value is not a string: " ++ show(json)
       );

let validateInt: json => Result.t(int, string) =
  json =>
    toInt(json)
    |> Result.fromOptionLazy(_ => "JSON value is not an int: " ++ show(json));

let validateFloat: json => Result.t(float, string) =
  json =>
    toFloat(json)
    |> Result.fromOptionLazy(_ => "JSON value is not a float: " ++ show(json));

////////////////////////////////////////////////////////////////////////////////
// Array validation (at index and whole array)
////////////////////////////////////////////////////////////////////////////////

let getJsonAtIndex: (int, json) => option(json) =
  (index, json) => {
    toArrayOfJson(json) |> Option.flatMap(Array.at(index));
  };

let validateJsonAtIndex:
  (int, json => Result.t('a, string), json) =>
  Validation.t('a, NonEmptyArray.t(string)) =
  (index, validate, json) => {
    getJsonAtIndex(index, json)
    |> Option.foldLazy(
         _ =>
           Result.error(
             string_of_int(index) ++ " was not found in JSON: " ++ show(json),
           ),
         json => validate(json),
       )
    |> Result.toValidationNea;
  };

let validateNullAtIndex:
  (int, json) => Validation.t(unit, NonEmptyArray.t(string)) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateNull(json)
        |> Result.mapError(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateBoolAtIndex:
  (int, json) => Validation.t(bool, NonEmptyArray.t(string)) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateBool(json)
        |> Result.mapError(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateIntAtIndex:
  (int, json) => Validation.t(int, NonEmptyArray.t(string)) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateInt(json)
        |> Result.mapError(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateFloatAtIndex:
  (int, json) => Validation.t(float, NonEmptyArray.t(string)) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateFloat(json)
        |> Result.mapError(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateStringAtIndex:
  (int, json) => Validation.t(string, NonEmptyArray.t(string)) =
  (index, json) =>
    validateJsonAtIndex(
      index,
      json =>
        validateString(json)
        |> Result.mapError(e => string_of_int(index) ++ ": " ++ e),
      json,
    );

let validateArrayOfJson:
  'a 'e.
  ((int, json) => Result.t('a, string), json) =>
  Validation.t(array('a), NonEmptyArray.t(string))
 =
  (validateItem, json) => {
    json
    |> toArrayOfJson
    |> Option.foldLazy(
         () =>
           Validation.error(
             NonEmptyArray.pure(
               "JSON value is not an array: " ++ show(json),
             ),
           ),
         jsonValues =>
           jsonValues
           |> Array.zipWithIndex
           |> Array.Validation.traverse(((json, index)) =>
                validateItem(index, json)
                |> Result.mapError(e => string_of_int(index) ++ ": " ++ e)
              ),
       );
  };

let validateArrayOfJsonAsList:
  'a 'e.
  ((int, json) => Result.t('a, string), json) =>
  Validation.t(list('a), NonEmptyList.t(string))
 =
  (validateItem, json) => {
    json
    |> toListOfJson
    |> Option.foldLazy(
         () =>
           Validation.error(
             NonEmptyList.pure("JSON value is not an array: " ++ show(json)),
           ),
         jsonValues =>
           jsonValues
           |> List.zipWithIndex
           |> List.Validation.traverse(((json, index)) =>
                validateItem(index, json)
                |> Result.mapError(e => string_of_int(index) ++ ": " ++ e)
              ),
       );
  };

let validateArrayAtIndex:
  'a.
  (int, (int, json) => Result.t('a, string), json) =>
  Validation.t(array('a), NonEmptyArray.t(string))
 =
  (index, validateItem, json) =>
    getJsonAtIndex(index, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             NonEmptyArray.pure(
               string_of_int(index)
               ++ " was not found in JSON: "
               ++ show(json),
             ),
           ),
         json => validateArrayOfJson(validateItem, json),
       );

let validateObjectAtIndex:
  'a.
  (int, json => Result.t('a, string), json) =>
  Validation.t('a, NonEmptyArray.t(string))
 =
  (index, validateItem, json) =>
    getJsonAtIndex(index, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             NonEmptyArray.pure(
               string_of_int(index)
               ++ " was not found in JSON: "
               ++ show(json),
             ),
           ),
         json => validateItem(json) |> Result.toValidationNea,
       );

////////////////////////////////////////////////////////////////////////////////
// Object validation for key and whole object
////////////////////////////////////////////////////////////////////////////////

let getJsonForKey: (string, json) => option(json) =
  (key, json) => {
    toDictOfJson(json) |> Option.flatMap(dict => Js.Dict.get(dict, key));
  };

let validateJsonForKey:
  (string, json => Result.t('a, string), json) =>
  Validation.t('a, NonEmptyArray.t(string)) =
  (key, validate, json) => {
    getJsonForKey(key, json)
    |> Option.foldLazy(
         _ => Result.error(key ++ " was not found in JSON: " ++ show(json)),
         json => validate(json),
       )
    |> Result.toValidationNea;
  };

let validateNullForKey:
  (string, json) => Validation.t(unit, NonEmptyArray.t(string)) =
  (key, json) =>
    validateJsonForKey(
      key,
      json => validateNull(json) |> Result.mapError(e => key ++ ": " ++ e),
      json,
    );

let validateBoolForKey:
  (string, json) => Validation.t(bool, NonEmptyArray.t(string)) =
  (key, json) =>
    validateJsonForKey(
      key,
      json => validateBool(json) |> Result.mapError(e => key ++ ": " ++ e),
      json,
    );

let validateIntForKey:
  (string, json) => Validation.t(int, NonEmptyArray.t(string)) =
  (key, json) =>
    validateJsonForKey(
      key,
      json => validateInt(json) |> Result.mapError(e => key ++ ": " ++ e),
      json,
    );

let validateFloatForKey:
  (string, json) => Validation.t(float, NonEmptyArray.t(string)) =
  (key, json) =>
    validateJsonForKey(
      key,
      json => validateFloat(json) |> Result.mapError(e => key ++ ": " ++ e),
      json,
    );

let validateStringForKey:
  (string, json) => Validation.t(string, NonEmptyArray.t(string)) =
  (key, json) =>
    validateJsonForKey(
      key,
      json => validateString(json) |> Result.mapError(e => key ++ ": " ++ e),
      json,
    );

let validateArrayForKey:
  'a.
  (string, (int, json) => Result.t('a, string), json) =>
  Validation.t(array('a), NonEmptyArray.t(string))
 =
  (key, validateItem, json) =>
    getJsonForKey(key, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             NonEmptyArray.pure(
               key ++ " was not found in JSON: " ++ show(json),
             ),
           ),
         json => validateArrayOfJson(validateItem, json),
       );

let validateObjectForKey:
  'a.
  (string, json => Result.t('a, string), json) =>
  Validation.t('a, NonEmptyArray.t(string))
 =
  (key, validateItem, json) =>
    getJsonForKey(key, json)
    |> Option.foldLazy(
         _ =>
           Validation.error(
             NonEmptyArray.pure(
               key ++ " was not found in JSON: " ++ show(json),
             ),
           ),
         json => validateItem(json) |> Result.toValidationNea,
       );

////////////////////////////////////////////////////////////////////////////////
// JSON DSL
////////////////////////////////////////////////////////////////////////////////

/**
Exposes a set of focused operations for use as a global or local open.

The map/apply operators are provided for applicative style validation
for decoding objects.
*/
module DSL = {
  // Note: decoding an object can be done using the map-ap-ap-ap-... approach
  // with the <$> (map) and <*> (apply) operators for validation.

  // Use a string as our error type to keep it simple
  module ErrorAsString: BsAbstract.Interface.TYPE with type t = string = {
    type t = string;
  };

  // Create Apply and Infix modules assuming a NonEmptyArray as our semigroup for collecting errors
  module ValidationApply =
    Validation.Apply(NonEmptyArray.SemigroupAny, ErrorAsString);

  module ValidationInfix =
    Validation.Infix(NonEmptyArray.SemigroupAny, ErrorAsString);

  // Bring some infix operators into scope
  let (<$>) = ValidationInfix.Functor.(<$>); // map
  let (<#>) = ValidationInfix.Functor.(<#>); // flipMap
  let (<*>) = ValidationInfix.Apply.(<*>); // apply

  ////////////////////////////////////////////////////////////////////////////////
  // Construct Js.Json.t values for encoding
  ////////////////////////////////////////////////////////////////////////////////

  let null = null;
  let bool = fromBool;
  let int = fromInt;
  let num = fromFloat;
  let str = fromString;
  let arr = fromArrayOfJson;
  let arrBy = fromArrayOfJsonBy;
  let arrOfDict = fromArrayOfDictOfJson;
  let arrOfKVP = fromArrayOfKeyValueTuples;
  let list = fromListOfJson;
  let listBy = fromListOfJsonBy;
  let listOfDict = fromListOfDictOfJson;
  let listOfKVP = fromListOfKeyValueTuples;
  let dict = fromDictOfJson;

  ////////////////////////////////////////////////////////////////////////////////
  // Plain value validation
  ////////////////////////////////////////////////////////////////////////////////

  let validateNull = validateNull;
  let validateBool = validateBool;
  let validateInt = validateInt;
  let validateFloat = validateFloat;
  let validateString = validateString;

  ////////////////////////////////////////////////////////////////////////////////
  // Array validation
  ////////////////////////////////////////////////////////////////////////////////

  let getJsonAtIndex = getJsonAtIndex;
  let validateJsonAtIndex = validateJsonAtIndex;
  let validateNullAtIndex = validateNullAtIndex;
  let validateBoolAtIndex = validateBoolAtIndex;
  let validateStringAtIndex = validateStringAtIndex;
  let validateIntAtIndex = validateIntAtIndex;
  let validateFloatAtIndex = validateFloatAtIndex;
  let validateArrayAtIndex = validateArrayAtIndex;
  let validateObjectAtIndex = validateObjectAtIndex;
  let validateArrayOfJson = validateArrayOfJson;
  let validateArrayOfJsonAsList = validateArrayOfJsonAsList;

  ////////////////////////////////////////////////////////////////////////////////
  // Object validation
  ////////////////////////////////////////////////////////////////////////////////

  let getJsonForKey = getJsonForKey;
  let validateJsonForKey = validateJsonForKey;
  let validateNullForKey = validateNullForKey;
  let validateBoolForKey = validateBoolForKey;
  let validateStringForKey = validateStringForKey;
  let validateIntForKey = validateIntForKey;
  let validateFloatForKey = validateFloatForKey;
  let validateArrayForKey = validateArrayForKey;
  let validateObjectForKey = validateObjectForKey;
};