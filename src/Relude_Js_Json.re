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
type dict = Js.Dict.t(json);

let show = (~indentSpaces: int=2, json: json): string =>
  Js.Json.stringifyWithSpace(json, indentSpaces);

////////////////////////////////////////////////////////////////////////////////
// Test functions
////////////////////////////////////////////////////////////////////////////////

let isNull: json => bool = json => Js.Json.test(json, Js.Json.Null);

let isBool: json => bool = json => Js.Json.test(json, Js.Json.Boolean);

let isString: json => bool = json => Js.Json.test(json, Js.Json.String);

let isNumber: json => bool = json => Js.Json.test(json, Js.Json.Number);

let isObject: json => bool = json => Js.Json.test(json, Js.Json.Object);

let isArray: json => bool = json => Js.Json.test(json, Js.Json.Array);

////////////////////////////////////////////////////////////////////////////////
// Constructor functions (encode values as Json)
////////////////////////////////////////////////////////////////////////////////

let null: json = Js.Json.null;

let fromBool: bool => json = Js.Json.boolean;

let fromString: string => json = Js.Json.string;

let fromInt: int => json = float_of_int >> Js.Json.number;

let fromFloat: float => json = Js.Json.number;

let fromArray: array(json) => json = Js.Json.array;

let fromArrayBy: 'a. ('a => json, array('a)) => json =
  (f, items) => Array.map(f, items) |> fromArray;

let fromList: list(json) => json = List.toArray >> fromArray;

let fromListBy: 'a. ('a => json, list('a)) => json =
  (f, items) => List.map(f, items) |> fromList;

let fromDict: dict => json = Js.Json.object_;

let fromArrayOfDict: array(dict) => json = Js.Json.objectArray;

let fromListOfDict: list(dict) => json = List.toArray >> Js.Json.objectArray;

////////////////////////////////////////////////////////////////////////////////
// Basic conversions from the top-level Js.Json.t type to the specific Json
// subtypes.  These functions do not decode nested json objects - i.e. asDict
// will attempt to convert a `Js.Json.t` value to an `option(Js.Dict.t(Js.Json.t))`
// but does not attempt to decode the resulting dictionary.
//
// Note: there are intentially very basic. For more advanced decoding, try a
// library like https://github.com/mlms13/bs-decode
////////////////////////////////////////////////////////////////////////////////

let toNull: json => option(unit) =
  json => json |> Js.Json.decodeNull |> Option.void;

let toBool: json => option(bool) = Js.Json.decodeBoolean;

let toString: json => option(string) = Js.Json.decodeString;

let toInt: json => option(int) =
  json => json |> Js.Json.decodeNumber |> Option.map(int_of_float);

let toFloat: json => option(float) = Js.Json.decodeNumber;

let toArray: json => option(array(json)) = Js.Json.decodeArray;

let toArrayOrElse: (array(json), json) => array(json) =
  (default, json) => json |> toArray |> Option.getOrElse(default);

let toArrayOrEmpty = toArrayOrElse([||]);

let toList: json => option(list(json)) =
  json => json |> toArray |> Option.map(Array.toList);

let toListOrElse = (default, json) =>
  json |> toList |> Option.getOrElse(default);

let toListOrEmpty = toListOrElse([]);

let toDict: json => option(dict) = Js.Json.decodeObject;

let toDictOrElse = (default, json) =>
  json |> toDict |> Option.getOrElse(default);

let toDictOrEmpty: json => dict = toDictOrElse(Js.Dict.empty());

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

/**
This function makes sure the given json value is actually a JSON array, and if it is,
it decodes each JSON object in the array using the given decode function.  The decode
function is passed the array index and corresponding `json` value.

The decoding result is returned as a `Result.t('a, string)`

If all items decode successfully, they are returned in the success channel of
the Validation.

If any items fail to decode, the errors are collected in a NonEmpty.Array of
errors. This means that the decoding will continue if any errors occur, but
the entire operation will still fail.
 */
let decodeArray:
  'a 'e.
  ((int, json) => Result.t('a, string), json) =>
  Validation.t(array('a), NonEmptyArray.t((int, string)))
 =
  (decodeItem, json) => {
    json
    |> toArray
    |> Option.foldLazy(
         () =>
           Validation.VError(
             NonEmptyArray.pure((
               (-1),
               "Cannot decode JSON value as an array - input value was not a JSON array: "
               ++ show(json),
             )),
           ),
         jsonValues =>
           jsonValues
           |> Array.zipWithIndex
           |> Array.Validation.traverse(((json, index)) =>
                decodeItem(index, json) |> Result.mapError(e => (index, e))
              ),
       );
  };

/**
Same as decodeArray, but uses lists rather than arrays.
*/
let decodeArrayAsList:
  'a 'e.
  ((int, json) => Result.t('a, string), json) =>
  Validation.t(list('a), NonEmptyList.t((int, string)))
 =
  (decodeItem, json) => {
    json
    |> toList
    |> Option.foldLazy(
         () =>
           Validation.VError(
             NonEmptyList.pure((
               (-1),
               "Cannot decode JSON value as list - input value was not a JSON array: "
               ++ show(json),
             )),
           ),
         jsonValues =>
           jsonValues
           |> List.zipWithIndex
           |> List.Validation.traverse(((json, index)) =>
                decodeItem(index, json) |> Result.mapError(e => (index, e))
              ),
       );
  };

let field: (string, json) => option(json) =
  (key, json) => {
    toDictOrEmpty(json)->Js.Dict.get(key);
  };

// Note: decoding an object can be done using the map-ap-ap-ap-... approach
// with the <$> (map) and <*> (apply) operators for validation.

module ErrorAsString: BsAbstract.Interface.TYPE with type t = string = {
  type t = string;
};

module ValidationNonEmptyArrayApply =
  Validation.Apply(NonEmptyArray.SemigroupAny, ErrorAsString);

module ValidationNonEmptyArrayInfix = {
  include BsAbstract.Infix.Apply(ValidationNonEmptyArrayApply);
};

/**
Expose a set of focused operations for use as a global or local open.

The map/apply operators are provided for applicative style validation
for decoding objects.
*/
module DSL = {
  let (<$>) = ValidationNonEmptyArrayInfix.(<$>);
  let (<#>) = ValidationNonEmptyArrayInfix.(<#>);
  let (<*>) = ValidationNonEmptyArrayInfix.(<*>);

  let null = null;
  let bool = fromBool;
  let str = fromString;
  let int = fromInt;
  let num = fromFloat;
  let arr = fromArray;
  let arrBy = fromArrayBy;
  let arrOfDict = fromArrayOfDict;
  let list = fromList;
  let listBy = fromListBy;
  let listOfDict = fromListOfDict;
  let dict = fromDict;
};