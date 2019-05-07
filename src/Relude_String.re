module Monoid = BsAbstract.String.Monoid;
module Semigroup = BsAbstract.String.Semigroup;
module Eq = BsAbstract.String.Eq;
module Ord = BsAbstract.String.Ord;

let concat: (string, string) => string = (a, b) => Js.String.concat(b, a); /* Js.String.concat has unexpected argument order */

let concatArray: array(string) => string =
  strs => Relude_Array_Types.foldLeft((acc, str) => acc ++ str, "", strs);

let length: string => int = Js.String.length;

let trim: string => string = Js.String.trim;

let isEmpty: string => bool = s => length(s) == 0;

let isNotEmpty: string => bool = s => !isEmpty(s);

let toNonEmpty: string => option(string) =
  s =>
    if (s |> isEmpty) {
      None;
    } else {
      Some(s);
    };

let eq: (string, string) => bool = (==);

let isWhitespace: string => bool = s => s |> trim |> isEmpty;

let toNonWhitespace: string => option(string) =
  s =>
    if (s |> isWhitespace) {
      None;
    } else {
      Some(s);
    };

let make: 'a => string = Js.String.make;

let fromCharCode: int => string = Js.String.fromCharCode;

let makeWithIndex: (int, int => string) => string =
  (i, f) => {
    let rec go = (acc, idx) =>
      idx >= i ? acc : go(concat(acc, f(idx)), idx + 1);
    go("", 0);
  };

let repeat: (int, string) => string =
  (i, str) => {
    let rec go = (acc, i) => i <= 0 ? acc : go(concat(acc, str), i - 1);
    go("", i);
  };

let toUpperCase: string => string = Js.String.toUpperCase;

let toLowerCase: string => string = Js.String.toLowerCase;

let charAt: (int, string) => option(string) =
  (i, str) =>
    Js.String.get(str, i) |> Js.Nullable.return |> Js.Nullable.toOption;

let charAtNullable: (int, string) => Js.Nullable.t(string) =
  (i, str) => Js.String.get(str, i) |> Js.Nullable.return;

let charAtOrThrow: (int, string) => string =
  (i, str) =>
    switch (charAt(i, str)) {
    | None =>
      Js.Exn.raiseRangeError(
        "Failed to get string at index "
        ++ string_of_int(i)
        ++ " for string: "
        ++ str,
      )
    | Some(v) => v
    };

let toList: string => list(string) =
  str =>
    Relude_List_Base.makeWithIndex(length(str), i => charAtOrThrow(i, str));

let toArray: string => array(string) =
  str =>
    Relude_Array_Base.makeWithIndex(length(str), i => charAtOrThrow(i, str));

let foldLeft: (('b, string) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List_Types.foldLeft(f, init, toList(str));

let foldRight: ((string, 'b) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List_Types.foldRight(f, init, toList(str));

let endsWith: (string, string) => bool =
  (test, str) => Js.String.endsWith(test, str);

let startsWith: (string, string) => bool =
  (test, str) => Js.String.startsWith(test, str);

let contains: (string, string) => bool =
  (test, str) => Js.String.includes(test, str);

let indexOf: (string, string) => option(int) =
  (test, str) => {
    let index = Js.String.indexOf(test, str);
    if (index < 0) {
      None;
    } else {
      Some(index);
    };
  };

let lastIndexOf: (string, string) => option(int) =
  (test, str) => {
    let index = Js.String.lastIndexOf(test, str);
    if (index < 0) {
      None;
    } else {
      Some(index);
    };
  };

/**
Extracts a substring from an `input` string starting at index `fromIndex` and ending before index `toIndex`
 */
let slice: (int, int, string) => string =
  (fromIndex, toIndex, input) =>
    Js.String.slice(~from=fromIndex, ~to_=toIndex, input);

/**
Extracts a substring from an `input` string starting at index `fromIndex`, to the end of the string.
 */
let sliceToEnd: (int, string) => string =
  (fromIndex, str) => Js.String.sliceToEnd(~from=fromIndex, str);

/**
Splits a string on a string delimiter, returning the values as an array.
*/
let splitArray: (string, string) => array(string) =
  (delimiter, input) => Js.String.split(delimiter, input);

/**
Splits a string on a string delimiter, returning the resulting values as an array.
*/
let splitList: (string, string) => list(string) =
  (delimiter, str) =>
    splitArray(delimiter, str) |> Relude_List_Types.fromArray;

/**
Splits a string at the given index, returning a tuple of the parts.

Note: for negative indices, it slices from the right.
*/
let splitAt: (int, string) => (string, string) =
  (index, input) => (slice(0, index, input), sliceToEnd(index, input));

/**
Maps a function over the individual characters of a string, returning a new string.
 */
let mapChars: (string => string, string) => string =
  (f, str) =>
    toList(str)
    |> Relude_List_Types.map(f)
    |> Relude_List_Types.fold((module Monoid));

/**
Replaces the first occurrence of `target` with `newValue` in `source`, returning a new string.
 */
let replaceFirst: (string, string, string) => string =
  (target, newValue, source) => Js.String.replace(target, newValue, source);

/**
Replaces each occurrence of `target` with `newValue` in `source`, returning a new string.
*/
let replaceEach: (string, string, string) => string =
  (target, newValue, source) =>
    splitList(target, source) |> String.concat(newValue);

let replaceRegex: (Js.Re.t, string, string) => string =
  (target, newValue, source) =>
    Js.String.replaceByRe(target, newValue, source);

let removeFirst: (string, string) => string =
  (target, source) => replaceFirst(target, "", source);

let removeEach: (string, string) => string =
  (target, source) => replaceEach(target, "", source);

let fromInt: int => string = string_of_int;

let toInt: string => option(int) =
  v =>
    try (Some(int_of_string(v))) {
    | _ => None
    };

let fromFloat: float => string = Js.Float.toString;

let toFloat: string => option(float) =
  v =>
    try (Some(float_of_string(v))) {
    | _ => None
    };