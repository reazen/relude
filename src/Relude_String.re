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
  (i, f) =>
    Relude_Int.rangeAsList(0, i)
    |> Relude_List.foldRight((i, acc) => [f(i), ...acc], [])
    |> Relude_List.mkString("");

let repeat: (int, string) => string =
  (i, str) =>
    Relude_Int.rangeAsList(0, i)
    |> Relude_List.foldRight((_, acc) => [str, ...acc], [])
    |> Relude_List.mkString("");

let toUpperCase: string => string = Js.String.toUpperCase;

let toLowerCase: string => string = Js.String.toLowerCase;

let get: (int, string) => option(string) =
  (i, str) =>
    Js.String.get(str, i) |> Js.Nullable.return |> Js.Nullable.toOption;

let getNullable: (int, string) => Js.Nullable.t(string) =
  (i, str) => Js.String.get(str, i) |> Js.Nullable.return;

let getOrThrow: (int, string) => string =
  (i, str) =>
    switch (get(i, str)) {
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
  str => Relude_List.makeWithIndex(length(str), i => getOrThrow(i, str));

let toArray: string => array(string) =
  str => Relude_Array.makeWithIndex(length(str), i => getOrThrow(i, str));

let map: (string => string, string) => string =
  (f, str) =>
    toList(str) |> Relude_List.map(f) |> Relude_List.mkString("");

let foldLeft: (('b, string) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List.foldLeft(f, init, toList(str));

let foldRight: ((string, 'b) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List.foldRight(f, init, toList(str));

let concat: (string, string) => string = (a, b) => Js.String.concat(b, a); /* Js.String.concat has unexpected argument order */

let concatArray: array(string) => string =
  strs => Relude_Array.foldLeft((acc, str) => acc ++ str, "", strs);

let concatList: list(string) => string =
  strs => Relude_List.foldLeft((acc, str) => acc ++ str, "", strs);

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

let replace: (string, string, string) => string =
  (target, newValue, source) => Js.String.replace(target, newValue, source);

let replaceRegex: (Js.Re.t, string, string) => string =
  (target, newValue, source) => Js.String.replaceByRe(target, newValue, source);

let slice: (int, int, string) => string = (fromIndex, toIndex, str) => Js.String.slice(~from=fromIndex, ~to_=toIndex, str);

let sliceToEnd: (int, string) => string = (fromIndex, str) => Js.String.sliceToEnd(~from=fromIndex, str);

let splitArray: (string, string) => array(string) = Js.String.split;

let splitList: (string, string) => list(string) =
  (delim, str) => splitArray(delim, str) |> Relude_List.fromArray;

let toInt: string => option(int) = v =>
  try (Some(int_of_string(v))) {
  | _ => None
  };

let toFloat: string => option(float) = v =>
  try (Some(float_of_string(v))) {
  | _ => None
  };

module Monoid = BsAbstract.String.Monoid;

module Semigroup = BsAbstract.String.Semigroup;

module Eq = BsAbstract.String.Eq;

module Ord = BsAbstract.String.Ord
