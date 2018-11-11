let length: string => int = String.length;

let trim: string => string = String.trim;

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

let makeWithIndex: (int, int => char) => string = String.init;

let repeat: (int, char) => string = String.make;

let toUpperCase: string => string = String.uppercase;

let toLowerCase: string => string = String.lowercase;

let capitalize: string => string = String.capitalize;

let uncapitalize: string => string = String.uncapitalize;

let unsafeGet: (int, string) => char = (i, str) => str.[i];

let get: (int, string) => option(char) =
  (i, str) =>
    try (Some(unsafeGet(i, str))) {
    | _ => None
    };

let toCharList: string => list(char) =
  str => Relude_List.makeWithIndex(length(str), i => unsafeGet(i, str));

let toCharArray: string => array(char) =
  str => Relude_Array.makeWithIndex(length(str), i => unsafeGet(i, str));

let map: (char => char, string) => string = String.map;

let foldLeft: (('b, char) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List.foldLeft(f, init, toCharList(str)); /* TODO performance */

let foldRight: ((char, 'b) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List.foldRight(f, init, toCharList(str)); /* TODO performance */

/*
 let split: (char, string) => list(string) = StringLabels.(
 */
