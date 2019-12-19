/**
Returns the empty string
*/
let empty: string = "";

/**
`length(str)` returns the length of the string. Since this function
calls the JavaScript `String.length` function, it works properly with
Unicode characters.

### Example
```re
length("example") == 7;
length({js|Glück|js}) == 5;
length({js|대한민국|js}) == 4;
```
*/
let length: string => int = Js.String.length;

/**
  `isEmpty(str)` returns `true` if `str` is the empty string `""`;
  `false` otherwise.
*/
let isEmpty: string => bool = s => length(s) == 0;

/**
  `isNotEmpty(str)` returns `true` if `str` is not the empty string `""`;
  `false` if it is empty.
*/
let isNonEmpty: string => bool = s => !isEmpty(s);

/**
Alias for isNonEmpty
*/
let isNotEmpty = isNonEmpty;

/**
  `toNonEmpty(str)` returns `Some(str)` if `str` is not the empty string `""`.
  It returns `None` if `str` is the empty string.

  ### Example
  ```re
  toNonEmpty("abc") == Some("abc");
  toNonEmpty("") == None;
  ```
*/
let toNonEmpty: string => option(string) = s => isEmpty(s) ? None : Some(s);

/**
  `trim(str)` returns a new string with leading and trailing whitespace
  (blank, tab, newline, non-breaking space and others as described
  in <https://www.ecma-international.org/ecma-262/5.1/#sec-7.2>) removed from `s`.

  ### Example
  ```re
  trim("  abc  ") == "abc";
  trim("  abc def  ") == "abc def";
  trim({js|\n\u00a0 \t abc \f\r \t|js}) == "abc";
  ```
*/
let trim: string => string = Js.String.trim;

// TODO
//let trimLeft: string => string = ???

// TODO
//let trimRight: string => string = ???

/**
  `isWhitespace(str)` returns true if the string consists
  entirely of whitespace characters as described
  in <https://www.ecma-international.org/ecma-262/5.1/#sec-7.2>

  ### Example
  ```re
  isWhitespace(" \n \t \r  ") == true;
  isWhitespace(" \n \t X \r ") == false;
  ```
*/
let isWhitespace: string => bool = s => s |> trim |> isEmpty;

/**
 * Indicates if the string contains any non-whitespace characters
 */
let isNonWhitespace: string => bool = s => !isWhitespace(s);

/**
  `toNonWhiteSpace(str)` returns `Some(str)` if `str` has any non-whitespace
  characters in it. The function returns `None` if `str` consists entirely
  of whitespace.

  ### Example
  ```re
  toNonWhitespace("\n\t abc \t\r") == Some("\n\t abc \t\r");
  toNonWhitespace("\t\n  \r") == None;
  ```
*/
let toNonWhitespace: string => option(string) =
  s => isWhitespace(s) ? None : Some(s);

/**
  `concat(str1, str2)` concatenate the two strings, returning
  a new string.

  ### Example
  ```re
  concat("door", "bell") == "doorbell";
  concat("", "next") == "next";
  concat("first", "") == "first";
  ```
*/
let concat: (string, string) => string = (a, b) => a ++ b;

module Semigroup: BsAbstract.Interface.SEMIGROUP with type t = string = {
  type t = string;
  let append = concat;
};
include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);

module Monoid: BsAbstract.Interface.MONOID with type t = string = {
  include Semigroup;
  let empty = empty;
};
include Relude_Extensions_Monoid.MonoidExtensions(Monoid);

/**
  `concatArray(xs)` returns a new string that is the result
  of concatenating all the strings in `xs`

  ### Example
  ```re
  concatArray([|"cat", "en", "ate"|]) == "catenate";
  concatArray([|"chair", "", "person"|]) == "chairperson";
  concatArray([| |]) == "";
  ```
*/
let concatArray: array(string) => string =
  array =>
    Relude_Array_Instances.foldLeft((acc, str) => acc ++ str, "", array);

/**
Like `concatArray`, but for `list`
*/
let concatList: list(string) => string =
  list => Relude_List_Instances.foldLeft((acc, str) => acc ++ str, "", list);

/**
  `make(x)` converts `x` to a string. If `x` is not a
  primitive type such as integer, float, string, or boolean,
  the result will reflect ReasonML’s internal format for
  that data type.

  ### Example
  ```re
  make(32.5) == "32.5";
  make(1.0e3) == "1000";
  make(true) == "true";
  make("already a string") == "already a string";
  make([1, 2, 3, 4]) == "1,2,3,4,0";
  make([|1, 2, 3, 4|]) == "1,2,3,4";
  ```
*/
let make: 'a => string = Js.String.make;

/**
  `makeWithIndex(n, f)` returns a string that is the result
  of concatenating `f(0)`, `f(1)`, ... `f(n - 1)`, where
  function `f()` takes an integer argument and returns a string.

  ### Example
  ```re
  let squareChar = (n) => {fromCharCode(97 + n * n)};
  makeWithIndex(4, squareChar) == "abej";
  ```
*/
let makeWithIndex: (int, int => string) => string =
  (i, f) => {
    let rec go = (acc, idx) =>
      idx >= i ? acc : go(concat(acc, f(idx)), idx + 1);
    go("", 0);
  };

/**
  `repeat(n, str)` returns a string consisting of `n` repetitions
  of `str`.

  ### Example
  ```re
  repeat(3, "ha") == "hahaha";
  repeat(0, "ha") == "";
  repeat(-1, "ha") == "";
  repeat(3, "") == "";
  ```
*/
let repeat: (int, string) => string =
  (i, str) => {
    let rec go = (acc, i) => i <= 0 ? acc : go(concat(acc, str), i - 1);
    go("", i);
  };

/**
  `toUpperCase(str)` converts `str` to upper case using
  the locale-insensitive case mappings in the Unicode
  Character Database. Notice that the conversion can
  expand the number of letters in the result; for example,
  the German `ß` capitalizes to two `S`es in a row.

  ### Example
  ```re
  toUpperCase("abc") == "ABC";
  toUpperCase({js|Straße|js}) == {js|STRASSE|js};
  toUpperCase({js|σπ|js}) == {js|ΣΠ|js};
  toUpperCase({js|πς|js}) == {js|ΠΣ|js}; // sigma in final position
  ```
*/
let toUpperCase: string => string = Js.String.toUpperCase;

/**
  `toLowerCase(str)` converts `str` to lower case using
  the locale-insensitive case mappings in the Unicode
  Character Database. Notice that the conversion might not
  lessen the number of letters in the result; for example,
  in some German words, two `S`es in a row can convert
  to the single lowercase glyph `ß`, but `toLowerCase()`
  will not do this transformation..

  ### Example
  ```re
  toLowerCase("ABC") == "abc";
  toLowerCase({js|STRASSE|js}) == {js|strasse|js};
  toLowerCase({js|ΣΠ|js}) == {js|σπ|js};
  toLowerCase({js|ΠΣ|js}) == {js|πς|js}; // sigma in final position
  ```
*/
let toLowerCase: string => string = Js.String.toLowerCase;

/**

  `fromCharCode(n)` creates a string containing the character
  corresponding to that number; n ranges from 0 to 65535.
  If out of range, the lower 16 bits of the value are used.
  Thus, `fromCharCode(0x1F63A)` gives the same result as
  `fromCharCode(0xF63A)`.


  ### Example
  ```re
  fromCharCode(65) == "A";
  fromCharCode(0x0920) == {js|ठ|js};
  fromCharCode(0x3c8) == {js|ψ|js};
  fromCharCode(-64568) == {js|ψ|js};
  ```
*/
let fromCharCode: int => string = Js.String.fromCharCode;

/**
  `charAt(n, str)` returns `Some(chStr)`, where `chStr` is a string
  consisting of the character at location `n` in the string. The
  first character in a string has position zero.

  If `n` is out of bounds, `charAt()` returns `None`.

  ### Example
  ```re
  charAt(0, "abc") == Some("a");
  charAt(2, "abc") == Some("c");
  charAt(1, {js|대한민국|js}) == Some({js|한|js});
  charAt(-1, "abc") == None;
  charAt(3, "abc") == None;
  ```
*/
let charAt: (int, string) => option(string) =
  (i, str) =>
    Js.String.get(str, i) |> Js.Nullable.return |> Js.Nullable.toOption;

/**
  `charAtNullable(n, str)` returns `Js.Nullable.return(chStr)`,
  where `chStr` is a string consisting of the character at
  location `n` in the string. The first character in a string has position zero.

  If `n` is out of bounds, `charAtNullable()` returns `Js.Nullable.undefined`.

  ### Example
  ```re
  charAtNullable(0, "abc") == Js.Nullable.return("a");
  charAtNullable(2, "abc") == Js.Nullable.return("c");
  charAtNullable(1, {js|대한민국|js}) == Js.Nullable.return({js|한|js});
  charAtNullable(-1, "abc") == Js.Nullable.undefined;
  charAtNullable(3, "abc") == Js.Nullable.undefined;
  ```
*/
let charAtNullable: (int, string) => Js.Nullable.t(string) =
  (i, str) => Js.String.get(str, i) |> Js.Nullable.return;

/**
  `charAtOrThrow(n, str)` returns a string consisting of the character at
  location `n` in the string. The first character in a string has position zero.

  If `n` is out of bounds, `charAtOrThrow()` throws a `RangeError`.

  ### Example
  ```re
  charAtOrThrow(0, "abc") == "a";
  charAtOrThrow(2, "abc") == "c";
  charAtOrThrow(1, {js|대한민국|js}) == {js|한|js};
  try (charAtOrThrow(-1, "abc")) {
    | Js.Exn.Error(_) => "Invalid index"
  } == "Invalid index";
  ```
*/
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

/**
  `toList(str)` creates a list with one character of `str` per element.

  ### Example
  ```re
  toList("abc") == ["a", "b", "c"];
  toList({js|日本|js}) == [{js|日|js}, {js|本|js}];
  toList("") == [];
  ```
*/
let toList: string => list(string) =
  str =>
    Relude_List_Base.makeWithIndex(length(str), i => charAtOrThrow(i, str));

/**
  `toArray(str)` creates an array with one character of `str` per element.

  ### Example
  ```re
  toArray("abc") == [|"a", "b", "c"|];
  toArray({js|日本|js}) == [|{js|日|js}, {js|本|js}|];
  toArray("") == [| |];
  ```
*/
let toArray: string => array(string) =
  str =>
    Relude_Array_Base.makeWithIndex(length(str), i => charAtOrThrow(i, str));

/**
  In `foldLeft(f, init, str)`, `f()` is a function that takes an accumulated
  value and a string as its arguments.  `foldLeft()` starts with `init` as the
  value of an accumulator. It then calls `f()` repeatedly with each character
  in the string, moving from left to right, with the result `f(accumulator, chStr)`
  becoming the new value of the accumulator.  When all characters have been processed,
  the return value is the value of the accumulator.

  ### Example
  ```re
  let valueOfChar = (chStr) => {int_of_float(Js.String.charCodeAt(0, chStr))};
  foldLeft( (acc, chStr) => {acc + valueOfChar(chStr)}, 0, "abc") == 97 + 98 + 99;
  foldLeft( (acc, chStr) => {acc ++ "-" ++ chStr}, "", "abc") == "-a-b-c";
  ```
*/
let foldLeft: (('b, string) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List_Instances.foldLeft(f, init, toList(str));

/**
  In `foldRight(f, init, str)`, `f()` is a function that takes a string and
  an accumulator as its arguments.  `foldRight()` starts with `init` as the
  value of an accumulator. It then calls `f()` repeatedly with each character
  in the string, moving from right to left, with the result `f(chStr, accumulator)`
  becoming the new value of the accumulator.  When all characters have been processed,
  the return value is the value of the accumulator.

  ### Example
  ```re
  let valueOfChar = (chStr) => {int_of_float(Js.String.charCodeAt(0, chStr))};
  foldRight( (chStr, acc) => {acc + valueOfChar(chStr)}, 0, "abc") == 97 + 98 + 99;
  foldRight( (chStr, acc) => {acc ++ "-" ++ chStr}, "", "abc") == "-c-b-a";
  ```
*/
let foldRight: ((string, 'b) => 'b, 'b, string) => 'b =
  (f, init, str) => Relude_List_Instances.foldRight(f, init, toList(str));

/**
 * Show function for string (identity)
 */
let show: string => string = a => a;

/**
 * SHOW module for string
 */
module Show: BsAbstract.Interface.SHOW with type t = string = {
  type t = string;
  let show = show;
};

/**
  `eq(s1, s2)` is a synonym for `s1 == s2`
*/
let eq: (string, string) => bool = (a, b) => a == b;

module Eq: BsAbstract.Interface.EQ with type t = string = {
  type t = string;
  let eq = eq;
};
include Relude_Extensions_Eq.EqExtensions(Eq);

/**
 * Compares two strings
 */
let compare = BsAbstract.String.Ord.compare;

module Ord: BsAbstract.Interface.ORD with type t = string = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

/**
 * Map module with a string key
 */
module Map = Relude_Map.WithOrd(Ord);

/**
 * Set module for strings
 */
module Set = Relude_Set.WithOrd(Ord);

/**
  `endsWith(~search, input)` returns `true` if `input` ends with the characters in `target`;
  `false` otherwise.

  ### Example
  ```re
  endsWith(~search="ing", "programming") == true;
  endsWith(~search="ing", "program") == false;
  endsWith(~search="ing", "in") == false;
  endsWith(~search="", "everything") == true;
  ```
*/
let endsWith = (~search: string, input: string): bool =>
  Js.String.endsWith(search, input);

/**
  `startsWith(~search, input)` returns `true` if `input` starts with the characters in `search`;
  `false` otherwise.

  ### Example
  ```re
  startsWith(~search="pro", "programming") == true;
  startsWith(~search="pre", "program") == false;
  startsWith(~search="pre", "pr") == false;
  startsWith(~search="", "everything") == true;
  ```
*/
let startsWith = (~search: string, input: string): bool =>
  Js.String.startsWith(search, input);

/**
  `contains(~search, input)` returns `true` if `search`
  appears anywhere in `input`; `false` otherwise.

  ### Example
  ```re
  contains(~search="cat", "catalog") == true;
  contains(~search="cat", "scatter") == true;
  contains(~search="log", "catalog") == true;
  contains(~search="ato", "fraction") == false;
  ```
*/
let contains = (~search: string, input: string): bool =>
  Js.String.includes(search, input);

/**
  `indexOf(test, str)` returns `Some(n)`, where `n`
  is the starting position of the first occurrence of `test`
  within `str`. If `test` is not in `str`, the return value
  is `None`.

  ### Example
  ```re
  indexOf("cat", "catalog") == Some(0);
  indexOf("cat", "scatter") == Some(1);
  indexOf("in", "stringing") == Some(3);
  indexOf("xyz", "blah") == None;
  ```
*/
let indexOf = (~search: string, input: string): option(int) => {
  let index = Js.String.indexOf(search, input);
  if (index < 0) {
    None;
  } else {
    Some(index);
  };
};

/**
  `lastIndexOf(test, str)` returns `Some(n)`, where `n`
  is the starting position of the last occurrence of `test`
  within `str`. If `test` is not in `str`, the return value
  is `false`.

  ### Example
  ```re
  lastIndexOf("cat", "catalog") == Some(0);
  lastIndexOf("cat", "scatter") == Some(1);
  lastIndexOf("in", "stringing") == Some(6);
  lastIndexOf("xyz", "blah") == None;
  ```
*/
let lastIndexOf = (~search: string, input: string): option(int) => {
  let index = Js.String.lastIndexOf(search, input);
  if (index < 0) {
    None;
  } else {
    Some(index);
  };
};

/**
  `slice(n1, n2, str)` returns the substring of `str` starting at
  character `n1` up to but not including `n2`.

  If either `n1` or `n2` is negative, then it is evaluated as
  `length(str) - n1` (or `length(str) - n2`).

  If `n2` is greater than the length of `str`, then it is treated as `length(str)`.

  If `n1` is greater than n2`, `slice()` returns the empty string.

  ### Example
  ```re
  slice(2, 5, "abcdefg") == "cde";
  slice(2, 9, "abcdefg") == "cdefg";
  slice(-4, -2, "abcdefg") == "de";
  slice(5, 1, "abcdefg") == "";
  ```
*/
let slice: (int, int, string) => string =
  (fromIndex, toIndex, input) =>
    Js.String.slice(~from=fromIndex, ~to_=toIndex, input);

/**
  `sliceToEnd(n, str)` returns the substring of `str` starting
  at character `n` to the end of the string

  If `n` is negative, then it is evaluated as `length(str) - n`.

  If `n` is greater than the length of `str`, then `sliceToEnd()`
  returns the empty string.

  ### Example
  ```re
  sliceToEnd(4, "abcdefg") == "efg";
  sliceToEnd(-2, "abcdefg") == "fg";
  sliceToEnd(7, "abcdefg") == "";
  ```
*/
let sliceToEnd: (int, string) => string =
  (fromIndex, str) => Js.String.sliceToEnd(~from=fromIndex, str);

/**
  `splitArray(delimiter, str)` splits the given `str`
  at every occurrence of `delimiter` and returns an array
  of the resulting substrings.

  ### Example
  ```re
  splitArray("-", "2019-01-02") == [|"2019", "01", "02"|];
  splitArray(",", "a,b,,c") == [|"a", "b", "", "c"|];
  splitArray("::", "good::better::best") == [|"good", "better", "best"|];
  splitArray(";", "has-no-delimiter") == [|"has-no-delimiter"|];
  ```
*/
let splitArray = (~delimiter: string, input: string): array(string) =>
  Js.String.split(delimiter, input);

/**
 * Alias for splitArray
 */
let splitAsArray = splitArray;

/**
  `splitList(delimiter, str)` splits the given `str`
  at every occurrence of `delimiter` and returns a list
  of the resulting substrings.

  ### Example
  ```re
  splitList("-", "2019-01-02") == ["2019", "01", "02"];
  splitList(",", "a,b,,c") == ["a", "b", "", "c"];
  splitList("::", "good::better::best") == ["good", "better", "best"];
  splitList(";", "has-no-delimiter") == ["has-no-delimiter"];
  ```
*/
let splitList = (~delimiter: string, input: string): list(string) =>
  splitArray(~delimiter, input) |> Relude_List_Instances.fromArray;

/**
 * Alias for splitlist
 */
let splitAsList = splitList;

/**
  `splitAt(index, str)` splits the string at the given index,
  returning a tuple of the parts. If `index` is negative,
  it is evaluated as `length(str) - index`.

  ### Example
  ```re
  splitAt(4, "abcdefg") == ("abcd", "efg");
  splitAt(0, "abcdefg") == ("", "abcdefg");
  splitAt(7, "abcdefg") == ("abcdefg", "");
  splitAt(8, "abcdefg") == ("abcdefg", "");
  splitAt(-3, "abcdefg") == ("abcd", "efg");
  splitAt(-9, "abcdefg") == ("", "abcdefg");
  ```
*/
let splitAt: (int, string) => (string, string) =
  (index, input) => (slice(0, index, input), sliceToEnd(index, input));

/**
  `mapChars(f, str)` applies the function `f()` to each character of the string,
  returning a new string.

  ### Example
  ```re
  let duplicate = (ch) => {ch ++ ch};
  mapChars(duplicate, "abcde") == "aabbccddee";
  let capsOnly = (ch) => {(ch >= "A" && ch <= "Z") ? ch : ""};
  mapChars(capsOnly, "AbCdE") == "ACE";
  ```
*/
let mapChars: (string => string, string) => string =
  (f, str) =>
    toList(str) |> Relude_List_Instances.foldMap((module Monoid), f);

/**
 * Pads the string to `targetLength` using `padWith` as a repeated padding on the left side of the `input` string
 */
let padStart: (~targetLength: int, ~padWith: string=?, string) => string =
  (~targetLength, ~padWith=" ", input) => {
    let inputLength = input |> length;
    let padWithLength = padWith |> length;
    if (inputLength >= targetLength) {
      // Input already longer than target
      input;
    } else if (padWithLength == 0) {
      // padWith is empty, can't do anything
      input;
    } else {
      let padLength = targetLength - inputLength;
      let padTimes = padLength / padWithLength + 1; // Add one so we get one extra, which we'll truncate
      let pad = repeat(padTimes, padWith) |> slice(0, padLength);
      pad ++ input;
    };
  };

/**
 * Pads the string to `targetLength` using `padWith` as a repeated padding on the right side of the `input` string
 */
let padEnd: (~targetLength: int, ~padWith: string=?, string) => string =
  (~targetLength, ~padWith=" ", input) => {
    let inputLength = input |> length;
    let padWithLength = padWith |> length;
    if (inputLength >= targetLength) {
      // Input already longer than target
      input;
    } else if (padWithLength == 0) {
      // padWith is empty, can't do anything
      input;
    } else {
      let padLength = targetLength - inputLength;
      let padTimes = padLength / padWithLength + 1; // Add one so we get one extra, which we'll truncate
      let pad = repeat(padTimes, padWith) |> slice(0, padLength);
      input ++ pad;
    };
  };

/**
  `replaceFirst(target, newValue, str)` replaces the first occurrence of `target` with
  `newValue` in `str`, returning a new string.

  ### Example
  ```re
  replaceFirst("in", "t", "the rain in spain") == "the rat in spain";
  replaceFirst("in", "t", "playground") == "playground";
  ```
 */
let replaceFirst =
    (~search: string, ~replaceWith: string, input: string): string =>
  Js.String.replace(search, replaceWith, input);

/**
  `replaceEach(target, newValue, str)`replaces each occurrence of
  `target` with `newValue` in `str`, returning a new string.

  ### Example
  ```re
  replaceEach("in", "t", "the rain in spain") == "the rat t spat";
  replaceEach("in", "t", "playground") == "playground";
  ```
*/
let replaceEach =
    (~search: string, ~replaceWith: string, input: string): string =>
  splitList(~delimiter=search, input) |> String.concat(replaceWith);

/**
  `replaceRegex(targetRe, newValue, str)` replaces the matched
  regular expression `targetRe` with `newValue` in `str`, returning a new string.

  If you use parentheses to store matching substrings in your pattern (as in the
  last two examples), you may refer to them as `$1`, `$2` etc. in your replacement
  pattern.

  ### Example
  ```re
  replaceRegex([%re"/b[aeiou]g/"], "---", "The big bog bug") == "The --- bog bug";
  replaceRegex([%re"/b[aeiou]g/g"], "---", "The big bog bug") == "The --- --- ---";
  replaceRegex([%re"/b([aeiou])g/g"], "$1", "The big bog bug") == "The i o u";
  replaceRegex([%re"/(\\w+)\\s+(\\w+)/"], "$2, $1", "Clyde Tolson") == "Tolson, Clyde";
  ```
*/
let replaceRegex =
    (~search: Js.Re.t, ~replaceWith: string, input: string): string =>
  Js.String.replaceByRe(search, replaceWith, input);

/**
  `removeFirst(target, str)` returns a new string with the first occurrence of `target`
  removed from `str`.

  ### Example
  ```re
  removeFirst("the ", "Paris in the the spring") == "Paris in the spring";
  removeFirst("the ", "ReasonML is cool") == "ReasonML is cool";
  ```
*/
let removeFirst = (~search: string, input: string): string =>
  replaceFirst(~search, ~replaceWith="", input);

/**
  `removeEach(target, str)` returns a new string with every occurrence of `target`
  removed from `str`.

  ### Example
  ```re
  removeEach("the ", "Paris in the the spring") == "Paris in spring";
  removeEach("the ", "ReasonML is cool") == "ReasonML is cool";
  ```
*/
let removeEach = (~search: string, input: string): string =>
  replaceEach(~search, ~replaceWith="", input);

/**
  `fromInt(n)` returns `n` as a string. This function is
  a synonym for the built-in `string_of_int()`.
*/
let fromInt: int => string = string_of_int;

/**
  `toInt(str)` returns `Some(n)` if `str` is a valid
  string representation of the integer `n`. Otherwise,
  the return value is `None`.

  ### Example
  ```re
  toInt("42") == Some(42);
  toInt("42.3") == None;
  toInt("four") == None;
  toInt("") == None;
  ```
*/
let toInt: string => option(int) =
  v =>
    try(Some(int_of_string(v))) {
    | _ => None
    };

/**
  `fromFloat(x)` converts the value to a string representation.
  Note that, as in the last examples, it may not be
  exactly the same as the representation you used
  when specifying `x`.

  ### Example
  ```re
  fromFloat(-3.5) == "-3.5";
  fromFloat(6.02E23) == "6.02e+23";
  fromFloat(1.0e3) == "1000";
  ```
*/
let fromFloat: float => string = Js.Float.toString;

/**
  `toFloat(str)` returns `Some(x)` if `str` is a valid
  string representation of the float value `x`. Otherwise,
  the return value is `None`.

  ### Example
  ```re
  toFloat("42") == Some(42.0);
  toFloat("42.3") == Some(42.3);
  toFloat("123400000") == Some(123400000.0);
  toFloat("four") == None;
  toFloat("") == None;
  ```
*/
let toFloat: string => option(float) =
  v =>
    try(Some(float_of_string(v))) {
    | _ => None
    };