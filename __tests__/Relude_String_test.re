open Jest;
open Expect;

module Str = Relude.String;

describe("String", () => {
  test("length empty string", () =>
    expect(Str.length(Str.empty)) |> toEqual(0)
  );

  test("length non-empty string", () =>
    expect(Str.length("abc")) |> toEqual(3)
  );

  test("isEmpty empty string", () =>
    expect(Str.isEmpty("")) |> toEqual(true)
  );

  test("isEmpty non-empty string", () =>
    expect(Str.isEmpty("abc")) |> toEqual(false)
  );

  test("isNotEmpty empty string", () =>
    expect(Str.isNotEmpty("")) |> toEqual(false)
  );

  test("isNotEmpty non-empty string", () =>
    expect(Str.isNotEmpty("abc")) |> toEqual(true)
  );

  test("toNonEmpty (empty)", () =>
    expect(Str.toNonEmpty("")) |> toEqual(None)
  );

  test("toNonEmpty (non-empty)", () =>
    expect(Str.toNonEmpty("abc")) |> toEqual(Some("abc"))
  );

  test("trim empty string", () =>
    expect(Str.trim("")) |> toEqual("")
  );

  test("trim whitespace string", () =>
    expect(Str.trim("  \t")) |> toEqual("")
  );

  test("trim no whitespace string", () =>
    expect(Str.trim("test")) |> toEqual("test")
  );

  test("trim whitespace string", () =>
    expect(Str.trim(" \t\t   hello\t world\t \t  "))
    |> toEqual("hello\t world")
  );

  test("isWhitespace empty string", () =>
    expect(Str.isWhitespace("")) |> toEqual(true)
  );

  test("isNonWhitespace (only newlines)", () =>
    expect(Str.isNonWhitespace("\n\n")) |> toEqual(false)
  );

  test("isNonWhitespace (un-trimmed)", () =>
    expect(Str.isNonWhitespace("\nfoo\n")) |> toEqual(true)
  );

  test("toNonWhitespace empty string", () =>
    expect(Str.toNonWhitespace("")) |> toEqual(None)
  );

  test("toNonWhitespace non-empty string", () =>
    expect(Str.toNonWhitespace(" hi ")) |> toEqual(Some(" hi "))
  );

  test("concat", () =>
    expect(Str.concat("a", "b")) |> toEqual("ab")
  );

  test("concatNamed", () =>
    expect(Str.concatNamed(~prefix="abc", "def")) |> toEqual("abcdef")
  );

  test("concatArray", () =>
    expect(Str.concatArray([|"a", "b"|])) |> toEqual("ab")
  );

  test("concatList", () =>
    expect(Str.concatList(["foo", " ", "bar"])) |> toEqual("foo bar")
  );

  test("guard true", () =>
    expect(Str.guard(true, "hi")) |> toEqual("hi")
  );

  test("guard false", () =>
    expect(Str.guard(false, "hi")) |> toEqual("")
  );

  test("power", () =>
    expect(Str.power("hi", 4)) |> toEqual("hihihihi")
  );

  test("make", () =>
    expect(Str.make(true)) |> toEqual("true")
  );

  test("makeWithIndex", () =>
    expect(Str.makeWithIndex(5, i => string_of_int(i))) |> toEqual("01234")
  );

  test("repeat", () =>
    expect(Str.repeat(5, "hi ")) |> toEqual("hi hi hi hi hi ")
  );

  test("toUpperCase", () =>
    expect(Str.toUpperCase("Relude!")) |> toEqual("RELUDE!")
  );

  test("toLowerCase", () =>
    expect(Str.toLowerCase("ReLude!")) |> toEqual("relude!")
  );

  test("fromCharCode", () =>
    expect(Str.fromCharCode(65)) |> toEqual("A")
  );

  test("charCodeAt (success)", () =>
    expect(Str.charCodeAt(0, "abc")) |> toEqual(Some(97))
  );

  test("charCodeAt (negative index)", () =>
    expect(Str.charCodeAt(-1, "abc")) |> toEqual(None)
  );

  test("charCodeAt (empty string)", () =>
    expect(Str.charCodeAt(0, "")) |> toEqual(None)
  );

  test("charAt success", () =>
    expect(Str.charAt(2, "abcdefg")) |> toEqual(Some("c"))
  );

  test("charAt failure", () =>
    expect(Str.charAt(7, "abcdefg")) |> toEqual(None)
  );

  test("charAtNullable success", () =>
    expect(Str.charAtNullable(2, "abcdefg"))
    |> toEqual(Js.Nullable.return("c"))
  );

  test("charAtNullable failure", () =>
    expect(Str.charAtNullable(7, "abcdefg"))
    |> toEqual(Js.Nullable.undefined)
  );

  test("charAtOrThrow success", () =>
    expect(Str.charAtOrThrow(2, "abcdefg")) |> toEqual("c")
  );

  test("charAtOrThrow failure", () =>
    expect(() =>
      Str.charAtOrThrow(7, "abcdefg")
    ) |> toThrow
  );

  test("charAtOrEmpty (in range)", () =>
    expect(Str.charAtOrEmpty(0, "abc")) |> toEqual("a")
  );

  test("charAtOrEmpty (empty string)", () =>
    expect(Str.charAtOrEmpty(0, "")) |> toEqual("")
  );

  test("charAtOrEmpty (above range)", () =>
    expect(Str.charAtOrEmpty(2, "a")) |> toEqual("")
  );

  test("charAtOrEmpty (below range)", () =>
    expect(Str.charAtOrEmpty(-1, "abc")) |> toEqual("")
  );

  test("toList", () =>
    expect(Str.toList("abcde")) |> toEqual(["a", "b", "c", "d", "e"])
  );

  test("toArray", () =>
    expect(Str.toArray("abcde")) |> toEqual([|"a", "b", "c", "d", "e"|])
  );

  test("foldLeft", () =>
    expect(
      Str.foldLeft(
        (acc, str) => Relude.List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["a", "b", "c"])
  );

  test("foldRight", () =>
    expect(
      Str.foldRight(
        (str, acc) => Relude.List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["c", "b", "a"])
  );

  test("endsWith", () =>
    expect(Str.endsWith(~search="de", "abcde")) |> toEqual(true)
  );

  test("startsWith", () =>
    expect(Str.startsWith(~search="ab", "abcde")) |> toEqual(true)
  );

  test("contains", () =>
    expect(Str.contains(~search="cd", "abcde")) |> toEqual(true)
  );

  test("indexOf success", () =>
    expect(Str.indexOf(~search="cd", "abcde")) |> toEqual(Some(2))
  );

  test("indexOf failure", () =>
    expect(Str.indexOf(~search="x", "abcde")) |> toEqual(None)
  );

  test("lastIndexOf success", () =>
    expect(Str.lastIndexOf(~search="cd", "abcdecd")) |> toEqual(Some(5))
  );

  test("indexOf failure", () =>
    expect(Str.lastIndexOf(~search="x", "abcde")) |> toEqual(None)
  );

  test("replaceFirst", () =>
    expect(Str.replaceFirst(~search="b", ~replaceWith="xyz", "abcde"))
    |> toEqual("axyzcde")
  );

  test("replaceEach", () =>
    expect(Str.replaceEach(~search="b", ~replaceWith="xyz", "abcdeb"))
    |> toEqual("axyzcdexyz")
  );

  test("replaceEach special characters", () =>
    expect(Str.replaceEach(~search="+", ~replaceWith=" ", "a+b+c"))
    |> toEqual("a b c")
  );

  test("replaceRegex", () =>
    expect(
      Str.replaceRegex(~search=[%re "/b/"], ~replaceWith="xyz", "abcde"),
    )
    |> toEqual("axyzcde")
  );

  test("removeFirst", () =>
    expect(Str.removeFirst(~search=" ", "foo bar baz"))
    |> toEqual("foobar baz")
  );

  test("removeEach", () =>
    expect(Str.removeEach(~search=" ", "foo bar baz"))
    |> toEqual("foobarbaz")
  );

  test("slice", () =>
    expect(Str.slice(2, 5, "abcdefg")) |> toEqual("cde")
  );

  test("sliceToEnd", () =>
    expect(Str.sliceToEnd(2, "abcdefg")) |> toEqual("cdefg")
  );

  test("splitArray", () =>
    expect(Str.splitArray(~delimiter=",", "a,b,c"))
    |> toEqual([|"a", "b", "c"|])
  );

  test("splitList", () =>
    expect(Str.splitList(~delimiter=",", "a,b,c"))
    |> toEqual(["a", "b", "c"])
  );

  test("splitAt -7", () =>
    expect(Str.splitAt(-7, "abcdef")) |> toEqual(("", "abcdef"))
  );

  test("splitAt -6", () =>
    expect(Str.splitAt(-6, "abcdef")) |> toEqual(("", "abcdef"))
  );

  test("splitAt -4", () =>
    expect(Str.splitAt(-4, "abcdef")) |> toEqual(("ab", "cdef"))
  );

  test("splitAt -1", () =>
    expect(Str.splitAt(-1, "abcdef")) |> toEqual(("abcde", "f"))
  );

  test("splitAt 0", () =>
    expect(Str.splitAt(0, "abcdef")) |> toEqual(("", "abcdef"))
  );

  test("splitAt 2", () =>
    expect(Str.splitAt(2, "abcdef")) |> toEqual(("ab", "cdef"))
  );

  test("splitAt 6", () =>
    expect(Str.splitAt(6, "abcdef")) |> toEqual(("abcdef", ""))
  );

  test("splitAt 7", () =>
    expect(Str.splitAt(7, "abcdef")) |> toEqual(("abcdef", ""))
  );

  test("mapChars", () =>
    expect(Str.mapChars(s => s ++ s, "abc")) |> toEqual("aabbcc")
  );

  testAll(
    "padStart",
    [
      ("abc", "", 10, "abc"),
      ("abc", " ", 10, "       abc"),
      ("abc", "foo", 10, "foofoofabc"),
      ("abc", "123456", 6, "123abc"),
      ("abc", "0", 8, "00000abc"),
      ("abc", " ", 1, "abc"),
    ],
    ((input, padWith, targetLength, expected)) => {
    expect(input |> Str.padStart(~targetLength, ~padWith))
    |> toEqual(expected)
  });

  testAll(
    "padEnd",
    [
      ("abc", "", 10, "abc"),
      ("abc", " ", 10, "abc       "),
      ("abc", "foo", 10, "abcfoofoof"),
      ("abc", "123456", 6, "abc123"),
      ("abc", "0", 8, "abc00000"),
      ("abc", " ", 1, "abc"),
    ],
    ((input, padWith, targetLength, expected)) => {
    expect(input |> Str.padEnd(~targetLength, ~padWith))
    |> toEqual(expected)
  });

  test("fromInt", () =>
    expect(Str.fromInt(-20)) |> toEqual("-20")
  );

  test("toInt success", () =>
    expect(Str.toInt("3")) |> toEqual(Some(3))
  );

  test("toInt failure on empty", () =>
    expect(Str.toInt("")) |> toEqual(None)
  );

  test("toInt failure on mixed", () =>
    expect(Str.toInt("3a")) |> toEqual(None)
  );

  test("toInt failure on float", () =>
    expect(Str.toInt("3.14")) |> toEqual(None)
  );

  test("toInt failure on alpha", () =>
    expect(Str.toInt("abc")) |> toEqual(None)
  );

  test("fromFloat", () =>
    expect(Str.fromFloat(3.1415)) |> toEqual("3.1415")
  );

  test("fromFloat (ends in zero)", () =>
    expect(Str.fromFloat(3.000)) |> toEqual("3")
  );

  test("toFloat success", () =>
    expect(Str.toFloat("3.14")) |> toEqual(Some(3.14))
  );

  test("toFloat success on int", () =>
    expect(Str.toFloat("3")) |> toEqual(Some(3.))
  );

  test("toFloat failure on empty", () =>
    expect(Str.toFloat("")) |> toEqual(None)
  );

  test("toFloat failure on mixed", () =>
    expect(Str.toFloat("3.14a")) |> toEqual(None)
  );

  test("toFloat failure on alpha", () =>
    expect(Str.toFloat("abc")) |> toEqual(None)
  );

  test("MonoidExtensions guard false", () =>
    expect(Str.guard(false, "hi")) |> toEqual("")
  );

  test("MonoidExtensions guard true", () =>
    expect(Str.guard(true, "hi")) |> toEqual("hi")
  );

  test("MonoidExtensions power", () =>
    expect(Str.power("hi", 5)) |> toEqual("hihihihihi")
  );
});
