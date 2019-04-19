open Jest;
open Expect;

module Str = Relude_String;

describe("String", () => {
  test("length empty string", () =>
    expect(Str.length("")) |> toEqual(0)
  );

  test("length non-empty string", () =>
    expect(Str.length("abc")) |> toEqual(3)
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

  test("isWhitespace empty string", () =>
    expect(Str.isWhitespace("")) |> toEqual(true)
  );

  test("toNonWhitespace empty string", () =>
    expect(Str.toNonWhitespace("")) |> toEqual(None)
  );

  test("toNonWhitespace non-empty string", () =>
    expect(Str.toNonWhitespace(" hi ")) |> toEqual(Some(" hi "))
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

  test("toList", () =>
    expect(Str.toList("abcde")) |> toEqual(["a", "b", "c", "d", "e"])
  );

  test("toCharArray", () =>
    expect(Str.toArray("abcde")) |> toEqual([|"a", "b", "c", "d", "e"|])
  );

  test("foldLeft", () =>
    expect(
      Str.foldLeft(
        (acc, str) => Relude_List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["a", "b", "c"])
  );

  test("foldRight", () =>
    expect(
      Str.foldRight(
        (str, acc) => Relude_List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["c", "b", "a"])
  );

  test("concat", () =>
    expect(Str.concat("a", "b")) |> toEqual("ab")
  );

  test("concatArray", () =>
    expect(Str.concatArray([|"a", "b"|])) |> toEqual("ab")
  );

  test("endsWith", () =>
    expect(Str.endsWith("de", "abcde")) |> toEqual(true)
  );

  test("startsWith", () =>
    expect(Str.startsWith("ab", "abcde")) |> toEqual(true)
  );

  test("contains", () =>
    expect(Str.contains("cd", "abcde")) |> toEqual(true)
  );

  test("indexOf success", () =>
    expect(Str.indexOf("cd", "abcde")) |> toEqual(Some(2))
  );

  test("indexOf failure", () =>
    expect(Str.indexOf("x", "abcde")) |> toEqual(None)
  );

  test("lastIndexOf success", () =>
    expect(Str.lastIndexOf("cd", "abcdecd")) |> toEqual(Some(5))
  );

  test("indexOf failure", () =>
    expect(Str.lastIndexOf("x", "abcde")) |> toEqual(None)
  );

  test("replaceFirst", () =>
    expect(Str.replaceFirst("b", "xyz", "abcde")) |> toEqual("axyzcde")
  );

  test("replaceEach", () =>
    expect(Str.replaceEach("b", "xyz", "abcdeb")) |> toEqual("axyzcdexyz")
  );

  test("replaceEach special characters", () =>
    expect(Str.replaceEach("+", " ", "a+b+c")) |> toEqual("a b c")
  );

  test("replaceRegex", () =>
    expect(Str.replaceRegex([%re "/b/"], "xyz", "abcde"))
    |> toEqual("axyzcde")
  );

  test("removeFirst", () =>
    expect(Str.removeFirst(" ", "foo bar baz")) |> toEqual("foobar baz")
  );

  test("removeEach", () =>
    expect(Str.removeEach(" ", "foo bar baz")) |> toEqual("foobarbaz")
  );

  test("slice", () =>
    expect(Str.slice(2, 5, "abcdefg")) |> toEqual("cde")
  );

  test("sliceToEnd", () =>
    expect(Str.sliceToEnd(2, "abcdefg")) |> toEqual("cdefg")
  );

  test("splitArray", () =>
    expect(Str.splitArray(",", "a,b,c")) |> toEqual([|"a", "b", "c"|])
  );

  test("splitList", () =>
    expect(Str.splitList(",", "a,b,c")) |> toEqual(["a", "b", "c"])
  );

  test("mapChars", () =>
    expect(Str.mapChars(s => s ++ s, "abc")) |> toEqual("aabbcc")
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
});
