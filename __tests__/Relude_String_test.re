open Jest;
open Expect;

module String = Relude_String;

describe("String", () => {
  test("length empty string", () =>
    expect(String.length("")) |> toEqual(0)
  );

  test("length non-empty string", () =>
    expect(String.length("abc")) |> toEqual(3)
  );

  test("trim empty string", () =>
    expect(String.trim("")) |> toEqual("")
  );

  test("trim whitespace string", () =>
    expect(String.trim("  \t")) |> toEqual("")
  );

  test("trim no whitespace string", () =>
    expect(String.trim("test")) |> toEqual("test")
  );

  test("trim whitespace string", () =>
    expect(String.trim(" \t\t   hello\t world\t \t  "))
    |> toEqual("hello\t world")
  );

  test("isEmpty empty string", () =>
    expect(String.isEmpty("")) |> toEqual(true)
  );

  test("isEmpty non-empty string", () =>
    expect(String.isEmpty("abc")) |> toEqual(false)
  );

  test("isNotEmpty empty string", () =>
    expect(String.isNotEmpty("")) |> toEqual(false)
  );

  test("isNotEmpty non-empty string", () =>
    expect(String.isNotEmpty("abc")) |> toEqual(true)
  );

  test("isWhitespace empty string", () =>
    expect(String.isWhitespace("")) |> toEqual(true)
  );

  test("toNonWhitespace empty string", () =>
    expect(String.toNonWhitespace("")) |> toEqual(None)
  );

  test("toNonWhitespace non-empty string", () =>
    expect(String.toNonWhitespace(" hi ")) |> toEqual(Some(" hi "))
  );

  test("makeWithIndex", () =>
    expect(String.makeWithIndex(5, i => string_of_int(i)))
    |> toEqual("01234")
  );

  test("repeat", () =>
    expect(String.repeat(5, "hi ")) |> toEqual("hi hi hi hi hi ")
  );

  test("toUpperCase", () =>
    expect(String.toUpperCase("Relude!")) |> toEqual("RELUDE!")
  );

  test("toLowerCase", () =>
    expect(String.toLowerCase("ReLude!")) |> toEqual("relude!")
  );

  test("get success", () =>
    expect(String.get(2, "abcdefg")) |> toEqual(Some("c"))
  );

  test("get failure", () =>
    expect(String.get(7, "abcdefg")) |> toEqual(None)
  );

  test("getNullable success", () =>
    expect(String.getNullable(2, "abcdefg"))
    |> toEqual(Js.Nullable.return("c"))
  );

  test("getNullable failure", () =>
    expect(String.getNullable(7, "abcdefg"))
    |> toEqual(Js.Nullable.undefined)
  );

  test("getOrThrow success", () =>
    expect(String.getOrThrow(2, "abcdefg")) |> toEqual("c")
  );

  test("getOrThrow failure", () =>
    expect(() =>
      String.getOrThrow(7, "abcdefg")
    ) |> toThrow
  );

  test("toList", () =>
    expect(String.toList("abcde")) |> toEqual(["a", "b", "c", "d", "e"])
  );

  test("toCharArray", () =>
    expect(String.toArray("abcde")) |> toEqual([|"a", "b", "c", "d", "e"|])
  );

  test("foldLeft", () =>
    expect(
      String.foldLeft(
        (acc, str) => Relude_List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["a", "b", "c"])
  );

  test("foldRight", () =>
    expect(
      String.foldRight(
        (str, acc) => Relude_List.concat(acc, [str]),
        [],
        "abc",
      ),
    )
    |> toEqual(["c", "b", "a"])
  );

  test("concat", () =>
    expect(String.concat("a", "b")) |> toEqual("ab")
  );

  test("concatArray", () =>
    expect(String.concatArray([|"a", "b"|])) |> toEqual("ab")
  );

  test("endsWith", () =>
    expect(String.endsWith("de", "abcde")) |> toEqual(true)
  );

  test("startsWith", () =>
    expect(String.startsWith("ab", "abcde")) |> toEqual(true)
  );

  test("contains", () =>
    expect(String.contains("cd", "abcde")) |> toEqual(true)
  );

  test("indexOf success", () =>
    expect(String.indexOf("cd", "abcde")) |> toEqual(Some(2))
  );

  test("indexOf failure", () =>
    expect(String.indexOf("x", "abcde")) |> toEqual(None)
  );

  test("lastIndexOf success", () =>
    expect(String.lastIndexOf("cd", "abcdecd")) |> toEqual(Some(5))
  );

  test("indexOf failure", () =>
    expect(String.lastIndexOf("x", "abcde")) |> toEqual(None)
  );

  test("replace", () =>
    expect(String.replace("b", "xyz", "abcde")) |> toEqual("axyzcde")
  );

  test("replaceRegex", () =>
    expect(String.replaceRegex([%re "/b/"], "xyz", "abcde"))
    |> toEqual("axyzcde")
  );

  test("slice", () =>
    expect(String.slice(2, 5, "abcdefg")) |> toEqual("cde")
  );

  test("sliceToEnd", () =>
    expect(String.sliceToEnd(2, "abcdefg")) |> toEqual("cdefg")
  );

  test("splitArray", () =>
    expect(String.splitArray(",", "a,b,c")) |> toEqual([|"a", "b", "c"|])
  );

  test("splitList", () =>
    expect(String.splitList(",", "a,b,c")) |> toEqual(["a", "b", "c"])
  );

  test("toInt success", () =>
    expect(String.toInt("3")) |> toEqual(Some(3))
  );

  test("toInt failure on empty", () =>
    expect(String.toInt("")) |> toEqual(None)
  );

  test("toInt failure on mixed", () =>
    expect(String.toInt("3a")) |> toEqual(None)
  );

  test("toInt failure on float", () =>
    expect(String.toInt("3.14")) |> toEqual(None)
  );

  test("toInt failure on alpha", () =>
    expect(String.toInt("abc")) |> toEqual(None)
  );

  test("toFloat success" ,() =>
    expect(String.toFloat("3.14")) |> toEqual(Some(3.14))
  );

  test("toFloat success on int", () =>
    expect(String.toFloat("3")) |> toEqual(Some(3.))
  );

  test("toFloat failure on empty" ,() =>
    expect(String.toFloat("")) |> toEqual(None)
  );

  test("toFloat failure on mixed" ,() =>
    expect(String.toFloat("3.14a")) |> toEqual(None)
  );

  test("toFloat failure on alpha", () =>
    expect(String.toFloat("abc")) |> toEqual(None)
  )
});
