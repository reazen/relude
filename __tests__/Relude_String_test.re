open Jest;
open Expect;

module String = Relude_String;

describe("String", () => {
  test("length empty string", () => {
    expect(String.length("")) |> toEqual(0)
  });

  test("length non-empty string", () => {
    expect(String.length("abc")) |> toEqual(3)
  });

  test("trim empty string", () => {
    expect(String.trim("")) |> toEqual("");
  });

  test("trim whitespace string", () => {
    expect(String.trim("  \t")) |> toEqual("");
  });

  test("trim no whitespace string", () => {
    expect(String.trim("test")) |> toEqual("test");
  });

  test("trim whitespace string", () => {
    expect(String.trim(" \t\t   hello\t world\t \t  ")) |> toEqual("hello\t world");
  });

  test("isEmpty empty string", () => {
    expect(String.isEmpty("")) |> toEqual(true)
  });

  test("isEmpty non-empty string", () => {
    expect(String.isEmpty("abc")) |> toEqual(false)
  });

  test("isNotEmpty empty string", () => {
    expect(String.isNotEmpty("")) |> toEqual(false)
  });

  test("isNotEmpty non-empty string", () => {
    expect(String.isNotEmpty("abc")) |> toEqual(true)
  });

  test("isWhitespace empty string", () => {
    expect(String.isWhitespace("")) |> toEqual(true)
  });
});
