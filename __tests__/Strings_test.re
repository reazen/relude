open Jest;
open Expect;

describe("String", () => {
  test("length empty string", () => {
    expect(Strings.length("")) |> toEqual(0)
  });

  test("length non-empty string", () => {
    expect(Strings.length("abc")) |> toEqual(3)
  });

  test("isEmpty empty string", () => {
    expect(Strings.isEmpty("")) |> toEqual(true)
  });

  test("isEmpty non-empty string", () => {
    expect(Strings.isEmpty("abc")) |> toEqual(false)
  });

  test("isNotEmpty empty string", () => {
    expect(Strings.isNotEmpty("")) |> toEqual(false)
  });

  test("isNotEmpty non-empty string", () => {
    expect(Strings.isNotEmpty("abc")) |> toEqual(true)
  });
});
