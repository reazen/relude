open Jest;
open Expect;

module Bool = Relude_Bool;

describe("Bool", () => {
  test("ifElse true", () => {
    expect(Bool.ifElse(() => "true", () => "false", true)) |> toEqual("true")
  });

  test("ifElse false", () => {
    expect(Bool.ifElse(() => "true", () => "false", false)) |> toEqual("false")
  });
});
