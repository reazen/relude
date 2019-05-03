
open Jest;
open Expect;

module Exn = Relude_Js_Exn;

describe("Js.Exn", () => {
  test("make", () => {
    let e = Exn.make("my error");
    expect(Js.Exn.message(e)) |> toEqual(Some("my error"));
  });
});
