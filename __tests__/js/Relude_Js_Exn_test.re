open Jest;
open Expect;

module Exn = Relude.Js.Exn;

describe("Js.Exn", () => {
  test("make", () => {
    let e = Exn.make("my error");
    expect(Js.Exn.message(e)) |> toEqual(Some("my error"));
  });

  test("throw", () => {
    expect(() =>
      Exn.throw("my error")
    ) |> toThrow
  });

  test("unsafeFromExn Js.Exn.Error", () => {
    switch (Js.Exn.raiseError("my error")) {
    | _ => fail("fail")
    | exception exn =>
      expect(exn |> Exn.unsafeFromExn |> Js.Exn.message)
      |> toEqual(Some("my error"))
    }
  });

  test("unsafeFromExn unknown", () => {
    let exn: exn = [%raw {|"my error"|}];
    expect(exn |> Exn.unsafeFromExn |> Js.Exn.message)
    |> toEqual(Some("Unexpected error: my error"));
  });

  test("unsafeToExn", () => {
    Exn.make("my error")
    |> Exn.unsafeToExn
    |> Exn.unsafeFromExn
    |> Js.Exn.message
    |> expect
    |> toEqual(Some("Unexpected error: Error: my error"))
  });
});
