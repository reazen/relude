open Jest;
open Expect;

module Identity = Relude_Identity;

describe("Identity", () => {
  test("pure", () => {
    expect(Identity.pure(5)) |> toEqual(5);
  });

  test("unwrap with pure", () => {
    expect(Identity.unwrap(Identity.pure(5))) |> toEqual(5)
  });

  test("unwrap without pure", () => {
    expect(Identity.unwrap(5)) |> toEqual(5)
  });

  test("map", () => {
    expect(Identity.map(a => a + 2, 5)) |> toEqual(7);
  });

  test("apply", () => {
    expect(Identity.apply(a => a + 2, 5)) |> toEqual(7);
  });

  test("flatMap", () => {
    expect(Identity.flatMap(5, a => a + 2)) |> toEqual(7);
  });

  test("show", () => {
    expect(Identity.show((module Relude_Int.Show), 5)) |> toEqual("5")
  });

  test("eq true", () => {
    expect(Identity.eq((module Relude_Int.Eq), 5, 5)) |> toEqual(true)
  });

  test("eq false", () => {
    expect(Identity.eq((module Relude_Int.Eq), 5, 10)) |> toEqual(false)
  });
});
