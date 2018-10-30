
open Jest;
open Expect;

let (<<) = Function.(<<);
let (>>) = Function.(>>);

describe("Function", () => {
  test("identity returns the input", () => {
    expect(Function.identity(1)) |> toBe(1);
  });

  test("const returns the first arg", () => {
    expect(Function.const(1, 2)) |> toBe(1);
  });

  test("compose combines functions from right to left", () => {
    let plus5 = a => a + 5;
    let times3 = a => a * 3;
    expect((plus5 << times3)(10)) |> toBe(35);
  });

  test("flipCompose combines functions from left to right", () => {
    let plus5 = a => a + 5;
    let times3 = a => a * 3;
    expect((plus5 >> times3)(10)) |> toBe(45);
  });
});
