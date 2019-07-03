open Jest;
open Expect;

module Function = Relude_Function;

let (<<) = Function.Infix.(<<);
let (>>) = Function.Infix.(>>);

let f2 = (a, b) => a + b;
let f3 = (a, b, c) => a + b + c;
let f4 = (a, b, c, d) => a + b + c + d;
let f5 = (a, b, c, d, e) => a + b + c + d + e;

let ft2 = ((a, b)) => a + b;
let ft3 = ((a, b, c)) => a + b + c;
let ft4 = ((a, b, c, d)) => a + b + c + d;
let ft5 = ((a, b, c, d, e)) => a + b + c + d + e;

describe("Function", () => {
  test("identity returns the input", () =>
    expect(Function.identity(1)) |> toBe(1)
  );

  test("const returns the first arg", () =>
    expect(Function.const(1, 2)) |> toBe(1)
  );

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

  test("curry2", () =>
    expect(Function.curry2(ft2, 1, 2)) |> toEqual(3)
  );

  test("curry3", () =>
    expect(Function.curry3(ft3, 1, 2, 3)) |> toEqual(6)
  );

  test("curry4", () =>
    expect(Function.curry4(ft4, 1, 2, 3, 4)) |> toEqual(10)
  );

  test("curry5", () =>
    expect(Function.curry5(ft5, 1, 2, 3, 4, 5)) |> toEqual(15)
  );

  test("uncurry2", () =>
    expect(Function.uncurry2(f2, (1, 2))) |> toEqual(3)
  );

  test("uncurry3", () =>
    expect(Function.uncurry3(f3, (1, 2, 3))) |> toEqual(6)
  );

  test("uncurry4", () =>
    expect(Function.uncurry4(f4, (1, 2, 3, 4))) |> toEqual(10)
  );

  test("uncurry5", () =>
    expect(Function.uncurry5(f5, (1, 2, 3, 4, 5))) |> toEqual(15)
  );
});