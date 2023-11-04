open Jest;
open Expect;

module Function = Relude.Function;
module StringArgument = {
  type t = string;
  module Type: BsBastet.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};
module FunctionWithStringArgument = Function.WithArgument(StringArgument);

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

  test("flip", () => {
    let formula = (x, y) => x + 2 * y;
    expect(formula(3, 5)) |> toBe(13) |> ignore;
    expect(Function.flip(formula, 5, 3)) |> toBe(13);
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

  test("map", () => {
    let plus5 = a => a + 5;
    let times3 = a => a * 3;
    expect(Function.map(plus5, times3, 10)) |> toBe(35);
  });

  test("apply", () => {
    let showResult = (n, x: float) =>
      "input " ++ string_of_int(n) ++ " yields " ++ Js.Float.toString(x);

    let cube = x => float_of_int(x * x * x);

    expect(Function.apply(showResult, cube, 5))
    |> toBe("input 5 yields 125");
  });

  test("pure returns the first arg", () =>
    expect(Function.pure(1, 2)) |> toBe(1)
  );

  test("bind", () => {
    let showResult = (x, n: int) =>
      "input " ++ string_of_int(n) ++ " yields " ++ Js.Float.toString(x);

    let cube = x => float_of_int(x * x * x);

    expect(Function.bind(cube, showResult, 5)) |> toBe("input 5 yields 125");
  });

  test("flatMap", () => {
    let showResult = (x, n: int) => {
      "input " ++ string_of_int(n) ++ " yields " ++ Js.Float.toString(x);
    };

    let cube = x => float_of_int(x * x * x);

    expect(Function.flatMap(showResult, cube, 5))
    |> toBe("input 5 yields 125");
  });

  test("memoize0", () => {
    let calls = ref(0);
    let f = () => {
      calls := calls^ + 1;
      string_of_int(calls^);
    };
    let memoized = Function.memoize0(f);
    let result1 = memoized();
    let result2 = memoized();
    let result3 = memoized();
    expect((calls^, result1, result2, result3))
    |> toEqual((1, "1", "1", "1"));
  });

  test("memoize1", () => {
    let calls = ref([]);
    let f = (i: int) => {
      calls := [i, ...calls^];
      string_of_int(i);
    };
    let memoized = Function.memoize1(~makeKey=string_of_int, f);
    let result1 = memoized(11);
    let result2 = memoized(11);
    let result3 = memoized(22);
    let result4 = memoized(22);
    let result5 = memoized(33);
    let result6 = memoized(33);
    expect((calls^, result1, result2, result3, result4, result5, result6))
    |> toEqual(([33, 22, 11], "11", "11", "22", "22", "33", "33"));
  });

  test("before", () => {
    let calls = ref(0);
    let f = () => {
      calls := calls^ + 1;
      calls^;
    };
    let before = Function.before(~times=3, f);
    let result1 = before();
    let result2 = before();
    let result3 = before();
    let result4 = before();
    let result5 = before();
    expect((calls^, result1, result2, result3, result4, result5))
    |> toEqual((3, 1, 2, 3, 3, 3));
  });

  test("after", () => {
    let calls = ref(0);
    let f = () => {
      calls := calls^ + 1;
      calls^;
    };
    let after = Function.after(~times=3, f);
    let result1 = after();
    let result2 = after();
    let result3 = after();
    let result4 = after();
    let result5 = after();
    expect((calls^, result1, result2, result3, result4, result5))
    |> toEqual((2, None, None, None, Some(1), Some(2)));
  });

  test("once", () => {
    let calls = ref(0);
    let f = () => {
      calls := calls^ + 1;
      calls^;
    };
    let once = Function.once(f);
    let result1 = once();
    let result2 = once();
    let result3 = once();
    expect((calls^, result1, result2, result3)) |> toEqual((1, 1, 1, 1));
  });

  test("wrap", () => {
    let f = a => string_of_int(a * 10);
    let before = a => a + 4;
    let after = str => str ++ "!";
    let f = Function.wrap(~before, ~after, f);
    let result = f(22);
    expect(result) |> toEqual("260!");
  });

  test("negate", () => {
    let f = str => str |> Relude.String.length == 0;
    let g = Function.negate(f);
    let resultF = f("");
    let resultG = g("");
    expect((resultF, resultG)) |> toEqual((true, false));
  });

  test("WithArgument", () => {
    open FunctionWithStringArgument.Infix;
    let plus5 = a => a + 5;
    let times3 = a => (a |> int_of_string) * 3;
    let actual = (plus5 <$> times3)("10");
    expect(actual) |> toBe(35);
  });
});
