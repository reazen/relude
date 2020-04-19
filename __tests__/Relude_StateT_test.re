open Jest;
open Expect;

[@coverage exclude_file];
afterAll(Bisect.Runtime.write_coverage_data);

module StateT = Relude.StateT;

module State =
  StateT.State.WithState({
    type t = list(int);
  });

let ((<$>), ($>), (<#>), ( *> ), (>>=)) =
  State.Infix.((<$>), ($>), (<#>), ( *> ), (>>=));

module Stack = {
  let push: int => StateT.State.t(int, list(int)) =
    x => StateT.State.modify(xs => [x, ...xs]) $> x;

  let pop: StateT.State.t(option(int), list(int)) =
    State.get
    >>= (
      values =>
        switch (values) {
        | [] => State.put([]) $> None
        | [x, ...xs] => State.put(xs) $> Some(x)
        }
    );
};

describe("StateT", () => {
  test("pure", () => {
    let result = State.pure(2) |> State.runStateT([]);
    expect(result) |> toEqual((2, []));
  });

  test("put", () => {
    let result =
      State.pure(2) >>= (a => State.put([a]) $> a) |> State.runStateT([]);
    expect(result) |> toEqual((2, [2]));
  });

  test("stack example 1 (push)", () => {
    let result =
      Stack.push(1) >>= (_ => Stack.push(2)) |> State.runStateT([]);
    expect(result) |> toEqual((2, [2, 1]));
  });

  test("stack example 2 (push, pop)", () => {
    let result =
      Stack.push(1)
      >>= (_ => Stack.push(2) >>= (_ => Stack.push(3) >>= (_ => Stack.pop)))
      |> State.runStateT([]);
    expect(result) |> toEqual((Some(3), [2, 1]));
  });

  test("stack example 3", () => {
    // do notation :(
    let result =
      Stack.push(1)
      >>= (
        _ =>
          Stack.push(2)
          >>= (
            _ =>
              Stack.push(3)
              >>= (
                _ =>
                  Stack.pop
                  >>= (
                    _ =>
                      Stack.pop
                      >>= (_ => Stack.push(4) >>= (_ => Stack.push(5)))
                  )
              )
          )
      )
      |> State.runStateT([]);
    expect(result) |> toEqual((5, [5, 4, 1]));
  });

  test("stack example 4", () => {
    // do notation :(
    let result =
      Stack.push(1)
      >>= (_ => Stack.push(2))
      >>= (_ => Stack.push(3))
      >>= (_ => Stack.pop)
      >>= (_ => Stack.pop)
      >>= (_ => Stack.push(4))
      >>= (_ => Stack.push(5) <#> a => a * 100)
      |> State.runStateT([]);
    expect(result) |> toEqual((500, [5, 4, 1]));
  });

  test("*> loses state", () => {
    let result =
      Stack.push(1)
      *> Stack.push(2)
      *> Stack.push(3)
      |> State.runStateT([]);
    // Not 100% sure if this is expected behavior, but the applicative behavior throws away the
    // state on the left side here.  It makes sense, because there is no attempt to merge the states
    // in apply.  I'll have to compare this with purescript/haskell to be sure.
    expect(result) |> toEqual((3, [3]));
  });
});