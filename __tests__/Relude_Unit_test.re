open Jest;
open Expect;
open Relude.Globals;

afterAll(Bisect.Runtime.write_coverage_data);

describe("Unit", () => {
  test("show", () => {
    expect(Unit.show()) |> toEqual("()")
  });

  test("eq", () => {
    expect(Unit.eq((), ())) |> toEqual(true)
  });

  test("compare", () => {
    expect(Unit.compare((), ())) |> toEqual(`equal_to)
  });
});

describe("Unit Bounded", () => {
  test("top", () =>
    expect(Unit.Bounded.top) |> toEqual()
  );

  test("bottom", () =>
    expect(Unit.Bounded.bottom) |> toEqual()
  );
});

describe("Unit Enum", () => {
  test("pred", () =>
    expect(Unit.Enum.pred()) |> toEqual(None)
  );

  test("succ", () =>
    expect(Unit.Enum.succ()) |> toEqual(None)
  );
});

describe("Unit BoundedEnum", () => {
  test("cardinality", () =>
    expect(Unit.BoundedEnum.cardinality) |> toEqual(1)
  );

  test("fromEnum", () =>
    expect(Unit.BoundedEnum.fromEnum()) |> toEqual(0)
  );

  testAll(
    "toEnum", [(0, Some()), ((-1), None), (1, None)], ((input, expected)) =>
    expect(Unit.BoundedEnum.toEnum(input)) |> toEqual(expected)
  );
});