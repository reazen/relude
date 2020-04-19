open Jest;
open Expect;

module Decimal = Relude.Decimal;
open Decimal;

[@coverage exclude_file];
afterAll(Bisect.Runtime.write_coverage_data);

describe("Decimal", () => {
  test("make", () =>
    expect(Decimal.make(12345, -2)) |> toEqual(Decimal(12345, -2))
  );

  test("fromInt", () =>
    expect(Decimal.fromInt(12345)) |> toEqual(Decimal(12345, 0))
  );

  test("show Decimal(12345, -2)", () =>
    expect(Decimal(12345, -2) |> Decimal.show) |> toEqual("123.45")
  );

  test("show Decimal(12345, 0)", () =>
    expect(Decimal(12345, 0) |> Decimal.show) |> toEqual("12345")
  );

  test("show Decimal(12345, 2)", () =>
    expect(Decimal(12345, 2) |> Decimal.show) |> toEqual("1234500")
  );

  test("tenToThePowerOfPositive -2", () =>
    expect(Decimal.tenToThePowerOfPositive(-2)) |> toEqual(1) // undefined
  );

  test("tenToThePowerOfPositive -1", () =>
    expect(Decimal.tenToThePowerOfPositive(-1)) |> toEqual(1) // undefined
  );

  test("tenToThePowerOfPositive 0 ", () =>
    expect(Decimal.tenToThePowerOfPositive(0)) |> toEqual(1)
  );

  test("tenToThePowerOfPositive 1 ", () =>
    expect(Decimal.tenToThePowerOfPositive(1)) |> toEqual(10)
  );

  test("tenToThePowerOfPositive 2 ", () =>
    expect(Decimal.tenToThePowerOfPositive(2)) |> toEqual(100)
  );

  test("tenToThePowerOfPositive 3 ", () =>
    expect(Decimal.tenToThePowerOfPositive(3)) |> toEqual(1000)
  );

  test("normalize", () => {
    let a = Decimal(12345, -2); // 123.45
    let b = Decimal(6789, 3); // 6789000
    let result = Decimal.normalize(a, b);
    expect(result)
    |> toEqual((Decimal(12345, -2), Decimal(678900000, -2), (-2)));
  });

  test("add", () => {
    let a = Decimal(12345, -2); // 123.45
    let b = Decimal(6789, 3); // 6789000
    let result = Decimal.add(a, b);
    expect(result) |> toEqual(Decimal(678912345, -2));
  });

  test("subtract", () => {
    let a = Decimal(12345, -2); // 123.45
    let b = Decimal(6789, 3); // 6789000
    let result = Decimal.subtract(a, b);
    expect(result) |> toEqual(Decimal(-678887655, -2));
  });
});