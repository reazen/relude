open Jest;
open Expect;
open Relude.Globals;

module Month = {
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec;

  let toInt1Based: t => int =
    fun
    | Jan => 1
    | Feb => 2
    | Mar => 3
    | Apr => 4
    | May => 5
    | Jun => 6
    | Jul => 7
    | Aug => 8
    | Sep => 9
    | Oct => 10
    | Nov => 11
    | Dec => 12;

  let fromInt1Based: int => option(t) =
    fun
    | 1 => Some(Jan)
    | 2 => Some(Feb)
    | 3 => Some(Mar)
    | 4 => Some(Apr)
    | 5 => Some(May)
    | 6 => Some(Jun)
    | 7 => Some(Jul)
    | 8 => Some(Aug)
    | 9 => Some(Sep)
    | 10 => Some(Oct)
    | 11 => Some(Nov)
    | 12 => Some(Dec)
    | _ => None;

  module Eq: BsBastet.Interface.EQ with type t = t =
    Int.EqBy({
      type a = t;
      type b = int;
      let f = toInt1Based;
    });

  module Ord: BsBastet.Interface.ORD with type t = t =
    Int.OrdBy({
      type a = t;
      type b = int;
      let f = toInt1Based;
    });

  module Bounded: BsBastet.Interface.BOUNDED with type t = t = {
    include Ord;
    let bottom = Jan;
    let top = Dec;
  };

  module Enum: Relude.Interface.ENUM with type t = t = {
    include Ord;
    let pred: t => option(t) =
      month => fromInt1Based(toInt1Based(month) - 1);

    let succ: t => option(t) =
      month => fromInt1Based(toInt1Based(month) + 1);
  };

  module BoundedEnum: Relude.Interface.BOUNDED_ENUM with type t = t = {
    include Bounded;
    include (Enum: Relude.Interface.ENUM with type t := t);
    let cardinality = 12;
    let toEnum = fromInt1Based;
    let fromEnum = toInt1Based;
  };

  module Show: BsBastet.Interface.SHOW with type t = t = {
    type nonrec t = t;
    let show: t => string =
      fun
      | Jan => "Jan"
      | Feb => "Feb"
      | Mar => "Mar"
      | Apr => "Apr"
      | May => "May"
      | Jun => "Jun"
      | Jul => "Jul"
      | Aug => "Aug"
      | Sep => "Sep"
      | Oct => "Oct"
      | Nov => "Nov"
      | Dec => "Dec";
  };
  include Relude.Extensions.Eq.EqExtensions(Eq);
  include Relude.Extensions.Bounded.BoundedExtensions(Bounded);
  include Relude.Extensions.Enum.EnumExtensions(Enum);
  include Relude.Extensions.BoundedEnum.BoundedEnumExtensions(BoundedEnum);
  include Relude.Extensions.Show.ShowExtensions(Show);
};

open Month;

afterAll(Bisect.Runtime.write_coverage_data);

describe("Relude_Extensions_Enum", () => {
  testAll(
    "fromToAsList",
    [
      (
        Jan,
        Dec,
        [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec],
      ),
      (Jan, Jan, [Jan]),
      (Jan, Feb, [Jan, Feb]),
      (Jan, Mar, [Jan, Feb, Mar]),
      (Jan, Nov, [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov]),
      (Feb, Nov, [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov]),
      (Feb, Dec, [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]),
      (
        Dec,
        Jan,
        [Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan],
      ),
      (Dec, Feb, [Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb]),
      (Nov, Jan, [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan]),
      (Nov, Feb, [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb]),
      (Dec, Nov, [Dec, Nov]),
      (Dec, Dec, [Dec]),
    ],
    ((start, finish, expected)) => {
      let actual = Month.fromToAsList(~start, ~finish);
      expect(actual) |> toEqual(expected);
    },
  );

  testAll(
    "upFromAsList",
    [
      (Jan, [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]),
      (Feb, [Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]),
      (Nov, [Dec]),
      (Dec, []),
    ],
    ((start, expected)) => {
      let actual = Month.upFromAsList(start);
      expect(actual) |> toEqual(expected);
    },
  );

  testAll(
    "upFromIncludingAsList",
    [
      (Jan, [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]),
      (Feb, [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]),
      (Nov, [Nov, Dec]),
      (Dec, [Dec]),
    ],
    ((start, expected)) => {
      let actual = Month.upFromIncludingAsList(start);
      expect(actual) |> toEqual(expected);
    },
  );

  testAll(
    "downFromAsList",
    [
      (Jan, []),
      (Feb, [Jan]),
      (Nov, [Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan]),
      (Dec, [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan]),
    ],
    ((start, expected)) => {
      let actual = Month.downFromAsList(start);
      expect(actual) |> toEqual(expected);
    },
  );

  testAll(
    "downFromIncludingAsList",
    [
      (Jan, [Jan]),
      (Feb, [Feb, Jan]),
      (Nov, [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan]),
      (Dec, [Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan]),
    ],
    ((start, expected)) => {
      let actual = Month.downFromIncludingAsList(start);
      expect(actual) |> toEqual(expected);
    },
  );
});

describe("Relude_Extensions_BoundedEnum", () => {
  testAll(
    "fromThenToAsList",
    [
      (
        Jan,
        Feb,
        Dec,
        [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec],
      ),
      (Jan, Feb, Jan, [Jan]),
      (Jan, Feb, Feb, [Jan, Feb]),
      (Jan, Feb, Mar, [Jan, Feb, Mar]),
      (
        Jan,
        Feb,
        Nov,
        [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov],
      ),
      (Feb, Mar, Nov, [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov]),
      (
        Feb,
        Mar,
        Dec,
        [Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec],
      ),
      (
        Dec,
        Nov,
        Jan,
        [Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan],
      ),
      (
        Dec,
        Nov,
        Feb,
        [Dec, Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb],
      ),
      (
        Nov,
        Oct,
        Jan,
        [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb, Jan],
      ),
      (Nov, Oct, Feb, [Nov, Oct, Sep, Aug, Jul, Jun, May, Apr, Mar, Feb]),
      (Dec, Nov, Nov, [Dec, Nov]),
      (Dec, Nov, Dec, [Dec]),
      (Jan, Mar, Dec, [Jan, Mar, May, Jul, Sep, Nov]),
      (Jan, Jun, Dec, [Jan, Jun, Nov]),
      (Jan, Oct, Dec, [Jan, Oct]),
      (Jan, Dec, Dec, [Jan, Dec]),
      (Dec, Oct, Jan, [Dec, Oct, Aug, Jun, Apr, Feb]),
      (Dec, Feb, Jan, [Dec, Feb]),
      (Dec, Jan, Jan, [Dec, Jan]),
    ],
    ((start, next, finish, expected)) => {
      let actual = Month.fromThenToAsList(~start, ~next, ~finish);
      expect(actual) |> toEqual(expected);
    },
  )
});
