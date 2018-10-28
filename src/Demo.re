Js.log("Hello, BuckleScript and Reason!");

module type SHOW = {
  type t;
  let show: t => string;
};

module type EQ = {
  type t;
  let eq: (t, t) => bool;
};

type ordering = [ | `less_than | `equal_to | `greater_than ];

module type ORD = {
  include EQ;
  let compare: (t, t) => ordering;
};

module Ordering = (O: ORD) => {
  let lessThan: (O.t, O.t) => bool = (a, b) => O.compare(a, b) == `less_than;
  let lessThanOrEqual: (O.t, O.t) => bool = (a, b) => O.compare(a, b) != `greater_than ;
  let greaterThan: (O.t, O.t) => bool = (a, b) => O.compare(a, b) == `greater_than;
  let greaterThanOrEqual: (O.t, O.t) => bool = (a, b) => O.compare(a, b) != `less_than;
};

module type FUNCTOR = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};

module type APPLY = {
  include FUNCTOR;
  let apply: (t('a => 'b), t('a)) => t('b);
};

module type APPLICATIVE = {
  include APPLY;
  let pure: 'a => t('a);
};

module type MONAD = {
  include APPLICATIVE;
  let flatMap: t('a) => ('a => t('b)) => t('b);
};

module String {
  module Show /*: SHOW with type t = string*/ {
    type t = string;
    let show = a => a;
  };

  module Eq: EQ {
    type t = string;
    let eq = (a, b) => a == b;
  };

  module Ord: ORD {
    include Eq;
    let compare = (a, b) => {
      if (a < b) `less_than
      else if (a > b) `greater_than
      else `equal_to
    };
  };
};

module Option {
  module type SHOW_F = (S: SHOW) => SHOW with type t = option(S.t);

  module Show: SHOW_F = (S: SHOW) => {
    type t = option(S.t);
    let show = opt => switch (opt) {
    | Some(a) => "Some(" ++ S.show(a) ++ ")"
    | None => "None"
    };
  };

  module type EQ_F = (E: EQ) => EQ with type t = option(E.t);

  module Eq: EQ_F = (E: EQ) => {
    type t = option(E.t);
    let eq = (a, b) => {
      switch (a, b) {
      | (Some(a), Some(b)) => E.eq(a, b);
      | (Some(_), None) => false;
      | (None, Some(_)) => false;
      | (None, None) => true;
      };
    };
  };

  module Functor: FUNCTOR with type t('a) = option('a) = {
    type t('a) = option('a);
    let map = (f, a) => {
      switch (a) {
      | Some(a) => Some(f(a));
      | None => None;
      };
    };
  };
};

module MyPrinter = (S: SHOW) => {
  let print: S.t => string = a => {
    S.show(a);
  };
};

module OptionStringPrinter = MyPrinter(Option.Show(String.Show));

module OptionStringPrinter2 = MyPrinter(Option.Show({
  type t = string;
  let show = a => a;
}));


let x = OptionStringPrinter.print(Some("hi"));
let y = OptionStringPrinter2.print(Some("bye"));

Js.Console.log(x);
Js.Console.log(y);

