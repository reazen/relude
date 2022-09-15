open Bastet.Interface;

type t = unit;

let show: t => string = _ => "()";

module Show: SHOW with type t = t = {
  type nonrec t = unit;
  let show = show;
};
include Relude_Extensions_Show.ShowExtensions(Show);

let eq: (t, t) => bool = ((), ()) => true;

module Eq: EQ with type t = t = {
  type nonrec t = t;
  let eq = eq;
};
include Relude_Extensions_Eq.EqExtensions(Eq);

let compare: (t, t) => ordering = ((), ()) => `equal_to;

module Ord: ORD with type t = t = {
  include Eq;
  let compare = compare;
};
include Relude_Extensions_Ord.OrdExtensions(Ord);

module Bounded: BOUNDED with type t = unit = {
  include Ord;
  let top = ();
  let bottom = ();
};
include Relude_Extensions_Bounded.BoundedExtensions(Bounded);

module Enum: Relude_Interface.ENUM with type t = unit = {
  include Ord;
  let succ = () => None;
  let pred = () => None;
};
include Relude_Extensions_Enum.EnumExtensions(Enum);

module BoundedEnum: Relude_Interface.BOUNDED_ENUM with type t = unit = {
  include Bounded;
  include (Enum: Relude_Interface.ENUM with type t := t);
  let cardinality = 1;
  let toEnum =
    fun
    | 0 => Some()
    | _ => None;
  let fromEnum = () => 0;
};
include Relude_Extensions_BoundedEnum.BoundedEnumExtensions(BoundedEnum);
