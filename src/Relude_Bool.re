/**
 * Folds a bool value into a value of a different type, using a function
 * for the true and false cases.
 */
let ifElse: (unit => 'a, unit => 'a, bool) => 'a =
  (onTrue, onFalse, value) =>
    if (value) {
      onTrue();
    } else {
      onFalse();
    };

/**
 * Negates the bool value
 *
 * Not named `not`, because refmt rewrites this as (!)
 */
let inverse: bool => bool = (!);

/**
 * Negates the bool value
 *
 * Alias for `inverse`.  Not named `not`, because refmt rewrites this as (!)
 */
let not_: bool => bool = inverse;

/**
 * Combines two boolean using an AND
 */
let and_: (bool, bool) => bool = (&&);

/**
 * Combines two boolean using an OR
 */
let or_: (bool, bool) => bool = (||);

/**
 * Combines two booleans using an NAND
 */
let nand: (bool, bool) => bool = (a, b) => !(a && b);

/**
 * Combines two booleans using an NOR
 */
let nor: (bool, bool) => bool = (a, b) => !(a || b);

/**
 * Combines two booleans using an XOR
 */
let xor: (bool, bool) => bool = (a, b) => !a && b || a && !b;

/**
 * Combines two booleans using an XNOR
 */
let xnor: (bool, bool) => bool = (a, b) => !xor(a, b);

/**
 * Combines two booleans using an implication
 */
let implies: (bool, bool) => bool = (a, b) => !a || b;

/**
 * Compares two booleans for equality
 */
let eq: (bool, bool) => bool =
  (a, b) =>
    switch (a, b) {
    | (true, true) => true
    | (false, false) => true
    | (true, false) => false
    | (false, true) => false
    };

/**
 * EQ instance for booleans
 */
module Eq: BsBastet.Interface.EQ with type t = bool = {
  type t = bool;
  let eq = eq;
};

/**
 * Compares two booleans for equality
 */
let compare: (bool, bool) => BsBastet.Interface.ordering =
  (a, b) =>
    switch (a, b) {
    | (true, true) => `equal_to
    | (false, false) => `equal_to
    | (true, false) => `greater_than
    | (false, true) => `less_than
    };

/**
 * ORD instance for booleans
 */
module Ord: BsBastet.Interface.ORD with type t = bool = {
  include Eq;
  let compare = compare;
};

/**
 * Converts a boolean value to a string
 */
let show: bool => string = b => b ? "true" : "false";

/**
 * SHOW instance for booleans
 */
module Show: BsBastet.Interface.SHOW with type t = bool = {
  type t = bool;
  let show = show;
};

module Conjunctive = {
  module Magma: BsBastet.Interface.MAGMA with type t = bool = {
    type t = bool;
    let append = (&&);
  };

  module MedialMagma: BsBastet.Interface.MEDIAL_MAGMA with type t = bool = Magma;

  module Semigroup: BsBastet.Interface.SEMIGROUP with type t = bool = Magma;
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);

  module Monoid: BsBastet.Interface.MONOID with type t = bool = {
    include Semigroup;
    let empty = true;
  };
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
};

module And = Conjunctive;

module Disjunctive = {
  module Magma: BsBastet.Interface.MAGMA with type t = bool = {
    type t = bool;
    let append = (||);
  };

  module MedialMagma: BsBastet.Interface.MEDIAL_MAGMA with type t = bool = Magma;

  module Semigroup: BsBastet.Interface.SEMIGROUP with type t = bool = Magma;
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);

  module Monoid: BsBastet.Interface.MONOID with type t = bool = {
    include Semigroup;
    let empty = false;
  };
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
};

module Or = Disjunctive;

module Bounded: BsBastet.Interface.BOUNDED with type t = bool = {
  include Ord;
  let top = true;
  let bottom = false;
};
include Relude_Extensions_Bounded.BoundedExtensions(Bounded);

module Enum: Relude_Interface.ENUM with type t = bool = {
  include Ord;
  let pred =
    fun
    | true => Some(false)
    | false => None;
  let succ =
    fun
    | true => None
    | false => Some(true);
};
include Relude_Extensions_Enum.EnumExtensions(Enum);

module BoundedEnum: Relude_Interface.BOUNDED_ENUM with type t = bool = {
  include Bounded;
  include (Enum: Relude_Interface.ENUM with type t := t);
  let cardinality = 2;
  let fromEnum =
    fun
    | false => 0
    | true => 1;
  let toEnum =
    fun
    | 0 => Some(false)
    | 1 => Some(true)
    | _ => None;
};
include Relude_Extensions_BoundedEnum.BoundedEnumExtensions(BoundedEnum);

module Infix = {
  include Relude_Extensions_Eq.EqInfix(Eq);
  include Relude_Extensions_Ord.OrdInfix(Ord);
};
