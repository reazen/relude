open Bastet.Interface;

/**
Module type signature for a type constructor with a single type hole.
*/
module type TYPE_ANY = {type t('a);};

/**
Module type which captures a simple a => b function
*/
module type FUNCTION_1 = {
  type a;
  type b;
  let f: a => b;
};

/**
Module type functor which captures a simple a => b function
*/
module type FUNCTION_1_F =
  (A: TYPE, B: TYPE) => FUNCTION_1 with type a = A.t and type b = B.t;

/**
Captures a natural tranformation

It seems surprisingly hard to do [forall a. f a -> g a] in OCaml, but maybe I'm
missing something. See the {!module:Relude_Free_Applicative} for an example
usage.
*/
module type NATURAL_TRANSFORMATION = {
  type f('a);
  type g('a);
  let f: f('a) => g('a);
  // Another encoding would be using FUNCTOR modules here, but the f('a)/g('a) version seems easier to work with inline,
  // and we've already gone off the deep end, so let's not make it any more clunky.
  //module F: FUNCTOR;
  //module G: FUNCTOR;
  //let f: F.t('a) => G.t('a);
};

/**
Module type signature for a module that represents a sequence of values and
related functions.
*/
module type SEQUENCE = {
  type t('a);

  let emptyLazy: unit => t('a);
  let length: t('a) => int;
  let isEmpty: t('a) => bool;
  let isNotEmpty: t('a) => bool;
  let head: t('a) => option('a);
  let tail: t('a) => option(t('a));
  let tailOrEmpty: t('a) => t('a);
  let uncons: t('a) => option(('a, t('a)));
  let prepend: ('a, t('a)) => t('a);
  let append: ('a, t('a)) => t('a);
  let concat: (t('a), t('a)) => t('a);
  let reverse: t('a) => t('a);
  let mkString: (string, t(string)) => string;
  let zip: (t('a), t('b)) => t(('a, 'b));
  let zipWith: (('a, 'b) => 'c, t('a), t('b)) => t('c);
  let fromArray: array('a) => t('a);
  let fromList: list('a) => t('a);
  let toArray: t('a) => array('a);
  let toList: t('a) => list('a);
  let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool;
  let showBy: ('a => string, t('a)) => string;

  module Functor: FUNCTOR with type t('a) := t('a);

  module Apply: APPLY with type t('a) := t('a);

  module Applicative: APPLICATIVE with type t('a) := t('a);

  module Monad: MONAD with type t('a) := t('a);

  module Foldable: FOLDABLE with type t('a) := t('a);

  module Traversable:
    (A: APPLICATIVE) =>

      TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a);

  module Eq: (EqA: EQ) => EQ with type t = t(EqA.t);

  module Show: (ShowA: SHOW) => SHOW with type t = t(ShowA.t);
};

/**
Module type signature for a module that is isomorphic with an array
*/
module type ISO_ARRAY = {
  type t('a);
  let fromArray: array('a) => t('a);
  let toArray: t('a) => array('a);
};

/**
Module type signature for a module that is isomorphic with a list
*/
module type ISO_LIST = {
  type t('a);
  let fromList: list('a) => t('a);
  let toList: t('a) => list('a);
};

/**
Module type signature for Ior-based zipping and unzipping
*/
module type SEMIALIGN = {
  include FUNCTOR;
  let align: (t('a), t('b)) => t(Relude_Ior_Type.t('a, 'b));
  let alignWith: (Relude_Ior_Type.t('a, 'b) => 'c, t('a), t('b)) => t('c);
};

/**
Module type signature for an extension of ALIGN that adds an empty [nil] value.

Note that this has a parallel to the applicative typeclasses:

APPLY/APPLICATIVE/TRAVERSABLE

SEMIALIGN/ALIGN/CROSSWALK (or maybe ALIGNABLE?)
*/
module type ALIGN = {
  include SEMIALIGN;
  let nil: t('a);
};

/**
Module type signature for a Monad that can produce an error in a monadic context
*/
module type MONAD_THROW = {
  include MONAD;
  type e;
  let throwError: e => t('a);
};

/**
Module type signature for a Monad that can handle an error by creating a new
monadic value.
*/
module type MONAD_ERROR = {
  include MONAD_THROW;
  let catchError: (e => t('a), t('a)) => t('a);
};

/**
Represents types that have a lower bound, like strings or positive ints
*/
module type LOWER_BOUNDED = {
  type t;
  let bottom: t;
};

/**
Represents types that have an upper bound
*/
module type UPPER_BOUNDED = {
  type t;
  let top: t;
};

/**
Module type which describes a type that can be ordered and for which we can
determine a lawful chain of successors and predecessors
*/
module type ENUM = {
  include ORD;
  let succ: t => option(t);
  let pred: t => option(t);
};

/**
Module type which describes a type that can be ordered and for which we can
determine a lawful chain of successors and predecessors, and there is a top and
bottom bound.
*/
module type BOUNDED_ENUM = {
  include BOUNDED;
  include ENUM with type t := t;
  let cardinality: int;
  let fromEnum: t => int;
  let toEnum: int => option(t);
};
