/**
 * Module type signature for a type constructor with a single type hole.
 */
module type TYPE_ANY = {type t('a);};

/**
 * Module type which captures a simple a => b function
 */
module type ARROW = {
  type a;
  type b;
  let f: a => b;
};

/**
 * Module type functor which captures a simple a => b function
 */
module type ARROW_F =
  (A: BsAbstract.Interface.TYPE, B: BsAbstract.Interface.TYPE) =>
   ARROW with type a = A.t and type b = B.t;

/**
 * Module type signature for a module that represents a sequence of values and related functions.
 */
module type SEQUENCE = {
  type t('a);

  let empty: t('a);
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

  module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = t('a);

  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a);

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a);

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a);

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a);

  module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = t('a);

  module Traversable:
    (A: BsAbstract.Interface.APPLICATIVE) =>

      BsAbstract.Interface.TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a);

  module Eq:
    (EqA: BsAbstract.Interface.EQ) =>
     BsAbstract.Interface.EQ with type t = t(EqA.t);

  module Show:
    (ShowA: BsAbstract.Interface.SHOW) =>
     BsAbstract.Interface.SHOW with type t = t(ShowA.t);
};

/**
 * Module type signature for a module that is isomorphic with an array
 */
module type ISO_ARRAY = {
  type t('a);
  let fromArray: array('a) => t('a);
  let toArray: t('a) => array('a);
};

/**
 * Module type signature for a module that is isomorphic with a list
 */
module type ISO_LIST = {
  type t('a);
  let fromList: list('a) => t('a);
  let toList: t('a) => list('a);
};

/**
 * Module type signature for a Monad that can produce an error in a monadic context
 */
module type MONAD_THROW = {
  include BsAbstract.Interface.MONAD;
  type e;
  let throwError: e => t('a);
};

/**
 * Module type signature for a Monad that can handle an error by creating a new monadic value.
 */
module type MONAD_ERROR = {
  include MONAD_THROW;
  let catchError: (e => t('a), t('a)) => t('a);
};

/**
 * Represents types that have a lower bound, like strings or positive ints
 */
module type LOWER_BOUNDED = {
  type t;
  let bottom: t;
};

/**
 * Represents types that have an upper bound
 */
module type UPPER_BOUNDED = {
  type t;
  let top: t;
};

/**
 * Module type which describes a type that can be ordered and for which we can determine
 * a lawful chain of successors and predecessors
 */
module type ENUM = {
  include BsAbstract.Interface.ORD;
  let succ: t => option(t);
  let pred: t => option(t);
};

/**
 * Module type which describes a type that can be ordered and for which we can determine
 * a lawful chain of successors and predecessors, and there is a top and bottom bound.
 */
module type BOUNDED_ENUM = {
  include BsAbstract.Interface.BOUNDED;
  include ENUM with type t := t;
  let cardinality: int;
  let fromEnum: t => int;
  let toEnum: int => option(t);
};