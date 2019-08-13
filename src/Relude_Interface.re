/**
 * Module type signature for a type constructor with a single type hole.
 */
module type TYPE_ANY = {type t('a);};

/**
 * Module type signature for a module that represents a sequence of values and related functions.
 */
module type SEQUENCE = {
  type t('a);

  let empty: t('a);
  let length: t('a) => int;
  let isEmpty: t('a) => bool;
  let isNotEmpty: t('a) => bool;
  let head: t('a) => option('a);
  let tail: t('a) => option(t('a));
  let tailOrEmpty: t('a) => t('a);
  let mkString: (string, t(string)) => string;
  let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool;
  let showBy: ('a => string, t('a)) => string;

  module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = t('a);

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