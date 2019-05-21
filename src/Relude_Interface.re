// These are our own typeclasses, which are used in addition to the bs-abstract ones

module type MONAD_THROW = {
  include BsAbstract.Interface.MONAD;
  type e;
  let throwError: e => t('a);
};

module type MONAD_ERROR = {
  include MONAD_THROW;
  let catchError: (e => t('a), t('a)) => t('a);
};

module type SEQUENCE = {
  type t('a);

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

module type ISO_ARRAY = {
  type t('a);
  let fromArray: array('a) => t('a);
  let toArray: t('a) => array('a);
};

module type ISO_LIST = {
  type t('a);
  let fromList: list('a) => t('a);
  let toList: t('a) => list('a);
};

// Experimental typeclasses for use with extensions

module type FUNCTOR2 = {
  type t('a, 'e);
  let map: ('a => 'b, t('a, 'e)) => t('b, 'e);
};

module type APPLY2 = {
  include FUNCTOR2;
  let apply: (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e);
};

module type APPLICATIVE2 = {
  include APPLY2;
  let pure: 'a => t('a, 'e);
};

module type MONAD2 = {
  include APPLICATIVE2;
  let flat_map: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e); // flat_map for consistency with bs-abstract, even though we call this "bind" in relude
};