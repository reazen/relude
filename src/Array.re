/*
Array
*/

/*
Common module implementations (typeclass instances)
*/
module Eq = BsAbstract.Array.Eq;

module Show = BsAbstract.Array.Show;

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = Belt.Array.concat;
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = [||];
};

module Functor = BsAbstract.Array.Functor;

module Apply = BsAbstract.Array.Apply;

module Applicative = BsAbstract.Array.Applicative;

module Monad = BsAbstract.Array.Monad;

module Foldable = BsAbstract.Array.Foldable;

module Traversable = BsAbstract.Array.Foldable;

module Sequence: Interface.SEQUENCE with type t('a) = array('a) = {
  type t('a) = array('a);
  let length = Belt.Array.length;
  let isEmpty = arr => length(arr) == 0;
  let isNonEmpty = arr => length(arr) > 0;
  let head = arr => arr->Belt.List.fromArray->List.head /* TODO: better impl */
  let tail = arr => arr->Belt.List.fromArray->List.tail->Belt.Option.map(Belt.List.toArray) /* TODO: better impl */
  let tailOrEmpty = arr => tail(arr)->Belt.Option.getWithDefault([||]);
  let fromList = Belt.List.toArray;
  let toList = Belt.List.fromArray;
  let fromArray = Function.identity;
  let toArray = Function.identity;
};

module Infix = BsAbstract.Array.Infix;

/*
Array functions exposed at the top-level for convenience
*/

/* TODO */
