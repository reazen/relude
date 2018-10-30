/* Array */

/* Functions */
let empty: array('a) = [||];

let length: array('a) => int = Belt.Array.length;

let isEmpty: array('a) => bool = arr => length(arr) == 0;

let isNotEmpty: array('a) => bool = arr => length(arr) > 0;

let head: array('a) => option('a) = arr => Belt.Array.get(arr, 0);

let tail: array('a) => option(array('a)) = arr => {
  let tail = Belt.Array.sliceToEnd(arr, 1);
  isNotEmpty(tail) ? Some(tail) : None;
};

let tailOrEmpty: array('a) => array('a) = arr => tail(arr)->Belt.Option.getWithDefault(empty);

let concat: (array('a), array('a)) => array('a) = Belt.Array.concat;

let toList: array('a) => list('a) = Belt.List.fromArray;

let fromList: list('a) => array('a) = Belt.List.toArray;

/* TODO more functions */

/* Modules */
module Eq = BsAbstract.Array.Eq;

module Show = BsAbstract.Array.Show;

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = concat
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = array('a) = {
  include SemigroupAny;
  let empty = empty;
};

module Functor = BsAbstract.Array.Functor;

module Apply = BsAbstract.Array.Apply;

module Applicative = BsAbstract.Array.Applicative;

module Monad = BsAbstract.Array.Monad;

module Foldable = BsAbstract.Array.Foldable;

module Traversable = BsAbstract.Array.Foldable;

module Sequence: Interface.SEQUENCE with type t('a) = array('a) = {
  type t('a) = array('a);
  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
  let fromList = fromList;
  let toList = toList;
  let fromArray = Function.identity;
  let toArray = Function.identity;
};

module Infix = BsAbstract.Array.Infix;
