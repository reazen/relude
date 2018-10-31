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

let append: ('a, array('a)) => array('a) = (item, array) => concat(array, [|item|]);

let prepend: ('a, array('a)) => array('a) = (item, array) => concat([|item|], array);

let toList: array('a) => list('a) = Belt.List.fromArray;

let fromList: list('a) => array('a) = Belt.List.toArray;

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

module Alt = BsAbstract.Array.Alt;

module Plus = BsAbstract.Array.Plus;

module Alternative = BsAbstract.Array.Alternative;

module Foldable = BsAbstract.Array.Foldable;

module Traversable = BsAbstract.Array.Foldable;

module Eq = BsAbstract.Array.Eq;

module Ord = BsAbstract.Array.Ord;

module Show = BsAbstract.Array.Show;

module Invariant = BsAbstract.Array.Invariant;

module MonadZero = BsAbstract.Array.Monad_Zero;

module MonadPlus = BsAbstract.Array.Monad_Plus;

module Extend = BsAbstract.Array.Extend;

module Infix = BsAbstract.Array.Infix;

module Sequence: Interface.SEQUENCE with type t('a) = array('a) = {
  type t('a) = array('a);
  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
}

module IsoList: Interface.ISO_LIST with type t('a) = array('a) = {
  type t('a) = array('a);
  let fromList = fromList;
  let toList = toList;
}
