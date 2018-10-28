open BsAbstract.Interface;

module type SEMIGROUP_ANY = BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a);

module SemigroupAny: SEMIGROUP_ANY {
  type t('a) = list('a);
  let append = (a, b) => Belt.List.concat(a, b);
}

let isEmpty: list('a) => bool = l => Belt.List.length(l) == 0;

let nonEmpty: list('a) => bool = l => Belt.List.length(l) > 0;

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let pure: 'a => list('a) = BsAbstract.List.Applicative.pure;

let one = pure;

let single = pure;

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;

let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

let traverseOption: ('a => option('a), list('a)) => option(list('a)) = BsAbstract.Functors.ListF.Option.Traversable.traverse;

let sequenceOption: list(option('a)) => option(list('a)) = BsAbstract.Functors.ListF.Option.Traversable.sequence;

module ListF {
  module Result {
    module type TRAVERSABLE_F = (B: TYPE) => BsAbstract.Interface.TRAVERSABLE with type t('a) = Belt.Result.t('a, B.t);
    module Traversable = (B: TYPE) => BsAbstract.List.Traversable(BsAbstract.Result.Applicative(B));

    module TraversableString = Traversable({ type t = string });
  }

  module Validation {
    module type TRAVERSABLE_F = (Errors: SEMIGROUP_ANY, Error: TYPE) => BsAbstract.Interface.TRAVERSABLE with type t('a) = Validation.t('a, Errors.t(Error.t));
    module Traversable = (Errors: SEMIGROUP_ANY, Error: TYPE) => BsAbstract.List.Traversable(Validation.Applicative(Errors, Error));

    module TraversableList = (Error: TYPE) => Traversable(SemigroupAny, Error);

    module TraversableListOfStrings = TraversableList({ type t = string });
  }
}

/*
let traverseResult: ('a => Belt.Result.t('a, 'b), list('a)) => Belt.Result.t(list('a), 'b) = ListF.Result.TraversableListOfStrings.traverse;
*/

let traverseValidation: ('a => Validation.t('a, list(string)), list('a)) => Validation.t(list('a), list(string)) = ListF.Validation.TraversableListOfStrings.traverse;

