open BsAbstract.Interface;

/********************************************************************************
This is intended to hold modules that are based on list-related module functors.

These are here to avoid circular dependencies between the base modules like List and Option, etc.

E.g.
List.Option.traverse
List.Result.traverse
List.Validation.traverse
********************************************************************************/

module Option {
  module Traversable = BsAbstract.Functors.ListF.Option.Traversable

  let traverse: ('a => option('a), list('a)) => option(list('a)) = Traversable.traverse;
  let sequence: list(option('a)) => option(list('a)) = Traversable.sequence;
}

module Result {
  module Traversable = (Error: TYPE) => BsAbstract.List.Traversable(BsAbstract.Result.Applicative(Error));
}

module Validation {
  module Traversable = (Errors: SEMIGROUP_ANY, Error: TYPE) => BsAbstract.List.Traversable(Validation.Applicative(Errors, Error));
  module TraversableWithErrorsAsList = (Error: TYPE) => Traversable(List.SemigroupAny, Error);
  module TraversableWithErrorsAsListOfStrings = TraversableWithErrorsAsList({ type t = string });
  module TraversableWithErrorsAsNonEmptyList = (Error: TYPE) => Traversable(NonEmpty.List.SemigroupAny, Error);

  let traverse: ('a => Validation.t('a, list(string)), list('a)) => Validation.t(list('a), list(string)) = TraversableWithErrorsAsListOfStrings.traverse;
}
