open BsAbstract.Interface;

/********************************************************************************
 This is intended to hold modules that are based on list-related module functors.

 These are here to avoid circular dependencies between the base modules like List and Option, etc.

 E.g.
 List.String.eq
 List.Float.min
 List.Option.traverse
 List.Result.traverse
 List.Validation.traverse
 ********************************************************************************/

/**
 * Helper modules for generating collections of functions depending on
 * properties of the inner type.
 */
module OfEq = (E: EQ) => {
  include Relude_List_Types.FoldableOfEq(E);
  let distinct = Relude_List_Base.distinctBy(E.eq);
  let remove = Relude_List_Base.removeBy(E.eq);
  let removeEach = Relude_List_Base.removeEachBy(E.eq);
  let eq = Relude_List_Types.eqBy(E.eq);
};

module OfOrd = (O: ORD) => {
  include OfEq(O);
  let sort = Relude_List_Base.sortBy(O.compare);
};

module OfMonoid = (M: MONOID) => {
  include Relude_List_Types.FoldableOfMonoid(M);
};

module String = {
  include OfOrd(Relude_String.Ord);
  include OfMonoid(Relude_String.Monoid);

  let join = fold;
  let joinWith = intercalate;
  /**
   * Remove all duplicate entries from a list of strings.
   *
   * Instead of passing String.Eq to the underlying List.distinct function, we
   * have a special-case implementation here that uses Js.Dict for faster
   * comparison. This is O(n) instead of O(n^2).
   */
  let distinct: list(string) => list(string) =
    xs =>
      Relude_List_Types.foldLeft(
        (acc, curr) => {
          Js.Dict.set(acc, curr, 0);
          acc;
        },
        Js.Dict.empty(),
        xs,
      )
      |> Js.Dict.keys
      |> Relude_List_Types.fromArray;
};

module Int = {
  include OfOrd(Relude_Int.Ord);

  let sum: list(int) => int =
    Relude_List_Types.fold((module Relude_Int.Additive.Monoid));

  let product: list(int) => int =
    Relude_List_Types.fold((module Relude_Int.Multiplicative.Monoid));
};

module Float = {
  include OfOrd(Relude_Float.Ord);

  let sum: list(float) => float =
    Relude_List_Types.fold((module Relude_Float.Additive.Monoid));

  let product: list(float) => float =
    Relude_List_Types.fold((module Relude_Float.Multiplicative.Monoid));
};

module Option = {
  module Traversable = BsAbstract.Functors.ListF.Option.Traversable;

  let traverse: ('a => option('a), list('a)) => option(list('a)) = Traversable.traverse;

  let sequence: list(option('a)) => option(list('a)) = Traversable.sequence;
};

module Result = {
  /*
   Traversing with Result has fail fast semantics, and errors are not collected.
   */
  let traverse =
      (type e, f: 'a => Belt.Result.t('b, e), list: list('a))
      : Belt.Result.t(list('b), e) => {
    module ResultFixedError =
      Relude_Result.Applicative({
        type t = e;
      });
    module TraverseResult = Relude_List_Types.Traversable(ResultFixedError);
    TraverseResult.traverse(f, list);
  };

  let sequence = (type e, xs): Belt.Result.t(list('a), e) => {
    module ResultFixedError =
      Relude_Result.Applicative({
        type t = e;
      });
    module TraverseResult = Relude_List_Types.Traversable(ResultFixedError);
    TraverseResult.sequence(xs);
  };
};

module Validation = {
  module Traversable =
         (
           Errors: BsAbstract.Interface.SEMIGROUP_ANY,
           Error: BsAbstract.Interface.TYPE,
         ) =>
    BsAbstract.List.Traversable(
      (Relude_Validation.Applicative(Errors, Error)),
    );
  module TraversableWithErrorsAsList = (Error: BsAbstract.Interface.TYPE) =>
    Traversable(Relude_List_Types.SemigroupAny, Error);
  module TraversableWithErrorsAsListOfStrings =
    TraversableWithErrorsAsList({
      type t = string;
    });
  module TraversableWithErrorsAsNonEmptyList =
         (Error: BsAbstract.Interface.TYPE) =>
    Traversable(Relude_NonEmpty.List.SemigroupAny, Error);

  /*
   This is a streamlined definition of traverse which allows you to return a Belt.Result.t for each item
   in the list, and all errors are collected in a NonEmpty.List of your error type, using applicative semantics
   for Validation.
   */
  let traverse =
      (
        type a,
        type e,
        f: a => Belt.Result.t(a, e), /* Each a produces a Result with a success value or a single error value */
        list: list(a),
      )
      : Relude_Validation.t(list(a), Relude_NonEmpty.List.t(e)) => {
    module Error = {
      type t = e;
    };
    module Traversable =
      Traversable(Relude_NonEmpty.List.SemigroupAny, Error);
    /* Map the reuslts to Validation.t(a, NonEmpty.List.t(e)), so we can accumulate the errors */
    Traversable.traverse(a => f(a)->Relude_Result.toValidationNel, list);
  };
};
