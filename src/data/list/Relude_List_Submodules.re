/********************************************************************************
 This is intended to hold modules that are based on list-related module functors.

 These are here to avoid circular dependencies between the base modules like List and Option, etc.

 E.g.
 List.String.show
 List.Float.min
 List.Option.traverse
 List.Result.traverse
 List.Validation.traverse
 ********************************************************************************/

module String = {
  include Relude_List_Base.OfOrd(Relude_String.Ord);
  include Relude_List_Base.OfMonoid(Relude_String.Monoid);

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

  // TODO: not convinced this belongs here...
  let map: (string => string, string) => string =
    (f, str) =>
      Relude_String.toList(str) |> Relude_List_Types.map(f) |> join;
};

module Int = {
  include Relude_List_Base.OfOrd(Relude_Int.Ord);

  let sum: list(int) => int =
    Relude_List_Types.fold((module Relude_Int.Additive.Monoid));

  let product: list(int) => int =
    Relude_List_Types.fold((module Relude_Int.Multiplicative.Monoid));
};

module Float = {
  include Relude_List_Base.OfOrd(Relude_Float.Ord);

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
  module Traversable = (Error: BsAbstract.Interface.TYPE) =>
    BsAbstract.List.Traversable((BsAbstract.Result.Applicative(Error)));

  /*
   Traversing with Result has fail fast semantics, and errors are not collected.
   */
  let traverse =
      (
        type a,
        type b,
        type e,
        f: a => Belt.Result.t(b, e), /* Each a produces a Result with a success value or a single error value */
        list: list(a),
      )
      : Belt.Result.t(list(b), e) => {
    module Error = {
      type t = e;
    };
    module Traversable = Traversable(Error);
    Traversable.traverse(f, list);
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
