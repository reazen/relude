/**
 * List extensions for when you have an EQ instance.
 */
module ListEqExtensions = (E: BsAbstract.Interface.EQ) => {
  include Relude_List_Instances.FoldableEqExtensions(E);
  /**
   * Gets the distinct items of the list, based on the given EQ module
   */
  let distinct = Relude_List_Base.distinctBy(E.eq);

  /**
   * Removes the first item of the list which equals the given item, based on the given EQ module
   */
  let removeFirst: (E.t, list(E.t)) => list(E.t) =
    Relude_List_Base.removeFirstBy(E.eq);

  /**
   * Removes all items of the list which equal the given item, based on the given EQ module
   */
  let removeEach: (E.t, list(E.t)) => list(E.t) =
    Relude_List_Base.removeEachBy(E.eq);

  /**
   * Indicates if all pairwise items in the given lists are equal, using the given EQ module
   */
  let eq: (list(E.t), list(E.t)) => bool = Relude_List_Instances.eqBy(E.eq);
};

/**
 * List extensions for when you have an ORD instance.
 */
module ListOrdExtensions = (O: BsAbstract.Interface.ORD) => {
  include ListEqExtensions(O);
  include Relude_List_Instances.FoldableOrdExtensions(O);
  /**
   * Sorts the list using the given ORD module.
   */
  let sort = Relude_List_Base.sortBy(O.compare);
};

/**
 * List extensions for when you have an MONOID instance.
 */
module ListMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
  include Relude_List_Instances.FoldableMonoidExtensions(M);
};

module String = {
  include ListOrdExtensions(Relude_String.Ord);
  include ListMonoidExtensions(Relude_String.Monoid);

  /**
   * Joins a list of strings using the empty string "" as a delimiter
   */
  let join: list(string) => string = foldWithMonoid;

  /**
   * Joins a list of strings using the given delimiter
   */
  let joinWith: (string, list(string)) => string = intercalate;

  /**
   * Remove all duplicate entries from a list of strings.
   *
   * Instead of passing String.Eq to the underlying List.distinct function, we
   * have a special-case implementation here that uses Js.Dict for faster
   * comparison. This is O(n) instead of O(n^2).
   */
  let distinct: list(string) => list(string) =
    xs =>
      Relude_List_Instances.foldLeft(
        (acc, curr) => {
          Js.Dict.set(acc, curr, 0);
          acc;
        },
        Js.Dict.empty(),
        xs,
      )
      |> Js.Dict.keys
      |> Relude_List_Instances.fromArray;
};

/**
 * List extensions for list(int)
 */
module Int = {
  include ListOrdExtensions(Relude_Int.Ord);

  /**
   * Finds the sum of all the ints in the list
   */
  let sum: list(int) => int =
    Relude_List_Instances.foldWithMonoid((module Relude_Int.Additive.Monoid));

  /**
   * Finds the product of all the ints in the list
   */
  let product: list(int) => int =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Int.Multiplicative.Monoid),
    );
};

/**
 * List extensions for list(float)
 */
module Float = {
  include ListOrdExtensions(Relude_Float.Ord);

  /**
   * Finds the sum of all the floats in the list
   */
  let sum: list(float) => float =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Float.Additive.Monoid),
    );

  /**
   * Finds the product of all the floats in the list
   */
  let product: list(float) => float =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Float.Multiplicative.Monoid),
    );
};

/**
 * List extensions for list(option('a))
 */
module Option = {
  include Relude_List_Instances.Traversable(Relude_Option.Applicative);
};

/**
 * List extensions for list(Result.t('a, 'e))
 */
module Result = {
  /**
   * Traverses a `'a => Result.t('b, 'e)` function over a `list('a)`, to produce a `Result.t(list('b), 'e)` using
   * fail-fast semantics.
   */
  let traverse =
      (type e, f: 'a => Belt.Result.t('b, e), list: list('a))
      : Belt.Result.t(list('b), e) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_List_Instances.Traversable(ResultE.Applicative);
    TraverseResult.traverse(f, list);
  };

/**
 * Sequences a `list(Result.t('a, 'e))` into `Result.t(list('a) 'e))` using fail
 * fast semantics.
 */
  let sequence = (type e, xs): Belt.Result.t(list('a), e) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_List_Instances.Traversable(ResultE.Applicative);
    TraverseResult.sequence(xs);
  };
};

/**
 * List extensions for `IO.t('a, 'e)`
 */
module IO = {
  let traverse =
      (type e, f: 'a => Relude_IO.t('b, e), list: list('a))
      : Relude_IO.t(list('b), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = Relude_List_Instances.Traversable(IoE.Applicative);
    TraverseIO.traverse(f, list);
  };

  let sequence =
      (type e, xs: list(Relude_IO.t('a, e))): Relude_IO.t(list('a), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = Relude_List_Instances.Traversable(IoE.Applicative);
    TraverseIO.sequence(xs);
  };
};

/**
 * List extensions for `Validation.t('a, 'e)`
 */
module Validation = {
  module Traversable =
         (
           Errors: BsAbstract.Interface.SEMIGROUP_ANY,
           Error: BsAbstract.Interface.TYPE,
         ) => {
    module ValidationE = Relude_Validation.WithErrors(Errors, Error);
    module ValidationEApplicative = ValidationE.Applicative;
    include BsAbstract.List.Traversable(ValidationEApplicative);
  };

  module TraversableWithErrorsAsList = (Error: BsAbstract.Interface.TYPE) =>
    Traversable(Relude_List_Instances.SemigroupAny, Error);

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
        type b,
        type e,
        f: a => Belt.Result.t(b, e), /* Each a produces a Result with a success value or a single error value */
        list: list(a),
      )
      : Relude_Validation.t(list(b), Relude_NonEmpty.List.t(e)) => {
    module Error = {
      type t = e;
    };
    module Traversable =
      Traversable(Relude_NonEmpty.List.SemigroupAny, Error);
    /* Map the reuslts to Validation.t(a, NonEmpty.List.t(e)), so we can accumulate the errors */
    Traversable.traverse(a => f(a)->Relude_Result.toValidationNel, list);
  };
};