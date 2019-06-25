module ListEqExtensions = (E: BsAbstract.Interface.EQ) => {
  include Relude_List_Instances.FoldableEqExtensions(E);
  let distinct = Relude_List_Base.distinctBy(E.eq);
  let removeFirst = Relude_List_Base.removeFirstBy(E.eq);
  let removeEach = Relude_List_Base.removeEachBy(E.eq);
  let eq = Relude_List_Instances.eqBy(E.eq);
};

module ListOrdExtensions = (O: BsAbstract.Interface.ORD) => {
  include ListEqExtensions(O);
  include Relude_List_Instances.FoldableOrdExtensions(O);
  let sort = Relude_List_Base.sortBy(O.compare);
};

module ListMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
  include Relude_List_Instances.FoldableMonoidExtensions(M);
};

module String = {
  include ListOrdExtensions(Relude_String.Ord);
  include ListMonoidExtensions(Relude_String.Monoid);

  let join = foldWithMonoid;
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

module Int = {
  include ListOrdExtensions(Relude_Int.Ord);

  let sum: list(int) => int =
    Relude_List_Instances.foldWithMonoid((module Relude_Int.Additive.Monoid));

  let product: list(int) => int =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Int.Multiplicative.Monoid),
    );
};

module Float = {
  include ListOrdExtensions(Relude_Float.Ord);

  let sum: list(float) => float =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Float.Additive.Monoid),
    );

  let product: list(float) => float =
    Relude_List_Instances.foldWithMonoid(
      (module Relude_Float.Multiplicative.Monoid),
    );
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
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_List_Instances.Traversable(ResultE.Applicative);
    TraverseResult.traverse(f, list);
  };

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
