module ArrayEqExtensions = (E: BsAbstract.Interface.EQ) => {
  include Relude_Array_Instances.FoldableEqExtensions(E);
  let distinct = Relude_Array_Base.distinctBy(E.eq);
  let removeFirst = Relude_Array_Base.removeFirstBy(E.eq);
  let removeEach = Relude_Array_Base.removeEachBy(E.eq);
  let eq = Relude_Array_Instances.eqBy(E.eq);
};

module ArrayOrdExtensions = (O: BsAbstract.Interface.ORD) => {
  include ArrayEqExtensions(O);
  include Relude_Array_Instances.FoldableOrdExtensions(O);
  let sort = Relude_Array_Base.sortBy(O.compare);
};

module ArrayMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
  include Relude_Array_Instances.FoldableMonoidExtensions(M);
};

module String = {
  include ArrayOrdExtensions(Relude_String.Ord);
  include ArrayMonoidExtensions(Relude_String.Monoid);
  let join = fold;
  let joinWith = intercalate;

  /**
   * Specialized `distinct` function that removes duplicate strings in O(n)
   * time by using `Js.Dict`.
   */
  let distinct = xs =>
    Relude_Array_Instances.foldLeft(
      (acc, curr) => {
        Js.Dict.set(acc, curr, 0);
        acc;
      },
      Js.Dict.empty(),
      xs,
    )
    |> Js.Dict.keys;
};

module Int = {
  include ArrayOrdExtensions(Relude_Int.Ord);
  let sum =
    Relude_Array_Instances.foldMonoid((module Relude_Int.Additive.Monoid));
  let product =
    Relude_Array_Instances.foldMonoid(
      (module Relude_Int.Multiplicative.Monoid),
    );
};

module Float = {
  include ArrayOrdExtensions(Relude_Float.Ord);
  let sum =
    Relude_Array_Instances.foldMonoid((module Relude_Float.Additive.Monoid));
  let product =
    Relude_Array_Instances.foldMonoid(
      (module Relude_Float.Multiplicative.Monoid),
    );
};

module Option = {
  include Relude_Array_Instances.Traversable(Relude_Option.Applicative);
};

module Result = {
  let traverse = (type e, f, xs) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_Array_Instances.Traversable(ResultE.Applicative);
    TraverseResult.traverse(f, xs);
  };

  let sequence = (type e, xs) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_Array_Instances.Traversable(ResultE.Applicative);
    TraverseResult.sequence(xs);
  };
};

module Validation = {
  module WithErrors =
         (
           Errors: BsAbstract.Interface.SEMIGROUP_ANY,
           Error: BsAbstract.Interface.TYPE,
         ) => {
    module ValidationE = Relude_Validation.WithErrors(Errors, Error);
    module Traversable =
      Relude_Array_Instances.Traversable(ValidationE.Applicative);
  };

  module WithErrorsAsArray = (Error: BsAbstract.Interface.TYPE) => {
    module ValidationE =
      Relude_Validation.WithErrors(
        Relude_Array_Instances.SemigroupAny,
        Error,
      );
    module Traversable =
      Relude_Array_Instances.Traversable(ValidationE.Applicative);
  };

  module WithErrorsAsArrayOfStrings =
    WithErrorsAsArray({
      type t = string;
    });

  module WithErrorsAsNonEmptyArray = (Error: BsAbstract.Interface.TYPE) => {
    module ValidationE =
      Relude_Validation.WithErrors(Relude_NonEmpty.Array.SemigroupAny, Error);
    module Traversable =
      Relude_Array_Instances.Traversable(ValidationE.Applicative);
  };

  let traverse =
      (type a, type b, type e, f: a => Belt.Result.t(b, e), array: array(a))
      : Relude_Validation.t(array(b), Relude_NonEmpty.Array.t(e)) => {
    module Error = {
      type t = e;
    };
    module ValidationE = WithErrorsAsNonEmptyArray(Error);
    module Traversable = ValidationE.Traversable;
    Traversable.traverse(a => f(a)->Relude_Result.toValidationNea, array);
  };
};