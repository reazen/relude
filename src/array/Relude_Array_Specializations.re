/**
 * Array extensions for when you have an EQ instance.
 */
module ArrayEqExtensions = (E: BsAbstract.Interface.EQ) => {
  include Relude_Array_Instances.FoldableEqExtensions(E);
  /**
   * Finds the distinct items of the array, based on the given EQ module.
   */
  let distinct: array(E.t) => array(E.t) =
    Relude_Array_Base.distinctBy(E.eq);

  /**
   * Removes the first item of the array which equals the given item, based on the given EQ module.
   */
  let removeFirst: (E.t, array(E.t)) => array(E.t) =
    Relude_Array_Base.removeFirstBy(E.eq);

  /**
   * Removes all item of the array which equal the given item, based on the given EQ module.
   */
  let removeEach: (E.t, array(E.t)) => array(E.t) =
    Relude_Array_Base.removeEachBy(E.eq);

  /**
   * Indicates if the two arrays are pairwise equal, based on the given EQ module.
   */
  let eq: (array(E.t), array(E.t)) => bool =
    Relude_Array_Instances.eqBy(E.eq);
};

/**
 * Array extensions for when you have an ORD instance.
 */
module ArrayOrdExtensions = (O: BsAbstract.Interface.ORD) => {
  include ArrayEqExtensions(O);
  include Relude_Array_Instances.FoldableOrdExtensions(O);

  /**
   * Sorts the array using the given ORD module.
   */
  let sort = Relude_Array_Base.sortBy(O.compare);
};

/**
 * Array extensions for when you have an MONOID instance.
 */
module ArrayMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
  include Relude_Array_Instances.FoldableMonoidExtensions(M);
};

/**
 * Array extensions for array(string)
 */
module String = {
  include ArrayOrdExtensions(Relude_String.Ord);
  include ArrayMonoidExtensions(Relude_String.Monoid);

  /**
   * Concatenates an array of strings using an empty string ("") delimiter.
   *
   * Alias of "foldWithMonoid" for array(string) with "" as the empty value and `++` as the append function.
   */
  let join: array(string) => string = foldWithMonoid;

  /**
   * Joins an array of strings using the given delimiter string.
   *
   * Alias of "intercalate" for array(string)
   */
  let joinWith: (string, array(string)) => string = intercalate;

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

/**
 * Array extensions for array(int)
 */
module Int = {
  include ArrayOrdExtensions(Relude_Int.Ord);

  /**
   * Finds the sum of all the ints in the array
   */
  let sum =
    Relude_Array_Instances.foldWithMonoid(
      (module Relude_Int.Additive.Monoid),
    );

  /**
   * Finds the product of all the ints in the array
   */
  let product =
    Relude_Array_Instances.foldWithMonoid(
      (module Relude_Int.Multiplicative.Monoid),
    );
};

/**
 * Array extensions for array(float)
 */
module Float = {
  include ArrayOrdExtensions(Relude_Float.Ord);
  /**
   * Finds the sum of all the floats in the array
   */
  let sum =
    Relude_Array_Instances.foldWithMonoid(
      (module Relude_Float.Additive.Monoid),
    );

  /**
   * Finds the product of all the floats in the array
   */
  let product =
    Relude_Array_Instances.foldWithMonoid(
      (module Relude_Float.Multiplicative.Monoid),
    );
};

/**
 * Array extensions for array(option('a))
 */
module Option = {
  include Relude_Array_Instances.Traversable(Relude_Option.Applicative);
};

/**
 * Array extensions for `array(Result.t('a, 'e))`
 */
module Result = {
  /**
   * Maps a function `'a => result('b, 'e)` over an `array('a)` and produces a
   * `result(array('b), 'e)`.
   */
  let traverse =
      (type e, f: 'a => result('b, e), xs: array('a))
      : result(array('b), e) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_Array_Instances.Traversable(ResultE.Applicative);
    TraverseResult.traverse(f, xs);
  };

  /**
   * Flips an `array(result('a, 'e))` into a `result(array('a), 'e)`
   */
  let sequence = (type e, xs: array(result('a, e))): result(array('a), e) => {
    module ResultE =
      Relude_Result.WithError({
        type t = e;
      });
    module TraverseResult =
      Relude_Array_Instances.Traversable(ResultE.Applicative);
    TraverseResult.sequence(xs);
  };
};

/**
 * Array extensions for array(IO.t('a, 'e))
 */
module IO = {
  /**
   * Maps a function `'a => IO.t('b, 'e)` over an `array('a)` and produces a `IO.t(array('b), 'e)`
   */
  let traverse =
      (type e, f: 'a => Relude_IO.t('b, e), xs: array('a))
      : Relude_IO.t(array('b), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = Relude_Array_Instances.Traversable(IoE.Applicative);
    TraverseIO.traverse(f, xs);
  };

  /**
   * Flips an `array(IO.t('a, 'e))` into a `IO.t(array('a), 'e)`
   */
  let sequence =
      (type e, xs: array(Relude_IO.t('a, e))): Relude_IO.t(array('a), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = Relude_Array_Instances.Traversable(IoE.Applicative);
    TraverseIO.sequence(xs);
  };
};

/**
 * Array extensions for `array(Validation.t('a, 'e))`
 */
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
      (type a, type b, type e, f: a => result(b, e), array: array(a))
      : Relude_Validation.t(array(b), Relude_NonEmpty.Array.t(e)) => {
    module Error = {
      type t = e;
    };
    module ValidationE = WithErrorsAsNonEmptyArray(Error);
    module Traversable = ValidationE.Traversable;
    Traversable.traverse(a => f(a)->Relude_Result.toValidationNea, array);
  };
};
