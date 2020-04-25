open BsBastet.Interface;

/**
`Relude.Extensions.Foldable` contains module functors which give you access to a wide variety of functions that you can get "for free" when you have a `Foldable` typeclass instance.  For example, if you have a `Foldable` instance for `array('a)`, you can get functions like `any`, `all`, `length`, `forEach`, and many more for `array('a)`.

If you have additional typeclass instances like a `Monoid`, `Eq`, `Ord`, etc. for the innner type of the `Foldable`, you can get access to additional functions using other module functors like `FoldableMonoidExtensions`, `FoldableEqExtensions`, etc.
*/

// break circular dependency
let optionAlt: (option('a), option('a)) => option('a) =
  (a, b) =>
    switch (a) {
    | Some(_) as v => v
    | None => b
    };

/**
 * Extensions for any FOLDABLE
 */
module FoldableExtensions = (F: FOLDABLE) => {
  module BsFoldableExtensions = BsBastet.Functions.Foldable(F);

  /**
   * Indicates if any item in the foldable satisfies the given predicate
   */
  let any: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v || f(x), false, xs);

  /**
   * Indicates if all items in the foldable satisfy the given predicate
   */
  let all: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v && f(x), true, xs);

  /**
   * Indicates if the foldable contains the given item, using the given equality function
   */
  let containsBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => bool =
    (f, x, xs) => any(f(x), xs);

  /**
   * Indicates if the foldable contains the given item, using the given EQ module
   */
  let contains = (type a, eqA: (module EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    containsBy(EqA.eq, x, xs);
  };

  /**
   * Finds the index of the given item in the foldable using the given equality function.
   * If the item is not found, the result is None.
   */
  let indexOfBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => option(int) =
    (f, x, xs) =>
      F.fold_left(
        ((i, v), y) => (i + 1, optionAlt(v, f(x, y) ? Some(i) : None)),
        (0, None),
        xs,
      )
      |> snd;

  /**
   * Finds the index of the given item in the foldable using the given EQ module.
   * If the item is not found, the result is None.
   */
  let indexOf = (type a, eqA: (module EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    indexOfBy(EqA.eq, x, xs);
  };

  /**
   * Finds the minimum item in the foldable, using the given comparison function.
   */
  let minBy: 'a. (('a, 'a) => ordering, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        (min, x) =>
          switch (min) {
          | None => Some(x)
          | Some(y) => Some(Relude_Ord.minBy(f, x, y))
          },
        None,
        xs,
      );

  /**
   * Finds the minimum item in the foldable, using the given ORD module.
   */
  let min = (type a, ordA: (module ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    minBy(OrdA.compare, xs);
  };

  /**
   * Finds the maximum item in the foldable, using the given comparison function.
   */
  let maxBy: 'a. (('a, 'a) => ordering, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        (min, x) =>
          switch (min) {
          | None => Some(x)
          | Some(y) => f(x, y) == `greater_than ? Some(x) : Some(y)
          },
        None,
        xs,
      );

  /**
   * Finds the maximum item in the foldable, using the given ORD module.
   */
  let max = (type a, ordA: (module ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    maxBy(OrdA.compare, xs);
  };

  /**
   * Counts the number of items in the foldable which satisfy the given predicate.
   */
  let countBy: 'a. ('a => bool, F.t('a)) => int =
    (f, xs) => F.fold_left((count, x) => f(x) ? count + 1 : count, 0, xs);

  /**
   * Gets the length of the foldable.
   *
   * Alias of size/count
   */
  let length: 'a. F.t('a) => int = xs => countBy(_ => true, xs);

  /**
   * Alias for length/count
   */
  let size: 'a. F.t('a) => int = length;

  /**
   * Alias for length/size
   */
  let count: 'a. F.t('a) => int = length;

  /**
   * Runs a side-effect function for each item in the foldable.
   */
  let forEach: 'a. ('a => unit, F.t('a)) => unit =
    (f, xs) => F.fold_left((_, x) => f(x), (), xs);

  /**
   * Runs a side-effect function for each indexed item in the foldable.
   */
  let forEachWithIndex: 'a. (('a, int) => unit, F.t('a)) => unit =
    (f, xs) =>
      F.fold_left(
        (i, x) => {
          f(x, i);
          i + 1;
        },
        0,
        xs,
      )
      |> ignore;

  /**
   * Finds the first item in the foldable which satisfies the given predicate.
   */
  let find: 'a. ('a => bool, F.t('a)) => option('a) =
    f => F.fold_left((v, x) => optionAlt(v, f(x) ? Some(x) : None), None);

  /**
   * Finds the first indexed item in the foldable which satisfies the given predicate.
   */
  let findWithIndex: 'a. (('a, int) => bool, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        ((i, v), x) => (i + 1, optionAlt(v, f(x, i) ? Some(x) : None)),
        (0, None),
        xs,
      )
      |> snd;

  /**
   * Converts the foldable into a list
   */
  let toList: F.t('a) => list('a) =
    fa => F.fold_right((a, acc) => [a, ...acc], [], fa);

  /**
   * Converts the foldable into an array
   */
  let toArray: F.t('a) => array('a) =
    fa => F.fold_left((acc, a) => Belt.Array.concat(acc, [|a|]), [||], fa);

  /**
   * Foldable extensions for when you have a Semigroup instance
   */
  module FoldableSemigroupExtensions = (S: SEMIGROUP) => {
    module BsFoldableSemigroupExtensions = BsFoldableExtensions.Semigroup(S);

    let surroundMap: 'a. (~delimiter: S.t, 'a => S.t, F.t('a)) => S.t = BsFoldableSemigroupExtensions.surround_map;

    let surround: (~delimiter: S.t, F.t(S.t)) => S.t = BsFoldableSemigroupExtensions.surround;
  };

  /**
   * Foldable extensions for when you have a Monoid instance
   */
  module FoldableMonoidExtensions = (M: MONOID) => {
    module BsFoldableMonoidExtensions = BsFoldableExtensions.Monoid(M);

    /**
     * Maps a function over the foldable, and collects the results via the Monoid instance.
     */
    let foldMap: 'a. ('a => M.t, F.t('a)) => M.t = BsFoldableMonoidExtensions.FM.fold_map;

    /**
     * Folds a foldable of a monoidal type by accumlating values via the monoid instance.
     */
    let foldWithMonoid: F.t(M.t) => M.t = BsFoldableMonoidExtensions.fold;

    /**
     * Folds a foldable, accumulating values in a monoid, combining adjacent elements using the specified
     * separator.
     */
    let intercalate: (M.t, F.t(M.t)) => M.t =
      (sep, xs) =>
        F.fold_left(
          ((init, acc), x) =>
            init ? (false, x) : (false, M.append(acc, M.append(sep, x))),
          (true, M.empty),
          xs,
        )
        |> snd;
  };

  /**
   * Maps a function over a foldable, and accumulates the results using the given Monoid module.
   */
  let foldMap = (type a, monoidA: (module MONOID with type t = a), f, xs) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.foldMap(f, xs);
  };

  /**
   * Folds a foldable of a monoidal type, accumulating the result using the given Monoid module.
   */
  let foldWithMonoid =
      (type a, monoidA: (module MONOID with type t = a), xs: F.t(a)) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.foldWithMonoid(xs);
  };

  /**
   * Folds a foldable, accumulating values in a monoid, combining adjacent elements using the specified
   * separator.
   */
  let intercalate =
      (type a, monoidA: (module MONOID with type t = a), sep, xs) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.intercalate(sep, xs);
  };

  /**
   * Foldable extensions for when you additionally have an Applicative instance.
   */
  module FoldableApplicativeExtensions = (A: APPLICATIVE) => {
    module BsFoldableApplicativeExtensions =
      BsFoldableExtensions.Applicative(A);

    /**
     * Traverse which performs the side effects of the traversal, but throws away the final result (produces unit).
     */
    let traverse_: 'a 'b. ('a => A.t('b), F.t('a)) => A.t(unit) = BsFoldableApplicativeExtensions.traverse';

    /**
     * Sequence which performs the side effects of the sequence, but throws away the final result (produces unit).
     */
    let sequence_: 'a 'b. F.t(A.t('a)) => A.t(unit) = BsFoldableApplicativeExtensions.sequence';
  };

  /**
   * Foldable extensions for when you additionally have a MONAD instance.
   */
  module FoldableMonadExtensions = (M: MONAD) => {
    module BsFoldableMonadExtensions = BsFoldableExtensions.Monad(M);

    /**
     * Folds the foldable sequentially, using a monadic function, chained on the previous value for each iteration.
     */
    let foldWithMonad:
      'a 'acc.
      (('acc, 'a) => M.t('acc), 'acc, F.t('a)) => M.t('acc)
     = BsFoldableMonadExtensions.fold_monad;
  };

  /**
   * Foldable extensions for when you additionally have an EQ instance.
   */
  module FoldableEqExtensions = (E: EQ) => {
    /**
     * Indicates of the foldable contains the given item using the given EQ module.
     */
    let contains: (E.t, F.t(E.t)) => bool = containsBy(E.eq);

    /**
     * Returns the index of the item in the foldable, using the given EQ module.
     */
    let indexOf: (E.t, F.t(E.t)) => option(int) = indexOfBy(E.eq);
  };

  /**
   * Foldable extensions for when you additionally have an ORD instance.
   */
  module FoldableOrdExtensions = (O: ORD) => {
    /**
     * Gets the minimum value of the foldable, using the given ORD module.
     */
    let min: F.t(O.t) => option(O.t) = minBy(O.compare);

    /**
     * Gets the maximum value of the foldable, using the given ORD module.
     */
    let max: F.t(O.t) => option(O.t) = maxBy(O.compare);
  };
};
