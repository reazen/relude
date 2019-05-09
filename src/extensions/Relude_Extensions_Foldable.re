type ordering = BsAbstract.Interface.ordering;

/**
 * This is a collection of functions (and more modules that produce functions)
 * that you get for free if your outer type is foldable.
 */
module FoldableExtensions = (F: BsAbstract.Interface.FOLDABLE) => {
  /**
   * Alias fold_left from Foldable
   */
  let foldLeft = F.fold_left;

  /**
   * Alias fold_right from Foldable
   */
  let foldRight = F.fold_right;

  /**
   * Compare each value with a predicate, returning true if any value is true,
   * or false otherwise.
   */
  let any: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => foldLeft((v, x) => v || f(x), false, xs);

  let all: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => foldLeft((v, x) => v && f(x), true, xs);

  let containsBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => bool =
    (f, x, xs) => any(f(x), xs);

  let indexOfBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => option(int) =
    (f, x, xs) =>
      foldLeft(
        ((i, v), y) =>
          (i + 1, Relude_Option.alt(v, f(x, y) ? Some(i) : None)),
        (0, None),
        xs,
      )
      |> snd;

  let minBy: 'a. (('a, 'a) => ordering, F.t('a)) => option('a) =
    (f, xs) =>
      foldLeft(
        (min, x) =>
          switch (min) {
          | None => Some(x)
          | Some(y) => f(x, y) == `less_than ? Some(x) : Some(y)
          },
        None,
        xs,
      );

  let maxBy: 'a. (('a, 'a) => ordering, F.t('a)) => option('a) =
    (f, xs) =>
      foldLeft(
        (min, x) =>
          switch (min) {
          | None => Some(x)
          | Some(y) => f(x, y) == `greater_than ? Some(x) : Some(y)
          },
        None,
        xs,
      );

  let countBy: 'a. ('a => bool, F.t('a)) => int =
    (f, xs) => foldLeft((count, x) => f(x) ? count + 1 : count, 0, xs);

  let length: 'a. F.t('a) => int = xs => countBy(_ => true, xs);

  let forEach: 'a. ('a => unit, F.t('a)) => unit =
    (f, xs) => foldLeft((_, x) => f(x), (), xs);

  let forEachWithIndex: 'a. (('a, int) => unit, F.t('a)) => unit =
    (f, xs) =>
      foldLeft(
        (i, x) => {
          f(x, i);
          i + 1;
        },
        0,
        xs,
      )
      |> ignore;

  /**
   * Find an inner value given a predicate function
   */
  let find: 'a. ('a => bool, F.t('a)) => option('a) =
    f =>
      foldLeft(
        (v, x) => Relude_Option.alt(v, f(x) ? Some(x) : None),
        None,
      );

  let findWithIndex: 'a. (('a, int) => bool, F.t('a)) => option('a) =
    (f, xs) =>
      foldLeft(
        ((i, v), x) =>
          (i + 1, Relude_Option.alt(v, f(x, i) ? Some(x) : None)),
        (0, None),
        xs,
      )
      |> snd;

  /**
     * When the inner type is a Monoid, we can do some interesting things like
     * collect all values into a single value.
     */
  module FoldableMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
    let fold: F.t(M.t) => M.t = foldLeft(M.append, M.empty);

    let intercalate: (M.t, F.t(M.t)) => M.t =
      (sep, xs) =>
        foldLeft(
          ((init, acc), x) =>
            init ? (false, x) : (false, M.append(acc, M.append(sep, x))),
          (true, M.empty),
          xs,
        )
        |> snd;
  };

  /**
     * When the inner type implements EQ, we can use that to find inner elements
     */
  module FoldableEqExtensions = (E: BsAbstract.Interface.EQ) => {
    let contains: (E.t, F.t(E.t)) => bool = containsBy(E.eq);
    let indexOf: (E.t, F.t(E.t)) => option(int) = indexOfBy(E.eq);
  };

  /**
     * When the inner type implements ORD, we can find min and max
     */
  module FoldableOrdExtensions = (O: BsAbstract.Interface.ORD) => {
    include FoldableEqExtensions(O);
    let min: F.t(O.t) => option(O.t) = minBy(O.compare);
    let max: F.t(O.t) => option(O.t) = maxBy(O.compare);
  };

  /**
     * Accumulate all values in the Foldable into a single value that is the same
     * type as the inner type.
     */
  let fold =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        xs,
      ) => {
    module MonoidExtensions = FoldableMonoidExtensions((val monoidA));
    MonoidExtensions.fold(xs);
  };

  let intercalate =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        sep,
        xs,
      ) => {
    module MonoidExtensions = FoldableMonoidExtensions((val monoidA));
    MonoidExtensions.intercalate(sep, xs);
  };

  let contains =
      (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    containsBy(EqA.eq, x, xs);
  };

  let indexOf =
      (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    indexOfBy(EqA.eq, x, xs);
  };

  let min =
      (type a, ordA: (module BsAbstract.Interface.ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    minBy(OrdA.compare, xs);
  };

  let max =
      (type a, ordA: (module BsAbstract.Interface.ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    maxBy(OrdA.compare, xs);
  };
};