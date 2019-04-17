open BsAbstract.Interface;

/**
 * This is a collection of functions (and more modules that produce functions)
 * that you get for free if your outer type is foldable.
 */
module Functions = (F: FOLDABLE) => {
  /**
   * Alias fold_left and _right
   */
  let foldLeft = F.fold_left;
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
  module FoldableOfMonoid = (M: MONOID) => {
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
  module FoldableOfEq = (E: EQ) => {
    let contains: (E.t, F.t(E.t)) => bool = containsBy(E.eq);
    let indexOf: (E.t, F.t(E.t)) => option(int) = indexOfBy(E.eq);
  };

  /**
   * Accumulate all values in the Foldable into a single value that is the same
   * type as the inner type.
   */
  let fold = (type a, monoidA: (module MONOID with type t = a), xs) => {
    module FoldableMonoid = FoldableOfMonoid((val monoidA));
    FoldableMonoid.fold(xs);
  };

  let intercalate =
      (type a, monoidA: (module MONOID with type t = a), sep, xs) => {
    module FoldableMonoid = FoldableOfMonoid((val monoidA));
    FoldableMonoid.intercalate(sep, xs);
  };

  let contains = (type a, eqA: (module EQ with type t = a), x, xs) => {
    module FoldableEq = FoldableOfEq((val eqA));
    FoldableEq.contains(x, xs);
  };

  let indexOf = (type a, eqA: (module EQ with type t = a), x, xs) => {
    module FoldableEq = FoldableOfEq((val eqA));
    FoldableEq.indexOf(x, xs);
  };
};
