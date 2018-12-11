/**
 * This is a collection of functions (and more modules that produce functions)
 * that you get for free if your outer type is foldable.
 */
module Functions = (F: BsAbstract.Interface.FOLDABLE) => {
  /**
   * Compare each value with a predicate, returning true if any value is true,
   * or false otherwise.
   */
  let any: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v || f(x), false, xs);

  let all: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v && f(x), true, xs);

  let forEach: 'a. ('a => unit, F.t('a)) => unit =
    (f, xs) => F.fold_left((_, x) => f(x), (), xs);

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
   * Find an inner value given a predicate function
   */
  let find: 'a. ('a => bool, F.t('a)) => option('a) =
    f =>
      F.fold_left(
        (v, x) => Relude_Option.alt(v, f(x) ? Some(x) : None),
        None,
      );

  let findWithIndex: 'a. (('a, int) => bool, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        ((i, v), x) => (
          i + 1,
          Relude_Option.alt(v, f(x, i) ? Some(x) : None),
        ),
        (0, None),
        xs,
      )
      |> snd;

  /**
   * When the inner type is a Monoid, the collection can be folded into a
   * single value (e.g. sum ints or floats, join strings)
   */
  let fold =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        xs: F.t(a),
      )
      : a => {
    module M = (val monoidA);
    F.fold_left((acc, curr) => M.append(acc, curr), M.empty, xs);
  };

  let intercalate =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        sep: a,
        xs: F.t(a),
      )
      : a => {
    module M = (val monoidA);
    let go = ((init, acc), x) =>
      init ? (false, x) : (false, M.append(acc, M.append(sep, x)));

    F.fold_left(go, (true, M.empty), xs) |> snd;
  };

  let countBy: 'a. ('a => bool, F.t('a)) => int =
    (f, xs) => F.fold_left((acc, curr) => f(curr) ? acc + 1 : acc, 0, xs);

  let length: 'a. F.t('a) => int =
    xs => F.fold_left((acc, _) => acc + 1, 0, xs);

  let containsF: 'a. (('a, 'a) => bool, 'a, F.t('a)) => bool =
    (f, x, xs) => any(f(x), xs);

  let contains =
      (
        type a,
        eqA: (module BsAbstract.Interface.EQ with type t = a),
        x: a,
        xs: F.t(a),
      )
      : bool => {
    module EqA = (val eqA);
    containsF(EqA.eq, x, xs);
  };

  let indexOfF: 'a. (('a, 'a) => bool, 'a, F.t('a)) => option(int) =
    (f, x, xs) =>
      F.fold_left(
        ((i, v), y) => (
          i + 1,
          Relude_Option.alt(v, f(x, y) ? Some(i) : None),
        ),
        (0, None),
        xs,
      )
      |> snd;

  let indexOf =
      (
        type a,
        eqA: (module BsAbstract.Interface.EQ with type t = a),
        x: a,
        xs: F.t(a),
      )
      : option(int) => {
    module EqA = (val eqA);
    indexOfF(EqA.eq, x, xs);
  };
};
