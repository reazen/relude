/**
 * This is a collection of functions (and more modules that produce functions)
 * that you get for free if your outer type is foldable.
 */
module Functions = (F: BsAbstract.Interface.FOLDABLE) => {
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
  /* let mapOption: 'a 'b. ('a => option('b), list('a)) => list('b) =
     (f, xs) =>
       F.fold_left(
         (acc, curr) =>
           Relude_Option.fold(() => acc, v => [v, ...acc], f(curr)),
           [],
           xs
       )
       |> */
};
