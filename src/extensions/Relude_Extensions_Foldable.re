type ordering = BsAbstract.Interface.ordering;

// break circular dependency
let optionAlt: (option('a), option('a)) => option('a) =
  (a, b) =>
    switch (a) {
    | Some(_) as v => v
    | None => b
    };

module FoldableExtensions = (F: BsAbstract.Interface.FOLDABLE) => {
  module BsFoldableExtensions = BsAbstract.Functions.Foldable(F);

  //let foldLeft = F.fold_left;

  //let foldRight = F.fold_right;

  let any: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v || f(x), false, xs);

  let all: 'a. ('a => bool, F.t('a)) => bool =
    (f, xs) => F.fold_left((v, x) => v && f(x), true, xs);

  let containsBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => bool =
    (f, x, xs) => any(f(x), xs);

  let contains =
      (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    containsBy(EqA.eq, x, xs);
  };

  let indexOfBy: 'a. (('a, 'a) => bool, 'a, F.t('a)) => option(int) =
    (f, x, xs) =>
      F.fold_left(
        ((i, v), y) => (i + 1, optionAlt(v, f(x, y) ? Some(i) : None)),
        (0, None),
        xs,
      )
      |> snd;

  let indexOf =
      (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), x, xs) => {
    module EqA = (val eqA);
    indexOfBy(EqA.eq, x, xs);
  };

  let minBy: 'a. (('a, 'a) => ordering, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        (min, x) =>
          switch (min) {
          | None => Some(x)
          | Some(y) => Some(Relude_Extensions_Ord.minBy(f, x, y))
          },
        None,
        xs,
      );

  let min =
      (type a, ordA: (module BsAbstract.Interface.ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    minBy(OrdA.compare, xs);
  };

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

  let max =
      (type a, ordA: (module BsAbstract.Interface.ORD with type t = a), xs) => {
    module OrdA = (val ordA);
    maxBy(OrdA.compare, xs);
  };

  let countBy: 'a. ('a => bool, F.t('a)) => int =
    (f, xs) => F.fold_left((count, x) => f(x) ? count + 1 : count, 0, xs);

  let length: 'a. F.t('a) => int = xs => countBy(_ => true, xs);

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

  let find: 'a. ('a => bool, F.t('a)) => option('a) =
    f => F.fold_left((v, x) => optionAlt(v, f(x) ? Some(x) : None), None);

  let findWithIndex: 'a. (('a, int) => bool, F.t('a)) => option('a) =
    (f, xs) =>
      F.fold_left(
        ((i, v), x) => (i + 1, optionAlt(v, f(x, i) ? Some(x) : None)),
        (0, None),
        xs,
      )
      |> snd;

  let toList: F.t('a) => list('a) =
    fa => F.fold_right((a, acc) => [a, ...acc], [], fa);

  let toArray: F.t('a) => array('a) =
    fa => F.fold_left((acc, a) => Belt.Array.concat(acc, [|a|]), [||], fa);

  module FoldableSemigroupExtensions = (S: BsAbstract.Interface.SEMIGROUP) => {
    module BsFoldableSemigroupExtensions = BsFoldableExtensions.Semigroup(S);

    // Note: Bug in bs-abstract - the universal quantification of 'a here doesn't jive
    //let surroundMap: 'a. (~delimiter: S.t, 'a => S.t, F.t('a)) => S.t = BsFoldableSemigroupExtensions.surround_map;
    let surroundMap: (~delimiter: S.t, 'a => S.t, F.t('a)) => S.t = BsFoldableSemigroupExtensions.surround_map;

    let surround: (~delimiter: S.t, F.t(S.t)) => S.t = BsFoldableSemigroupExtensions.surround;
  };

  module FoldableMonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
    module BsFoldableMonoidExtensions = BsFoldableExtensions.Monoid(M);

    let foldMap: 'a. ('a => M.t, F.t('a)) => M.t = BsFoldableMonoidExtensions.FM.fold_map;

    let foldWithMonoid: F.t(M.t) => M.t = BsFoldableMonoidExtensions.fold;

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

  let foldMap =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        f,
        xs,
      ) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.foldMap(f, xs);
  };

  // Name this foldMonoid to avoid conflicting with things like Option.fold and Result.fold, which have a different purpose
  let foldWithMonoid =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        xs: F.t(a),
      ) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.foldWithMonoid(xs);
  };

  let intercalate =
      (
        type a,
        monoidA: (module BsAbstract.Interface.MONOID with type t = a),
        sep,
        xs,
      ) => {
    module FoldableMonoidExtensions = FoldableMonoidExtensions((val monoidA));
    FoldableMonoidExtensions.intercalate(sep, xs);
  };

  module FoldableApplicativeExtensions = (A: BsAbstract.Interface.APPLICATIVE) => {
    module BsFoldableApplicativeExtensions =
      BsFoldableExtensions.Applicative(A);

    let traverse_: 'a 'b. ('a => A.t('b), F.t('a)) => A.t(unit) = BsFoldableApplicativeExtensions.traverse';

    let sequence_: 'a 'b. F.t(A.t('a)) => A.t(unit) = BsFoldableApplicativeExtensions.sequence';
  };

  module FoldableMonadExtensions = (M: BsAbstract.Interface.MONAD) => {
    module BsFoldableMonadExtensions = BsFoldableExtensions.Monad(M);

    let foldWithMonad:
      'a 'acc.
      (('acc, 'a) => M.t('acc), 'acc, F.t('a)) => M.t('acc)
     = BsFoldableMonadExtensions.fold_monad;
  };

  module FoldableEqExtensions = (E: BsAbstract.Interface.EQ) => {
    let contains: (E.t, F.t(E.t)) => bool = containsBy(E.eq);

    let indexOf: (E.t, F.t(E.t)) => option(int) = indexOfBy(E.eq);
  };

  module FoldableOrdExtensions = (O: BsAbstract.Interface.ORD) => {
    let min: F.t(O.t) => option(O.t) = minBy(O.compare);

    let max: F.t(O.t) => option(O.t) = maxBy(O.compare);
  };
};
