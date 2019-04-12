module NonEmptyF = (TailSequence: Relude_Sequence.SEQUENCE) => {
  type t('a) =
    | NonEmpty('a, TailSequence.t('a));

  let length: t('a) => int =
    fun
    | NonEmpty(_, t) => 1 + TailSequence.length(t);

  let pure: 'a => t('a) =
    head => NonEmpty(head, TailSequence.MonoidAny.empty);

  let make: ('a, TailSequence.t('a)) => t('a) =
    (head, tailSequence) => NonEmpty(head, tailSequence);

  let fromSequence: TailSequence.t('a) => option(t('a)) =
    sequence =>
      TailSequence.head(sequence)
      ->Belt.Option.map(head =>
          NonEmpty(head, TailSequence.tailOrEmpty(sequence))
        );

  let toSequence: t('a) => TailSequence.t('a) =
    fun
    | NonEmpty(head, tail) =>
      TailSequence.MonoidAny.append(
        TailSequence.Monad.pure(head),
        tail,
      );

  let cons: ('a, t('a)) => t('a) =
    (head, tailNonEmpty) => NonEmpty(head, toSequence(tailNonEmpty));

  let head: t('a) => 'a =
    fun
    | NonEmpty(head, _) => head;

  let tail: t('a) => TailSequence.t('a) =
    fun
    | NonEmpty(_, tail) => tail;

  let concat: (t('a), t('a)) => t('a) =
    (nonEmpty1, nonEmpty2) =>
      NonEmpty(
        head(nonEmpty1),
        TailSequence.MonoidAny.append(
          tail(nonEmpty1),
          toSequence(nonEmpty2),
        ),
      );

  let reduceLeft: (('a, 'a) => 'a, t('a)) => 'a =
    (f, NonEmpty(x, xs)) => TailSequence.Foldable.fold_left(f, x, xs);

  let foldLeft: (('b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      TailSequence.Foldable.fold_left(f, f(init, x), xs);

  let foldRight: (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      f(x, TailSequence.Foldable.fold_right(f, init, xs));

  let flatten: t(t('a)) => t('a) =
    nonEmpty => reduceLeft(concat, nonEmpty);

  let join = flatten;

  let map: ('a => 'b, t('a)) => t('b) =
    (f, NonEmpty(x, xs)) => NonEmpty(f(x), TailSequence.Monad.map(f, xs));

  let apply: (t('a => 'b), t('a)) => t('b) =
    (ff, fa) => map(f => map(f, fa), ff) |> flatten;

  let bind: (t('a), 'a => t('b)) => t('b) =
    (nonEmpty, f) => map(f, nonEmpty) |> flatten;

  let flatMap: ('a => t('b), t('a)) => t('b) = (f, fa) => bind(fa, f);

  let mkString: (string, t(string)) => string =
    (delim, xs) =>
      switch (xs) {
      | NonEmpty(y, ys) => y ++ delim ++ TailSequence.mkString(delim, ys)
      };

  let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool =
    (eqA, xs, ys) =>
      switch (xs, ys) {
      | (NonEmpty(x, xs), NonEmpty(y, ys)) =>
        eqA(x, y) && TailSequence.eqBy(eqA, xs, ys)
      };

  let eq =
      (
        type a,
        eqA: (module BsAbstract.Interface.EQ with type t = a),
        xs: t(a),
        ys: t(a),
      )
      : bool => {
    module EqA = (val eqA);
    eqBy(EqA.eq, xs, ys);
  };

  let showBy: ('a => string, t('a)) => string =
    (showX, xs) => {
      let strings = map(showX, xs);
      "[!" ++ mkString(", ", strings) ++ "!]";
    };

  let show =
      (
        type a,
        showA: (module BsAbstract.Interface.SHOW with type t = a),
        xs: t(a),
      )
      : string => {
    module ShowA = (val showA);
    showBy(ShowA.show, xs);
  };

  module SemigroupAny:
    BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = concat;
  };

  module MagmaAny: BsAbstract.Interface.MAGMA_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = concat;
  };

  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = bind;
  };

  module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = t('a) = {
    type nonrec t('a) = t('a);

    let fold_left = foldLeft;

    let fold_right = foldRight;

    module Fold_Map = (FoldMapMonoid: BsAbstract.Interface.MONOID) => {
      module TailFoldMap = TailSequence.Foldable.Fold_Map(FoldMapMonoid);
      let fold_map: ('a => FoldMapMonoid.t, t('a)) => FoldMapMonoid.t =
        (f, NonEmpty(x, xs)) =>
          FoldMapMonoid.append(f(x), TailFoldMap.fold_map(f, xs));
    };

    module Fold_Map_Plus = (FoldMapPlus: BsAbstract.Interface.PLUS) => {
      module TailFoldMapPlus =
        TailSequence.Foldable.Fold_Map_Plus(FoldMapPlus);
      let fold_map: ('a => FoldMapPlus.t('a), t('a)) => FoldMapPlus.t('a) =
        (f, NonEmpty(x, xs)) =>
          FoldMapPlus.alt(f(x), TailFoldMapPlus.fold_map(f, xs));
    };

    module Fold_Map_Any = (FoldMapAny: BsAbstract.Interface.MONOID_ANY) => {
      module SequenceFoldMapAny =
        TailSequence.Foldable.Fold_Map_Any(FoldMapAny);
      let fold_map: ('a => FoldMapAny.t('a), t('a)) => FoldMapAny.t('a) =
        (f, NonEmpty(x, xs)) =>
          FoldMapAny.append(f(x), SequenceFoldMapAny.fold_map(f, xs));
    };
  };

  module type TRAVERSABLE_F =
    (A: BsAbstract.Interface.APPLICATIVE) =>

      BsAbstract.Interface.TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a);

  module Traversable: TRAVERSABLE_F =
    (A: BsAbstract.Interface.APPLICATIVE) => {
      type nonrec t('a) = t('a);
      type applicative_t('a) = A.t('a);
      include (
                Functor: BsAbstract.Interface.FUNCTOR with type t('a) := t('a)
              );
      include (
                Foldable:
                  BsAbstract.Interface.FOLDABLE with type t('a) := t('a)
              );
      module TailTraversable = TailSequence.Traversable(A);

      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b)) =
        (f, NonEmpty(x, xs)) => {
          let headA: A.t('b) = f(x);
          let tailA: A.t(TailSequence.t('b)) =
            TailTraversable.traverse(f, xs);
          A.apply(A.map(make, headA), tailA);
        };

      let sequence: t(applicative_t('a)) => applicative_t(t('a)) =
        fa => traverse(x => x, fa);
    };

  module type EQ_F =
    (EqA: BsAbstract.Interface.EQ) =>
     BsAbstract.Interface.EQ with type t = t(EqA.t);

  module Eq: EQ_F =
    (EqA: BsAbstract.Interface.EQ) => {
      type nonrec t = t(EqA.t);
      let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
    };
};

/* NonEmpty.List */
module List =
  NonEmptyF({
    type t('a) = list('a);
    let length = Relude_List_Types.length;
    let isEmpty = Relude_List_Base.isEmpty;
    let isNotEmpty = Relude_List_Base.isNotEmpty;
    let head = Relude_List_Base.head;
    let tail = Relude_List_Base.tail;
    let tailOrEmpty = Relude_List_Base.tailOrEmpty;
    let mkString =
      Relude_List_Types.intercalate((module BsAbstract.String.Monoid));
    let eqBy = Relude_List_Types.eqBy;
    let showBy = Relude_List_Types.showBy;
    module SemigroupAny = Relude_List_Types.SemigroupAny;
    module MonoidAny = Relude_List_Types.MonoidAny;
    module Functor = Relude_List_Types.Functor;
    module Apply = Relude_List_Types.Apply;
    module Applicative = Relude_List_Types.Applicative;
    module Monad = Relude_List_Types.Monad;
    module Foldable = Relude_List_Types.Foldable;
    module Traversable = Relude_List_Types.Traversable;
    module Eq = Relude_List_Types.Eq;
    module Show = Relude_List_Types.Show;
  });

/* NonEmpty.Array */
module Array = NonEmptyF(Relude_Array.Sequence);
