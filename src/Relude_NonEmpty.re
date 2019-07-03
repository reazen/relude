module WithSequence = (TailSequence: Relude_Interface.SEQUENCE) => {
  type t('a) =
    | NonEmpty('a, TailSequence.t('a));

  let one: 'a. 'a => t('a) =
    head => NonEmpty(head, TailSequence.MonoidAny.empty);

  let make: 'a. ('a, TailSequence.t('a)) => t('a) =
    (head, tailSequence) => NonEmpty(head, tailSequence);

  let fromSequence: 'a. TailSequence.t('a) => option(t('a)) =
    sequence =>
      TailSequence.head(sequence)
      ->Belt.Option.map(head =>
          NonEmpty(head, TailSequence.tailOrEmpty(sequence))
        );

  let toSequence: 'a. t('a) => TailSequence.t('a) =
    fun
    | NonEmpty(head, tail) =>
      TailSequence.MonoidAny.append(TailSequence.Monad.pure(head), tail);

  let cons: 'a. ('a, t('a)) => t('a) =
    (head, tailNonEmpty) => NonEmpty(head, toSequence(tailNonEmpty));

  let uncons: 'a. t('a) => ('a, TailSequence.t('a)) =
    fun
    | NonEmpty(head, tail) => (head, tail);

  let head: 'a. t('a) => 'a =
    fun
    | NonEmpty(head, _) => head;

  let tail: 'a. t('a) => TailSequence.t('a) =
    fun
    | NonEmpty(_, tail) => tail;

  let concat: 'a. (t('a), t('a)) => t('a) =
    (nonEmpty1, nonEmpty2) =>
      NonEmpty(
        head(nonEmpty1),
        TailSequence.MonoidAny.append(
          tail(nonEmpty1),
          toSequence(nonEmpty2),
        ),
      );

  module SemigroupAny:
    BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = concat;
  };

  module MagmaAny: BsAbstract.Interface.MAGMA_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = concat;
  };

  let reduceLeft: 'a. (('a, 'a) => 'a, t('a)) => 'a =
    (f, NonEmpty(x, xs)) => TailSequence.Foldable.fold_left(f, x, xs);

  let foldLeft: 'a 'b. (('b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      TailSequence.Foldable.fold_left(f, f(init, x), xs);

  let foldRight: 'a 'b. (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      f(x, TailSequence.Foldable.fold_right(f, init, xs));

  module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = t('a) = {
    type nonrec t('a) = t('a);

    let fold_left = foldLeft;

    let fold_right = foldRight;

    module Fold_Map = (FoldMapMonoid: BsAbstract.Interface.MONOID) => {
      module TailFoldMap = TailSequence.Foldable.Fold_Map(FoldMapMonoid);
      let fold_map: 'a. ('a => FoldMapMonoid.t, t('a)) => FoldMapMonoid.t =
        (f, NonEmpty(x, xs)) =>
          FoldMapMonoid.append(f(x), TailFoldMap.fold_map(f, xs));
    };

    module Fold_Map_Plus = (FoldMapPlus: BsAbstract.Interface.PLUS) => {
      module TailFoldMapPlus =
        TailSequence.Foldable.Fold_Map_Plus(FoldMapPlus);
      let fold_map:
        'a.
        ('a => FoldMapPlus.t('a), t('a)) => FoldMapPlus.t('a)
       =
        (f, NonEmpty(x, xs)) =>
          FoldMapPlus.alt(f(x), TailFoldMapPlus.fold_map(f, xs));
    };

    module Fold_Map_Any = (FoldMapAny: BsAbstract.Interface.MONOID_ANY) => {
      module SequenceFoldMapAny =
        TailSequence.Foldable.Fold_Map_Any(FoldMapAny);
      let fold_map: 'a. ('a => FoldMapAny.t('a), t('a)) => FoldMapAny.t('a) =
        (f, NonEmpty(x, xs)) =>
          FoldMapAny.append(f(x), SequenceFoldMapAny.fold_map(f, xs));
    };
  };
  include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

  let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (f, NonEmpty(x, xs)) => NonEmpty(f(x), TailSequence.Monad.map(f, xs));

  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  let flatten: 'a. t(t('a)) => t('a) =
    nonEmpty => reduceLeft(concat, nonEmpty);

  let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
    (ff, fa) => map(f => map(f, fa), ff) |> flatten;

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  let pure = one;

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
    (nonEmpty, f) => map(f, nonEmpty) |> flatten;

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = bind;
  };
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  let mkString: (string, t(string)) => string =
    (delim, xs) =>
      switch (xs) {
      | NonEmpty(y, ys) => y ++ delim ++ TailSequence.mkString(delim, ys)
      };

  let eqBy: 'a. (('a, 'a) => bool, t('a), t('a)) => bool =
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

  module type EQ_F =
    (EqA: BsAbstract.Interface.EQ) =>
     BsAbstract.Interface.EQ with type t = t(EqA.t);

  module Eq: EQ_F =
    (EqA: BsAbstract.Interface.EQ) => {
      type nonrec t = t(EqA.t);
      let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
    };

  let showBy: 'a. ('a => string, t('a)) => string =
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

  module type SHOW_F =
    (S: BsAbstract.Interface.SHOW) =>
     BsAbstract.Interface.SHOW with type t = t(S.t);

  module Show: SHOW_F =
    (S: BsAbstract.Interface.SHOW) => {
      type nonrec t = t(S.t);
      let show = showBy(S.show);
    };

  module WithApplicative = (A: BsAbstract.Interface.APPLICATIVE) => {
    module Traversable:
      BsAbstract.Interface.TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a) = {
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
        'a 'b.
        ('a => applicative_t('b), t('a)) => applicative_t(t('b))
       =
        (f, NonEmpty(x, xs)) => {
          A.apply(A.map(make, f(x)), TailTraversable.traverse(f, xs));
        };

      let sequence: t(applicative_t('a)) => applicative_t(t('a)) =
        fa => traverse(x => x, fa);
    };
    let traverse = Traversable.traverse;
    let sequence = Traversable.sequence;
    include Relude_Extensions_Traversable.TraversableExtensions(Traversable);
  };
};

module List =
  WithSequence({
    type t('a) = list('a);
    let empty = [];
    let length = Relude_List_Instances.length;
    let isEmpty = Relude_List_Base.isEmpty;
    let isNotEmpty = Relude_List_Base.isNotEmpty;
    let head = Relude_List_Base.head;
    let tail = Relude_List_Base.tail;
    let tailOrEmpty = Relude_List_Base.tailOrEmpty;
    let eqBy = Relude_List_Instances.eqBy;
    let showBy = Relude_List_Instances.showBy;
    let mkString =
      Relude_List_Instances.intercalate((module BsAbstract.String.Monoid));
    module MonoidAny = Relude_List_Instances.MonoidAny;
    module Monad = Relude_List_Instances.Monad;
    module Foldable = Relude_List_Instances.Foldable;
    module Traversable = Relude_List_Instances.Traversable;
    module Eq = Relude_List_Instances.Eq;
    module Show = Relude_List_Instances.Show;
  });

module Array =
  WithSequence({
    type t('a) = array('a);
    let empty = [||];
    let length = Relude_Array_Base.length;
    let isEmpty = Relude_Array_Base.isEmpty;
    let isNotEmpty = Relude_Array_Base.isNotEmpty;
    let head = Relude_Array_Base.head;
    let tail = Relude_Array_Base.tail;
    let tailOrEmpty = Relude_Array_Base.tailOrEmpty;
    let eqBy = Relude_Array_Instances.eqBy;
    let showBy = Relude_Array_Instances.showBy;
    let mkString =
      Relude_Array_Instances.intercalate((module BsAbstract.String.Monoid));
    module MonoidAny = Relude_Array_Instances.MonoidAny;
    module Monad = Relude_Array_Instances.Monad;
    module Foldable = Relude_Array_Instances.Foldable;
    module Traversable = Relude_Array_Instances.Traversable;
    module Eq = Relude_Array_Instances.Eq;
    module Show = Relude_Array_Instances.Show;
  });