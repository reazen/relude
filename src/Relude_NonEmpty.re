module Interface = Relude_Interface;

module NonEmptyF =
       (
         TailSequence: Interface.SEQUENCE,
         TailMonoid:
           BsAbstract.Interface.MONOID_ANY with
             type t('a) = TailSequence.t('a),
         TailApplicative:
           BsAbstract.Interface.APPLICATIVE with
             type t('a) = TailSequence.t('a),
         TailFoldable:
           BsAbstract.Interface.FOLDABLE with
             type t('a) = TailSequence.t('a),
       ) => {
  /*
   TailTraversable: BsAbstract.Interface.TRAVERSABLE with type t('a) = TailSequence.t('a),
   */

  type t('a) =
    | NonEmpty('a, TailSequence.t('a));

  let length: t('a) => int =
    fun
    | NonEmpty(_, t) => 1 + TailSequence.length(t);

  let pure: 'a => t('a) = head => NonEmpty(head, TailMonoid.empty);

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
      TailMonoid.append(TailApplicative.pure(head), tail);

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
        TailMonoid.append(tail(nonEmpty1), toSequence(nonEmpty2)),
      );

  let reduceLeft: (('a, 'a) => 'a, t('a)) => 'a =
    (f, NonEmpty(x, xs)) => TailFoldable.fold_left(f, x, xs);

  let foldLeft: (('b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      TailFoldable.fold_left(f, f(init, x), xs);

  let foldRight: (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) =>
      f(x, TailFoldable.fold_right(f, init, xs));

  let flatten: t(t('a)) => t('a) =
    nonEmpty => reduceLeft(concat, nonEmpty);

  let join = flatten;

  let map: ('a => 'b, t('a)) => t('b) =
    (f, NonEmpty(x, xs)) => NonEmpty(f(x), TailApplicative.map(f, xs));

  let apply: (t('a => 'b), t('a)) => t('b) =
    (ff, fa) => map(f => map(f, fa), ff) |> flatten;

  let flatMap = (nonEmpty, f) => map(f, nonEmpty) |> flatten;

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
    let flat_map = flatMap;
  };

  module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = t('a) = {
    type nonrec t('a) = t('a);

    let fold_left = foldLeft;

    let fold_right = foldRight;

    module Fold_Map = (FoldMapMonoid: BsAbstract.Interface.MONOID) => {
      module TailFoldMap = TailFoldable.Fold_Map(FoldMapMonoid);
      let fold_map: ('a => FoldMapMonoid.t, t('a)) => FoldMapMonoid.t =
        (f, NonEmpty(x, xs)) =>
          FoldMapMonoid.append(f(x), TailFoldMap.fold_map(f, xs));
    };

    module Fold_Map_Plus = (FoldMapPlus: BsAbstract.Interface.PLUS) => {
      module TailFoldMapPlus = TailFoldable.Fold_Map_Plus(FoldMapPlus);
      let fold_map: ('a => FoldMapPlus.t('a), t('a)) => FoldMapPlus.t('a) =
        (f, NonEmpty(x, xs)) =>
          FoldMapPlus.alt(f(x), TailFoldMapPlus.fold_map(f, xs));
    };

    module Fold_Map_Any = (FoldMapAny: BsAbstract.Interface.MONOID_ANY) => {
      module SequenceFoldMapAny = TailFoldable.Fold_Map_Any(FoldMapAny);
      let fold_map: ('a => FoldMapAny.t('a), t('a)) => FoldMapAny.t('a) =
        (f, NonEmpty(x, xs)) =>
          FoldMapAny.append(f(x), SequenceFoldMapAny.fold_map(f, xs));
    };
  };
  /*
   module type TRAVERSABLE_F = (A: BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE with type t('a) = t('a) and type applicative_t('a) = A.t('a);

   module Traversable: TRAVERSABLE_F = (A: BsAbstract.Interface.APPLICATIVE) => {
     type nonrec t('a) = t('a);
     type applicative_t('a) = A.t('a);
     include (Functor: BsAbstract.Interface.FUNCTOR with type t('a) := t('a));
     include (Foldable: BsAbstract.Interface.FOLDABLE with type t('a) := t('a));

     let traverse: ('a => applicative_t('a), t('a)) => applicative_t(t('a)) = (f, NonEmpty(x, xs)) => {
       let headA: A.t('a) = f(x);
       let tailA = TailTraversable.traverse(f, xs);
     }
   }
   */
};

/* NonEmpty.List */
module List =
  NonEmptyF(Relude_List.Sequence, Relude_List.MonoidAny, Relude_List.Applicative, Relude_List.Foldable);

/* NonEmpty.Array */
module Array =
  NonEmptyF(
    Relude_Array.Sequence,
    Relude_Array.MonoidAny,
    Relude_Array.Applicative,
    Relude_Array.Foldable,
  );
