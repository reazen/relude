module NonEmptyF =
       (
         S: Interface.SEQUENCE,
         M: BsAbstract.Interface.MONOID_ANY with type t('a) = S.t('a),
         F: BsAbstract.Interface.FOLDABLE with type t('a) = S.t('a),
         A: BsAbstract.Interface.APPLICATIVE with type t('a) = S.t('a),
       ) => {
  type t('a) =
    | NonEmpty('a, S.t('a));

  let fromSequence: S.t('a) => option(t('a)) =
    sequence =>
      S.head(sequence)
      ->Belt.Option.map(head => NonEmpty(head, S.tailOrEmpty(sequence)));

  let toSequence: t('a) => S.t('a) =
    fun
    | NonEmpty(head, tail) => M.append(A.pure(head), tail);

  let pure: 'a => t('a) = head => NonEmpty(head, M.empty);

  let one: 'a => t('a) = pure;

  let single: 'a => t('a) = pure;

  let make: ('a, S.t('a)) => t('a) =
    (head, tailSequence) => NonEmpty(head, tailSequence);

  let cons: ('a, t('a)) => t('a) =
    (head, tailNonEmpty) => NonEmpty(head, toSequence(tailNonEmpty));

  let length: t('a) => int =
    fun
    | NonEmpty(_, t) => 1 + S.length(t);

  let head: t('a) => 'a =
    fun
    | NonEmpty(head, _) => head;

  let tail: t('a) => S.t('a) =
    fun
    | NonEmpty(_, tail) => tail;

  let concat: (t('a), t('a)) => t('a) =
    (nonEmpty1, nonEmpty2) =>
      NonEmpty(
        head(nonEmpty1),
        M.append(tail(nonEmpty1), toSequence(nonEmpty2)),
      );

  let reduceLeft: (('a, 'a) => 'a, t('a)) => 'a =
    (f, NonEmpty(x, xs)) => F.fold_left(f, x, xs);

  let foldLeft: (('b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) => F.fold_left(f, f(init, x), xs);

  let foldRight: (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, init, NonEmpty(x, xs)) => f(x, F.fold_right(f, init, xs));

  let flatten: t(t('a)) => t('a) =
    nonEmpty => reduceLeft(concat, nonEmpty);

  let join = flatten;

  let map: ('a => 'b, t('a)) => t('b) =
    (f, NonEmpty(x, xs)) => NonEmpty(f(x), A.map(f, xs));

  let apply: (t('a => 'b), t('a)) => t('b) =
    (ff, fa) => map(f => map(f, fa), ff) |> flatten;

  let flatMap = (nonEmpty, f) => map(f, nonEmpty) |> flatten;

  module SemigroupAny: BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = t('a) = {
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

  module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };

  module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
    include Applicative;
    let flat_map = flatMap;
  };
  /*
   module Foldable: FOLDABLE with type t('a) = t('a) = {
     type nonrec t('a) = t('a);
     let fold_left = foldLeft;
     let fold_right = foldRight;

     module Fold_Map_Plus {
     }

     module Fold_Map_Any {
     }

     module Fold_Map {
       let fold_map = foldMap;
     }
   };
   */
  /*
   module Traversable
   */
};

/* NonEmpty.List */
module List =
  NonEmptyF(List.Sequence, List.MonoidAny, List.Foldable, List.Applicative);

/* NonEmpty.Array */
module Array =
  NonEmptyF(
    Array.Sequence,
    Array.MonoidAny,
    Array.Foldable,
    Array.Applicative,
  );
