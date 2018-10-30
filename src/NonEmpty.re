open BsAbstract.Interface;
open Relude.Interface;

module type NON_EMPTY_F =
  (
    M: MONOID_ANY,
    F: FOLDABLE with type t('a) = M.t('a),
    A: APPLICATIVE with type t('a) = M.t('a),
    S: SEQUENCE with type t('a) = M.t('a),
  ) =>
   {
    /*
     Basically the same as { head: 'a, tail: M.t('a) }, but easier to construct and pattern match.
     */
    type t('a) =
      | NonEmpty('a, M.t('a));

    /* Related modules that we will provide for free for an NonEmpty module */
    module SemigroupAny: SEMIGROUP_ANY;
    /*module Foldable: FOLDABLE;*/

    /* Utility methods for NonEmpty */
    let fromT: M.t('a) => option(t('a));
    let toT: t('a) => M.t('a);
    let pure: 'a => t('a);
    let one: 'a => t('a);
    let single: 'a => t('a);
    let make: ('a, M.t('a)) => t('a);
    let head: t('a) => 'a;
    let tail: t('a) => M.t('a);
  };

module NonEmptyF: NON_EMPTY_F =
  (
    M: MONOID_ANY,
    F: FOLDABLE with type t('a) = M.t('a),
    A: APPLICATIVE with type t('a) = M.t('a),
    S: SEQUENCE with type t('a) = M.t('a),
  ) => {
    type t('a) =
      | NonEmpty('a, M.t('a));

    module SemigroupAny: SEMIGROUP_ANY with type t('a) = t('a) = {
      type nonrec t('a) = t('a);
      let append = (a, _b) => a;
    };

    let fromT = t =>
      S.head(t)->Belt.Option.map(head => NonEmpty(head, S.tailOrEmpty(t)));

    let toT =
      fun
      | NonEmpty(head, tail) => M.append(A.pure(head), tail);

    let pure = head => NonEmpty(head, M.empty);

    let one = pure;

    let single = pure;

    let make = (head, tail) => NonEmpty(head, tail);

    let head =
      fun
      | NonEmpty(head, _) => head;

    let tail =
      fun
      | NonEmpty(_, tail) => tail;
  };

/* NonEmpty.List */
module List =
  NonEmptyF(List.MonoidAny, List.Foldable, List.Applicative, List.Sequence);

/* NonEmpty.Array */
/*
 module Array = NonEmpty(Array.MonoidAny);
 */
