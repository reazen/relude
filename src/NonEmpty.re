open BsAbstract.Interface;
open Relude.Interface;

module type NON_EMPTY_F =
  (
    S: SEQUENCE,
    M: MONOID_ANY with type t('a) = S.t('a),
    F: FOLDABLE with type t('a) = S.t('a),
    A: APPLICATIVE with type t('a) = S.t('a),
  ) =>
   {
    /*
     Basically the same as { head: 'a, tail: M.t('a) }, but easier to construct and pattern match.
     */
    type t('a) =
      | NonEmpty('a, M.t('a));

    /* Related modules that we will provide for free for an NonEmpty module */
    module SemigroupAny: SEMIGROUP_ANY;
    /* No Monoid for this type, as there is no empty */
    /* TODO:
       module Foldable: FOLDABLE;
       ...
       */

    /* Utility methods for NonEmpty */
    let fromSequence: S.t('a) => option(t('a));
    let toSequence: t('a) => S.t('a);
    let pure: 'a => t('a);
    let one: 'a => t('a);
    let single: 'a => t('a);
    let make: ('a, S.t('a)) => t('a);
    let head: t('a) => 'a;
    let tail: t('a) => S.t('a);
    let cons: ('a, t('a)) => t('a);
  };

module NonEmptyF: NON_EMPTY_F =
  (
    S: SEQUENCE,
    M: MONOID_ANY with type t('a) = S.t('a),
    F: FOLDABLE with type t('a) = S.t('a),
    A: APPLICATIVE with type t('a) = S.t('a),
  ) => {
    type t('a) =
      | NonEmpty('a, M.t('a));

    module SemigroupAny: SEMIGROUP_ANY with type t('a) = t('a) = {
      type nonrec t('a) = t('a);
      let append = (a, _b) => a;
    };

    let fromSequence = t =>
      S.head(t)->Belt.Option.map(head => NonEmpty(head, S.tailOrEmpty(t)));

    let toSequence =
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

    let cons = (h, nel) => NonEmpty(h, toSequence(nel));
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
