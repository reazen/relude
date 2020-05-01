open BsBastet.Interface;

/**
Creates a Zipper using the given SEQUENCE as the backing implementation.

Heavily inspired by Queensland Function Programming Lab Haskell implementation,
although without many of the advanced capabilities, like the ListZipperOp stuff.
https://github.com/qfpl/list-zipper/blob/master/src/Data/ListZipper.hs

See also this very enlightening presentation about zippers for more background:
http://data.tmorris.net/talks/zippers/0a1062fd0526d7ac1f41ade1e4db1465d311b4fd/zippers.pdf
*/
module WithSequence = (S: Relude_Interface.SEQUENCE) => {
  /**
  A Zipper type contains a sequence on the left (in reverse order, so that the
  head of the left sequence is treated as if it's the item immediately to the
  left of the focus), a focused item, and a sequence on the right.

  The left sequence is reversed so that moving the focus one item to the left is
  an O(1) operation for list-based implementations. Prepending to an array-based
  implementation might not be O(1), but list is probably the more common
  use-case for a zipper since it has ideal performance for moving the focus to
  the left or right. The array-backed implementation has the left reversed too,
  for API consistency.
  */
  type t('a) =
    | Zipper(S.t('a), 'a, S.t('a));

  /**
  Constructs a Zipper from a left sequence, a focus, and a right sequence.

  NB: the left sequence should be given in reverse - the head of the left
  sequence is treated as the item immediately to the left of the focus. The
  right sequence is in "natural" order where the head of the right sequence is
  treated as the item immediately to the right of the focus.
  */
  let make: 'a. (S.t('a), 'a, S.t('a)) => t('a) =
    (left, focus, right) => Zipper(left, focus, right);

  /**
  Constructs a Zipper from a left sequence and a focus.

  NB: the left sequence should be given in reverse - the head of the left
  sequence is treated as the item immediately to the left of the focus.
  */
  let makeWithLeft: 'a. (S.t('a), 'a) => t('a) =
    (left, focus) => Zipper(left, focus, S.emptyLazy());

  /**
  Constructs a Zipper from a focus and a right sequence.

  NB: The right sequence is in "natural" order where the head of the right
  sequence is treated as the item immediately to the right of the focus.
  */
  let makeWithRight: 'a. ('a, S.t('a)) => t('a) =
    (focus, right) => Zipper(S.emptyLazy(), focus, right);

  /**
  Makes a zipper with a focus at the start and with an array at the tail end
  */
  let makeWithRightArray: 'a. ('a, array('a)) => t('a) =
    (focus, array) => makeWithRight(focus, S.fromArray(array));

  /**
  Makes a zipper with a focus at the start and with a list at the tail end
  */
  let makeWithRightList: 'a. ('a, list('a)) => t('a) =
    (focus, list) => makeWithRight(focus, S.fromList(list));

  /**
  Constructs a zipper with empty left and right sequence, and a single focus.
  */
  let makeWithFocus: 'a. 'a => t('a) =
    focus => Zipper(S.emptyLazy(), focus, S.emptyLazy());

  /**
  Creates a Zipper from a sequence putting the focus on the first item. If
  the sequence is empty, None is returned.
  */
  let fromSequence: 'a. S.t('a) => option(t('a)) =
    sequence => {
      let head = sequence |> S.head;
      let tail = sequence |> S.tail;
      Relude_Option.map2(makeWithRight, head, tail);
    };

  /**
  Makes a zipper from a non-empty list.  Returns None if empty.
  */
  let fromArray: 'a. array('a) => option(t('a)) =
    array =>
      array
      |> Relude_Array_Base.uncons
      |> Relude_Option.map(Relude_Function.uncurry2(makeWithRightArray));

  /**
  Makes a zipper from a non-empty list.  Returns None if empty.
  */
  let fromList: 'a. list('a) => option(t('a)) =
    array =>
      array
      |> Relude_List_Base.uncons
      |> Relude_Option.map(Relude_Function.uncurry2(makeWithRightList));

  /**
  Makes a zipper from a NonEmptyArray, with the focus on the first item
  */
  let fromNonEmptyArray: 'a. Relude_NonEmpty.Array.t('a) => t('a) =
    (NonEmpty(h, tail)) => makeWithRightArray(h, tail);

  /**
  Makes a zipper from a NonEmptyArray, with the focus on the first item

  Alias for fromNonEmptyArray
  */
  let fromNea = fromNonEmptyArray;

  /**
  Makes a zipper from a NonEmptyList, with the focus on the first item
  */
  let fromNonEmptyList: 'a. Relude_NonEmpty.List.t('a) => t('a) =
    (NonEmpty(h, tail)) => makeWithRightList(h, tail);

  /**
  Makes a zipper from a NonEmptyList, with the focus on the first item

  Alias for fromNonEmptyList
  */
  let fromNel = fromNonEmptyList;

  /**
  Maps a pure function over the values of a Zipper
  */
  let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (f, Zipper(left, focus, right)) =>
      Zipper(S.Functor.map(f, left), f(focus), S.Functor.map(f, right));

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  /**
  Maps a Zipper of functions over a Zipper

  Implementation taken from: https://github.com/qfpl/list-zipper/blob/master/src/Data/ListZipper.hs#L151
  At the time of this writing, I don't personally understand why it's implemented this way.
  */
  let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
    (Zipper(l1, f1, r1), Zipper(l2, f2, r2)) =>
      Zipper(S.zipWith(a => a, l1, l2), f1(f2), S.zipWith(a => a, r1, r2));

  module Apply: APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  /**
  Lifts a pure value into a Zipper, as the focused value.

  Alias for [makeWithFocus]

  Note: QFPL implementation has [repeat a] on the left and right sides.
  At the time of this writing, I don't understand why it's implemented this way.
  I'm not sure if we can implement it this way in a strict language.
  */
  let pure: 'a. 'a => t('a) = makeWithFocus;

  module Applicative: APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  // Zipper doesn't have a Monad by itself b/c of the focus item
  // Zipper is a Comonad, but I'm not going to implement that for now.

  /**
  Folds a zipper from left-to-right using an accumulator
  */
  let foldLeft: (('b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, acc, Zipper(left, focus, right)) => {
      let flipF: ('a, 'b) => 'b = (a, b) => f(b, a);
      // Fold the left list from the right b/c it's reversed
      let accL = S.Foldable.fold_right(flipF, acc, left);
      let accF = f(accL, focus);
      S.Foldable.fold_left(f, accF, right);
    };

  /**
  Folds a zipper from right-to-left using an accumulator
  */
  let foldRight: (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, acc, Zipper(left, focus, right)) => {
      let flipF: ('b, 'a) => 'b = (b, a) => f(a, b);
      let accR = S.Foldable.fold_right(f, acc, right);
      let accF = f(focus, accR);
      // Fold the left list from the left, b/c it's reversed
      S.Foldable.fold_left(flipF, accF, left);
    };

  module Foldable: FOLDABLE with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let fold_left = foldLeft;
    let fold_right = foldRight;

    module Fold_Map = (M: MONOID) => {
      module D =
        BsBastet.Default.Fold_Map(
          M,
          {
            type nonrec t('a) = t('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Any = (M: MONOID_ANY) => {
      module D =
        BsBastet.Default.Fold_Map_Any(
          M,
          {
            type nonrec t('a) = t('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Plus = (P: PLUS) => {
      module D =
        BsBastet.Default.Fold_Map_Plus(
          P,
          {
            type nonrec t('a) = t('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
  };
  include Relude_Extensions_Foldable.FoldableExtensions(Foldable);

  module type TRAVERSABLE_F =
    (A: APPLICATIVE) =>

      TRAVERSABLE with
        type t('a) = t('a) and type applicative_t('a) = A.t('a);

  module Traversable: TRAVERSABLE_F =
    (A: APPLICATIVE) => {
      type nonrec t('a) = t('a);
      type applicative_t('a) = A.t('a);

      include (Functor: FUNCTOR with type t('a) := t('a));
      include (Foldable: FOLDABLE with type t('a) := t('a));

      let (<$>) = A.map;
      let (<*>) = A.apply;
      module STraversable = S.Traversable(A);

      let traverse: 'a 'b. ('a => A.t('b), t('a)) => A.t(t('b)) =
        (f, Zipper(left, focus, right)) => {
          make
          <$> STraversable.traverse(f, left)
          <*> f(focus)
          <*> STraversable.traverse(f, right);
        };

      let sequence: 'a. t(A.t('a)) => A.t(t('a)) =
        fa => traverse(a => a, fa);
    };

  /**
  Converts the Zipper into a Sequence
  */
  let toSequence: 'a. t('a) => S.t('a) = {
    (Zipper(left, focus, right)) =>
      S.concat(S.append(focus, S.reverse(left)), right);
  };

  /**
  Converts the Zipper into an array

  TODO: not needed with Foldable extensions
  */
  let toArray: 'a. t('a) => array('a) =
    zipper => zipper |> toSequence |> S.toArray;

  /**
  Converts the Zipper into a list

  TODO: not needed with Foldable extensions
  */
  let toList: 'a. t('a) => list('a) =
    zipper => zipper |> toSequence |> S.toList;

  /**
  Converts the Zipper into a NonEmptyArray
  */
  let toNonEmptyArray: 'a. t('a) => Relude_NonEmpty.Array.t('a) =
    (Zipper(left, focus, right)) =>
      left
      |> S.reverse
      |> S.uncons
      |> Relude_Option.foldLazy(
           () => Relude_NonEmpty.Array.make(focus, right |> S.toArray),
           ((lh, lt)) =>
             Relude_NonEmpty.Array.make(
               lh,
               S.concat(lt |> S.append(focus), right) |> S.toArray,
             ),
         );

  /**
  Converts the Zipper into a NonEmptyArray

  Alias for toNonEmptyArray
  */
  let toNea: 'a. t('a) => Relude_NonEmpty.Array.t('a) = toNonEmptyArray;

  /**
  Converts the Zipper into a NonEmptyList
  */
  let toNonEmptyList: 'a. t('a) => Relude_NonEmpty.List.t('a) =
    (Zipper(left, focus, right)) =>
      left
      |> S.reverse
      |> S.uncons
      |> Relude_Option.foldLazy(
           () => Relude_NonEmpty.List.make(focus, right |> S.toList),
           ((lh, lt)) =>
             Relude_NonEmpty.List.make(
               lh,
               S.concat(lt |> S.append(focus), right) |> S.toList,
             ),
         );

  /**
  Converts the Zipper into a NonEmptyList

  Alias for toNonEmptyList
  */
  let toNel: 'a. t('a) => Relude_NonEmpty.List.t('a) = toNonEmptyList;

  /**
  Concatenates two Zippers, keeping the focus from the left Zipper
  */
  let concatWithKeepLeftFocus = (~prefix: t('a), suffix: t('a)): t('a) => {
    let Zipper(l1, f1, r1) = prefix;
    Zipper(l1, f1, S.concat(r1, toSequence(suffix)));
  };

  /**
  Concatenates two Zippers, keeping the focus from the left Zipper

  Alias for [concatWithKeepLeftFocus]
  */
  let concat = concatWithKeepLeftFocus;

  /**
  Concatenates two Zippers, keeping the focus from the right Zipper
  */
  let concatWithKeepRightFocus = (~prefix: t('a), suffix: t('a)): t('a) => {
    let Zipper(l2, f2, r2) = suffix;
    Zipper(S.concat(l2, prefix |> toSequence |> S.reverse), f2, r2);
  };

  module Semigroup_Any: SEMIGROUP_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = (l, r) => concat(~prefix=l, r);
  };

  /**
  Reverses the Zipper
  */
  let reverse: 'a. t('a) => t('a) = (Zipper(l, f, r)) => Zipper(r, f, l);

  /**
  Zips two zippers together pair-wise using the given function
  */
  let zipWith: 'a 'b 'c. (('a, 'b) => 'c, t('a), t('b)) => t('c) =
    (f, Zipper(l1, f1, r1), Zipper(l2, f2, r2)) =>
      Zipper(S.zipWith(f, l1, l2), f(f1, f2), S.zipWith(f, r1, r2));

  /**
  Zips two zippers together in pairs.
  */
  let zip: 'a 'b. (t('a), t('b)) => t(('a, 'b)) =
    (z1, z2) => zipWith((a, b) => (a, b), z1, z2);

  /**
  Pairs each item in the zipper with it's index as if the zipper was
  converted to a list or array.
  */
  let zipWithIndex: 'a. t('a) => t(('a, int)) =
    (Zipper(left, focus, right)) => {
      let leftLength = S.length(left);
      let rightLength = S.length(right);
      let totalLength = leftLength + 1 + rightLength;
      let leftRange =
        Relude_Int.rangeAsList(0, leftLength) |> Relude_List.reverse;
      let focusIndex = leftLength;
      let rightRange = Relude_Int.rangeAsList(focusIndex + 1, totalLength);
      Zipper(
        S.zip(left, S.fromList(leftRange)),
        (focus, focusIndex),
        S.zip(right, S.fromList(rightRange)),
      );
    };

  /**
  Gets the item at the focus
  */
  let getFocus: 'a. t('a) => 'a = (Zipper(_, focus, _)) => focus;

  /**
  Modifies the focus value with the given function
  */
  let setFocusBy: 'a. ('a => 'a, t('a)) => t('a) =
    (f, Zipper(left, focus, right)) => Zipper(left, f(focus), right);

  /**
  Sets a new value at the focus
  */
  let setFocus: 'a. ('a, t('a)) => t('a) =
    (focus, Zipper(left, _, right)) => Zipper(left, focus, right);

  /**
  Gets the sequence to the left of the focus (in the unchanged reverse order in
  which its stored)
  */
  let getLeft: 'a. t('a) => S.t('a) = (Zipper(left, _, _)) => left;

  /**
  Sets the sequence to the left of the focus.  Note:  the given sequence
  should be in reverse so that the head of the sequence is the item that
  should immediately to the left of the focus.
  */
  let setLeft: 'a. (S.t('a), t('a)) => t('a) =
    (left, Zipper(_, focus, right)) => Zipper(left, focus, right);

  /**
  Gets the sequence to the left of the focus (in its sequential order, i.e.
  reversed from how its stored in the zipper)
  */
  let getLeftInOrder: 'a. t('a) => S.t('a) =
    (Zipper(left, _, _)) => left |> S.reverse;

  /**
  Sets the sequence to the left of the focus from an in-order sequence. I.e. the
  given sequence will be reversed by this function when stored in the new
  zipper.
  */
  let setLeftFromInOrder: 'a. (S.t('a), t('a)) => t('a) =
    (left, Zipper(_, focus, right)) =>
      Zipper(left |> S.reverse, focus, right);

  /**
  Gets the sequence to the right of the focus (order is in order, i.e. not
  changed)
  */
  let getRight: 'a. t('a) => S.t('a) = (Zipper(_, _, right)) => right;

  /**
  Sets the sequence to the right of the focus (order is not changed)
  */
  let setRight: 'a. (S.t('a), t('a)) => t('a) =
    (right, Zipper(left, focus, _)) => Zipper(left, focus, right);

  /**
  Gets the item to the immediate left of the focus
  */
  let peekLeft: 'a. t('a) => option('a) =
    (Zipper(left, _, _)) => left |> S.head;

  /**
  Gets the item to the immediate right of the focus
  */
  let peekRight: 'a. t('a) => option('a) =
    (Zipper(_, _, right)) => right |> S.head;

  /**
  Indicates if the focus is at the start of the zipper
  */
  let isAtStart: 'a. t('a) => bool =
    (Zipper(left, _, _)) => left |> S.isEmpty;

  /**
  Indicates if the focus is at the end of the zipper
  */
  let isAtEnd: 'a. t('a) => bool =
    (Zipper(_, _, right)) => right |> S.isEmpty;

  /**
  Indicates if the focus is at the given index of the zipper
  */
  let isAtIndex: 'a. (int, t('a)) => bool =
    (target, z) => z |> zipWithIndex |> getFocus |> snd == target;

  /**
  Indicates if the focus is at the item, based on the given equality function
  */
  let isAtItemBy: 'a. (('a, 'a) => bool, 'a, t('a)) => bool =
    (eq, target, z) => eq(z |> getFocus, target);

  /**
  Indicates if the focus is at the item, based on the given EQ module
  */
  let isAtItem =
      (type a, eq: (module EQ with type t = a), item: a, zipper: t(a)): bool => {
    module Eq = (val eq);
    isAtItemBy(Eq.eq, item, zipper);
  };

  /**
  Moves the focus one item to the left.  If there are no items to the left,
  returns None.
  */
  let moveLeft: 'a. t('a) => option(t('a)) =
    (Zipper(left, focus, right)) =>
      left
      |> S.uncons
      |> Relude_Option.map(((leftH, leftT)) =>
           Zipper(leftT, leftH, S.prepend(focus, right))
         );

  /**
  Moves the focus one item to the right.  If there are no items to the right,
  returns None.
  */
  let moveRight: 'a. t('a) => option(t('a)) =
    (Zipper(left, focus, right)) =>
      right
      |> S.uncons
      |> Relude_Option.map(((rightH, rightT)) =>
           Zipper(S.prepend(focus, left), rightH, rightT)
         );

  /**
  Moves the focus one item to the left, unless we are at the start.
  */
  let moveLeftWithClamp: 'a. t('a) => t('a) =
    z => moveLeft(z) |> Relude_Option.getOrElse(z);

  /**
  Moves the focus one item to the right, unless we are at the end.
  */
  let moveRightWithClamp: 'a. t('a) => t('a) =
    z => moveRight(z) |> Relude_Option.getOrElse(z);

  /**
  Moves the focus to the start of the zipper
  */
  let moveStart: 'a. t('a) => t('a) =
    (Zipper(left, focus, right) as z) =>
      left
      |> S.reverse
      |> S.uncons
      |> Relude_Option.foldLazy(
           () => z,
           ((leftH, leftT)) =>
             Zipper(
               S.emptyLazy(),
               leftH,
               S.concat(leftT |> S.append(focus), right),
             ),
         );

  /**
  Moves the focus to the end of the zipper
  */
  let moveEnd: 'a. t('a) => t('a) =
    (Zipper(left, focus, right) as z) =>
      right
      |> S.reverse
      |> S.uncons
      |> Relude_Option.foldLazy(
           () => z,
           ((rightH, rightT)) =>
             Zipper(
               S.concat(rightT, left |> S.prepend(focus)),
               rightH,
               S.emptyLazy(),
             ),
         );

  /**
  Moves the focus one item to the left, wrapping to the end if we are currently
  at the start.
  */
  let moveLeftWithWrap: 'a. t('a) => t('a) =
    z => moveLeft(z) |> Relude_Option.getOrElseLazy(() => moveEnd(z));

  /**
  Moves the focus one item to the right, wrapping to the start if we are
  currently at the end.
  */
  let moveRightWithWrap: 'a. t('a) => t('a) =
    z => moveRight(z) |> Relude_Option.getOrElseLazy(() => moveStart(z));

  /**
  Moves the focus a number of times to the left.  If the count is out of range,
  None is returned
  */
  let rec moveLeftTimes: 'a. (int, t('a)) => option(t('a)) =
    (times, z) =>
      if (times < 0) {
        None;
      } else if (times == 0) {
        Some(z);
      } else {
        moveLeft(z) |> Relude_Option.flatMap(moveLeftTimes(times - 1));
      };

  /**
  Moves the focus a number of times to the right.  If the count is out of range,
  None is returned
  */
  let rec moveRightTimes: 'a. (int, t('a)) => option(t('a)) =
    (times, z) =>
      if (times < 0) {
        None;
      } else if (times == 0) {
        Some(z);
      } else {
        moveRight(z) |> Relude_Option.flatMap(moveRightTimes(times - 1));
      };

  /**
  Moves the focus a number of times to the left.  If the count is out of range,
  the focus is moved to the start.
  */
  let moveLeftTimesWithClamp: 'a. (int, t('a)) => t('a) =
    (times, z) =>
      moveLeftTimes(times, z)
      |> Relude_Option.getOrElseLazy(() => moveStart(z));

  /**
  Moves the focus a number of times to the right.  If the count is out of range,
  the focus is moved to the start.
  */
  let moveRightTimesWithClamp: 'a. (int, t('a)) => t('a) =
    (times, z) =>
      moveRightTimes(times, z)
      |> Relude_Option.getOrElseLazy(() => moveEnd(z));

  /**
  Moves the focus to the given index.  If the index is out of range, None is
  returned.
  */
  let rec moveToIndex: 'a. (int, t('a)) => option(t('a)) =
    (target, z) => {
      let Zipper(_, (_, index), _) = z |> zipWithIndex;
      if (index == target) {
        Some(z);
      } else if (index < target) {
        z |> moveRight |> Relude_Option.flatMap(moveToIndex(target));
      } else {
        z |> moveLeft |> Relude_Option.flatMap(moveToIndex(target));
      };
    };

  /**
  Moves the focus to the given index modulus the length of the zipper.
  */
  let moveToIndexWithMod: 'a. (int, t('a)) => t('a) =
    (target, z) => {
      let modTarget = target mod (z |> length);
      moveToIndex(modTarget, z) |> Relude_Option.getOrThrow; // Should be safe with index mod length
    };

  /**
  Moves the focus to the given index, but no farther than the start or end if
  the index is out of range in either direction.
  */
  let moveToIndexWithClamp: 'a. (int, t('a)) => t('a) =
    (target, z) => {
      let maxIndex = (z |> length) - 1;
      let newTarget =
        if (target < 0) {
          0;
        } else if (target > maxIndex) {
          maxIndex;
        } else {
          target;
        };
      z |> moveToIndex(newTarget) |> Relude_Option.getOrThrow; // should be safewith range checks
    };

  /**
  Finds the first item that satisfies the given predicate, and returns a zipper
  focused on that item.

  Note this only searches the focus and the left side of the zipper.
  */
  let rec findLeftBy =
          (~checkFocus=true, f: 'a => bool, Zipper(_, focus, _) as z: t('a))
          : option(t('a)) =>
    if (checkFocus && f(focus)) {
      Some(z);
    } else {
      z |> moveLeft |> Relude_Option.flatMap(findLeftBy(~checkFocus=true, f));
    };

  /**
  Finds the first item that satisfies the given predicate, and returns a zipper
  focused on that item.

  Note this only searches the focus and the right side of the zipper.
  */
  let rec findRightBy =
          (~checkFocus=true, f: 'a => bool, Zipper(_, focus, _) as z: t('a))
          : option(t('a)) =>
    if (checkFocus && f(focus)) {
      Some(z);
    } else {
      z
      |> moveRight
      |> Relude_Option.flatMap(findRightBy(~checkFocus=true, f));
    };

  /**
  Finds the first item that satisfies the given predicate, and returns a zipper
  focused on that itme. The left side is searched first, then the right.
  */
  let findBy = (~checkFocus=true, f: 'a => bool, z: t('a)): option(t('a)) =>
    findLeftBy(~checkFocus, f, z)
    |> Relude_Option.orElseLazy(~fallback=() =>
         findRightBy(~checkFocus=false, f, z)
       );

  /**
  Finds the given item at the focus or to the left, using the given eq function.
  */
  let findItemLeftBy =
      (~checkFocus=true, eq: ('a, 'a) => bool, item: 'a, zipper: t('a))
      : option(t('a)) => {
    let p = eq(item);
    findLeftBy(~checkFocus, p, zipper);
  };

  /**
  Finds the given item at the focus or to the right, using the given eq
  function.
  */
  let findItemRightBy =
      (~checkFocus=true, eq: ('a, 'a) => bool, item: 'a, zipper: t('a))
      : option(t('a)) => {
    let p = eq(item);
    findRightBy(~checkFocus, p, zipper);
  };

  /**
  Finds the given item using the given eq function.  Searches the left side
  first, then the right.
  */
  let findItemBy =
      (~checkFocus=true, eq: ('a, 'a) => bool, item: 'a, zipper: t('a))
      : option(t('a)) => {
    findItemLeftBy(~checkFocus, eq, item, zipper)
    ->Relude_Option.orElseLazy(~fallback=() =>
        findItemRightBy(~checkFocus=false, eq, item, zipper)
      );
  };

  /**
  Finds the given item in the left side, using the given EQ module.
  */
  let findItemLeft =
      (
        type a,
        eq: (module EQ with type t = a),
        ~checkFocus=true,
        item: a,
        zipper: t(a),
      )
      : option(t(a)) => {
    module Eq = (val eq);
    findItemLeftBy(~checkFocus, Eq.eq, item, zipper);
  };

  /**
  Finds the given item in the right side, using the given EQ module.
  */
  let findItemRight =
      (
        type a,
        eq: (module EQ with type t = a),
        ~checkFocus=true,
        item: a,
        zipper: t(a),
      )
      : option(t(a)) => {
    module Eq = (val eq);
    findItemRightBy(~checkFocus, Eq.eq, item, zipper);
  };

  /**
  Finds the given item using the given EQ module.
  The left side is searched first, then the right.
  */
  let findItem =
      (
        type a,
        eq: (module EQ with type t = a),
        ~checkFocus=true,
        item: a,
        zipper: t(a),
      )
      : option(t(a)) => {
    findItemLeft(~checkFocus, eq, item, zipper)
    |> Relude_Option.orElseLazy(~fallback=() =>
         findItemRight(~checkFocus=false, eq, item, zipper)
       );
  };

  /**
  Inserts a new item at the focus, and pushes the previous focus to the left
  side
  */
  let insertWithPushLeft: 'a. ('a, t('a)) => t('a) =
    (item, Zipper(left, focus, right)) =>
      Zipper(S.prepend(focus, left), item, right);

  /**
  Inserts a new item at the focus, and pushes the previous focus to the right
  side
  */
  let insertWithPushRight: 'a. ('a, t('a)) => t('a) =
    (item, Zipper(left, focus, right)) =>
      Zipper(left, item, S.prepend(focus, right));

  /**
  Deletes the item at the focus, and pulls a value from the left side into
  focus.

  If there is no value on the left, None is returned.
  */
  let deleteWithPullLeft: 'a. t('a) => option(t('a)) =
    (Zipper(left, _, right)) =>
      left
      |> S.uncons
      |> Relude_Option.map(((leftH, leftT)) => Zipper(leftT, leftH, right));

  /**
  Deletes the item at the focus, and pulls a value from the right side into
  focus.

  If there is no value on the right, None is returned.
  */
  let deleteWithPullRight: 'a. t('a) => option(t('a)) =
    (Zipper(left, _, right)) =>
      right
      |> S.uncons
      |> Relude_Option.map(((rightH, rightT)) =>
           Zipper(left, rightH, rightT)
         );

  /**
  Deletes the item at the focus, and tries to pull an item from the left into
  focus. If there is no item on the left, it tries to pull an item from the
  right. If there is no item on the right, None is returned.
  */
  let deleteWithPullLeftOrRight: 'a. t('a) => option(t('a)) =
    z =>
      deleteWithPullLeft(z)
      |> Relude_Option.orElseLazy(~fallback=() => deleteWithPullRight(z));

  /**
  Converts a Zipper to a string using the given function
  */
  let showBy: 'a. ('a => string, t('a)) => string =
    (showA, Zipper(left, focus, right)) =>
      "Zipper("
      ++ S.showBy(showA, left)
      ++ ", "
      ++ showA(focus)
      ++ ", "
      ++ S.showBy(showA, right)
      ++ ")";

  /**
  Converts a Zipper to a string using the given SHOW module
  */
  let show = (type a, showA: (module SHOW with type t = a), xs) => {
    module ShowA = (val showA);
    showBy(ShowA.show, xs);
  };

  module Show = (ShowA: SHOW) => {
    type nonrec t = t(ShowA.t);
    let show = xs => showBy(ShowA.show, xs);
  };

  /**
  Compares two Zippers for length and pair-wise equality
  */
  let eqBy: 'a. (('a, 'a) => bool, t('a), t('a)) => bool =
    (eqA, Zipper(left1, focus1, right1), Zipper(left2, focus2, right2)) =>
      S.eqBy(eqA, left1, left2)
      && eqA(focus1, focus2)
      && S.eqBy(eqA, right1, right2);

  /**
  Compares two lists for length and pair-wise equality using the given EQ
  module.
  */
  let eq = (type a, eqA: (module EQ with type t = a), xs, ys) => {
    module EqA = (val eqA);
    eqBy(EqA.eq, xs, ys);
  };

  module Eq = (EqA: EQ) => {
    type nonrec t = t(EqA.t);
    let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
  };

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
  };
  // TODO: ListZipperOps?
  // https://github.com/qfpl/list-zipper/blob/master/src/Data/ListZipper.hs#L419
};
