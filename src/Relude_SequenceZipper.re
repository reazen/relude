let id = Relude_Function.id;

/**
 * Creates a Zipper using the given SEQUENCE as the backing implementation
 *
 * Heavily inspired by Queensland Function Programming Lab Haskell implementation:
 * https://github.com/qfpl/list-zipper/blob/master/src/Data/ListZipper.hs
 */
module WithSequence = (S: Relude_Interface.SEQUENCE) => {
  module SFoldableExtensions =
    Relude_Extensions_Foldable.FoldableExtensions(S.Foldable);

  /**
   * A Zipper type contains a sequence on the left (in reverse order, so that the head
   * of the left sequence is treated as if it's the item immediatley to the left of the focus),
   * a focused item, and a sequence on the right.
   *
   * The left sequence is reversed so that moving the focus one item to the left is an O(1) operation
   * for list-based implementations.  Pre-pending to an array-based implementation might not be O(1),
   * but list is probably the more common use-case for a zipper since it has ideal performance for moving
   * the focus to the left or right.
   */
  type t('a) =
    | Zipper(S.t('a), 'a, S.t('a));

  /**
   * Constructs a Zipper from a left sequence, a focus, and a right sequence.
   *
   * NB: the left sequence should be given in reverse - the head of the left sequence
   * is treated as the item immediately to the left of the focus.  The right sequence
   * is in "natural" order where the head of the right sequence is treated as the item
   * immediately to the right of the focus.
   */
  let make: 'a. (S.t('a), 'a, S.t('a)) => t('a) =
    (left, focus, right) => Zipper(left, focus, right);

  /**
 * Constructs a Zipper from a left sequence and a focus.
 *
 * NB: the left sequence should be given in reverse - the head of the left sequence
 * is treated as the item immediately to the left of the focus.
 */
  let makeWithLeft: 'a. (S.t('a), 'a) => t('a) =
    (left, focus) => Zipper(left, focus, S.emptyLazy());

  /**
   * Constructs a Zipper from a focus and a right sequence.
   *
   * NB: * The right sequence is in "natural" order where the head of the right sequence is treated as the item
   * immediately to the right of the focus.
   */
  let makeWithRight: 'a. ('a, S.t('a)) => t('a) =
    (focus, right) => Zipper(S.emptyLazy(), focus, right);

  /**
   * Makes a zipper with a focus at the start and with an array at the tail end
   */
  let makeWithRightArray: 'a. ('a, array('a)) => t('a) =
    (focus, array) => makeWithRight(focus, S.fromArray(array));

  /**
   * Makes a zipper with a focus at the start and with a list at the tail end
   */
  let makeWithRightList: 'a. ('a, list('a)) => t('a) =
    (focus, list) => makeWithRight(focus, S.fromList(list));

  /**
   * Constructs a zipper with empty left and right sequence, and a single focus.
   */
  let makeWithFocus: 'a. 'a => t('a) =
    focus => Zipper(S.emptyLazy(), focus, S.emptyLazy());

  /**
   * Creates a Zipper from a sequence putting the focus on the first item.  If the sequence is empty,
   * None is returned.
   */
  let fromSequence: 'a. S.t('a) => option(t('a)) =
    sequence => {
      let head = sequence |> S.head;
      let tail = sequence |> S.tail;
      Relude_Option.map2(makeWithRight, head, tail);
    };

  /**
   * Makes a zipper from a non-empty list.  Returns None if empty.
   */
  let fromArray: 'a. array('a) => option(t('a)) =
    array =>
      array
      |> Relude_Array_Base.uncons
      |> Relude_Option.map(Relude_Function.uncurry2(makeWithRightArray));

  /**
   * Makes a zipper from a non-empty list.  Returns None if empty.
   */
  let fromList: 'a. list('a) => option(t('a)) =
    array =>
      array
      |> Relude_List_Base.uncons
      |> Relude_Option.map(Relude_Function.uncurry2(makeWithRightList));

  /**
   * Makes a zipper from a NonEmptyArray, with the focus on the first item
   */
  let fromNonEmptyArray: 'a. Relude_NonEmpty.Array.t('a) => t('a) =
    (NonEmpty(h, tail)) => makeWithRightArray(h, tail);

  /**
   * Makes a zipper from a NonEmptyArray, with the focus on the first item
   *
   * Alias for fromNonEmptyArray
   */
  let fromNea = fromNonEmptyArray;

  /**
   * Makes a zipper from a NonEmptyList, with the focus on the first item
   */
  let fromNonEmptyList: 'a. Relude_NonEmpty.List.t('a) => t('a) =
    (NonEmpty(h, tail)) => makeWithRightList(h, tail);

  /**
   * Makes a zipper from a NonEmptyList, with the focus on the first item
   *
   * Alias for fromNonEmptyList
   */
  let fromNel = fromNonEmptyList;

  /**
   * Maps a pure function over the values of a Zipper
   */
  let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (f, Zipper(left, focus, right)) =>
      Zipper(S.Functor.map(f, left), f(focus), S.Functor.map(f, right));

  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  /**
   * Maps a Zipper of functions over a Zipper
   *
   * Implementation taken from: https://github.com/qfpl/list-zipper/blob/master/src/Data/ListZipper.hs#L151
   * At the time of this writing, I don't personally understand why it's implemented this way.
   */
  let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
    (Zipper(l1, f1, r1), Zipper(l2, f2, r2)) =>
      Zipper(S.zipWith(id, l1, l2), f1(f2), S.zipWith(id, r1, r2));

  module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
    include Functor;
    let apply = apply;
  };
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  /**
   * Lifts a pure value into a Zipper, as the focused value.
   *
   * Alias for `makeWithFocus`
   *
   * Note: QFPL implementation has `repeat a` on the left and right sides.
   * At the time of this writing, I don't understand why it's implemented this way.
   * I'm not sure if we can implement it this way in a strict language.
   */
  let pure: 'a. 'a => t('a) = makeWithFocus;

  module Applicative:
    BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
    include Apply;
    let pure = pure;
  };
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  // Zipper doesn't have a Monad by itself b/c of the focus item
  // Zipper is a Comonad, but I'm not going to implement that for now.

  /**
   * Folds a zipper from left-to-right using an accumulator
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
   * Folds a zipper from right-to-left using an accumulator
   */
  let foldRight: (('a, 'b) => 'b, 'b, t('a)) => 'b =
    (f, acc, Zipper(left, focus, right)) => {
      let flipF: ('b, 'a) => 'b = (b, a) => f(a, b);
      // Fold the left list from the right b/c it's reversed
      let accR = S.Foldable.fold_right(f, acc, right);
      let accF = f(focus, accR);
      S.Foldable.fold_left(flipF, accF, left);
    };

  module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let fold_left = foldLeft;
    let fold_right = foldRight;

    module Fold_Map = (M: BsAbstract.Interface.MONOID) => {
      module D =
        BsAbstract.Default.Fold_Map(
          M,
          {
            type nonrec t('a) = t('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Any = (M: BsAbstract.Interface.MONOID_ANY) => {
      module D =
        BsAbstract.Default.Fold_Map_Any(
          M,
          {
            type nonrec t('a) = t('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Plus = (P: BsAbstract.Interface.PLUS) => {
      module D =
        BsAbstract.Default.Fold_Map_Plus(
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
   * Converts the Zipper into a Sequence
   */
  let toSequence: 'a. t('a) => S.t('a) = {
    (Zipper(left, focus, right)) =>
      S.concat(S.append(focus, S.reverse(left)), right);
  };

  /**
   * Converts the Zipper into an array
   *
   * TODO: not needed with Foldable extensions
   */
  let toArray: 'a. t('a) => array('a) =
    zipper => zipper |> toSequence |> SFoldableExtensions.toArray;

  /**
   * Converts the Zipper into a list
   *
   * TODO: not needed with Foldable extensions
   */
  let toList: 'a. t('a) => list('a) =
    zipper => zipper |> toSequence |> SFoldableExtensions.toList;

  /**
   * Converts the Zipper into a NonEmptyArray
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
   * Converts the Zipper into a NonEmptyArray
   *
   * Alias for toNonEmptyArray
   */
  let toNea: 'a. t('a) => Relude_NonEmpty.Array.t('a) = toNonEmptyArray;

  /**
   * Converts the Zipper into a NonEmptyList
   *
   * TODO: better implementation
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
   * Converts the Zipper into a NonEmptyList
   *
   * Alias for toNonEmptyList
   */
  let toNel: 'a. t('a) => Relude_NonEmpty.List.t('a) = toNonEmptyList;

  /**
   * Concatenates two Zippers, keeping the focus from the left Zipper
   */
  let concatWithKeepLeftFocus = (~prefix: t('a), suffix: t('a)): t('a) => {
    let Zipper(l1, f1, r1) = prefix;
    Zipper(l1, f1, S.concat(r1, toSequence(suffix)));
  };

  /**
   * Concatenates two Zippers, keeping the focus from the left Zipper
   *
   * Alias for `concatWithKeepLeftFocus`
   */
  let concat = concatWithKeepLeftFocus;

  /**
   * Concatenates two Zippers, keeping the focus from the right Zipper
   */
  let concatWithKeepRightFocus = (~prefix: t('a), suffix: t('a)): t('a) => {
    let Zipper(l2, f2, r2) = suffix;
    Zipper(S.concat(l2, prefix |> toSequence |> S.reverse), f2, r2);
  };

  module Semigroup_Any:
    BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let append = (l, r) => concat(~prefix=l, r);
  };

  /**
   * Reverses the Zipper
   */
  let reverse: 'a. t('a) => t('a) = (Zipper(l, f, r)) => Zipper(r, f, l);

  /**
   * Zips two zippers together pair-wise using the given function
   */
  let zipWith: 'a 'b 'c. (('a, 'b) => 'c, t('a), t('b)) => t('c) =
    (f, Zipper(l1, f1, r1), Zipper(l2, f2, r2)) =>
      Zipper(S.zipWith(f, l1, l2), f(f1, f2), S.zipWith(f, r1, r2));

  /**
   * Zips two zippers together in pairs.
   */
  let zip: 'a 'b. (t('a), t('b)) => t(('a, 'b)) =
    (z1, z2) => zipWith((a, b) => (a, b), z1, z2);

  /**
   * Pairs each item in the zipper with it's index as if the zipper was converted to a list or array
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
   * Gets the item at the focus
   */
  let getFocus: 'a. t('a) => 'a = (Zipper(_, focus, _)) => focus;

  /**
   * Modifies the focus value with the given function
   */
  let setFocusBy: 'a. ('a => 'a, t('a)) => t('a) =
    (f, Zipper(left, focus, right)) => Zipper(left, f(focus), right);

  /**
   * Sets a new value at the focus
   */
  let setFocus: 'a. ('a, t('a)) => t('a) =
    (focus, Zipper(left, _, right)) => Zipper(left, focus, right);

  /**
   * Gets the sequence to the left of the focus (in its sequential order, i.e. reversed from how its stored in the zipper)
   */
  let getLeftInOrder: 'a. t('a) => S.t('a) =
    (Zipper(left, _, _)) => left |> S.reverse;

  /**
   * Sets the sequence to the left of the focus from an in-order sequence.  I.e. the given sequence will be reversed by this function when stored in the new zipper.
   */
  let setLeftFromInOrder: 'a. (S.t('a), t('a)) => t('a) =
    (left, Zipper(_, focus, right)) =>
      Zipper(left |> S.reverse, focus, right);

  /**
   * Gets the sequence to the right of the focus (order is in order, i.e. not changed)
   */
  let getRight: 'a. t('a) => S.t('a) = (Zipper(_, _, right)) => right;

  /**
   * Sets the sequence to the right of the focus (order is not changed)
   */
  let setRight: 'a. (S.t('a), t('a)) => t('a) =
    (right, Zipper(left, focus, _)) => Zipper(left, focus, right);

  /**
   * Gets the item to the immediate left of the focus
   */
  let peekLeft: 'a. t('a) => option('a) =
    (Zipper(left, _, _)) => left |> S.head;

  /**
   * Gets the item to the immediate right of the focus
   */
  let peekRight: 'a. t('a) => option('a) =
    (Zipper(_, _, right)) => right |> S.head;

  /**
   * Moves the focus to the start of the zipper
   */
  let moveStart = "TODO";

  /**
   * Moves the focus to the end of the zipper
   */
  let moveEnd = "TODO";

  /**
   * Moves the focus to the given index.  If the index is out of range, None is returned.
   */
  let moveToIndex = "TODO";

  /**
   * Moves the focus to the given index modulus the length of the zipper.
   */
  let moveToIndexWithMod = "TODO";

  /**
   * Moves the focus to the given index, but no farther than the start or end if the index is out of range in either direction.
   */
  let moveToIndexWithClamp = "TODO";

  /**
   * Moves the focus to the given item, based on the given equality function
   */
  let moveToItemBy = "TODO"; // eq

  /**
   * Moves the focus to the given item, based on the given EQ module
   */
  let moveToItem = "TODO"; // eq

  /**
   * Indicates if the focus is at the start of the zipper
   */
  let isAtStart = "TODO";

  /**
   * Indicates if the focus is at the end of the zipper
   */
  let isAtEnd = "TODO";

  /**
   * Indicates if the focus is at the given index of the zipper
   */
  let isAtIndex = "TODO";

  /**
   * Indicates if the focus is at the item, based on the given equality function
   */
  let isAtItemBy = "TODO"; // eq

  /**
   * Indicates if the focus is at the item, based on the given EQ module
   */
  let isAtItem = "TODO"; // eq

  /**
   * Moves the focus one item to the left.  If there are no items to the left, returns None.
   */
  let moveLeft = "TODO"; // option

  /**
   * Moves the focus one item to the right.  If there are no items to the right, returns None.
   */
  let moveRight = "TODO"; // option

  /**
   * Moves the focus one item to the left, unless we are at the start.
   */
  let moveLeftWithClamp = "TODO";

  /**
   * Moves the focus one item to the right, unless we are at the end.
   */
  let moveRightWithClamp = "TODO";

  /**
   * Moves the focus one item to the left, wrapping to the end if we are currently at the start.
   */
  let moveLeftWithWrap = "TODO";

  /**
   * Moves the focus one item to the right, wrapping to the start if we are currently at the end.
   */
  let moveRightWithWrap = "TODO";

  /**
   * Inserts a new item at the focus, and pushes the previous focus to the left side
   */
  let insertWithPushLeft = "TODO";

  /**
   * Inserts a new item at the focus, and pushes the previous focus to the right side
   */
  let insertWithPushRight = "TODO";

  /**
   * Deletes the item at the focus, and pulls a value from the left side into focus
   */
  let deleteWithPullLeft = "TODO";

  /**
   * Deletes the item at the focus, and pulls a value from the right side into focus
   */
  let deleteWithPullRight = "TODO";

  /**
   * Deletes the item at the focus, and moves an item from the left or right to be the new focus (whichever has a value, if either)
   */
  let deleteWithPromoteLeftOrRight = "TODO";

  /*
   let findLeft: 'a. ('a => bool, t('a)) => option(t('a)) = (f, Zipper(left, focus, _) as z) => {
     if (f(focus)) Some(z)
     else {
       S.find(
     }

   };

   let findRight = "TODO";

   let find = "TODO:";
   */

  /**
   * Converts a Zipper to a string using the given function
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
 * Converts a Zipper to a string using the given SHOW module
 */
  let show =
      (type a, showA: (module BsAbstract.Interface.SHOW with type t = a), xs) => {
    module ShowA = (val showA);
    showBy(ShowA.show, xs);
  };

  module Show = (ShowA: BsAbstract.Interface.SHOW) => {
    type nonrec t = t(ShowA.t);
    let show = xs => showBy(ShowA.show, xs);
  };

  /**
   * Compares two Zippers for length and pair-wise equality
   */
  let eqBy: 'a. (('a, 'a) => bool, t('a), t('a)) => bool =
    (eqA, Zipper(left1, focus1, right1), Zipper(left2, focus2, right2)) =>
      S.eqBy(eqA, left1, left2)
      && eqA(focus1, focus2)
      && S.eqBy(eqA, right1, right2);

  /**
   * Compares two lists for length and pair-wise equality using the given EQ module
   */
  let eq =
      (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs, ys) => {
    module EqA = (val eqA);
    eqBy(EqA.eq, xs, ys);
  };

  module Eq = (EqA: BsAbstract.Interface.EQ) => {
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