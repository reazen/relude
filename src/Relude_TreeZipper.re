/**
 * A zipper for a non-empty multi-way/rose tree
 *
 * The leftSiblings are stored in reverse order for O(1) sideways movements.
 *
 * Based on the ideas/implementation from Tony Morris' talk here:
 * http://data.tmorris.net/talks/zippers/bd054c210649101b84662c614fc45af3c27a5eef/zippers.pdf
 */
type t('a) = {
  ancestors:
    list((list(Relude_Tree.t('a)), 'a, list(Relude_Tree.t('a)))),
  leftSiblings: list(Relude_Tree.t('a)),
  focus: 'a,
  rightSiblings: list(Relude_Tree.t('a)),
  children: list(Relude_Tree.t('a)),
};

/**
 * Creates a tree zipper containing a single item
 */
let pure: 'a => t('a) =
  a => {
    ancestors: [],
    leftSiblings: [],
    focus: a,
    rightSiblings: [],
    children: [],
  };

/**
 * Alias for pure
 */
let singleton = pure;

/**
 * Constructs a tree zipper from parts
 */
let make:
  'a.
  (
    list((list(Relude_Tree.t('a)), 'a, list(Relude_Tree.t('a)))),
    list(Relude_Tree.t('a)),
    'a,
    list(Relude_Tree.t('a)),
    list(Relude_Tree.t('a))
  ) =>
  t('a)
 =
  (ancestors, leftSiblings, focus, rightSiblings, children) => {
    ancestors,
    leftSiblings,
    focus,
    rightSiblings,
    children,
  };

/**
 * Constructs a tree zipper from (labelled) parts
 */
let makeWithLabels:
  'a.
  (
    ~ancestors: list(
                  (list(Relude_Tree.t('a)), 'a, list(Relude_Tree.t('a))),
                ),
    ~leftSiblings: list(Relude_Tree.t('a)),
    ~focus: 'a,
    ~rightSiblings: list(Relude_Tree.t('a)),
    ~children: list(Relude_Tree.t('a))
  ) =>
  t('a)
 =
  (~ancestors, ~leftSiblings, ~focus, ~rightSiblings, ~children) => {
    ancestors,
    leftSiblings,
    focus,
    rightSiblings,
    children,
  };

/**
 * Converts a Tree into a Tree Zipper
 */
let fromTree: 'a. Relude_Tree.t('a) => t('a) =
  ({value, children}) =>
    makeWithLabels(
      ~ancestors=[],
      ~leftSiblings=[],
      ~focus=value,
      ~rightSiblings=[],
      ~children,
    );

/**
 * Gets the list of ancestor levels for the given TreeZipper
 */
let getAncestors:
  'a.
  t('a) => list((list(Relude_Tree.t('a)), 'a, list(Relude_Tree.t('a))))
 =
  ({ancestors, leftSiblings: _, focus: _, rightSiblings: _, children: _}) => ancestors;

/**
 * Gets the value at the focus of the TreeZipper
 */
let getFocusValue: 'a. t('a) => 'a =
  ({ancestors: _, leftSiblings: _, focus, rightSiblings: _, children: _}) => focus;

/**
 * Applies a side-effect function with the current focus value, and returns the zipper unchanged
 */
let tapFocusValue: 'a. ('a => unit, t('a)) => t('a) =
  (f, zipper) => {
    f(zipper |> getFocusValue);
    zipper;
  };

/**
 * Overwrites the focus with the given value
 */
let setFocusValue: 'a. ('a, t('a)) => t('a) =
  (newFocus, {ancestors, leftSiblings, focus: _, rightSiblings, children}) => {
    ancestors,
    leftSiblings,
    focus: newFocus,
    rightSiblings,
    children,
  };

/**
 * Modifies the focus with the given function
 */
let modifyFocusValue: 'a. ('a => 'a, t('a)) => t('a) =
  (f, {ancestors, leftSiblings, focus, rightSiblings, children}) => {
    ancestors,
    leftSiblings,
    focus: f(focus),
    rightSiblings,
    children,
  };

/**
 * Gets the value and children at the focus of the TreeZipper as a Tree
 */
let getFocusTree: 'a. t('a) => Relude_Tree.t('a) =
  ({ancestors: _, leftSiblings: _, focus, rightSiblings: _, children}) => {
    value: focus,
    children,
  };

/**
 * Gets the siblings to the left of the focus value, in reverse order.
 * (the first item of the resulting list is the item that is immediately to the left of the focus).
 */
let getLeftSiblings: 'a. t('a) => list(Relude_Tree.t('a)) =
  ({ancestors: _, leftSiblings, focus: _, rightSiblings: _, children: _}) => leftSiblings;

/**
 * Gets the siblings to the left of the focus value, in order.
 * (the first item of the resulting list is the item that is the leftmost sibling (furthest from focus))
 */
let getLeftSiblingsInOrder: 'a. t('a) => list(Relude_Tree.t('a)) =
  tree => tree |> getLeftSiblings |> Relude_List.reverse;

/**
 * Sets the left siblings from a reversed list (where the first item of the list should be closest to the focus)
 */
let setLeftSiblings: 'a. (list(Relude_Tree.t('a)), t('a)) => option(t('a)) =
  (
    newLeftSiblings,
    {ancestors, leftSiblings: _, focus, rightSiblings, children},
  ) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      Some({
        ancestors,
        leftSiblings: newLeftSiblings,
        focus,
        rightSiblings,
        children,
      });
    } else {
      None;
    };

/**
 * Sets the left siblings from an in-order list (where the first item of the list should be farthest from the focus)
 */
let setLeftSiblingsFromInOrder:
  'a.
  (list(Relude_Tree.t('a)), t('a)) => option(t('a))
 =
  (
    newLeftSiblingsInOrder,
    {ancestors, leftSiblings: _, focus, rightSiblings, children},
  ) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      Some({
        ancestors,
        leftSiblings: newLeftSiblingsInOrder |> Relude_List.reverse,
        focus,
        rightSiblings,
        children,
      });
    } else {
      None;
    };

/**
 * Gets the siblings to the right of the current focus
 */
let getRightSiblings: 'a. t('a) => list(Relude_Tree.t('a)) =
  ({ancestors: _, leftSiblings: _, focus: _, rightSiblings, children: _}) => rightSiblings;

/**
 * Sets the right siblings from a reversed list (where the first item of the list should be closest to the focus)
 */
let setRightSiblings:
  'a.
  (list(Relude_Tree.t('a)), t('a)) => option(t('a))
 =
  (
    newRightSiblings,
    {ancestors, leftSiblings, focus, rightSiblings: _, children},
  ) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      Some({
        ancestors,
        leftSiblings,
        focus,
        rightSiblings: newRightSiblings,
        children,
      });
    } else {
      None;
    };

/**
 * Gets the children sub-trees of the current focus
 */
let getChildren: 'a. t('a) => list(Relude_Tree.t('a)) =
  ({ancestors: _, leftSiblings: _, focus: _, rightSiblings: _, children}) => children;

/**
 * Sets the children
 */
let setChildren: 'a. (list(Relude_Tree.t('a)), t('a)) => t('a) =
  (newChildren, {ancestors, leftSiblings, focus, rightSiblings, children: _}) => {
    {ancestors, leftSiblings, focus, rightSiblings, children: newChildren};
  };

/**
 * Moves the focus one sibling to the left (if possible)
 */
let moveLeft: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus, rightSiblings, children}) => {
    leftSiblings
    |> Relude_List.uncons
    |> Relude_Option.map(((leftHead, leftTail)) => {
         {
           ancestors,
           leftSiblings: leftTail,
           focus: leftHead |> Relude_Tree.getValue,
           rightSiblings: [
             Relude_Tree.make(focus, children),
             ...rightSiblings,
           ],
           children: leftHead |> Relude_Tree.getChildren,
         }
       });
  };

/**
 * Moves to the left, unless we are already at the leftmost item
 */
let moveLeftWithClamp: 'a. t('a) => t('a) =
  zipper => zipper |> moveLeft |> Relude_Option.getOrElse(zipper);

/**
 * Moves the focus as far as possible to the left
 */
let rec moveLeftToStart: 'a. t('a) => t('a) =
  zipper =>
    zipper
    |> moveLeft
    |> Relude_Option.foldLazy(() => zipper, moveLeftToStart);

/**
 * Moves left a number of times
 */
let rec moveLeftTimes: 'a. (int, t('a)) => option(t('a)) =
  (times, zipper) =>
    if (times < 0) {
      None;
    } else if (times == 0) {
      Some(zipper);
    } else {
      zipper |> moveLeft |> Relude_Option.flatMap(moveLeftTimes(times - 1));
    };

/**
 * Move the focus to the left a number of times, stopping if the leftmost sibling is reached
 */
let moveLeftTimesWithClamp: 'a. (int, t('a)) => t('a) =
  (times, zipper) => {
    zipper
    |> moveLeftTimes(times)
    |> Relude_Option.getOrElseLazy(() => zipper |> moveLeftToStart);
  };

/**
 * Moves the focus one sibling to the right (if possible)
 */
let moveRight: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus, rightSiblings, children}) => {
    rightSiblings
    |> Relude_List.uncons
    |> Relude_Option.map(((rightHead, rightTail)) => {
         {
           ancestors,
           leftSiblings: [
             Relude_Tree.make(focus, children),
             ...leftSiblings,
           ],
           focus: rightHead |> Relude_Tree.getValue,
           rightSiblings: rightTail,
           children: rightHead |> Relude_Tree.getChildren,
         }
       });
  };

/**
 * Moves the zipper to the right one time, unless we are already at the rightmost item
 */
let moveRightWithClamp: 'a. t('a) => t('a) =
  zipper => zipper |> moveRight |> Relude_Option.getOrElse(zipper);

/**
 * Moves the focus as far as possible to the right
 */
let rec moveRightToEnd: 'a. t('a) => t('a) =
  zipper =>
    zipper
    |> moveRight
    |> Relude_Option.foldLazy(() => zipper, moveRightToEnd);

/**
 * Moves right a number of times
 */
let rec moveRightTimes: 'a. (int, t('a)) => option(t('a)) =
  (times, zipper) =>
    if (times < 0) {
      None;
    } else if (times == 0) {
      Some(zipper);
    } else {
      zipper |> moveRight |> Relude_Option.flatMap(moveRightTimes(times - 1));
    };

/**
 * Move the focus to the right a number of times, stopping if the rightmost sibling is reached
 */
let moveRightTimesWithClamp: 'a. (int, t('a)) => t('a) =
  (times, zipper) => {
    zipper
    |> moveRightTimes(times)
    |> Relude_Option.getOrElseLazy(() => zipper |> moveRightToEnd);
  };

/**
 * Moves the focus up one level to the parent (if possible)
 */
let moveUp: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus, rightSiblings, children}) => {
    ancestors
    |> Relude_List.uncons
    |> Relude_Option.map(
         (
           (
             (ancestorsHeadLeft, ancestorsHeadFocus, ancestorsHeadRight),
             ancestorsTail,
           ),
         ) => {
         {
           ancestors: ancestorsTail,
           leftSiblings: ancestorsHeadLeft,
           focus: ancestorsHeadFocus,
           rightSiblings: ancestorsHeadRight,
           children:
             Relude_List.flatten([
               leftSiblings |> Relude_List.reverse,
               [Relude_Tree.make(focus, children)],
               rightSiblings,
             ]),
         }
       });
  };

/**
 * Moves the zipper up a level, unless it's already at the top
 */
let moveUpWithClamp: 'a. t('a) => t('a) =
  zipper => zipper |> moveUp |> Relude_Option.getOrElse(zipper);

/**
 * Moves the zipper to focus the top of the tree
 */
let rec moveUpToTop: 'a. t('a) => t('a) =
  zipper =>
    zipper |> moveUp |> Relude_Option.foldLazy(() => zipper, moveUpToTop);

/**
 * Moves the zipper up a number of times (if possible)
 */
let rec moveUpTimes: 'a. (int, t('a)) => option(t('a)) =
  (times, zipper) =>
    if (times < 0) {
      None;
    } else if (times == 0) {
      Some(zipper);
    } else {
      zipper |> moveUp |> Relude_Option.flatMap(moveUpTimes(times - 1));
    };

/**
 * Moves the zipper up a number of times, stopping if the top is reached
 */
let moveUpTimesWithClamp: 'a. (int, t('a)) => t('a) =
  (times, zipper) => {
    zipper
    |> moveUpTimes(times)
    |> Relude_Option.getOrElseLazy(() => zipper |> moveUpToTop);
  };

/**
 * Moves the focus down to the first child (if possible)
 */
let moveDown: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus, rightSiblings, children}) => {
    children
    |> Relude_List.uncons
    |> Relude_Option.map(((childrenHead, childrenTail)) => {
         {
           ancestors: [(leftSiblings, focus, rightSiblings), ...ancestors],
           leftSiblings: [],
           focus: childrenHead |> Relude_Tree.getValue,
           rightSiblings: childrenTail,
           children: childrenHead |> Relude_Tree.getChildren,
         }
       });
  };

/**
 * Moves the zipper down one time, unless there are no children
 */
let moveDownWithClamp: 'a. t('a) => t('a) =
  zipper => zipper |> moveDown |> Relude_Option.getOrElse(zipper);

/**
 * Moves the zipper to focus the bottom of the tree (on the left-most child branches)
 */
let rec moveDownToBottom: 'a. t('a) => t('a) =
  zipper =>
    zipper
    |> moveDown
    |> Relude_Option.foldLazy(() => zipper, moveDownToBottom);

/**
 * Moves the focus down a number of times (if possible)
 */
let rec moveDownTimes: 'a. (int, t('a)) => option(t('a)) =
  (times, zipper) =>
    if (times < 0) {
      None;
    } else if (times == 0) {
      Some(zipper);
    } else {
      zipper |> moveDown |> Relude_Option.flatMap(moveDownTimes(times - 1));
    };

/**
 * Moves the zipper down a number of times, stopping when we get as low as we can,
 * staying on the left-most child branches.
 */
let moveDownTimesWithClamp: 'a. (int, t('a)) => t('a) =
  (times, zipper) => {
    zipper
    |> moveDownTimes(times)
    |> Relude_Option.getOrElseLazy(() => zipper |> moveDownToBottom);
  };

/**
 * Types of movements we can make in a TreeZipper
 */
type movement = [
  | `Up(int)
  | `UpWithClamp(int)
  | `UpToTop
  | `Down(int)
  | `DownWithClamp(int)
  | `DownToBottom
  | `Left(int)
  | `LeftWithClamp(int)
  | `LeftToStart
  | `Right(int)
  | `RightWithClamp(int)
  | `RightToEnd
];

/**
 * Applies a single movement command to a zipper
 */
let moveOnceBy: 'a. (movement, t('a)) => option(t('a)) =
  (move, zipper) => {
    switch (move) {
    | `Up(n) => zipper |> moveUpTimes(n)
    | `UpWithClamp(n) => Some(zipper |> moveUpTimesWithClamp(n))
    | `UpToTop => Some(zipper |> moveUpToTop)
    | `Down(n) => zipper |> moveDownTimes(n)
    | `DownWithClamp(n) => Some(zipper |> moveDownTimesWithClamp(n))
    | `DownToBottom => Some(zipper |> moveDownToBottom)
    | `Left(n) => zipper |> moveLeftTimes(n)
    | `LeftWithClamp(n) => Some(zipper |> moveLeftTimesWithClamp(n))
    | `LeftToStart => Some(zipper |> moveLeftToStart)
    | `Right(n) => zipper |> moveRightTimes(n)
    | `RightWithClamp(n) => Some(zipper |> moveRightTimesWithClamp(n))
    | `RightToEnd => Some(zipper |> moveRightToEnd)
    };
  };

/**
 * Applies a list of movement commands to a zipper
 */
let moveBy: 'a. (list(movement), t('a)) => option(t('a)) =
  (moves, zipper) => {
    moves
    |> Relude_List.foldLeft(
         (zipperOpt, move) =>
           zipperOpt |> Relude_Option.flatMap(moveOnceBy(move)),
         Some(zipper),
       );
  };

/**
 * Applies a list of movement commands to a zipper and collects an accumulated value when visiting each new focus
 */
let foldBy:
  'a 'b.
  (list(movement), ('b, 'a) => 'b, 'b, t('a)) => option((t('a), 'b))
 =
  (moves, f, init, zipper) => {
    moves
    |> Relude_List.foldLeft(
         (zipperAccOpt, move) =>
           zipperAccOpt
           |> Relude_Option.flatMap(((zipper, acc)) => {
                zipper
                |> moveOnceBy(move)
                |> Relude_Option.map(nextZipper => {
                     (nextZipper, f(acc, nextZipper |> getFocusValue))
                   })
              }),
         Some((zipper, init)),
       );
  };

/**
 * Converts a zipper of 'a to a zipper of 'b using a pure function
 */
let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
  (aToB, {ancestors, leftSiblings, focus, rightSiblings, children}) => {
    {
      ancestors:
        ancestors
        |> Relude_List.map(((left, focus, right)) =>
             (
               left |> Relude_List.map(Relude_Tree.map(aToB)),
               aToB(focus),
               right |> Relude_List.map(Relude_Tree.map(aToB)),
             )
           ),
      leftSiblings: leftSiblings |> Relude_List.map(Relude_Tree.map(aToB)),
      focus: aToB(focus),
      rightSiblings: rightSiblings |> Relude_List.map(Relude_Tree.map(aToB)),
      children: children |> Relude_List.map(Relude_Tree.map(aToB)),
    };
  };

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

// TODO: could implement Apply/Applicative/Monad/Comonad/etc.

let findInFocus: 'a. ('a => bool, t('a)) => option(t('a)) =
  (pred, zipper) => pred(getFocusValue(zipper)) ? Some(zipper) : None;

/**
 * Finds a value in the curernt focus and recursively the children.
 * Equivalent to a depth first search in the currently focused tree.
 */
let findInFocusAndChildren: 'a. ('a => bool, t('a)) => option(t('a)) =
  (pred, zipper) => {
    let rec dfs = zipper =>
      findInFocus(pred, zipper)
      |> Relude_Option.orElseLazy(~fallback=() =>
           moveDown(zipper) |> Relude_Option.flatMap(dfs)
         )
      |> Relude_Option.orElseLazy(~fallback=() =>
           moveRight(zipper) |> Relude_Option.flatMap(dfs)
         );

    findInFocus(pred, zipper)
    |> Relude_Option.orElseLazy(~fallback=() =>
         moveDown(zipper) |> Relude_Option.flatMap(dfs)
       );
  };

/**
 * Attempts to find a value by searching the current focus and left siblings
 */
let rec findLeft:
  'a.
  (~checkFocus: bool=?, 'a => bool, t('a)) => option(t('a))
 =
  (~checkFocus=true, pred, zipper) =>
    if (checkFocus) {
      findInFocusAndChildren(pred, zipper)
      |> Relude_Option.orElseLazy(~fallback=() => {
           zipper |> moveLeft |> Relude_Option.flatMap(findLeft(pred))
         });
    } else {
      zipper |> moveLeft |> Relude_Option.flatMap(findLeft(pred));
    }

/**
 * Attempts to find a value by searching the current focus and right siblings
 */
and findRight: 'a. (~checkFocus: bool=?, 'a => bool, t('a)) => option(t('a)) =
  (~checkFocus=true, pred, zipper) =>
    if (checkFocus) {
      findInFocusAndChildren(pred, zipper)
      |> Relude_Option.orElseLazy(~fallback=() => {
           zipper |> moveRight |> Relude_Option.flatMap(findRight(pred))
         });
    } else {
      zipper |> moveRight |> Relude_Option.flatMap(findRight(pred));
    }

/**
 * Attempts to find a value by searching the current focus, then the left siblings
 * from the focus outward, then the right siblings from the focus outward.
 */
and findLeftOrRight:
  'a.
  (~checkFocus: bool=?, 'a => bool, t('a)) => option(t('a))
 =
  (~checkFocus=true, pred, zipper) => {
    zipper
    |> findLeft(~checkFocus, pred)
    |> Relude_Option.orElseLazy(~fallback=() =>
         zipper |> findRight(~checkFocus=false, pred)
       );
  }

/**
 * Attempts to find a value by moving up a level, then searching left and right on the parent level,
 * then progressing upward.
 */
and findUp: 'a. ('a => bool, t('a)) => option(t('a)) =
  (pred, zipper) =>
    zipper
    |> moveUp
    |> Relude_Option.flatMap(parentZipper => {
         parentZipper
         |> findLeftOrRight(pred)  // TODO: I think this is repeatedly searching some of the same values as we move up
         |> Relude_Option.orElseLazy(~fallback=() =>
              parentZipper |> findUp(pred)
            )
       })

/**
 * Attempts to find a value by moving down a level, then searching left and right on the child level,
 * then progressing downward.
 */
and findDown: 'a. ('a => bool, t('a)) => option(t('a)) =
  (pred, zipper) => {
    zipper
    |> moveDown
    |> Relude_Option.flatMap(childZipper => {
         childZipper
         |> findRight(pred)
         |> Relude_Option.orElseLazy(~fallback=() =>
              childZipper |> findDown(pred)
            )
       });
  }

/**
 * Attempts to find a value anywhere in the zipper, left/right/up/down
 */
and find: 'a. ('a => bool, t('a)) => option(t('a)) =
  (pred, zipper) => {
    zipper
    |> findLeftOrRight(pred)
    |> Relude_Option.orElseLazy(~fallback=() => zipper |> findUp(pred))
    |> Relude_Option.orElseLazy(~fallback=() => zipper |> findDown(pred));
  };

/**
 * Inserts a new tree, and pushes the current focus to the left
 */
let insertTreeWithPushLeft: 'a. (Relude_Tree.t('a), t('a)) => option(t('a)) =
  (newTree, {ancestors, leftSiblings, focus, rightSiblings, children}) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      Some({
        ancestors,
        leftSiblings: [{value: focus, children}, ...leftSiblings],
        focus: newTree |> Relude_Tree.getValue,
        rightSiblings,
        children: newTree |> Relude_Tree.getChildren,
      });
    } else {
      None;
    };

/**
 * Inserts a new value (singleton tree), and pushes the current focus to the left
 */
let insertWithPushLeft: 'a. ('a, t('a)) => option(t('a)) =
  (newFocus, tree) =>
    insertTreeWithPushLeft(Relude_Tree.pure(newFocus), tree);

/**
 * Inserts a new tree, and pushes the current focus to the right
 */
let insertTreeWithPushRight:
  'a.
  (Relude_Tree.t('a), t('a)) => option(t('a))
 =
  (newTree, {ancestors, leftSiblings, focus, rightSiblings, children}) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      Some({
        ancestors,
        leftSiblings,
        focus: newTree |> Relude_Tree.getValue,
        rightSiblings: [{value: focus, children}, ...rightSiblings],
        children: newTree |> Relude_Tree.getChildren,
      });
    } else {
      None;
    };

/**
 * Inserts a new value (singleton tree), and pushes the current focus to the right
 */
let insertWithPushRight: 'a. ('a, t('a)) => option(t('a)) =
  (newFocus, tree) =>
    insertTreeWithPushRight(Relude_Tree.pure(newFocus), tree);

/**
 * Deletes the tree at the focus, and pulls the left sibling into focus (if possible)
 */
let deleteWithPullLeft: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus: _, rightSiblings, children: _}) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      leftSiblings
      |> Relude_List.uncons
      |> Relude_Option.map(((leftHead, leftTail)) => {
           {
             ancestors,
             leftSiblings: leftTail,
             focus: leftHead |> Relude_Tree.getValue,
             rightSiblings,
             children: leftHead |> Relude_Tree.getChildren,
           }
         });
    } else {
      None;
    };

/**
 * Deletes the tree at the focus, and pulls the right sibling into focus (if possible)
 */
let deleteWithPullRight: 'a. t('a) => option(t('a)) =
  ({ancestors, leftSiblings, focus: _, rightSiblings, children: _}) =>
    if (ancestors |> Relude_List.isNotEmpty) {
      rightSiblings
      |> Relude_List.uncons
      |> Relude_Option.map(((rightHead, rightTail)) => {
           {
             ancestors,
             leftSiblings,
             focus: rightHead |> Relude_Tree.getValue,
             rightSiblings: rightTail,
             children: rightHead |> Relude_Tree.getChildren,
           }
         });
    } else {
      None;
    };

/**
 * Attempts to delete by deleting and pulling from the left.  If there is no item on the left,
 * it tries to pull from the right.  If there is no item on the right, it moves the focus up a level,
 * discarding the current focus and children.
 */
let delete: 'a. t('a) => option(t('a)) =
  zipper =>
    zipper
    |> deleteWithPullLeft
    |> Relude_Option.orElseLazy(~fallback=() => zipper |> deleteWithPullRight)
    |> Relude_Option.orElseLazy(~fallback=() =>
         zipper
         |> getAncestors
         |> Relude_List.uncons
         |> Relude_Option.map(
              (
                (
                  (parentLeftSiblings, parentFocus, parentRightSiblings),
                  ancestorsTail,
                ),
              ) => {
              {
                ancestors: ancestorsTail,
                leftSiblings: parentLeftSiblings,
                focus: parentFocus,
                rightSiblings: parentRightSiblings,
                children: [],
              }
            })
       );

let showBy: 'a. ('a => string, t('a)) => string =
  (showA, {ancestors, leftSiblings, focus, rightSiblings, children}) => {
    let ancestorsStr =
      ancestors
      |> Relude_List.showBy(
           Relude_Tuple.showBy3(
             Relude_List.showBy(Relude_Tree.showBy(showA)),
             showA,
             Relude_List.showBy(Relude_Tree.showBy(showA)),
           ),
         );

    let leftSiblingsStr =
      leftSiblings |> Relude_List.showBy(Relude_Tree.showBy(showA));

    let focusStr = showA(focus);

    let rightSiblingsStr =
      rightSiblings |> Relude_List.showBy(Relude_Tree.showBy(showA));

    let childrenStr =
      children |> Relude_List.showBy(Relude_Tree.showBy(showA));

    "TreeZipper"
    ++ "\n"
    ++ "ancestors = "
    ++ ancestorsStr
    ++ "\n"
    ++ "leftSiblings = "
    ++ leftSiblingsStr
    ++ "\n"
    ++ "focus = "
    ++ focusStr
    ++ "\n"
    ++ "rightSiblings = "
    ++ rightSiblingsStr
    ++ "\n"
    ++ "children = "
    ++ childrenStr;
  };