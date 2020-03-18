open Jest;
open Expect;
open Relude.Globals;
open Relude.TreeZipper;

let testTree1 =
  Tree.make(
    1,
    [
      Tree.make(
        2,
        [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      ),
      Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
      Tree.make(4, []),
    ],
  );

let testTree2 =
  Tree.make(
    1,
    [
      Tree.make(
        2,
        [
          Tree.make(21, [Tree.pure(211), Tree.pure(212), Tree.pure(213)]),
          Tree.pure(22),
          Tree.pure(23),
          Tree.make(
            24,
            [
              Tree.pure(241),
              Tree.pure(242),
              Tree.make(
                243,
                [Tree.pure(2431), Tree.pure(2432), Tree.pure(2433)],
              ),
            ],
          ),
        ],
      ),
      Tree.pure(3),
      Tree.pure(4),
      Tree.make(
        5,
        [
          Tree.pure(51),
          Tree.pure(52),
          Tree.make(53, [Tree.pure(531), Tree.pure(532), Tree.pure(533)]),
        ],
      ),
    ],
  );

describe("TreeZipper", () => {
  test("pure", () => {
    expect(TreeZipper.pure(42))
    |> toEqual({
         ancestors: [],
         leftSiblings: [],
         focus: 42,
         rightSiblings: [],
         children: [],
       })
  });

  test("make", () => {
    expect(
      TreeZipper.make(
        [([Tree.pure(1)], 2, [Tree.pure(3)])],
        [Tree.pure(4)],
        5,
        [Tree.pure(6)],
        [Tree.pure(7)],
      ),
    )
    |> toEqual({
         ancestors: [([Tree.pure(1)], 2, [Tree.pure(3)])],
         leftSiblings: [Tree.pure(4)],
         focus: 5,
         rightSiblings: [Tree.pure(6)],
         children: [Tree.pure(7)],
       })
  });

  test("makeWithLabels", () => {
    expect(
      TreeZipper.makeWithLabels(
        ~ancestors=[([Tree.pure(1)], 2, [Tree.pure(3)])],
        ~leftSiblings=[Tree.pure(4)],
        ~focus=5,
        ~rightSiblings=[Tree.pure(6)],
        ~children=[Tree.pure(7)],
      ),
    )
    |> toEqual({
         ancestors: [([Tree.pure(1)], 2, [Tree.pure(3)])],
         leftSiblings: [Tree.pure(4)],
         focus: 5,
         rightSiblings: [Tree.pure(6)],
         children: [Tree.pure(7)],
       })
  });

  test("fromTree", () => {
    let actual = TreeZipper.fromTree(testTree1);
    let expected = {
      ancestors: [],
      leftSiblings: [],
      focus: testTree1 |> Tree.getValue,
      rightSiblings: [],
      children: testTree1 |> Tree.getChildren,
    };
    expect(actual) |> toEqual(expected);
  });

  test("getAncestors", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> moveDownTimes(3)
      |> Option.map(TreeZipper.getAncestors);
    let expected =
      Some([
        ([], 21, [Tree.pure(22), Tree.pure(23)]),
        (
          [],
          2,
          [
            Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
            Tree.pure(4),
          ],
        ),
        ([], 1, []),
      ]);
    expect(actual) |> toEqual(expected);
  });

  test("getFocusValue", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> moveDownTimes(3)
      |> Option.map(TreeZipper.getFocusValue);
    let expected = Some(211);
    expect(actual) |> toEqual(expected);
  });

  test("tapFocusValue", () => {
    let r = ref(None);
    testTree1
    |> TreeZipper.fromTree
    |> TreeZipper.tapFocusValue(a => r := Some(a))
    |> ignore;
    let actual = r^;
    let expected = Some(1);
    expect(actual) |> toEqual(expected);
  });

  test("setFocusValue", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> moveDownTimes(3)
      |> Option.map(setFocusValue(42));
    let expected =
      Some({
        ancestors: [
          ([], 21, [Tree.pure(22), Tree.pure(23)]),
          (
            [],
            2,
            [
              Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
              Tree.pure(4),
            ],
          ),
          ([], 1, []),
        ],
        leftSiblings: [],
        focus: 42,
        rightSiblings: [Tree.pure(212)],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("modifyFocusValue", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> moveDownTimes(3)
      |> Option.map(modifyFocusValue(a => a + 42));
    let expected =
      Some({
        ancestors: [
          ([], 21, [Tree.pure(22), Tree.pure(23)]),
          (
            [],
            2,
            [
              Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
              Tree.pure(4),
            ],
          ),
          ([], 1, []),
        ],
        leftSiblings: [],
        focus: 211 + 42,
        rightSiblings: [Tree.pure(212)],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("getFocusTree", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.map(TreeZipper.getFocusTree);
    let expected: option(Tree.t(int)) =
      Some({value: 3, children: [Tree.make(31, [Tree.pure(311)])]});
    expect(actual) |> toEqual(expected);
  });

  test("getLeftSiblings", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2))
      |> Option.map(TreeZipper.getLeftSiblings);
    let expected =
      Some([
        Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
        Tree.make(
          2,
          [
            Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
            Tree.pure(22),
            Tree.pure(23),
          ],
        ),
      ]);
    expect(actual) |> toEqual(expected);
  });

  test("getLeftSiblingsInOrder", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2))
      |> Option.map(TreeZipper.getLeftSiblingsInOrder);
    let expected =
      Some([
        Tree.make(
          2,
          [
            Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
            Tree.pure(22),
            Tree.pure(23),
          ],
        ),
        Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
      ]);
    expect(actual) |> toEqual(expected);
  });

  test("setLeftSiblings", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(
           TreeZipper.setLeftSiblings([Tree.pure(42), Tree.pure(43)]),
         );

    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [Tree.pure(42), Tree.pure(43)],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });

    expect(actual) |> toEqual(expected);
  });

  test("setLeftSiblingsFromInOrder", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(
           TreeZipper.setLeftSiblingsFromInOrder([
             Tree.pure(43),
             Tree.pure(42),
           ]),
         );

    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [Tree.pure(42), Tree.pure(43)],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });

    expect(actual) |> toEqual(expected);
  });

  test("getRightSiblings", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.map(TreeZipper.getRightSiblings);
    let expected =
      Some([
        Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
        Tree.pure(4),
      ]);
    expect(actual) |> toEqual(expected);
  });

  test("setRightSiblings", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(
           TreeZipper.setRightSiblings([Tree.pure(42), Tree.pure(43)]),
         );

    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [Tree.pure(42), Tree.pure(43)],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });

    expect(actual) |> toEqual(expected);
  });

  test("getChildren", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.map(TreeZipper.getChildren);
    let expected =
      Some([
        Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
        Tree.pure(22),
        Tree.pure(23),
      ]);
    expect(actual) |> toEqual(expected);
  });

  test("setChildren", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.setChildren([Tree.pure(42), Tree.pure(43)]);
    let expected = {
      ancestors: [],
      leftSiblings: [],
      focus: 1,
      rightSiblings: [],
      children: [Tree.pure(42), Tree.pure(43)],
    };
    expect(actual) |> toEqual(expected);
  });

  test("moveLeft None", () => {
    let actual = testTree1 |> TreeZipper.fromTree |> TreeZipper.moveLeft;
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveLeft", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRight)
      |> Option.flatMap(TreeZipper.moveLeft);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveLeftWithClamp", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveLeftWithClamp;
    let expected = testTree1 |> TreeZipper.fromTree;
    expect(actual) |> toEqual(expected);
  });

  test("moveLeftToStart", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2))
      |> Option.map(TreeZipper.moveLeftToStart);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveLeftTimes", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2))
      |> Option.flatMap(TreeZipper.moveLeftTimes(2));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveLeftTimes negative", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveLeftTimes(-1);
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveLeftTimesWithClamp", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2))
      |> Option.map(TreeZipper.moveLeftTimesWithClamp(5));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveRight None", () => {
    let actual = testTree1 |> TreeZipper.fromTree |> TreeZipper.moveRight;
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveRight", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRight);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 3,
        rightSiblings: [Tree.make(4, [])],
        children: [Tree.make(31, [Tree.pure(311)])],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveRightWithClamp", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveRightWithClamp;
    let expected = testTree1 |> TreeZipper.fromTree;
    expect(actual) |> toEqual(expected);
  });

  test("moveRightTimes", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRightTimes(2));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 4,
        rightSiblings: [],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveRightTimes negative", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveRightTimes(-1);
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveRightTimesWithClamp", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.map(TreeZipper.moveRightTimesWithClamp(4));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 4,
        rightSiblings: [],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveRightToEnd", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.map(TreeZipper.moveRightToEnd);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 4,
        rightSiblings: [],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveUp None", () => {
    let actual = testTree1 |> TreeZipper.fromTree |> TreeZipper.moveUp;
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveUpWithClamp", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveUpWithClamp;
    let expected = testTree1 |> TreeZipper.fromTree;
    expect(actual) |> toEqual(expected);
  });

  test("moveUpTimes", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDownTimes(2)
      |> Option.flatMap(TreeZipper.moveUpTimes(2));
    let expected = Some(testTree1 |> TreeZipper.fromTree);
    expect(actual) |> toEqual(expected);
  });

  test("moveUpTimes negative", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveUpTimes(-1);
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveUpTimesWithClamp", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDownTimes(2)
      |> Option.map(TreeZipper.moveUpTimesWithClamp(5));
    let expected = Some(testTree1 |> TreeZipper.fromTree);
    expect(actual) |> toEqual(expected);
  });

  test("moveUpToTop", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDownTimes(2)
      |> Option.map(TreeZipper.moveUpToTop);
    let expected = Some(testTree1 |> TreeZipper.fromTree);
    expect(actual) |> toEqual(expected);
  });

  test("moveUpToTop maintains structure", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveBy([`Down(2), `Right(2)])
      |> Option.map(TreeZipper.moveUpToTop);
    let expected = Some(testTree1 |> TreeZipper.fromTree);
    expect(actual) |> toEqual(expected);
  });

  test("moveDown", () => {
    let actual = testTree1 |> TreeZipper.fromTree |> TreeZipper.moveDown;
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveDownWithClamp", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDownTimes(3)
      |> Option.map(TreeZipper.moveDownWithClamp);
    let expected =
      Some({
        ancestors: [
          ([], 21, [Tree.pure(22), Tree.pure(23)]),
          (
            [],
            2,
            [
              Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
              Tree.pure(4),
            ],
          ),
          ([], 1, []),
        ],
        leftSiblings: [],
        focus: 211,
        rightSiblings: [Tree.pure(212)],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveDownToBottom", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveDownToBottom;
    let expected = {
      ancestors: [
        ([], 21, [Tree.pure(22), Tree.pure(23)]),
        (
          [],
          2,
          [
            Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
            Tree.pure(4),
          ],
        ),
        ([], 1, []),
      ],
      leftSiblings: [],
      focus: 211,
      rightSiblings: [Tree.pure(212)],
      children: [],
    };
    expect(actual) |> toEqual(expected);
  });

  test("moveDownTimes", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveDownTimes(3);
    let expected =
      Some({
        ancestors: [
          ([], 21, [Tree.pure(22), Tree.pure(23)]),
          (
            [],
            2,
            [
              Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
              Tree.pure(4),
            ],
          ),
          ([], 1, []),
        ],
        leftSiblings: [],
        focus: 211,
        rightSiblings: [Tree.pure(212)],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("moveDownTimes negative", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveDownTimes(-1);
    let expected = None;
    expect(actual) |> toEqual(expected);
  });

  test("moveDownTimesWithClamp", () => {
    let actual =
      testTree1 |> TreeZipper.fromTree |> TreeZipper.moveDownTimesWithClamp(5);
    let expected = {
      ancestors: [
        ([], 21, [Tree.pure(22), Tree.pure(23)]),
        (
          [],
          2,
          [
            Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
            Tree.pure(4),
          ],
        ),
        ([], 1, []),
      ],
      leftSiblings: [],
      focus: 211,
      rightSiblings: [Tree.pure(212)],
      children: [],
    };
    expect(actual) |> toEqual(expected);
  });

  test("moveBy", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveBy([
           `Down(1),
           `Right(1),
           `Left(1),
           `Up(1),
           `DownWithClamp(1),
           `RightWithClamp(1),
           `LeftWithClamp(1),
           `UpWithClamp(1),
           `DownToBottom,
           `UpToTop,
           `RightToEnd,
           `LeftToStart,
           `Down(1),
           `Right(1),
           `Down(1),
         ]);
    let expected =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.flatMap(moveDown);
    expect(actual) |> toEqual(expected);
  });

  test("foldBy", () => {
    let actual =
      testTree1
      |> fromTree
      |> foldBy(
           [
             `Down(1),
             `Right(1),
             `Left(1),
             `Up(1),
             `DownWithClamp(1),
             `RightWithClamp(1),
             `LeftWithClamp(1),
             `UpWithClamp(1),
             `DownToBottom,
             `UpToTop,
             `RightToEnd,
             `LeftToStart,
             `Down(1),
             `Right(1),
             `Down(1),
           ],
           (l, v) => l |> Relude_List.append(v),
           [],
         );
    let expectedZipper =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.flatMap(moveDown);
    expect(actual)
    |> toEqual(
         expectedZipper
         |> Option.map(z =>
              (z, [2, 3, 2, 1, 2, 3, 2, 1, 211, 1, 1, 1, 2, 3, 31])
            ),
       );
  });

  test("map", () => {
    let actual =
      testTree1
      |> TreeZipper.fromTree
      |> TreeZipper.moveDown
      |> Option.flatMap(TreeZipper.moveRight)
      |> Option.map(TreeZipper.map(string_of_int));
    let expected =
      Some({
        ancestors: [([], "1", [])],
        leftSiblings: [
          Tree.make(
            "2",
            [
              Tree.make("21", [Tree.pure("211"), Tree.pure("212")]),
              Tree.pure("22"),
              Tree.pure("23"),
            ],
          ),
        ],
        focus: "3",
        rightSiblings: [Tree.make("4", [])],
        children: [Tree.make("31", [Tree.pure("311")])],
      });
    expect(actual) |> toEqual(expected);
  });

  test("findInFocus", () => {
    let actual = testTree1 |> fromTree |> findInFocus(a => a == 1);
    let expected = testTree1 |> fromTree |> Relude_Option.pure; // 1
    let actual2 = testTree1 |> fromTree |> findInFocus(a => a == 0);
    let expected2 = None;
    expect((actual, actual2)) |> toEqual((expected, expected2));
  });

  test("findInFocusAndChildren", () => {
    let a = testTree2 |> fromTree |> findInFocusAndChildren(a => a == 213);
    let e =
      testTree2
      |> fromTree  // 1
      |> moveDownTimes(3)  // 211
      |> Option.flatMap(moveRightTimes(2)); // 213
    let a2 = testTree2 |> fromTree |> findInFocusAndChildren(a => a == 2433);
    let e2 =
      testTree2
      |> fromTree  // 1
      |> moveDownTimes(2)  // 21
      |> Option.flatMap(moveRightTimes(3))  // 24
      |> Option.flatMap(moveDown)  // 241
      |> Option.flatMap(moveRightTimes(2))  // 243
      |> Option.flatMap(moveDown)  // 2431
      |> Option.flatMap(moveRightTimes(2)); // 2433
    let a3 = testTree2 |> fromTree |> findInFocusAndChildren(a => a == 533);
    let e3 =
      testTree2
      |> fromTree  // 1
      |> moveDown  // 2
      |> Option.flatMap(moveRightTimes(3))  // 5
      |> Option.flatMap(moveDown)  // 51
      |> Option.flatMap(moveRightTimes(2))  // 53
      |> Option.flatMap(moveDown)  // 531
      |> Option.flatMap(moveRightTimes(2)); // 533
    expect((a, a2, a3)) |> toEqual((e, e2, e3));
  });

  test("findLeft", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.flatMap(findLeft(a => a == 2));
    let expected = testTree1 |> fromTree |> moveDown;
    expect(actual) |> toEqual(expected);
  });

  test("findRight", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(findRight(a => a == 4));
    let expected =
      testTree1 |> fromTree |> moveDown |> Option.flatMap(moveRightTimes(2));
    expect(actual) |> toEqual(expected);
  });

  test("findUp", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDownTimes(3)
      |> Option.flatMap(moveRight)
      |> Option.flatMap(findUp(a => a == 1));
    let expected = Some(testTree1 |> fromTree);
    expect(actual) |> toEqual(expected);
  });

  test("findDown", () => {
    let actual = testTree1 |> fromTree |> findDown(a => a == 311);
    let expected =
      testTree1
      |> fromTree  // 1
      |> moveDown  // 2
      |> Option.flatMap(moveRight)  // 3
      |> Option.flatMap(moveDownTimes(2)); // 31, 311
    expect(actual) |> toEqual(expected);
  });

  test("find", () => {
    let actual =
      testTree1 |> fromTree |> moveDown |> Option.flatMap(find(a => a == 311));
    let expected =
      testTree1
      |> fromTree  // 1
      |> moveDown  // 2
      |> Option.flatMap(moveRight)  // 3
      |> Option.flatMap(moveDownTimes(2)); // 31, 311
    expect(actual) |> toEqual(expected);
  });

  test("insertTreeWithPushLeft", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(
           insertTreeWithPushLeft(Tree.make(42, [Tree.pure(43)])),
         );
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 42,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [Tree.pure(43)],
      });
    expect(actual) |> toEqual(expected);
  });

  test("insertWithPushLeft", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(insertWithPushLeft(42));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 42,
        rightSiblings: [
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("insertTreeWithPushRight", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(
           insertTreeWithPushRight(Tree.make(42, [Tree.pure(43)])),
         );
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 42,
        rightSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [Tree.pure(43)],
      });
    expect(actual) |> toEqual(expected);
  });

  test("insertWithPushRight", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(insertWithPushRight(42));
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 42,
        rightSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
          Tree.make(3, [Tree.make(31, [Tree.pure(311)])]),
          Tree.make(4, []),
        ],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("deleteWithPullLeft", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.flatMap(deleteWithPullLeft);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 2,
        rightSiblings: [Tree.make(4, [])],
        children: [
          Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
          Tree.pure(22),
          Tree.pure(23),
        ],
      });
    expect(actual) |> toEqual(expected);
  });

  test("deleteWithPullRight", () => {
    let actual =
      testTree1
      |> fromTree
      |> moveDown
      |> Option.flatMap(moveRight)
      |> Option.flatMap(deleteWithPullRight);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [
          Tree.make(
            2,
            [
              Tree.make(21, [Tree.pure(211), Tree.pure(212)]),
              Tree.pure(22),
              Tree.pure(23),
            ],
          ),
        ],
        focus: 4,
        rightSiblings: [],
        children: [],
      });
    expect(actual) |> toEqual(expected);
  });

  test("delete", () => {
    let actual = testTree1 |> fromTree |> moveDown |> Option.flatMap(delete);
    let expected =
      Some({
        ancestors: [([], 1, [])],
        leftSiblings: [],
        focus: 3,
        rightSiblings: [Tree.pure(4)],
        children: [Tree.make(31, [Tree.pure(311)])],
      });
    expect(actual) |> toEqual(expected);
  });
});