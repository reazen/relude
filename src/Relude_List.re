/*******************************************************************************
 * Organization of List functions:
 *
 * - Functions needed by typeclasses
 * - Functions unrelated to typeclasses and indifferent of inner 'a
 * - Typeclass implementations that don't care about inner 'a
 * - Freebies from those typeclasses
 * - Functions and typeclasses that _do_ care about inner 'a
 ******************************************************************************/

/*******************************************************************************
 * Needed by typeclasses
 ******************************************************************************/

let concat: (list('a), list('a)) => list('a) = Belt.List.concat;
let empty: list('a) = [];
let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;
let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;
let pure: 'a => list('a) = a => [a];
let bind: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;
let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;
let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

/*******************************************************************************
 * List-specific utilities (construct, destructure, transform)
 ******************************************************************************/

let fromArray: array('a) => list('a) = Belt.List.fromArray;
let toArray: list('a) => array('a) = Belt.List.toArray;

let append: ('a, list('a)) => list('a) = (x, xs) => concat(xs, [x]);
let cons: ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];
let prepend: ('a, list('a)) => list('a) = cons;
let uncons: list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let repeat: (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);
let makeWithIndex: (int, int => 'a) => list('a) = Belt.List.makeBy;

let reverse: list('a) => list('a) = Belt.List.reverse;
let shuffle: list('a) => list('a) = Belt.List.shuffle;

let isEmpty: list('a) => bool =
  fun
  | [] => true
  | _ => false;

let isNotEmpty: list('a) => bool = xs => !isEmpty(xs);

let get: (int, list('a)) => option('a) = (i, xs) => Belt.List.get(xs, i);

let head: list('a) => option('a) =
  fun
  | [] => None
  | [x, ..._] => Some(x);

let tail: list('a) => option(list('a)) =
  fun
  | [] => None
  | [_, ...xs] => Some(xs);

let tailOrEmpty: list('a) => list('a) =
  xs => tail(xs) |> Relude_Option.getOrElseStrict([]);

let rec init: list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] =>
    Some(cons(x, Relude_Option.getOrElseStrict([], init(xs))));

let rec last: list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: (int, list('a)) => option(list('a)) =
  (i, xs) => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | _ when count <= 0 => Some(acc)
      | [] => None
      | [y, ...ys] => go([y, ...acc], count - 1, ys)
      };
    go([], i, xs) |> Relude_Option.map(reverse);
  };

let takeUpTo: (int, list('a)) => list('a) =
  (i, xs) => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | _ when count <= 0 => acc
      | [] => acc
      | [y, ...ys] => go([y, ...acc], count - 1, ys)
      };
    go([], i, xs) |> reverse;
  };

let takeWhile: ('a => bool, list('a)) => list('a) =
  (f, xs) => {
    let rec go = (acc, rest) =>
      switch (rest) {
      | [] => acc
      | [y, ..._] when !f(y) => acc
      | [y, ...ys] => go([y, ...acc], ys)
      };
    go([], xs) |> reverse;
  };

let drop: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

let rec dropUpTo: (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ..._] when i <= 0 => xs
    | [_, ...ys] => dropUpTo(i - 1, ys)
    };

let rec dropWhile: ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [y, ...ys] when f(y) => dropWhile(f, ys)
    | _ => xs
    };

let filter: ('a => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keep(xs, f);

let filterWithIndex: (('a, int) => bool, list('a)) => list('a) =
  (f, xs) => Belt.List.keepWithIndex(xs, f);

/**
 * Map all list values from 'a to option('b), filtering out the `None`s
 */
let mapOption: ('a => option('b), list('a)) => list('b) =
  (f, xs) =>
    foldLeft(
      (acc, curr) =>
        Relude_Option.fold(() => acc, v => [v, ...acc], f(curr)),
      [],
      xs,
    )
    |> reverse;

let catOptions: list(option('a)) => list('a) = xs => mapOption(a => a, xs);

let mapWithIndex: (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, f);

let partition: ('a => bool, list('a)) => (list('a), list('a)) =
  (f, xs) => Belt.List.partition(xs, f);

let splitAt: (int, list('a)) => option((list('a), list('a))) =
  (i, xs) => Belt.List.splitAt(xs, i);

let rec prependToAll: ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [delim, y, ...prependToAll(delim, ys)]
    };

let intersperse: ('a, list('a)) => list('a) =
  (delim, xs) =>
    switch (xs) {
    | [] => []
    | [y, ...ys] => [y, ...prependToAll(delim, ys)]
    };

let replicate: (int, list('a)) => list('a) =
  (i, xs) => {
    let rec go = (count, acc) =>
      count <= 1 ? acc : go(count - 1, concat(xs, acc));
    go(i, xs);
  };

let zip: (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

let zipWithIndex: list('a) => list(('a, int)) =
  xs => {
    let rec go = (acc, count, rest) =>
      switch (rest) {
      | [] => acc
      | [y, ...ys] => go([(y, count), ...acc], count + 1, ys)
      };
    go([], 0, xs) |> reverse;
  };

let unzip: list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

let sortWithInt: (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

let sortF: (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let sort =
    (
      type a,
      ordA: (module BsAbstract.Interface.ORD with type t = a),
      xs: list(a),
    )
    : list(a) => {
  module OrdA = (val ordA);
  sortF(OrdA.compare, xs);
};

/*******************************************************************************
 * Typeclass Implementations
 ******************************************************************************/

module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = concat;
};

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = empty;
};

module Alt = BsAbstract.List.Alt;
module Plus = BsAbstract.List.Plus;
module Alternative = BsAbstract.List.Alternative;

module Functor = BsAbstract.List.Functor;
module Apply = BsAbstract.List.Apply;
module Applicative = BsAbstract.List.Applicative;
module Monad = BsAbstract.List.Monad;

module Foldable = BsAbstract.List.Foldable;
module Traversable = BsAbstract.List.Traversable;

/*******************************************************************************
 * Free utilities from typeclasses
 ******************************************************************************/

/**
 * Freebies from Monad/Apply/Functor
 */
module MonadFunctions = Relude_Monads.Functions(Monad);

let void: list('a) => list(unit) = MonadFunctions.void;
let flap: (list('a => 'b), 'a) => list('b) = MonadFunctions.flap;

let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c) = MonadFunctions.lift2;
let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d) = MonadFunctions.lift3;
let map4:
  (('a, 'b, 'c, 'd) => 'e, list('a), list('b), list('c), list('d)) =>
  list('e) = MonadFunctions.lift4;
let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    list('a),
    list('b),
    list('c),
    list('d),
    list('e)
  ) =>
  list('f) = MonadFunctions.lift5;

let flatMap: 'a 'b. ('a => list('b), list('a)) => list('b) = MonadFunctions.flatMap;
let flatten: list(list('a)) => list('a) = MonadFunctions.flatten;

/**
 * Freebies from Foldable/Traversable
 */
module FoldableFunctions = Relude_Foldables.Functions(Foldable);

let any: 'a. ('a => bool, list('a)) => bool = FoldableFunctions.any;
let all: 'a. ('a => bool, list('a)) => bool = FoldableFunctions.all;

let forEach: 'a. ('a => unit, list('a)) => unit = FoldableFunctions.forEach;
let forEachWithIndex: 'a. (('a, int) => unit, list('a)) => unit = FoldableFunctions.forEachWithIndex;

let find: 'a. ('a => bool, list('a)) => option('a) = FoldableFunctions.find;
let findWithIndex: (('a, int) => bool, list('a)) => option('a) = FoldableFunctions.findWithIndex;

let fold = FoldableFunctions.fold;
let intercalate = FoldableFunctions.intercalate;

let countBy: 'a. ('a => bool, list('a)) => int = FoldableFunctions.countBy;
let length: list('a) => int = FoldableFunctions.length;

let containsF: 'a. (('a, 'a) => bool, 'a, list('a)) => bool = FoldableFunctions.containsF;
let contains = FoldableFunctions.contains;

let indexOfF: 'a. (('a, 'a) => bool, 'a, list('a)) => option(int) = FoldableFunctions.indexOfF;
let indexOf = FoldableFunctions.indexOf;

/* TODO: scanLeft and scanRight come for free with traversable? */
let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    snd(
      foldLeft(
        ((acc, result), curr) => {
          let nextAcc = f(acc, curr);
          (nextAcc, append(nextAcc, result));
        },
        (init, []),
        xs,
      ),
    );

let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b) =
  (f, init, xs) =>
    snd(
      foldRight(
        (curr, (acc, result)) => {
          let nextAcc = f(curr, acc);
          (nextAcc, prepend(nextAcc, result));
        },
        (init, []),
        xs,
      ),
    );

/*******************************************************************************
 * Helper functions and typeclasses that do care about inner 'a
 ******************************************************************************/

module Show = BsAbstract.List.Show;

let showF: ('a => string, list('a)) => string =
  (innerShow, xs) => {
    let joinStrings = intercalate((module BsAbstract.String.Monoid));
    "[" ++ joinStrings(", ", map(innerShow, xs)) ++ "]";
  };

let show =
    (
      type a,
      showA: (module BsAbstract.Interface.SHOW with type t = a),
      xs: list(a),
    )
    : string => {
  module ShowA = (val showA);
  showF(ShowA.show, xs);
};

module Eq = BsAbstract.List.Eq;

let rec eqF: (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) when innerEq(x, y) => eqF(innerEq, xs, ys)
    | _ => false
    };

let eq =
    (
      type a,
      eqA: (module BsAbstract.Interface.EQ with type t = a),
      xs: list(a),
      ys: list(a),
    )
    : bool => {
  module EqA = (val eqA);
  eqF(EqA.eq, xs, ys);
};

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */
let distinctF: 'a. (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    foldLeft(
      /* foldRight would probably be faster with cons, but you lose the original ordering on the list */
      (acc, curr) => containsF(eq, curr, acc) ? acc : [curr, ...acc],
      [],
      xs,
    )
    |> reverse;

let distinct =
    (
      type a,
      eqA: (module BsAbstract.Interface.EQ with type t = a),
      xs: list(a),
    )
    : list(a) => {
  module EqA = (val eqA);
  distinctF(EqA.eq, xs);
};

let removeF: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, v, xs) => {
    let go = ((found, ys), x) =>
      found ?
        (true, [x, ...ys]) :
        innerEq(v, x) ? (true, ys) : (false, [x, ...ys]);
    foldLeft(go, (false, []), xs) |> snd |> reverse;
  };

let remove =
    (
      type a,
      eqA: (module BsAbstract.Interface.EQ with type t = a),
      x: a,
      xs: list(a),
    )
    : list(a) => {
  module EqA = (val eqA);
  removeF(EqA.eq, x, xs);
};

let removeEachF: 'a. (('a, 'a) => bool, 'a, list('a)) => list('a) =
  (innerEq, x, xs) =>
    foldLeft((ys, v) => innerEq(x, v) ? ys : [v, ...ys], [], xs) |> reverse;

let removeEach =
    (
      type a,
      eqA: (module BsAbstract.Interface.EQ with type t = a),
      x: a,
      xs: list(a),
    )
    : list(a) => {
  module EqA = (val eqA);
  removeEachF(EqA.eq, x, xs);
};

let sumFloat: list(float) => float =
  xs => FoldableFunctions.fold((module Relude_Float.Additive.Monoid), xs);

module Infix = {
  include BsAbstract.List.Infix;
  include MonadFunctions.ApplyFunctions.Infix;
};

module Sequence: Relude_Sequence.SEQUENCE with type t('a) = list('a) = {
  type t('a) = list('a);

  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
  let mkString = intercalate((module BsAbstract.String.Monoid));
  let eqF = eqF;
  let showF = showF;

  module SemigroupAny = SemigroupAny;
  module MonoidAny = MonoidAny;
  module Functor = Functor;
  module Apply = Apply;
  module Applicative = Applicative;
  module Monad = Monad;
  module Foldable = Foldable;
  module Traversable = Traversable;
  module Eq = Eq;
  module Show = Show;
};

module IsoArray: Relude_IsoArray.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = fromArray;
  let toArray = toArray;
};
