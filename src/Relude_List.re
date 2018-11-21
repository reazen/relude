let length: list('a) => int = Belt.List.length;

let isEmpty: list('a) => bool =
  fun
  | [] => true
  | _ => false;

let isNotEmpty: list('a) => bool = xs => !isEmpty(xs);

let empty: list('a) = [];

let pure: 'a => list('a) = a => [a];

let one: 'a => list('a) = pure;

let repeat: (int, 'a) => list('a) = (i, x) => Belt.List.make(i, x);

let makeWithIndex: (int, int => 'a) => list('a) = Belt.List.makeBy;

let concat: (list('a), list('a)) => list('a) = Belt.List.concat;

let cons: ('a, list('a)) => list('a) = (x, xs) => [x, ...xs];

let uncons: list('a) => option(('a, list('a))) =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let prepend: ('a, list('a)) => list('a) = cons;

let append: ('a, list('a)) => list('a) = (x, xs) => concat(xs, [x]);

let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;

let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

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

let get: (int, list('a)) => option('a) = (i, xs) => Belt.List.get(xs, i);

let head: list('a) => option('a) = Belt.List.head;

let tail: list('a) => option(list('a)) = Belt.List.tail;

let tailOrEmpty: list('a) => list('a) =
  xs => tail(xs)->Belt.Option.getWithDefault(empty);

let rec init: list('a) => option(list('a)) =
  fun
  | [] => None
  | [_] => Some([])
  | [x, ...xs] => Some(cons(x, init(xs)->Belt.Option.getWithDefault([])));

let rec last: list('a) => option('a) =
  fun
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last(xs);

let take: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.take(xs, i);

let rec takeUpTo: (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | _ when i == 0 => []
    | [y, ...ys] => [y, ...takeUpTo(i - 1, ys)]
    };

let rec takeWhile: ('a => bool, list('a)) => list('a) =
  (f, xs) =>
    switch (xs) {
    | [] => []
    | [x, ...xs] when f(x) => [x, ...takeWhile(f, xs)]
    | _ => []
    };

let drop: (int, list('a)) => option(list('a)) =
  (i, xs) => Belt.List.drop(xs, i);

let rec dropUpTo: (int, list('a)) => list('a) =
  (i, xs) =>
    switch (xs) {
    | [] => []
    | [_, ..._] when i == 0 => xs
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

let rec find: ('a => bool, list('a)) => option('a) =
  (f, xs) =>
    switch (xs) {
    | [] => None
    | [y, ..._] when f(y) => Some(y)
    | [_, ...ys] => find(f, ys)
    };

let findWithIndex: (('a, int) => bool, list('a)) => option('a) =
  (f, xs) => {
    let rec go = (f, ys, i) =>
      switch (ys) {
      | [] => None
      | [z, ..._] when f(z, i) => Some(z)
      | [_, ...zs] => go(f, zs, i + 1)
      };
    go(f, xs, 0);
  };

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
  (i, xs) =>
    foldLeft((acc, _i) => concat(acc, xs), [], Relude_Int.rangeAsList(0, i));

let zip: (list('a), list('b)) => list(('a, 'b)) = Belt.List.zip;

let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c) =
  (f, xs, ys) => Belt.List.zipBy(xs, ys, f);

let zipWithIndex: list('a) => list(('a, int)) =
  xs => zip(xs, Relude_Int.rangeAsList(0, length(xs)));

let unzip: list(('a, 'b)) => (list('a), list('b)) = Belt.List.unzip;

let sortWithInt: (('a, 'a) => int, list('a)) => list('a) =
  (f, xs) => Belt.List.sort(xs, f);

let sort: (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a) =
  (f, xs) => sortWithInt((a, b) => f(a, b) |> Relude_Ordering.toInt, xs);

let shuffle: list('a) => list('a) = Belt.List.shuffle;

let reverse: list('a) => list('a) = Belt.List.reverse;

let rec any: ('a => bool, list('a)) => bool =
  (f, xs) =>
    switch (xs) {
    | [] => false
    | [y, ..._] when f(y) => true
    | [_, ...ys] => any(f, ys)
    };

let contains: (('a, 'a) => bool, 'a, list('a)) => bool =
  (f, x, xs) => any(f(x), xs);

let indexOf: (('a, 'a) => bool, 'a, list('a)) => option(int) =
  (f, x, xs) => {
    let rec go = (f, ys, i) =>
      switch (ys) {
      | [] => None
      | [z, ..._] when f(x, z) => Some(i)
      | [_, ...zs] => go(f, zs, i + 1)
      };
    go(f, xs, 0);
  };

let rec all: ('a => bool, list('a)) => bool =
  (f, xs) =>
    switch (xs) {
    | [] => true
    | [y, ...ys] when f(y) => all(f, ys)
    | _ => false
    };

/* TODO: distinct function that uses ordering so we can use a faster Set (Belt.Set?) to check for uniqueness */

let distinct: (('a, 'a) => bool, list('a)) => list('a) =
  (eq, xs) =>
    foldLeft(
      /* foldRight would probably be faster with cons, but you lose the original ordering on the list */
      (acc, curr) =>
        if (contains(eq, curr, acc)) {
          acc;
        } else {
          append(curr, acc);
        },
      [],
      xs,
    );

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let mapWithIndex: (('a, int) => 'b, list('a)) => list('b) =
  (f, xs) => Belt.List.mapWithIndex(xs, f);

let forEach: ('a => unit, list('a)) => unit =
  (f, xs) => Belt.List.forEach(xs, f);

let forEachWithIndex: (('a, int) => unit, list('a)) => unit =
  (f, xs) => Belt.List.forEachWithIndex(xs, f);

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let bind: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let flatMap: ('a => list('b), list('a)) => list('b) = (f, fa) => bind(fa, f);

let flatten: list(list('a)) => list('a) =
  xss => bind(xss, Relude_Function.identity);

let fromArray: array('a) => list('a) = Belt.List.fromArray;

let toArray: list('a) => array('a) = Belt.List.toArray;

let mkString: (string, list(string)) => string =
  (delim, xs) => {
    let delimited = intersperse(delim, xs);
    foldLeft((acc, curr) => acc ++ curr, "", delimited);
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

module Show = BsAbstract.List.Show;

let showF: ('a => string, list('a)) => string =
  (showA, xs) => {
    let strings = map(showA, xs);
    "[" ++ mkString(", ", strings) ++ "]";
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

module FunctorFunctions = BsAbstract.Functions.Functor(Functor);

let void: list('a) => list(unit) = FunctorFunctions.void;

let flap: (list('a => 'b), 'a) => list('b) = FunctorFunctions.flap;

module Apply = BsAbstract.List.Apply;

module ApplyFunctions = BsAbstract.Functions.Apply(Apply);

/* TODO: not sure if we want to include these... these apply the function to all combinations of values (apply lift semantics), not index-by-index */
let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c) = ApplyFunctions.lift2;

let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d) = ApplyFunctions.lift3;

let map4:
  (('a, 'b, 'c, 'd) => 'e, list('a), list('b), list('c), list('d)) =>
  list('e) = ApplyFunctions.lift4;

let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    list('a),
    list('b),
    list('c),
    list('d),
    list('e)
  ) =>
  list('f) = ApplyFunctions.lift5;

module Applicative = BsAbstract.List.Applicative;

module Monad = BsAbstract.List.Monad;

module Foldable = BsAbstract.List.Foldable;

module Traversable = BsAbstract.List.Traversable;

module Infix = {
  include BsAbstract.List.Infix;
  include ApplyFunctions.Infix;
};

module Sequence: Relude_Sequence.SEQUENCE with type t('a) = list('a) = {
  type t('a) = list('a);

  let length = length;
  let isEmpty = isEmpty;
  let isNotEmpty = isNotEmpty;
  let head = head;
  let tail = tail;
  let tailOrEmpty = tailOrEmpty;
  let mkString = mkString;
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
