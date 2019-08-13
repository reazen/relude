/**
 * Type of an empty HList
 */
type nil = unit;

/**
 * Type of a non-empty HList
 */
type cons('h, 't) = 'h => 't;

/**
 * HList.t is a heterogenous list type which can store a list of differently-typed values
 * while retaining full type safety.
 *
 * TODO: originally this was using the special constructors [] and ::, but these seem to constantly
 * conflict with the list versions in Pervasives.  What a turd.
 */
type t('l) =
  //| []: t(nil)
  //| ::('h, t('t)) : t(cons('h, 't));
  | HNil: t(nil)
  | HCons('h, t('t)): t(cons('h, 't));

/**
 * Returns the empty HList
 */
let empty: t(nil) = HNil;

/**
 * Lifts a pure value into a singleton HLlist
 */
let pure: 'a => t(cons('a, unit)) = a => HCons(a, HNil);

/**
 * Creates an HList from head and tail parts.
 */
let cons: ('h, t('t)) => t(cons('h, 't)) = (h, t) => HCons(h, t);

/**
 * Splits an HList into head and tail parts.
 */
let uncons: t(cons('h, 't)) => ('h, t('t)) =
  fun
  | HCons(h, t) => (h, t);

/**
 * Returns the first element in a HList of 1 or more elements.
 */
let head: t(cons('h, _)) => 'h =
  fun
  | HCons(h, _) => h;

/**
 * Returns the tail of an HList of 1 or more elements.
 */
let tail: t(cons(_, 't)) => t('t) =
  fun
  | HCons(_, t) => t;

/**
 * Returns the second element of an HList of at least 2 elements
 */
let second: t(cons(_, cons('b, _))) => 'b =
  fun
  | HCons(_, HCons(x, _)) => x;

/**
 * Returns the third element of an HList of at least 3 elements
 */
let third: t(cons(_, cons(_, cons('c, _)))) => 'c =
  fun
  | HCons(_, HCons(_, HCons(x, _))) => x;

/**
 * Returns the fourth element of an HList of at least 4 elements
 */
let fourth: t(cons(_, cons(_, cons(_, cons('d, _))))) => 'd =
  fun
  | HCons(_, HCons(_, HCons(_, HCons(x, _)))) => x;

/**
 * Returns the fifth element of an HList of at least 5 elements
 */
let fifth: t(cons(_, cons(_, cons(_, cons(_, cons('e, _)))))) => 'e =
  fun
  | HCons(_, HCons(_, HCons(_, HCons(_, HCons(x, _))))) => x;

/**
 * Creates an HList from a 2-tuple
 */
let fromTuple2: (('a, 'b)) => t(cons('a, cons('b, nil))) =
  ((a, b)) => HCons(a, HCons(b, HNil));

/**
 * Creates an HList from a 3-tuple
 */
let fromTuple3: (('a, 'b, 'c)) => t(cons('a, cons('b, cons('c, nil)))) =
  ((a, b, c)) => HCons(a, HCons(b, HCons(c, HNil)));

/**
 * Creates an HList from a 4-tuple
 */
let fromTuple4:
  (('a, 'b, 'c, 'd)) => t(cons('a, cons('b, cons('c, cons('d, nil))))) =
  ((a, b, c, d)) => HCons(a, HCons(b, HCons(c, HCons(d, HNil))));

/**
 * Creates an HList from a 5-tuple
 */
let fromTuple5:
  (('a, 'b, 'c, 'd, 'e)) =>
  t(cons('a, cons('b, cons('c, cons('d, cons('e, nil)))))) =
  ((a, b, c, d, e)) =>
    HCons(a, HCons(b, HCons(c, HCons(d, HCons(e, HNil)))));

/**
 * Converts an HList of 2 elements to a tuple
 */
let toTuple2: t(cons('a, cons('b, nil))) => ('a, 'b) =
  (HCons(a, HCons(b, HNil))) => (a, b);

/**
 * Converts an HList of 3 elements to a tuple
 */
let toTuple3: t(cons('a, cons('b, cons('c, nil)))) => ('a, 'b, 'c) =
  (HCons(a, HCons(b, HCons(c, HNil)))) => (a, b, c);

/**
 * Converts an HList of 4 elements to a tuple
 */
let toTuple4:
  t(cons('a, cons('b, cons('c, cons('d, nil))))) => ('a, 'b, 'c, 'd) =
  (HCons(a, HCons(b, HCons(c, HCons(d, HNil))))) => (a, b, c, d);

/**
 * Converts an HList of 4 elements to a tuple
 */
let toTuple5:
  t(cons('a, cons('b, cons('c, cons('d, cons('e, nil)))))) =>
  ('a, 'b, 'c, 'd, 'e) =
  (HCons(a, HCons(b, HCons(c, HCons(d, HCons(e, HNil)))))) => (
    a,
    b,
    c,
    d,
    e,
  );