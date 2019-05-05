/**
 * List is a member of the following typeclasses, regardless of the inner type
 */
module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = list('a);
module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a);
module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a);
module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = list('a);
module Apply: BsAbstract.Interface.APPLY with type t('a) = list('a);
module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = list('a);
module Monad: BsAbstract.Interface.MONAD with type t('a) = list('a);
module Alt: BsAbstract.Interface.ALT with type t('a) = list('a);
module Plus: BsAbstract.Interface.PLUS with type t('a) = list('a);
module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = list('a);
module IsoArray: Relude_IsoArray.ISO_ARRAY with type t('a) = list('a);

/**
 * The following functions come from List's membership in Semigroup and Monoid
 */
let concat: (list('a), list('a)) => list('a);
let empty: list('a);

/**
 * The following functions come for free because List is a member of Functor,
 * Apply, Applicative, and Monad
 */
let map: ('a => 'b, list('a)) => list('b);
let void: list('a) => list(unit);
let apply: (list('a => 'b), list('a)) => list('b);
let flap: (list('a => 'b), 'a) => list('b);
let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c);
let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d);
let map4:
  (('a, 'b, 'c, 'd) => 'e, list('a), list('b), list('c), list('d)) =>
  list('e);
let map5:
  (
    ('a, 'b, 'c, 'd, 'e) => 'f,
    list('a),
    list('b),
    list('c),
    list('d),
    list('e)
  ) =>
  list('f);
let pure: 'a => list('a);
let bind: (list('a), 'a => list('b)) => list('b);
let flatMap: ('a => list('b), list('a)) => list('b);
let flatten: list(list('a)) => list('a);

/**
 * The following functions come for free because List is a member of Foldable
 */
let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b;
let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b;
let any: ('a => bool, list('a)) => bool;
let all: ('a => bool, list('a)) => bool;
let containsBy: (('a, 'a) => bool, 'a, list('a)) => bool;
let indexOfBy: (('a, 'a) => bool, 'a, list('a)) => option(int);
let minBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => option('a);
let maxBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => option('a);
let countBy: ('a => bool, list('a)) => int;
let length: list('a) => int;
let forEach: ('a => unit, list('a)) => unit;
let forEachWithIndex: (('a, int) => unit, list('a)) => unit;
let find: ('a => bool, list('a)) => option('a);
let findWithIndex: (('a, int) => bool, list('a)) => option('a);
let fold:
  ((module BsAbstract.Interface.MONOID with type t = 'a), list('a)) => 'a;
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a, list('a)) => 'a;
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) => bool;
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  option(int);
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) =>
  option('a);

/**
 * The following functions come from List's membership in IsoArray
 */
let fromArray: array('a) => list('a);
let toArray: list('a) => array('a);

/**
 * The following functions and modules care about properties of the inner type
 */

module Traversable:
  (BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE;

let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b);
let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b);

let eqBy: (('a, 'a) => bool, list('a), list('a)) => bool;
module Eq: (BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a), list('a)) =>
  bool;

let showBy: ('a => string, list('a)) => string;
module Show: (BsAbstract.Interface.SHOW) => BsAbstract.Interface.SHOW;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), list('a)) => string;

/**
 * The following functions are list-specific. They may use
 */
let mapWithIndex: (('a, int) => 'b, list('a)) => list('b);
let cons: ('a, list('a)) => list('a);
let prepend: ('a, list('a)) => list('a);
let uncons: list('a) => option(('a, list('a)));
let append: ('a, list('a)) => list('a);
let repeat: (int, 'a) => list('a);
let makeWithIndex: (int, int => 'a) => list('a);
let reverse: list('a) => list('a);
let shuffle: list('a) => list('a);
let isEmpty: list('a) => bool;
let isNotEmpty: list('a) => bool;
let at: (int, list('a)) => option('a);
let head: list('a) => option('a);
let tail: list('a) => option(list('a));
let tailOrEmpty: list('a) => list('a);
let init: list('a) => option(list('a));
let last: list('a) => option('a);
let take: (int, list('a)) => list('a);
let takeExactly: (int, list('a)) => option(list('a));
let takeWhile: ('a => bool, list('a)) => list('a);
let drop: (int, list('a)) => list('a);
let dropExactly: (int, list('a)) => option(list('a));
let dropWhile: ('a => bool, list('a)) => list('a);
let filter: ('a => bool, list('a)) => list('a);
let filterWithIndex: (('a, int) => bool, list('a)) => list('a);
let mapOption: ('a => option('b), list('a)) => list('b);
let catOptions: list(option('a)) => list('a);
let partition: ('a => bool, list('a)) => (list('a), list('a));
let splitAt: (int, list('a)) => option((list('a), list('a)));
let prependToAll: ('a, list('a)) => list('a);
let intersperse: ('a, list('a)) => list('a);
let replicate: (int, list('a)) => list('a);
let zip: (list('a), list('b)) => list(('a, 'b));
let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c);
let zipWithIndex: list('a) => list(('a, int));
let unzip: list(('a, 'b)) => (list('a), list('b));
let sortWithInt: (('a, 'a) => int, list('a)) => list('a);
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) => list('a);
let distinctBy: (('a, 'a) => bool, list('a)) => list('a);
let removeFirstBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let removeEachBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a)) => list('a);
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);

module String: {
  let contains: (string, list(string)) => bool;
  let indexOf: (string, list(string)) => option(int);
  let distinct: list(string) => list(string);
  let removeFirst: (string, list(string)) => list(string);
  let removeEach: (string, list(string)) => list(string);
  let eq: (list(string), list(string)) => bool;
  let min: list(string) => option(string);
  let max: list(string) => option(string);
  let sort: list(string) => list(string);
  let fold: list(string) => string;
  let join: list(string) => string;
  let intercalate: (string, list(string)) => string;
  let joinWith: (string, list(string)) => string;
};

module Int: {
  let contains: (int, list(int)) => bool;
  let indexOf: (int, list(int)) => option(int);
  let distinct: list(int) => list(int);
  let removeFirst: (int, list(int)) => list(int);
  let removeEach: (int, list(int)) => list(int);
  let eq: (list(int), list(int)) => bool;
  let min: list(int) => option(int);
  let max: list(int) => option(int);
  let sort: list(int) => list(int);
  let sum: list(int) => int;
  let product: list(int) => int;
};

module Float: {
  let contains: (float, list(float)) => bool;
  let indexOf: (float, list(float)) => option(int);
  let distinct: list(float) => list(float);
  let removeFirst: (float, list(float)) => list(float);
  let removeEach: (float, list(float)) => list(float);
  let eq: (list(float), list(float)) => bool;
  let min: list(float) => option(float);
  let max: list(float) => option(float);
  let sort: list(float) => list(float);
  let sum: list(float) => float;
  let product: list(float) => float;
};

module Option: {
  let traverse: ('a => option('a), list('a)) => option(list('a));
  let sequence: list(option('a)) => option(list('a));
};

module Result: {
  let traverse:
    ('a => Belt.Result.t('b, 'c), list('a)) => Belt.Result.t(list('b), 'c);

  let sequence: list(Belt.Result.t('a, 'c)) => Belt.Result.t(list('a), 'c);
};

module Validation: {
  module Traversable:
    (
      Errors: BsAbstract.Interface.SEMIGROUP_ANY,
      Error: BsAbstract.Interface.TYPE,
    ) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.Applicative(Errors)(Error).t('a);
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  module TraversableWithErrorsAsList:
    (Error: BsAbstract.Interface.TYPE) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.Applicative(Relude_List_Types.SemigroupAny)(Error).t(
          'a,
        );
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  module TraversableWithErrorsAsListOfStrings: {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
    let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
    module Fold_Map:
      (M: BsAbstract.Interface.MONOID) =>
       {let fold_map: ('a => M.t, t('a)) => M.t;};
    module Fold_Map_Any:
      (M: BsAbstract.Interface.MONOID_ANY) =>
       {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
    module Fold_Map_Plus:
      (P: BsAbstract.Interface.PLUS) =>
       {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
    type applicative_t('a) =
      Relude_Validation.t('a, Relude_List_Types.SemigroupAny.t(string));
    let traverse: ('a => applicative_t('b), t('a)) => applicative_t(t('b));
    let sequence: t(applicative_t('a)) => applicative_t(t('a));
  };

  module TraversableWithErrorsAsNonEmptyList:
    (Error: BsAbstract.Interface.TYPE) =>
     {
      type t('a) = list('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: BsAbstract.Interface.MONOID) =>
         {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: BsAbstract.Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('a), t('a)) => M.t('a);};
      module Fold_Map_Plus:
        (P: BsAbstract.Interface.PLUS) =>
         {let fold_map: ('a => P.t('a), t('a)) => P.t('a);};
      type applicative_t('a) =
        Relude_Validation.Applicative(Relude_NonEmpty.List.SemigroupAny)(Error).t(
          'a,
        );
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };

  let traverse:
    ('a => Belt.Result.t('b, 'e), list('a)) =>
    Relude_Validation.t(list('b), Relude_NonEmpty.List.t('e));
};

module Infix: {
  let (>>=): (list('a), 'a => list('b)) => list('b);
  let (=<<): ('a => list('b), list('a)) => list('b);
  let (>=>): ('a => list('b), 'b => list('c), 'a) => list('c);
  let (<=<): ('a => list('b), 'c => list('a), 'c) => list('b);
  let (<|>): (list('a), list('a)) => list('a);
  let (<$>): ('a => 'b, list('a)) => list('b);
  let (<#>): (list('a), 'a => 'b) => list('b);
  let (<*>): (list('a => 'b), list('a)) => list('b);
  let ( <* ): (list('a), list('b)) => list('a);
  let ( *> ): (list('a), list('b)) => list('b);
};