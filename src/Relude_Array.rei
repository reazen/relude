module Foldable:
  {
    type t('a) = array('a);
    let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
    let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
    module Fold_Map:
      (M : BsAbstract.Interface.MONOID) => {
                                             let fold_map:
                                               ('a => M.t, t('a)) => M.t;
                                           };
    module Fold_Map_Any:
      (M : BsAbstract.Interface.MONOID_ANY) => {
                                                 let fold_map:
                                                   ('a => M.t('a), t('a)) =>
                                                   M.t('a);
                                               };
    module Fold_Map_Plus:
      (P : BsAbstract.Interface.PLUS) => {
                                           let fold_map:
                                             ('a => P.t('a), t('a)) =>
                                             P.t('a);
                                         };
  };
module SemigroupAny:
  { type t('a) = array('a); let append: (t('a), t('a)) => t('a); };
module MonoidAny:
  {
    type t('a) = array('a);
    let append: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Functor:
  { type t('a) = array('a); let map: ('a => 'b, t('a)) => t('b); };
module Apply:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
  };
module Applicative:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
  };
module Monad:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
  };
module Alt:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
  };
module Plus:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Alternative:
  {
    type t('a) = array('a);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Invariant:
  { type t('a) = array('a); let imap: ('a => 'b, 'b => 'a, t('a)) => t('b); };
module MonadZero:
  {
    type t('a) = array('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module MonadPlus:
  {
    type t('a) = array('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Extend:
  {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let extend: (t('a) => 'b, t('a)) => t('b);
  };
module IsoList:
  {
    type t('a) = array('a);
    let fromList: list('a) => t('a);
    let toList: t('a) => list('a);
  };
let concat: (array('a), array('a)) => array('a);
let empty: array('a);
let map: ('a => 'b, array('a)) => array('b);
let void: array('a) => array(unit);
let apply: (array('a => 'b), array('a)) => array('b);
let flap: (array('a => 'b), 'a) => array('b);
let map2: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let map3: (('a, 'b, 'c) => 'd, array('a), array('b), array('c)) => array('d);
let map4:
  (('a, 'b, 'c, 'd) => 'e, array('a), array('b), array('c), array('d)) =>
  array('e);
let map5:
  (('a, 'b, 'c, 'd, 'e) => 'f, array('a), array('b), array('c), array('d),
  array('e)) => array('f);
let pure: 'a => array('a);
let bind: (array('a), 'a => array('b)) => array('b);
let flatMap: ('a => array('b), array('a)) => array('b);
let flatten: array(array('a)) => array('a);
let foldLeft: (('b, 'a) => 'b, 'b, array('a)) => 'b;
let foldRight: (('a, 'b) => 'b, 'b, array('a)) => 'b;
let any: ('a => bool, array('a)) => bool;
let all: ('a => bool, array('a)) => bool;
let containsBy: (('a, 'a) => bool, 'a, array('a)) => bool;
let indexOfBy: (('a, 'a) => bool, 'a, array('a)) => option(int);
let minBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => option('a);
let maxBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => option('a);
let countBy: ('a => bool, array('a)) => int;
let length: array('a) => int;
let forEach: ('a => unit, array('a)) => unit;
let forEachWithIndex: (('a, int) => unit, array('a)) => unit;
let find: ('a => bool, array('a)) => option('a);
let findWithIndex: (('a, int) => bool, array('a)) => option('a);
let foldMonoid:
  ((module BsAbstract.Interface.MONOID with type t = 'a), array('a)) => 'a;
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a, array('a)) =>
  'a;
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) => bool;
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  option(int);
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  option('a);
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  option('a);
let fromList: list('a) => array('a);
let toList: array('a) => list('a);
module Traversable:
  (_ : BsAbstract.Interface.APPLICATIVE) => BsAbstract.Interface.TRAVERSABLE;
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b);
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b);
let eqBy: (('a, 'a) => bool, array('a), array('a)) => bool;
module Eq: (_ : BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a),
  array('a)) => bool;
let showBy: ('a => string, array('a)) => string;
module Show: (_ : BsAbstract.Interface.SHOW) => BsAbstract.Interface.SHOW;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), array('a)) => string;
module Ord: (_ : BsAbstract.Interface.ORD) => BsAbstract.Interface.ORD;
let mapWithIndex: (('a, int) => 'b, array('a)) => array('b);
let cons: ('a, array('a)) => array('a);
let prepend: ('a, array('a)) => array('a);
let uncons: array('a) => option(('a, array('a)));
let append: ('a, array('a)) => array('a);
let repeat: (int, 'a) => array('a);
let makeWithIndex: (int, int => 'a) => array('a);
let reverse: array('a) => array('a);
let shuffleInPlace: array('a) => array('a);
let shuffle: array('a) => array('a);
let isEmpty: array('a) => bool;
let isNotEmpty: array('a) => bool;
let at: (int, array('a)) => option('a);
let setAt: (int, 'a, array('a)) => option(array('a));
let head: array('a) => option('a);
let tail: array('a) => option(array('a));
let tailOrEmpty: array('a) => array('a);
let init: array('a) => option(array('a));
let last: array('a) => option('a);
let take: (int, array('a)) => array('a);
let takeExactly: (int, array('a)) => option(array('a));
let takeWhile: ('a => bool, array('a)) => array('a);
let drop: (int, array('a)) => array('a);
let dropExactly: (int, array('a)) => option(array('a));
let dropWhile: ('a => bool, array('a)) => array('a);
let filter: ('a => bool, array('a)) => array('a);
let filterWithIndex: (('a, int) => bool, array('a)) => array('a);
let filterNot: ('a => bool, array('a)) => array('a);
let filterNotWithIndex: (('a, int) => bool, array('a)) => array('a);
let partition: ('a => bool, array('a)) => (array('a), array('a));
let splitAt: (int, array('a)) => option((array('a), array('a)));
let prependToAll: ('a, array('a)) => array('a);
let intersperse: ('a, array('a)) => array('a);
let replicate: (int, array('a)) => array('a);
let zip: (array('a), array('b)) => array(('a, 'b));
let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let zipWithIndex: array('a) => array(('a, int));
let unzip: array(('a, 'b)) => (array('a), array('b));
let sortWithInt: (('a, 'a) => int, array('a)) => array('a);
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  array('a);
let distinctBy: (('a, 'a) => bool, array('a)) => array('a);
let removeFirstBy: (('a, 'a) => bool, 'a, array('a)) => array('a);
let removeEachBy: (('a, 'a) => bool, 'a, array('a)) => array('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a)) => array('a);
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);
module String:
  {
    let contains: (string, array(string)) => bool;
    let indexOf: (string, array(string)) => option(int);
    let distinct: array(string) => array(string);
    let removeFirst: (string, array(string)) => array(string);
    let removeEach: (string, array(string)) => array(string);
    let eq: (array(string), array(string)) => bool;
    let min: array(string) => option(string);
    let max: array(string) => option(string);
    let sort: array(string) => array(string);
    let fold: array(string) => string;
    let join: array(string) => string;
    let intercalate: (string, array(string)) => string;
    let joinWith: (string, array(string)) => string;
  };
module Int:
  {
    let contains: (int, array(int)) => bool;
    let indexOf: (int, array(int)) => option(int);
    let distinct: array(int) => array(int);
    let removeFirst: (int, array(int)) => array(int);
    let removeEach: (int, array(int)) => array(int);
    let eq: (array(int), array(int)) => bool;
    let min: array(int) => option(int);
    let max: array(int) => option(int);
    let sort: array(int) => array(int);
    let sum: array(int) => int;
    let product: array(int) => int;
  };
module Float:
  {
    let contains: (float, array(float)) => bool;
    let indexOf: (float, array(float)) => option(int);
    let distinct: array(float) => array(float);
    let removeFirst: (float, array(float)) => array(float);
    let removeEach: (float, array(float)) => array(float);
    let eq: (array(float), array(float)) => bool;
    let min: array(float) => option(float);
    let max: array(float) => option(float);
    let sort: array(float) => array(float);
    let sum: array(float) => float;
    let product: array(float) => float;
  };
module Option:
  {
    let traverse: ('a => option('a), array('a)) => option(array('a));
    let sequence: array(option('a)) => option(array('a));
  };
module Result:
  {
    let traverse:
      ('a => Belt.Result.t('b, 'c), array('a)) =>
      Belt.Result.t(array('b), 'c);
    let sequence:
      array(Belt.Result.t('a, 'c)) => Belt.Result.t(array('a), 'c);
  };
module Validation:
  {
    module WithErrors:
      (Errors : BsAbstract.Interface.SEMIGROUP_ANY) => (Error : BsAbstract.Interface.TYPE) => 
        {
          module Traversable:
            {
              type t('a) = array('a);
              let map: ('a => 'b, t('a)) => t('b);
              let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
              let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
              module Fold_Map:
                (M : BsAbstract.Interface.MONOID) => {
                                                       let fold_map:
                                                         ('a => M.t,
                                                         t('a)) => M.t;
                                                     };
              module Fold_Map_Any:
                (M : BsAbstract.Interface.MONOID_ANY) => {
                                                           let fold_map:
                                                             ('a => M.t('a),
                                                             t('a)) =>
                                                             M.t('a);
                                                         };
              module Fold_Map_Plus:
                (P : BsAbstract.Interface.PLUS) => {
                                                     let fold_map:
                                                       ('a => P.t('a),
                                                       t('a)) => P.t('a);
                                                   };
              type applicative_t('a) =
                  Relude_Validation.WithErrors(Errors)(Error).Applicative.t('a);
              let traverse:
                ('a => applicative_t('b), t('a)) => applicative_t(t('b));
              let sequence: t(applicative_t('a)) => applicative_t(t('a));
            };
        };
    module WithErrorsAsArray:
      (Error : BsAbstract.Interface.TYPE) => {
                                               module Traversable:
                                                 {
                                                   type t('a) = array('a);
                                                   let map:
                                                     ('a => 'b, t('a)) =>
                                                     t('b);
                                                   let fold_left:
                                                     (('a, 'b) => 'a, 'a,
                                                     t('b)) => 'a;
                                                   let fold_right:
                                                     (('b, 'a) => 'a, 'a,
                                                     t('b)) => 'a;
                                                   module Fold_Map:
                                                     (M : BsAbstract.Interface.MONOID) => 
                                                       {
                                                         let fold_map:
                                                           ('a => M.t,
                                                           t('a)) => 
                                                           M.t;
                                                       };
                                                   module Fold_Map_Any:
                                                     (M : BsAbstract.Interface.MONOID_ANY) => 
                                                       {
                                                         let fold_map:
                                                           ('a => M.t('a),
                                                           t('a)) => 
                                                           M.t('a);
                                                       };
                                                   module Fold_Map_Plus:
                                                     (P : BsAbstract.Interface.PLUS) => 
                                                       {
                                                         let fold_map:
                                                           ('a => P.t('a),
                                                           t('a)) => 
                                                           P.t('a);
                                                       };
                                                   type applicative_t('a) =
                                                       Relude_Validation.WithErrors(Relude_Array_Instances.SemigroupAny)(Error).Applicative.t('a);
                                                   let traverse:
                                                     ('a => applicative_t('b),
                                                     t('a)) =>
                                                     applicative_t(t('b));
                                                   let sequence:
                                                     t(applicative_t('a)) =>
                                                     applicative_t(t('a));
                                                 };
                                             };
    module WithErrorsAsArrayOfStrings:
      {
        module Traversable:
          {
            type t('a) = array('a);
            let map: ('a => 'b, t('a)) => t('b);
            let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
            let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
            module Fold_Map:
              (M : BsAbstract.Interface.MONOID) => {
                                                     let fold_map:
                                                       ('a => M.t, t('a)) =>
                                                       M.t;
                                                   };
            module Fold_Map_Any:
              (M : BsAbstract.Interface.MONOID_ANY) => {
                                                         let fold_map:
                                                           ('a => M.t('a),
                                                           t('a)) => 
                                                           M.t('a);
                                                       };
            module Fold_Map_Plus:
              (P : BsAbstract.Interface.PLUS) => {
                                                   let fold_map:
                                                     ('a => P.t('a),
                                                     t('a)) => P.t('a);
                                                 };
            type applicative_t('a) =
                Relude_Validation.t('a,
                                     Relude_Array_Instances.SemigroupAny.t(
                                     string));
            let traverse:
              ('a => applicative_t('b), t('a)) => applicative_t(t('b));
            let sequence: t(applicative_t('a)) => applicative_t(t('a));
          };
      };
    module WithErrorsAsNonEmptyArray:
      (Error : BsAbstract.Interface.TYPE) => {
                                               module Traversable:
                                                 {
                                                   type t('a) = array('a);
                                                   let map:
                                                     ('a => 'b, t('a)) =>
                                                     t('b);
                                                   let fold_left:
                                                     (('a, 'b) => 'a, 'a,
                                                     t('b)) => 'a;
                                                   let fold_right:
                                                     (('b, 'a) => 'a, 'a,
                                                     t('b)) => 'a;
                                                   module Fold_Map:
                                                     (M : BsAbstract.Interface.MONOID) => 
                                                       {
                                                         let fold_map:
                                                           ('a => M.t,
                                                           t('a)) => 
                                                           M.t;
                                                       };
                                                   module Fold_Map_Any:
                                                     (M : BsAbstract.Interface.MONOID_ANY) => 
                                                       {
                                                         let fold_map:
                                                           ('a => M.t('a),
                                                           t('a)) => 
                                                           M.t('a);
                                                       };
                                                   module Fold_Map_Plus:
                                                     (P : BsAbstract.Interface.PLUS) => 
                                                       {
                                                         let fold_map:
                                                           ('a => P.t('a),
                                                           t('a)) => 
                                                           P.t('a);
                                                       };
                                                   type applicative_t('a) =
                                                       Relude_Validation.WithErrors(Relude_NonEmpty.Array.SemigroupAny)(Error).Applicative.t('a);
                                                   let traverse:
                                                     ('a => applicative_t('b),
                                                     t('a)) =>
                                                     applicative_t(t('b));
                                                   let sequence:
                                                     t(applicative_t('a)) =>
                                                     applicative_t(t('a));
                                                 };
                                             };
    let traverse:
      ('a => Belt.Result.t('b, 'e), array('a)) =>
      Relude_Validation.t(array('b), Relude_NonEmpty.Array.t('e));
  };
module Infix:
  {
    let ( >>= ): (array('a), 'a => array('b)) => array('b);
    let ( =<< ): ('a => array('b), array('a)) => array('b);
    let ( >=> ): ('a => array('b), 'b => array('c), 'a) => array('c);
    let ( <=< ): ('a => array('b), 'c => array('a), 'c) => array('b);
    let ( <|> ): (array('a), array('a)) => array('a);
    let ( <$> ): ('a => 'b, array('a)) => array('b);
    let ( <#> ): (array('a), 'a => 'b) => array('b);
    let ( <*> ): (array('a => 'b), array('a)) => array('b);
  };
