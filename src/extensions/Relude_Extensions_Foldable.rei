type ordering = BsAbstract.Interface.ordering;
let optionAlt: (option('a), option('a)) => option('a);
module FoldableExtensions:
  (F : BsAbstract.Interface.FOLDABLE) => {
                                           module BsFoldableExtensions:
                                             {
                                               module Semigroup:
                                                 (S : BsAbstract.Interface.SEMIGROUP) => 
                                                   {
                                                     module FM:
                                                       {
                                                         let fold_map:
                                                           ('a =>
                                                            BsAbstract.Endo.Monoid.t('a),
                                                           F.t('a)) =>
                                                           BsAbstract.Endo.Monoid.t('a);
                                                       };
                                                     module I:
                                                       {
                                                         let ( <:> ):
                                                           (S.t, S.t) => S.t;
                                                       };
                                                     let surround_map:
                                                       (~delimiter: S.t,
                                                       S.t => S.t,
                                                       F.t(S.t)) => S.t;
                                                     let surround:
                                                       (~delimiter: S.t,
                                                       F.t(S.t)) => S.t;
                                                   };
                                               module Monoid:
                                                 (M : BsAbstract.Interface.MONOID) => 
                                                   {
                                                     module FM:
                                                       {
                                                         let fold_map:
                                                           ('a => M.t,
                                                           F.t('a)) => 
                                                           M.t;
                                                       };
                                                     module I:
                                                       {
                                                         let ( <:> ):
                                                           (M.t, M.t) => M.t;
                                                       };
                                                     type acc =
                                                       BsAbstract.Functions.Foldable(F).Monoid(M).acc = {
                                                       init: bool,
                                                       acc: M.t,
                                                     };
                                                     let fold:
                                                       F.t(M.t) => M.t;
                                                     let intercalate:
                                                       (~separator: M.t,
                                                       F.t(M.t)) => M.t;
                                                   };
                                               module Applicative:
                                                 (A : BsAbstract.Interface.APPLICATIVE) => 
                                                   {
                                                     module Fn:
                                                       {
                                                         module I:
                                                           {
                                                             let ( <$> ):
                                                               ('a => 'b,
                                                               A.t('a)) =>
                                                               A.t('b);
                                                             let ( <#> ):
                                                               (A.t('a),
                                                               'a => 'b) =>
                                                               A.t('b);
                                                             let ( <*> ):
                                                               (A.t('a => 'b),
                                                               A.t('a)) =>
                                                               A.t('b);
                                                           };
                                                         let apply_first:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t('a);
                                                         let apply_second:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t('b);
                                                         let apply_both:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t(('a, 'b));
                                                         let lift2:
                                                           (('a, 'b) => 'c,
                                                           A.t('a),
                                                           A.t('b)) =>
                                                           A.t('c);
                                                         let lift3:
                                                           (('a, 'b, 'c) =>
                                                            'd,
                                                           A.t('a), A.t('b),
                                                           A.t('c)) =>
                                                           A.t('d);
                                                         let lift4:
                                                           (('a, 'b, 'c,
                                                            'd) => 'e,
                                                           A.t('a), A.t('b),
                                                           A.t('c),
                                                           A.t('d)) =>
                                                           A.t('e);
                                                         let lift5:
                                                           (('a, 'b, 'c, 'd,
                                                            'e) => 'f,
                                                           A.t('a), A.t('b),
                                                           A.t('c), A.t('d),
                                                           A.t('e)) =>
                                                           A.t('f);
                                                         module Infix:
                                                           {
                                                             let ( <* ):
                                                               (A.t('a),
                                                               A.t('b)) =>
                                                               A.t('a);
                                                             let ( *> ):
                                                               (A.t('a),
                                                               A.t('b)) =>
                                                               A.t('b);
                                                           };
                                                       };
                                                     let traverse':
                                                       ('a => A.t('b),
                                                       F.t('a)) => A.t(unit);
                                                     let sequence':
                                                       F.t(A.t('a)) =>
                                                       A.t(unit);
                                                   };
                                               module Plus:
                                                 (P : BsAbstract.Interface.PLUS) => 
                                                   {
                                                     let one_of:
                                                       F.t(P.t('a)) =>
                                                       P.t('a);
                                                   };
                                               module Monad:
                                                 (M : BsAbstract.Interface.MONAD) => 
                                                   {
                                                     module I:
                                                       {
                                                         let ( <$> ):
                                                           ('a => 'b,
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( <#> ):
                                                           (M.t('a),
                                                           'a => 'b) =>
                                                           M.t('b);
                                                         let ( <*> ):
                                                           (M.t('a => 'b),
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( >>= ):
                                                           (M.t('a),
                                                           'a => M.t('b)) =>
                                                           M.t('b);
                                                         let ( =<< ):
                                                           ('a => M.t('b),
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( >=> ):
                                                           ('a => M.t('b),
                                                           'b => M.t('c),
                                                           'a) => M.t('c);
                                                         let ( <=< ):
                                                           ('a => M.t('b),
                                                           'c => M.t('a),
                                                           'c) => M.t('b);
                                                       };
                                                     let fold_monad:
                                                       (('a, 'b) => M.t('a),
                                                       'a, F.t('b)) =>
                                                       M.t('a);
                                                   };
                                             };
                                           let foldLeft:
                                             (('a, 'b) => 'a, 'a, F.t('b)) =>
                                             'a;
                                           let foldRight:
                                             (('a, 'b) => 'b, 'b, F.t('a)) =>
                                             'b;
                                           let any:
                                             ('a => bool, F.t('a)) => bool;
                                           let all:
                                             ('a => bool, F.t('a)) => bool;
                                           let containsBy:
                                             (('a, 'a) => bool, 'a,
                                             F.t('a)) => bool;
                                           let contains:
                                             ((module BsAbstract.Interface.EQ with type t = 'a),
                                             'a, F.t('a)) => bool;
                                           let indexOfBy:
                                             (('a, 'a) => bool, 'a,
                                             F.t('a)) => option(int);
                                           let indexOf:
                                             ((module BsAbstract.Interface.EQ with type t = 'a),
                                             'a, F.t('a)) => option(int);
                                           let minBy:
                                             (('a, 'a) => ordering,
                                             F.t('a)) => option('a);
                                           let min:
                                             ((module BsAbstract.Interface.ORD with type t = 'a),
                                             F.t('a)) => option('a);
                                           let maxBy:
                                             (('a, 'a) => ordering,
                                             F.t('a)) => option('a);
                                           let max:
                                             ((module BsAbstract.Interface.ORD with type t = 'a),
                                             F.t('a)) => option('a);
                                           let countBy:
                                             ('a => bool, F.t('a)) => int;
                                           let length: F.t('a) => int;
                                           let forEach:
                                             ('a => unit, F.t('a)) => unit;
                                           let forEachWithIndex:
                                             (('a, int) => unit, F.t('a)) =>
                                             unit;
                                           let find:
                                             ('a => bool, F.t('a)) =>
                                             option('a);
                                           let findWithIndex:
                                             (('a, int) => bool, F.t('a)) =>
                                             option('a);
                                           let toList: F.t('a) => list('a);
                                           let toArray: F.t('a) => array('a);
                                           module FoldableSemigroupExtensions:
                                             (S : BsAbstract.Interface.SEMIGROUP) => 
                                               {
                                                 module BsFoldableSemigroupExtensions:
                                                   {
                                                     module FM:
                                                       {
                                                         let fold_map:
                                                           ('a =>
                                                            BsAbstract.Endo.Monoid.t('a),
                                                           F.t('a)) =>
                                                           BsAbstract.Endo.Monoid.t('a);
                                                       };
                                                     module I:
                                                       {
                                                         let ( <:> ):
                                                           (S.t, S.t) => S.t;
                                                       };
                                                     let surround_map:
                                                       (~delimiter: S.t,
                                                       S.t => S.t,
                                                       F.t(S.t)) => S.t;
                                                     let surround:
                                                       (~delimiter: S.t,
                                                       F.t(S.t)) => S.t;
                                                   };
                                                 let surroundMap:
                                                   (~delimiter: S.t,
                                                   S.t => S.t, F.t(S.t)) =>
                                                   S.t;
                                                 let surround:
                                                   (~delimiter: S.t,
                                                   F.t(S.t)) => S.t;
                                               };
                                           module FoldableMonoidExtensions:
                                             (M : BsAbstract.Interface.MONOID) => 
                                               {
                                                 module BsFoldableMonoidExtensions:
                                                   {
                                                     module FM:
                                                       {
                                                         let fold_map:
                                                           ('a => M.t,
                                                           F.t('a)) => 
                                                           M.t;
                                                       };
                                                     module I:
                                                       {
                                                         let ( <:> ):
                                                           (M.t, M.t) => M.t;
                                                       };
                                                     type acc =
                                                       BsAbstract.Functions.Foldable(F).Monoid(M).acc = {
                                                       init: bool,
                                                       acc: M.t,
                                                     };
                                                     let fold:
                                                       F.t(M.t) => M.t;
                                                     let intercalate:
                                                       (~separator: M.t,
                                                       F.t(M.t)) => M.t;
                                                   };
                                                 let foldMap:
                                                   ('a => M.t, F.t('a)) =>
                                                   M.t;
                                                 let fold: F.t(M.t) => M.t;
                                                 let intercalate:
                                                   (M.t, F.t(M.t)) => M.t;
                                               };
                                           let foldMap:
                                             ((module BsAbstract.Interface.MONOID with type t = 'a),
                                             'b => 'a, F.t('b)) => 'a;
                                           let foldMonoid:
                                             ((module BsAbstract.Interface.MONOID with type t = 'a),
                                             F.t('a)) => 'a;
                                           let intercalate:
                                             ((module BsAbstract.Interface.MONOID with type t = 'a),
                                             'a, F.t('a)) => 'a;
                                           module FoldableApplicativeExtensions:
                                             (A : BsAbstract.Interface.APPLICATIVE) => 
                                               {
                                                 module BsFoldableApplicativeExtensions:
                                                   {
                                                     module Fn:
                                                       {
                                                         module I:
                                                           {
                                                             let ( <$> ):
                                                               ('a => 'b,
                                                               A.t('a)) =>
                                                               A.t('b);
                                                             let ( <#> ):
                                                               (A.t('a),
                                                               'a => 'b) =>
                                                               A.t('b);
                                                             let ( <*> ):
                                                               (A.t('a => 'b),
                                                               A.t('a)) =>
                                                               A.t('b);
                                                           };
                                                         let apply_first:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t('a);
                                                         let apply_second:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t('b);
                                                         let apply_both:
                                                           (A.t('a),
                                                           A.t('b)) =>
                                                           A.t(('a, 'b));
                                                         let lift2:
                                                           (('a, 'b) => 'c,
                                                           A.t('a),
                                                           A.t('b)) =>
                                                           A.t('c);
                                                         let lift3:
                                                           (('a, 'b, 'c) =>
                                                            'd,
                                                           A.t('a), A.t('b),
                                                           A.t('c)) =>
                                                           A.t('d);
                                                         let lift4:
                                                           (('a, 'b, 'c,
                                                            'd) => 'e,
                                                           A.t('a), A.t('b),
                                                           A.t('c),
                                                           A.t('d)) =>
                                                           A.t('e);
                                                         let lift5:
                                                           (('a, 'b, 'c, 'd,
                                                            'e) => 'f,
                                                           A.t('a), A.t('b),
                                                           A.t('c), A.t('d),
                                                           A.t('e)) =>
                                                           A.t('f);
                                                         module Infix:
                                                           {
                                                             let ( <* ):
                                                               (A.t('a),
                                                               A.t('b)) =>
                                                               A.t('a);
                                                             let ( *> ):
                                                               (A.t('a),
                                                               A.t('b)) =>
                                                               A.t('b);
                                                           };
                                                       };
                                                     let traverse':
                                                       ('a => A.t('b),
                                                       F.t('a)) => A.t(unit);
                                                     let sequence':
                                                       F.t(A.t('a)) =>
                                                       A.t(unit);
                                                   };
                                                 let traverse_:
                                                   ('a => A.t('b),
                                                   F.t('a)) => A.t(unit);
                                                 let sequence_:
                                                   F.t(A.t('a)) => A.t(unit);
                                               };
                                           module FoldableMonadExtensions:
                                             (M : BsAbstract.Interface.MONAD) => 
                                               {
                                                 module BsFoldableMonadExtensions:
                                                   {
                                                     module I:
                                                       {
                                                         let ( <$> ):
                                                           ('a => 'b,
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( <#> ):
                                                           (M.t('a),
                                                           'a => 'b) =>
                                                           M.t('b);
                                                         let ( <*> ):
                                                           (M.t('a => 'b),
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( >>= ):
                                                           (M.t('a),
                                                           'a => M.t('b)) =>
                                                           M.t('b);
                                                         let ( =<< ):
                                                           ('a => M.t('b),
                                                           M.t('a)) =>
                                                           M.t('b);
                                                         let ( >=> ):
                                                           ('a => M.t('b),
                                                           'b => M.t('c),
                                                           'a) => M.t('c);
                                                         let ( <=< ):
                                                           ('a => M.t('b),
                                                           'c => M.t('a),
                                                           'c) => M.t('b);
                                                       };
                                                     let fold_monad:
                                                       (('a, 'b) => M.t('a),
                                                       'a, F.t('b)) =>
                                                       M.t('a);
                                                   };
                                                 let foldM:
                                                   (('acc, 'a) => M.t('acc),
                                                   'acc, F.t('a)) =>
                                                   M.t('acc);
                                               };
                                           module FoldableEqExtensions:
                                             (E : BsAbstract.Interface.EQ) => 
                                               {
                                                 let contains:
                                                   (E.t, F.t(E.t)) => bool;
                                                 let indexOf:
                                                   (E.t, F.t(E.t)) =>
                                                   option(int);
                                               };
                                           module FoldableOrdExtensions:
                                             (O : BsAbstract.Interface.ORD) => 
                                               {
                                                 let min:
                                                   F.t(O.t) => option(O.t);
                                                 let max:
                                                   F.t(O.t) => option(O.t);
                                               };
                                         };
