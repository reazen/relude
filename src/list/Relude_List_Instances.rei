module SemigroupAny:
  { type t('a) = list('a); let append: (t('a), t('a)) => t('a); };
let concat: (SemigroupAny.t('a), SemigroupAny.t('a)) => SemigroupAny.t('a);
module MonoidAny:
  {
    type t('a) = list('a);
    let append: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
let empty: MonoidAny.t('a);
module Functor:
  { type t('a) = list('a); let map: ('a => 'b, t('a)) => t('b); };
module BsFunctorExtensions:
  {
    let void: Functor.t('a) => Functor.t(unit);
    let void_right: ('a, Functor.t('b)) => Functor.t('a);
    let void_left: (Functor.t('a), 'b) => Functor.t('b);
    let flap: (Functor.t('a => 'b), 'a) => Functor.t('b);
  };
let map: ('a => 'b, Functor.t('a)) => Functor.t('b);
let flipMap: (Functor.t('a), 'a => 'b) => Functor.t('b);
let void: Functor.t('a) => Functor.t(unit);
let voidRight: ('a, Functor.t('b)) => Functor.t('a);
let voidLeft: (Functor.t('a), 'b) => Functor.t('b);
let flap: (Functor.t('a => 'b), 'a) => Functor.t('b);
module Apply:
  {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
  };
module BsApplyExtensions:
  {
    module I:
      {
        let ( <$> ): ('a => 'b, Apply.t('a)) => Apply.t('b);
        let ( <#> ): (Apply.t('a), 'a => 'b) => Apply.t('b);
        let ( <*> ): (Apply.t('a => 'b), Apply.t('a)) => Apply.t('b);
      };
    let apply_first: (Apply.t('a), Apply.t('b)) => Apply.t('a);
    let apply_second: (Apply.t('a), Apply.t('b)) => Apply.t('b);
    let apply_both: (Apply.t('a), Apply.t('b)) => Apply.t(('a, 'b));
    let lift2: (('a, 'b) => 'c, Apply.t('a), Apply.t('b)) => Apply.t('c);
    let lift3:
      (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b), Apply.t('c)) =>
      Apply.t('d);
    let lift4:
      (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b), Apply.t('c),
      Apply.t('d)) => Apply.t('e);
    let lift5:
      (('a, 'b, 'c, 'd, 'e) => 'f, Apply.t('a), Apply.t('b), Apply.t('c),
      Apply.t('d), Apply.t('e)) => Apply.t('f);
    module Infix:
      {
        let ( <* ): (Apply.t('a), Apply.t('b)) => Apply.t('a);
        let ( *> ): (Apply.t('a), Apply.t('b)) => Apply.t('b);
      };
  };
let apply: (Apply.t('a => 'b), Apply.t('a)) => Apply.t('b);
let applyFirst: (Apply.t('a), Apply.t('b)) => Apply.t('a);
let applySecond: (Apply.t('a), Apply.t('b)) => Apply.t('b);
let map2: (('a, 'b) => 'c, Apply.t('a), Apply.t('b)) => Apply.t('c);
let map3:
  (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b), Apply.t('c)) => Apply.t('d);
let map4:
  (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b), Apply.t('c),
  Apply.t('d)) => Apply.t('e);
let map5:
  (('a, 'b, 'c, 'd, 'e) => 'f, Apply.t('a), Apply.t('b), Apply.t('c),
  Apply.t('d), Apply.t('e)) => Apply.t('f);
let tuple2: (Apply.t('a), Apply.t('b)) => Apply.t(('a, 'b));
let tuple3: (Apply.t('a), Apply.t('b), Apply.t('c)) => Apply.t(('a, 'b, 'c));
let tuple4:
  (Apply.t('a), Apply.t('b), Apply.t('c), Apply.t('d)) =>
  Apply.t(('a, 'b, 'c, 'd));
let tuple5:
  (Apply.t('a), Apply.t('b), Apply.t('c), Apply.t('d), Apply.t('e)) =>
  Apply.t(('a, 'b, 'c, 'd, 'e));
module Applicative:
  {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
  };
module BsApplicativeExtensions:
  {
    module I:
      {
        let ( <$> ): ('a => 'b, Applicative.t('a)) => Applicative.t('b);
        let ( <#> ): (Applicative.t('a), 'a => 'b) => Applicative.t('b);
        let ( <*> ):
          (Applicative.t('a => 'b), Applicative.t('a)) => Applicative.t('b);
      };
    let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
    let when_: (bool, Applicative.t(unit)) => Applicative.t(unit);
    let unless: (bool, Applicative.t(unit)) => Applicative.t(unit);
  };
let pure: 'a => Applicative.t('a);
let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
module Monad:
  {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
  };
module BsMonadExtensions:
  {
    module I:
      {
        let ( <$> ): ('a => 'b, Monad.t('a)) => Monad.t('b);
        let ( <#> ): (Monad.t('a), 'a => 'b) => Monad.t('b);
        let ( <*> ): (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
        let ( >>= ): (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
        let ( =<< ): ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
        let ( >=> ):
          ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
        let ( <=< ):
          ('a => Monad.t('b), 'c => Monad.t('a), 'c) => Monad.t('b);
      };
    module A:
      {
        module I:
          {
            let ( <$> ): ('a => 'b, Monad.t('a)) => Monad.t('b);
            let ( <#> ): (Monad.t('a), 'a => 'b) => Monad.t('b);
            let ( <*> ): (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
          };
        let liftA1: ('a => 'b, Monad.t('a)) => Monad.t('b);
        let when_: (bool, Monad.t(unit)) => Monad.t(unit);
        let unless: (bool, Monad.t(unit)) => Monad.t(unit);
      };
    let flatten: Monad.t(Monad.t('a)) => Monad.t('a);
    let compose_kliesli:
      ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
    let compose_kliesli_flipped:
      ('b => Monad.t('c), 'a => Monad.t('b), 'a) => Monad.t('c);
    let if_m: (Monad.t(bool), Monad.t('a), Monad.t('a)) => Monad.t('a);
    let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
    let ap: (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
    let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
    let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
  };
let bind: (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
let flatMap: ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
let flatten: Monad.t(Monad.t('a)) => Monad.t('a);
let composeKleisli: ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
let flipComposeKleisli:
  ('b => Monad.t('c), 'a => Monad.t('b), 'a) => Monad.t('c);
let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
module Alt:
  {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
  };
let alt: (Alt.t('a), Alt.t('a)) => Alt.t('a);
module Plus:
  {
    type t('a) = list('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Alternative:
  {
    type t('a) = list('a);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
    let empty: t('a);
  };
module Foldable:
  {
    type t('a) = list('a);
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
module FoldableExtensions:
  {
    module BsFoldableExtensions:
      {
        module Semigroup:
          (S : BsAbstract.Interface.SEMIGROUP) => {
                                                    module FM:
                                                      {
                                                        let fold_map:
                                                          ('a =>
                                                           BsAbstract.Endo.Monoid.t('a),
                                                          Foldable.t('a)) =>
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
                                                      Foldable.t(S.t)) => 
                                                      S.t;
                                                    let surround:
                                                      (~delimiter: S.t,
                                                      Foldable.t(S.t)) => 
                                                      S.t;
                                                  };
        module Monoid:
          (M : BsAbstract.Interface.MONOID) => {
                                                 module FM:
                                                   {
                                                     let fold_map:
                                                       ('a => M.t,
                                                       Foldable.t('a)) => 
                                                       M.t;
                                                   };
                                                 module I:
                                                   {
                                                     let ( <:> ):
                                                       (M.t, M.t) => M.t;
                                                   };
                                                 type acc =
                                                   BsAbstract.Functions.Foldable(Foldable).Monoid(M).acc = {
                                                   init: bool,
                                                   acc: M.t,
                                                 };
                                                 let fold:
                                                   Foldable.t(M.t) => M.t;
                                                 let intercalate:
                                                   (~separator: M.t,
                                                   Foldable.t(M.t)) => 
                                                   M.t;
                                               };
        module Applicative:
          (A : BsAbstract.Interface.APPLICATIVE) => {
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
                                                                (A.t(
                                                                 'a => 'b),
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
                                                            A.t('a), 
                                                            A.t('b),
                                                            A.t('c)) =>
                                                            A.t('d);
                                                          let lift4:
                                                            (('a, 'b, 'c,
                                                             'd) => 'e,
                                                            A.t('a), 
                                                            A.t('b), 
                                                            A.t('c),
                                                            A.t('d)) =>
                                                            A.t('e);
                                                          let lift5:
                                                            (('a, 'b, 'c, 'd,
                                                             'e) => 'f,
                                                            A.t('a), 
                                                            A.t('b), 
                                                            A.t('c), 
                                                            A.t('d),
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
                                                        Foldable.t('a)) =>
                                                        A.t(unit);
                                                      let sequence':
                                                        Foldable.t(A.t('a)) =>
                                                        A.t(unit);
                                                    };
        module Plus:
          (P : BsAbstract.Interface.PLUS) => {
                                               let one_of:
                                                 Foldable.t(P.t('a)) =>
                                                 P.t('a);
                                             };
        module Monad:
          (M : BsAbstract.Interface.MONAD) => {
                                                module I:
                                                  {
                                                    let ( <$> ):
                                                      ('a => 'b, M.t('a)) =>
                                                      M.t('b);
                                                    let ( <#> ):
                                                      (M.t('a), 'a => 'b) =>
                                                      M.t('b);
                                                    let ( <*> ):
                                                      (M.t('a => 'b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >>= ):
                                                      (M.t('a),
                                                      'a => M.t('b)) =>
                                                      M.t('b);
                                                    let ( =<< ):
                                                      ('a => M.t('b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >=> ):
                                                      ('a => M.t('b),
                                                      'b => M.t('c), 'a) =>
                                                      M.t('c);
                                                    let ( <=< ):
                                                      ('a => M.t('b),
                                                      'c => M.t('a), 'c) =>
                                                      M.t('b);
                                                  };
                                                let fold_monad:
                                                  (('a, 'b) => M.t('a), 'a,
                                                  Foldable.t('b)) => 
                                                  M.t('a);
                                              };
      };
    let foldLeft: (('a, 'b) => 'a, 'a, Foldable.t('b)) => 'a;
    let foldRight: (('a, 'b) => 'b, 'b, Foldable.t('a)) => 'b;
    let any: ('a => bool, Foldable.t('a)) => bool;
    let all: ('a => bool, Foldable.t('a)) => bool;
    let containsBy: (('a, 'a) => bool, 'a, Foldable.t('a)) => bool;
    let contains:
      ((module BsAbstract.Interface.EQ with type t = 'a), 'a,
      Foldable.t('a)) => bool;
    let indexOfBy: (('a, 'a) => bool, 'a, Foldable.t('a)) => option(int);
    let indexOf:
      ((module BsAbstract.Interface.EQ with type t = 'a), 'a,
      Foldable.t('a)) => option(int);
    let minBy:
      (('a, 'a) => Relude_Extensions_Foldable.ordering, Foldable.t('a)) =>
      option('a);
    let min:
      ((module BsAbstract.Interface.ORD with type t = 'a), Foldable.t('a)) =>
      option('a);
    let maxBy:
      (('a, 'a) => Relude_Extensions_Foldable.ordering, Foldable.t('a)) =>
      option('a);
    let max:
      ((module BsAbstract.Interface.ORD with type t = 'a), Foldable.t('a)) =>
      option('a);
    let countBy: ('a => bool, Foldable.t('a)) => int;
    let length: Foldable.t('a) => int;
    let forEach: ('a => unit, Foldable.t('a)) => unit;
    let forEachWithIndex: (('a, int) => unit, Foldable.t('a)) => unit;
    let find: ('a => bool, Foldable.t('a)) => option('a);
    let findWithIndex: (('a, int) => bool, Foldable.t('a)) => option('a);
    let toList: Foldable.t('a) => list('a);
    let toArray: Foldable.t('a) => array('a);
    module FoldableSemigroupExtensions:
      (S : BsAbstract.Interface.SEMIGROUP) => {
                                                module BsFoldableSemigroupExtensions:
                                                  {
                                                    module FM:
                                                      {
                                                        let fold_map:
                                                          ('a =>
                                                           BsAbstract.Endo.Monoid.t('a),
                                                          Foldable.t('a)) =>
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
                                                      Foldable.t(S.t)) => 
                                                      S.t;
                                                    let surround:
                                                      (~delimiter: S.t,
                                                      Foldable.t(S.t)) => 
                                                      S.t;
                                                  };
                                                let surroundMap:
                                                  (~delimiter: S.t,
                                                  S.t => S.t,
                                                  Foldable.t(S.t)) => 
                                                  S.t;
                                                let surround:
                                                  (~delimiter: S.t,
                                                  Foldable.t(S.t)) => 
                                                  S.t;
                                              };
    module FoldableMonoidExtensions:
      (M : BsAbstract.Interface.MONOID) => {
                                             module BsFoldableMonoidExtensions:
                                               {
                                                 module FM:
                                                   {
                                                     let fold_map:
                                                       ('a => M.t,
                                                       Foldable.t('a)) => 
                                                       M.t;
                                                   };
                                                 module I:
                                                   {
                                                     let ( <:> ):
                                                       (M.t, M.t) => M.t;
                                                   };
                                                 type acc =
                                                   BsAbstract.Functions.Foldable(Foldable).Monoid(M).acc = {
                                                   init: bool,
                                                   acc: M.t,
                                                 };
                                                 let fold:
                                                   Foldable.t(M.t) => M.t;
                                                 let intercalate:
                                                   (~separator: M.t,
                                                   Foldable.t(M.t)) => 
                                                   M.t;
                                               };
                                             let foldMap:
                                               ('a => M.t, Foldable.t('a)) =>
                                               M.t;
                                             let fold: Foldable.t(M.t) => M.t;
                                             let intercalate:
                                               (M.t, Foldable.t(M.t)) => M.t;
                                           };
    let foldMap:
      ((module BsAbstract.Interface.MONOID with type t = 'a), 'b => 'a,
      Foldable.t('b)) => 'a;
    let foldMonoid:
      ((module BsAbstract.Interface.MONOID with type t = 'a),
      Foldable.t('a)) => 'a;
    let intercalate:
      ((module BsAbstract.Interface.MONOID with type t = 'a), 'a,
      Foldable.t('a)) => 'a;
    module FoldableApplicativeExtensions:
      (A : BsAbstract.Interface.APPLICATIVE) => {
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
                                                                (A.t(
                                                                 'a => 'b),
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
                                                            A.t('a), 
                                                            A.t('b),
                                                            A.t('c)) =>
                                                            A.t('d);
                                                          let lift4:
                                                            (('a, 'b, 'c,
                                                             'd) => 'e,
                                                            A.t('a), 
                                                            A.t('b), 
                                                            A.t('c),
                                                            A.t('d)) =>
                                                            A.t('e);
                                                          let lift5:
                                                            (('a, 'b, 'c, 'd,
                                                             'e) => 'f,
                                                            A.t('a), 
                                                            A.t('b), 
                                                            A.t('c), 
                                                            A.t('d),
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
                                                        Foldable.t('a)) =>
                                                        A.t(unit);
                                                      let sequence':
                                                        Foldable.t(A.t('a)) =>
                                                        A.t(unit);
                                                    };
                                                  let traverse_:
                                                    ('a => A.t('b),
                                                    Foldable.t('a)) =>
                                                    A.t(unit);
                                                  let sequence_:
                                                    Foldable.t(A.t('a)) =>
                                                    A.t(unit);
                                                };
    module FoldableMonadExtensions:
      (M : BsAbstract.Interface.MONAD) => {
                                            module BsFoldableMonadExtensions:
                                              {
                                                module I:
                                                  {
                                                    let ( <$> ):
                                                      ('a => 'b, M.t('a)) =>
                                                      M.t('b);
                                                    let ( <#> ):
                                                      (M.t('a), 'a => 'b) =>
                                                      M.t('b);
                                                    let ( <*> ):
                                                      (M.t('a => 'b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >>= ):
                                                      (M.t('a),
                                                      'a => M.t('b)) =>
                                                      M.t('b);
                                                    let ( =<< ):
                                                      ('a => M.t('b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >=> ):
                                                      ('a => M.t('b),
                                                      'b => M.t('c), 'a) =>
                                                      M.t('c);
                                                    let ( <=< ):
                                                      ('a => M.t('b),
                                                      'c => M.t('a), 'c) =>
                                                      M.t('b);
                                                  };
                                                let fold_monad:
                                                  (('a, 'b) => M.t('a), 'a,
                                                  Foldable.t('b)) => 
                                                  M.t('a);
                                              };
                                            let foldM:
                                              (('acc, 'a) => M.t('acc), 'acc,
                                              Foldable.t('a)) => M.t('acc);
                                          };
    module FoldableEqExtensions:
      (E : BsAbstract.Interface.EQ) => {
                                         let contains:
                                           (E.t, Foldable.t(E.t)) => bool;
                                         let indexOf:
                                           (E.t, Foldable.t(E.t)) =>
                                           option(int);
                                       };
    module FoldableOrdExtensions:
      (O : BsAbstract.Interface.ORD) => {
                                          let min:
                                            Foldable.t(O.t) => option(O.t);
                                          let max:
                                            Foldable.t(O.t) => option(O.t);
                                        };
  };
module BsFoldableExtensions = FoldableExtensions.BsFoldableExtensions;
let foldLeft: (('a, 'b) => 'a, 'a, Foldable.t('b)) => 'a;
let foldRight: (('a, 'b) => 'b, 'b, Foldable.t('a)) => 'b;
let any: ('a => bool, Foldable.t('a)) => bool;
let all: ('a => bool, Foldable.t('a)) => bool;
let containsBy: (('a, 'a) => bool, 'a, Foldable.t('a)) => bool;
let contains:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, Foldable.t('a)) =>
  bool;
let indexOfBy: (('a, 'a) => bool, 'a, Foldable.t('a)) => option(int);
let indexOf:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, Foldable.t('a)) =>
  option(int);
let minBy:
  (('a, 'a) => Relude_Extensions_Foldable.ordering, Foldable.t('a)) =>
  option('a);
let min:
  ((module BsAbstract.Interface.ORD with type t = 'a), Foldable.t('a)) =>
  option('a);
let maxBy:
  (('a, 'a) => Relude_Extensions_Foldable.ordering, Foldable.t('a)) =>
  option('a);
let max:
  ((module BsAbstract.Interface.ORD with type t = 'a), Foldable.t('a)) =>
  option('a);
let countBy: ('a => bool, Foldable.t('a)) => int;
let length: Foldable.t('a) => int;
let forEach: ('a => unit, Foldable.t('a)) => unit;
let forEachWithIndex: (('a, int) => unit, Foldable.t('a)) => unit;
let find: ('a => bool, Foldable.t('a)) => option('a);
let findWithIndex: (('a, int) => bool, Foldable.t('a)) => option('a);
let toList: Foldable.t('a) => list('a);
module FoldableSemigroupExtensions =
  FoldableExtensions.FoldableSemigroupExtensions;
module FoldableMonoidExtensions = FoldableExtensions.FoldableMonoidExtensions;
let foldMap:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'b => 'a,
  Foldable.t('b)) => 'a;
let foldMonoid:
  ((module BsAbstract.Interface.MONOID with type t = 'a), Foldable.t('a)) =>
  'a;
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a,
  Foldable.t('a)) => 'a;
module FoldableApplicativeExtensions =
  FoldableExtensions.FoldableApplicativeExtensions;
module FoldableMonadExtensions = FoldableExtensions.FoldableMonadExtensions;
module FoldableEqExtensions = FoldableExtensions.FoldableEqExtensions;
module FoldableOrdExtensions = FoldableExtensions.FoldableOrdExtensions;
module Traversable: BsAbstract.List.TRAVERSABLE_F;
let eqBy: (('a, 'a) => bool, list('a), list('a)) => bool;
module Eq:
  (EqA : BsAbstract.Interface.EQ) => {
                                       type t = list(EqA.t);
                                       let eq:
                                         (list(EqA.t), list(EqA.t)) => bool;
                                     };
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a), list('a)) =>
  bool;
let showBy: ('a => string, list('a)) => string;
module Show:
  (ShowA : BsAbstract.Interface.SHOW) => {
                                           type t = list(ShowA.t);
                                           let show: list(ShowA.t) => string;
                                         };
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), list('a)) => string;
module IsoArray:
  {
    type t('a) = list('a);
    let fromArray: array('a) => t('a);
    let toArray: t('a) => array('a);
  };
let fromArray: array('a) => IsoArray.t('a);
let toArray: IsoArray.t('a) => array('a);
