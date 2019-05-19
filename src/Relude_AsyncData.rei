type t('a) = Init | Loading | Reloading('a) | Complete('a);
let init: t('a);
let loading: t('a);
let reloading: 'a => t('a);
let complete: 'a => t('a);
let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool;
let isInit: t('a) => bool;
let isLoading: t('a) => bool;
let isReloading: t('a) => bool;
let isComplete: t('a) => bool;
let isBusy: t('a) => bool;
let isIdle: t('a) => bool;
let toBusy: t('a) => t('a);
let toIdle: t('a) => t('a);
let getValue: t('a) => option('a);
let getReloading: t('a) => option('a);
let getComplete: t('a) => option('a);
let fold: ('b, 'b, 'a => 'b, 'a => 'b, t('a)) => 'b;
let foldLazy: (unit => 'b, unit => 'b, 'a => 'b, 'a => 'b, t('a)) => 'b;
let foldByValue: ('b, 'a => 'b, t('a)) => 'b;
let foldByValueLazy: (unit => 'b, 'a => 'b, t('a)) => 'b;
module Functor:
  { type nonrec t('a) = t('a); let map: ('a => 'b, t('a)) => t('b); };
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
module Alt:
  {
    type nonrec t('a) = t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
  };
let alt: (Alt.t('a), Alt.t('a)) => Alt.t('a);
module Apply:
  {
    type nonrec t('a) = t('a);
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
    type nonrec t('a) = t('a);
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
    type nonrec t('a) = t('a);
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
module Eq: (E : BsAbstract.Interface.EQ) => BsAbstract.Interface.EQ;
module Infix:
  {
    module FunctorExtensions:
      {
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
      };
    let ( <$> ): ('a => 'b, Functor.t('a)) => Functor.t('b);
    let ( <#> ): (Functor.t('a), 'a => 'b) => Functor.t('b);
    let ( <$ ): ('a, Functor.t('b)) => Functor.t('a);
    let ( $> ): (Functor.t('a), 'b) => Functor.t('b);
    let ( <@> ): (Functor.t('a => 'b), 'a) => Functor.t('b);
    let ( <|> ): (Alt.t('a), Alt.t('a)) => Alt.t('a);
    module ApplyExtensions:
      {
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
            let lift2:
              (('a, 'b) => 'c, Apply.t('a), Apply.t('b)) => Apply.t('c);
            let lift3:
              (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b), Apply.t('c)) =>
              Apply.t('d);
            let lift4:
              (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b), Apply.t('c),
              Apply.t('d)) => Apply.t('e);
            let lift5:
              (('a, 'b, 'c, 'd, 'e) => 'f, Apply.t('a), Apply.t('b),
              Apply.t('c), Apply.t('d), Apply.t('e)) => Apply.t('f);
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
          (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b), Apply.t('c)) =>
          Apply.t('d);
        let map4:
          (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b), Apply.t('c),
          Apply.t('d)) => Apply.t('e);
        let map5:
          (('a, 'b, 'c, 'd, 'e) => 'f, Apply.t('a), Apply.t('b), Apply.t('c),
          Apply.t('d), Apply.t('e)) => Apply.t('f);
        let tuple2: (Apply.t('a), Apply.t('b)) => Apply.t(('a, 'b));
        let tuple3:
          (Apply.t('a), Apply.t('b), Apply.t('c)) => Apply.t(('a, 'b, 'c));
        let tuple4:
          (Apply.t('a), Apply.t('b), Apply.t('c), Apply.t('d)) =>
          Apply.t(('a, 'b, 'c, 'd));
        let tuple5:
          (Apply.t('a), Apply.t('b), Apply.t('c), Apply.t('d),
          Apply.t('e)) => Apply.t(('a, 'b, 'c, 'd, 'e));
      };
    let ( <*> ): (Apply.t('a => 'b), Apply.t('a)) => Apply.t('b);
    let ( <* ): (Apply.t('a), Apply.t('b)) => Apply.t('a);
    let ( *> ): (Apply.t('a), Apply.t('b)) => Apply.t('b);
    module MonadExtensions:
      {
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
                    let ( <*> ):
                      (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
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
            let if_m:
              (Monad.t(bool), Monad.t('a), Monad.t('a)) => Monad.t('a);
            let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
            let ap: (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
            let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
            let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
          };
        let bind: (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
        let flatMap: ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
        let flatten: Monad.t(Monad.t('a)) => Monad.t('a);
        let composeKleisli:
          ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
        let flipComposeKleisli:
          ('b => Monad.t('c), 'a => Monad.t('b), 'a) => Monad.t('c);
        let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
        let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
        let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
      };
    let ( >>= ): (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
    let ( =<< ): ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
    let ( >=> ): ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
    let ( <=< ): ('a => Monad.t('b), 'c => Monad.t('a), 'c) => Monad.t('b);
  };
