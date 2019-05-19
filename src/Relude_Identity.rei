type t('a) = 'a;
let unwrap: t('a) => 'a;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), t('a), t('a)) => bool;
let eqBy: ((t(t('a)), t(t('a))) => bool, t(t(t('a))), t(t(t('a)))) => bool;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), t('a)) => string;
let showBy: ('a => string, t('a)) => string;
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
module type EQ_F =
  (EQ : BsAbstract.Interface.EQ) => {
                                      type nonrec t = t(EQ.t);
                                      let eq: (t, t) => bool;
                                    };
module Eq: EQ_F;
module type SHOW_F =
  (S : BsAbstract.Interface.SHOW) => {
                                       type nonrec t = t(S.t);
                                       let show: t => string;
                                     };
module Show: SHOW_F;
