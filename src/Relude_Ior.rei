type t('a, 'e) = IOk('a) | IError('e) | IBoth('a, 'e);
let pure: 'a => t('a, 'e);
let ok: 'a => t('a, 'e);
let error: 'e => t('a, 'e);
let both: ('a, 'e) => t('a, 'e);
let isOk: t('a, 'e) => bool;
let isError: t('a, 'e) => bool;
let isBoth: t('a, 'e) => bool;
let map: ('a => 'b, t('a, 'e)) => t('b, 'e);
let apply: (t('a => 'b, 'e), t('a, 'e), ('e, 'e) => 'e) => t('b, 'e);
let bind: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e);
let flatMap: ('a => t('b, 'e), t('a, 'e)) => t('b, 'e);
let map2: (('a, 'b) => 'c, t('a, 'x), t('b, 'x), ('x, 'x) => 'x) => t('c, 'x);
let map3:
  (('a, 'b, 'c) => 'd, t('a, 'x), t('b, 'x), t('c, 'x), ('x, 'x) => 'x) =>
  t('d, 'x);
let map4:
  (('a, 'b, 'c, 'd) => 'e, t('a, 'x), t('b, 'x), t('c, 'x), t('d, 'x),
  ('x, 'x) => 'x) => t('e, 'x);
let map5:
  (('a, 'b, 'c, 'd, 'e) => 'f, t('a, 'x), t('b, 'x), t('c, 'x), t('d, 'x),
  t('e, 'x), ('x, 'x) => 'x) => t('f, 'x);
module WithErrors:
  (Errors : BsAbstract.Interface.SEMIGROUP_ANY) => (Error : BsAbstract.Interface.TYPE) => 
    {
      module Functor:
        {
          type nonrec t('a) = t('a, Errors.t(Error.t));
          let map: ('a => 'b, t('a)) => t('b);
        };
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
          type nonrec t('a) = t('a, Errors.t(Error.t));
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
        (Apply.t('a), Apply.t('b), Apply.t('c), Apply.t('d), Apply.t('e)) =>
        Apply.t(('a, 'b, 'c, 'd, 'e));
      module Applicative:
        {
          type nonrec t('a) = t('a, Errors.t(Error.t));
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
                (Applicative.t('a => 'b), Applicative.t('a)) =>
                Applicative.t('b);
            };
          let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
          let when_: (bool, Applicative.t(unit)) => Applicative.t(unit);
          let unless: (bool, Applicative.t(unit)) => Applicative.t(unit);
        };
      let pure: 'a => Applicative.t('a);
      let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
      module Monad:
        {
          type nonrec t('a) = t('a, Errors.t(Error.t));
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
          let if_m: (Monad.t(bool), Monad.t('a), Monad.t('a)) => Monad.t('a);
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
          module ApplyExtensions:
            {
              module BsApplyExtensions:
                {
                  module I:
                    {
                      let ( <$> ): ('a => 'b, Apply.t('a)) => Apply.t('b);
                      let ( <#> ): (Apply.t('a), 'a => 'b) => Apply.t('b);
                      let ( <*> ):
                        (Apply.t('a => 'b), Apply.t('a)) => Apply.t('b);
                    };
                  let apply_first: (Apply.t('a), Apply.t('b)) => Apply.t('a);
                  let apply_second: (Apply.t('a), Apply.t('b)) => Apply.t('b);
                  let apply_both:
                    (Apply.t('a), Apply.t('b)) => Apply.t(('a, 'b));
                  let lift2:
                    (('a, 'b) => 'c, Apply.t('a), Apply.t('b)) => Apply.t('c);
                  let lift3:
                    (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b),
                    Apply.t('c)) => Apply.t('d);
                  let lift4:
                    (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b),
                    Apply.t('c), Apply.t('d)) => Apply.t('e);
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
              let map2:
                (('a, 'b) => 'c, Apply.t('a), Apply.t('b)) => Apply.t('c);
              let map3:
                (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b),
                Apply.t('c)) => Apply.t('d);
              let map4:
                (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b),
                Apply.t('c), Apply.t('d)) => Apply.t('e);
              let map5:
                (('a, 'b, 'c, 'd, 'e) => 'f, Apply.t('a), Apply.t('b),
                Apply.t('c), Apply.t('d), Apply.t('e)) => Apply.t('f);
              let tuple2: (Apply.t('a), Apply.t('b)) => Apply.t(('a, 'b));
              let tuple3:
                (Apply.t('a), Apply.t('b), Apply.t('c)) =>
                Apply.t(('a, 'b, 'c));
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
                      let ( <*> ):
                        (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
                      let ( >>= ):
                        (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
                      let ( =<< ):
                        ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
                      let ( >=> ):
                        ('a => Monad.t('b), 'b => Monad.t('c), 'a) =>
                        Monad.t('c);
                      let ( <=< ):
                        ('a => Monad.t('b), 'c => Monad.t('a), 'c) =>
                        Monad.t('b);
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
          let ( >=> ):
            ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
          let ( <=< ):
            ('a => Monad.t('b), 'c => Monad.t('a), 'c) => Monad.t('b);
        };
    };
