module Function = Relude_Function;
module JsExn = Relude_Js_Exn;
module Option = Relude_Option;
module Result = Relude_Result;
module Void = Relude_Void;
let ( << ): ('a => 'b, 'c => 'a, 'c) => 'b;
let ( >> ): ('a => 'b, 'b => 'c, 'a) => 'c;
type t('a, 'e) =
    Pure('a): t('a, 'e)
  | Throw('e): t('a, 'e)
  | Suspend(unit => 'a): t('a, 'e)
  | SuspendIO(unit => t('a, 'e)): t('a, 'e)
  | Async((Result.t('a, 'e) => unit) => unit): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e)
  | FlatMap('r => t('a, 'e), t('r, 'e)): t('a, 'e);
let pure: 'a => t('a, 'e);
let pureWithVoid: 'a => t('a, Void.t);
let unit: t(unit, 'e);
let unitWithVoid: t(unit, Void.t);
let throw: 'e => t('a, 'e);
let throwWithVoid: 'e => t(Void.t, 'e);
let suspend: (unit => 'a) => t('a, 'e);
let suspendWithVoid: (unit => 'a) => t('a, Void.t);
let suspendThrow: (unit => 'e) => t('a, 'e);
let suspendIO: (unit => t('a, 'e)) => t('a, 'e);
let async: ((Result.t('a, 'e) => unit) => unit) => t('a, 'e);
let fromOption: (unit => 'e, option('a)) => t('a, 'e);
let fromResult: Result.t('a, 'e) => t('a, 'e);
let map: ('a => 'b, t('a, 'e)) => t('b, 'e);
let tap: ('a => unit, t('a, 'e)) => t('a, 'e);
let flatMap: ('a => t('b, 'e), t('a, 'e)) => t('b, 'e);
let bind: (t('a, 'e), 'a => t('b, 'e)) => t('b, 'e);
let apply: (t('a => 'b, 'e), t('a, 'e)) => t('b, 'e);
let unsafeRunAsync: (Result.t('a, 'e) => unit, t('a, 'e)) => unit;
let mapError: ('e1 => 'e2, t('a, 'e1)) => t('a, 'e2);
let tapError: ('e => unit, t('a, 'e)) => t('a, 'e);
let catchError: ('e => t('a, 'e), t('a, 'e)) => t('a, 'e);
let bimap: ('a => 'b, 'e1 => 'e2, t('a, 'e1)) => t('b, 'e2);
let bitap: ('a => unit, 'e => unit, t('a, 'e)) => t('a, 'e);
let tries: (unit => 'a) => t('a, exn);
let triesJS: (unit => 'a) => t('a, Js.Exn.t);
let flip: t('a, 'e) => t('e, 'a);
let summonError: t('a, 'e) => t(Result.t('a, 'e), Void.t);
let unsummonError: t(Result.t('a, 'e), Void.t) => t('a, 'e);
let delay: int => t(unit, 'e);
let delayWithVoid: int => t(unit, Void.t);
let withDelay: (int, t('a, 'e)) => t('a, 'e);
module WithError:
  (E : BsAbstract.Interface.TYPE) => {
                                       module Functor:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                         };
                                       module BsFunctorExtensions:
                                         {
                                           let void:
                                             Functor.t('a) => Functor.t(unit);
                                           let void_right:
                                             ('a, Functor.t('b)) =>
                                             Functor.t('a);
                                           let void_left:
                                             (Functor.t('a), 'b) =>
                                             Functor.t('b);
                                           let flap:
                                             (Functor.t('a => 'b), 'a) =>
                                             Functor.t('b);
                                         };
                                       let map:
                                         ('a => 'b, Functor.t('a)) =>
                                         Functor.t('b);
                                       let flipMap:
                                         (Functor.t('a), 'a => 'b) =>
                                         Functor.t('b);
                                       let void:
                                         Functor.t('a) => Functor.t(unit);
                                       let voidRight:
                                         ('a, Functor.t('b)) => Functor.t('a);
                                       let voidLeft:
                                         (Functor.t('a), 'b) => Functor.t('b);
                                       let flap:
                                         (Functor.t('a => 'b), 'a) =>
                                         Functor.t('b);
                                       module Bifunctor:
                                         BsAbstract.Interface.BIFUNCTOR;
                                       let bimap:
                                         ('a => 'b, 'c => 'd,
                                         Bifunctor.t('a, 'c)) =>
                                         Bifunctor.t('b, 'd);
                                       module Apply:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                           let apply:
                                             (t('a => 'b), t('a)) => t('b);
                                         };
                                       module BsApplyExtensions:
                                         {
                                           module I:
                                             {
                                               let ( <$> ):
                                                 ('a => 'b, Apply.t('a)) =>
                                                 Apply.t('b);
                                               let ( <#> ):
                                                 (Apply.t('a), 'a => 'b) =>
                                                 Apply.t('b);
                                               let ( <*> ):
                                                 (Apply.t('a => 'b),
                                                 Apply.t('a)) => Apply.t('b);
                                             };
                                           let apply_first:
                                             (Apply.t('a), Apply.t('b)) =>
                                             Apply.t('a);
                                           let apply_second:
                                             (Apply.t('a), Apply.t('b)) =>
                                             Apply.t('b);
                                           let apply_both:
                                             (Apply.t('a), Apply.t('b)) =>
                                             Apply.t(('a, 'b));
                                           let lift2:
                                             (('a, 'b) => 'c, Apply.t('a),
                                             Apply.t('b)) => Apply.t('c);
                                           let lift3:
                                             (('a, 'b, 'c) => 'd,
                                             Apply.t('a), Apply.t('b),
                                             Apply.t('c)) => Apply.t('d);
                                           let lift4:
                                             (('a, 'b, 'c, 'd) => 'e,
                                             Apply.t('a), Apply.t('b),
                                             Apply.t('c), Apply.t('d)) =>
                                             Apply.t('e);
                                           let lift5:
                                             (('a, 'b, 'c, 'd, 'e) => 'f,
                                             Apply.t('a), Apply.t('b),
                                             Apply.t('c), Apply.t('d),
                                             Apply.t('e)) => Apply.t('f);
                                           module Infix:
                                             {
                                               let ( <* ):
                                                 (Apply.t('a),
                                                 Apply.t('b)) => Apply.t('a);
                                               let ( *> ):
                                                 (Apply.t('a),
                                                 Apply.t('b)) => Apply.t('b);
                                             };
                                         };
                                       let apply:
                                         (Apply.t('a => 'b), Apply.t('a)) =>
                                         Apply.t('b);
                                       let applyFirst:
                                         (Apply.t('a), Apply.t('b)) =>
                                         Apply.t('a);
                                       let applySecond:
                                         (Apply.t('a), Apply.t('b)) =>
                                         Apply.t('b);
                                       let map2:
                                         (('a, 'b) => 'c, Apply.t('a),
                                         Apply.t('b)) => Apply.t('c);
                                       let map3:
                                         (('a, 'b, 'c) => 'd, Apply.t('a),
                                         Apply.t('b), Apply.t('c)) =>
                                         Apply.t('d);
                                       let map4:
                                         (('a, 'b, 'c, 'd) => 'e,
                                         Apply.t('a), Apply.t('b),
                                         Apply.t('c), Apply.t('d)) =>
                                         Apply.t('e);
                                       let map5:
                                         (('a, 'b, 'c, 'd, 'e) => 'f,
                                         Apply.t('a), Apply.t('b),
                                         Apply.t('c), Apply.t('d),
                                         Apply.t('e)) => Apply.t('f);
                                       let tuple2:
                                         (Apply.t('a), Apply.t('b)) =>
                                         Apply.t(('a, 'b));
                                       let tuple3:
                                         (Apply.t('a), Apply.t('b),
                                         Apply.t('c)) =>
                                         Apply.t(('a, 'b, 'c));
                                       let tuple4:
                                         (Apply.t('a), Apply.t('b),
                                         Apply.t('c), Apply.t('d)) =>
                                         Apply.t(('a, 'b, 'c, 'd));
                                       let tuple5:
                                         (Apply.t('a), Apply.t('b),
                                         Apply.t('c), Apply.t('d),
                                         Apply.t('e)) =>
                                         Apply.t(('a, 'b, 'c, 'd, 'e));
                                       module Applicative:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                           let apply:
                                             (t('a => 'b), t('a)) => t('b);
                                           let pure: 'a => t('a);
                                         };
                                       module BsApplicativeExtensions:
                                         {
                                           module I:
                                             {
                                               let ( <$> ):
                                                 ('a => 'b,
                                                 Applicative.t('a)) =>
                                                 Applicative.t('b);
                                               let ( <#> ):
                                                 (Applicative.t('a),
                                                 'a => 'b) =>
                                                 Applicative.t('b);
                                               let ( <*> ):
                                                 (Applicative.t('a => 'b),
                                                 Applicative.t('a)) =>
                                                 Applicative.t('b);
                                             };
                                           let liftA1:
                                             ('a => 'b, Applicative.t('a)) =>
                                             Applicative.t('b);
                                           let when_:
                                             (bool, Applicative.t(unit)) =>
                                             Applicative.t(unit);
                                           let unless:
                                             (bool, Applicative.t(unit)) =>
                                             Applicative.t(unit);
                                         };
                                       let pure: 'a => Applicative.t('a);
                                       let liftA1:
                                         ('a => 'b, Applicative.t('a)) =>
                                         Applicative.t('b);
                                       module Monad:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                           let apply:
                                             (t('a => 'b), t('a)) => t('b);
                                           let pure: 'a => t('a);
                                           let flat_map:
                                             (t('a), 'a => t('b)) => t('b);
                                         };
                                       module BsMonadExtensions:
                                         {
                                           module I:
                                             {
                                               let ( <$> ):
                                                 ('a => 'b, Monad.t('a)) =>
                                                 Monad.t('b);
                                               let ( <#> ):
                                                 (Monad.t('a), 'a => 'b) =>
                                                 Monad.t('b);
                                               let ( <*> ):
                                                 (Monad.t('a => 'b),
                                                 Monad.t('a)) => Monad.t('b);
                                               let ( >>= ):
                                                 (Monad.t('a),
                                                 'a => Monad.t('b)) =>
                                                 Monad.t('b);
                                               let ( =<< ):
                                                 ('a => Monad.t('b),
                                                 Monad.t('a)) => Monad.t('b);
                                               let ( >=> ):
                                                 ('a => Monad.t('b),
                                                 'b => Monad.t('c), 'a) =>
                                                 Monad.t('c);
                                               let ( <=< ):
                                                 ('a => Monad.t('b),
                                                 'c => Monad.t('a), 'c) =>
                                                 Monad.t('b);
                                             };
                                           module A:
                                             {
                                               module I:
                                                 {
                                                   let ( <$> ):
                                                     ('a => 'b,
                                                     Monad.t('a)) =>
                                                     Monad.t('b);
                                                   let ( <#> ):
                                                     (Monad.t('a),
                                                     'a => 'b) => Monad.t('b);
                                                   let ( <*> ):
                                                     (Monad.t('a => 'b),
                                                     Monad.t('a)) =>
                                                     Monad.t('b);
                                                 };
                                               let liftA1:
                                                 ('a => 'b, Monad.t('a)) =>
                                                 Monad.t('b);
                                               let when_:
                                                 (bool, Monad.t(unit)) =>
                                                 Monad.t(unit);
                                               let unless:
                                                 (bool, Monad.t(unit)) =>
                                                 Monad.t(unit);
                                             };
                                           let flatten:
                                             Monad.t(Monad.t('a)) =>
                                             Monad.t('a);
                                           let compose_kliesli:
                                             ('a => Monad.t('b),
                                             'b => Monad.t('c), 'a) =>
                                             Monad.t('c);
                                           let compose_kliesli_flipped:
                                             ('b => Monad.t('c),
                                             'a => Monad.t('b), 'a) =>
                                             Monad.t('c);
                                           let if_m:
                                             (Monad.t(bool), Monad.t('a),
                                             Monad.t('a)) => Monad.t('a);
                                           let liftM1:
                                             ('a => 'b, Monad.t('a)) =>
                                             Monad.t('b);
                                           let ap:
                                             (Monad.t('a => 'b),
                                             Monad.t('a)) => Monad.t('b);
                                           let when_:
                                             (Monad.t(bool),
                                             Monad.t(unit)) => Monad.t(unit);
                                           let unless:
                                             (Monad.t(bool),
                                             Monad.t(unit)) => Monad.t(unit);
                                         };
                                       let bind:
                                         (Monad.t('a), 'a => Monad.t('b)) =>
                                         Monad.t('b);
                                       let flatMap:
                                         ('a => Monad.t('b), Monad.t('a)) =>
                                         Monad.t('b);
                                       let flatten:
                                         Monad.t(Monad.t('a)) => Monad.t('a);
                                       let composeKleisli:
                                         ('a => Monad.t('b),
                                         'b => Monad.t('c), 'a) =>
                                         Monad.t('c);
                                       let flipComposeKleisli:
                                         ('b => Monad.t('c),
                                         'a => Monad.t('b), 'a) =>
                                         Monad.t('c);
                                       let liftM1:
                                         ('a => 'b, Monad.t('a)) =>
                                         Monad.t('b);
                                       let when_:
                                         (Monad.t(bool), Monad.t(unit)) =>
                                         Monad.t(unit);
                                       let unless:
                                         (Monad.t(bool), Monad.t(unit)) =>
                                         Monad.t(unit);
                                       module MonadThrow:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                           let apply:
                                             (t('a => 'b), t('a)) => t('b);
                                           let pure: 'a => t('a);
                                           let flat_map:
                                             (t('a), 'a => t('b)) => t('b);
                                           type e = E.t;
                                           let throwError: e => t('a);
                                         };
                                       module MonadError:
                                         {
                                           type nonrec t('a) = t('a, E.t);
                                           let map:
                                             ('a => 'b, t('a)) => t('b);
                                           let apply:
                                             (t('a => 'b), t('a)) => t('b);
                                           let pure: 'a => t('a);
                                           let flat_map:
                                             (t('a), 'a => t('b)) => t('b);
                                           type e = E.t;
                                           let throwError: e => t('a);
                                           let catchError:
                                             (e => t('a), t('a)) => t('a);
                                         };
                                       module Infix:
                                         {
                                           module FunctorExtensions:
                                             {
                                               module BsFunctorExtensions:
                                                 {
                                                   let void:
                                                     Functor.t('a) =>
                                                     Functor.t(unit);
                                                   let void_right:
                                                     ('a, Functor.t('b)) =>
                                                     Functor.t('a);
                                                   let void_left:
                                                     (Functor.t('a), 'b) =>
                                                     Functor.t('b);
                                                   let flap:
                                                     (Functor.t('a => 'b),
                                                     'a) => Functor.t('b);
                                                 };
                                               let map:
                                                 ('a => 'b, Functor.t('a)) =>
                                                 Functor.t('b);
                                               let flipMap:
                                                 (Functor.t('a), 'a => 'b) =>
                                                 Functor.t('b);
                                               let void:
                                                 Functor.t('a) =>
                                                 Functor.t(unit);
                                               let voidRight:
                                                 ('a, Functor.t('b)) =>
                                                 Functor.t('a);
                                               let voidLeft:
                                                 (Functor.t('a), 'b) =>
                                                 Functor.t('b);
                                               let flap:
                                                 (Functor.t('a => 'b), 'a) =>
                                                 Functor.t('b);
                                             };
                                           let ( <$> ):
                                             ('a => 'b, Functor.t('a)) =>
                                             Functor.t('b);
                                           let ( <#> ):
                                             (Functor.t('a), 'a => 'b) =>
                                             Functor.t('b);
                                           let ( <$ ):
                                             ('a, Functor.t('b)) =>
                                             Functor.t('a);
                                           let ( $> ):
                                             (Functor.t('a), 'b) =>
                                             Functor.t('b);
                                           let ( <@> ):
                                             (Functor.t('a => 'b), 'a) =>
                                             Functor.t('b);
                                           let ( <<$>> ):
                                             ('a => 'b, 'c => 'd,
                                             Bifunctor.t('a, 'c)) =>
                                             Bifunctor.t('b, 'd);
                                           module ApplyExtensions:
                                             {
                                               module BsApplyExtensions:
                                                 {
                                                   module I:
                                                     {
                                                       let ( <$> ):
                                                         ('a => 'b,
                                                         Apply.t('a)) =>
                                                         Apply.t('b);
                                                       let ( <#> ):
                                                         (Apply.t('a),
                                                         'a => 'b) =>
                                                         Apply.t('b);
                                                       let ( <*> ):
                                                         (Apply.t('a => 'b),
                                                         Apply.t('a)) =>
                                                         Apply.t('b);
                                                     };
                                                   let apply_first:
                                                     (Apply.t('a),
                                                     Apply.t('b)) =>
                                                     Apply.t('a);
                                                   let apply_second:
                                                     (Apply.t('a),
                                                     Apply.t('b)) =>
                                                     Apply.t('b);
                                                   let apply_both:
                                                     (Apply.t('a),
                                                     Apply.t('b)) =>
                                                     Apply.t(('a, 'b));
                                                   let lift2:
                                                     (('a, 'b) => 'c,
                                                     Apply.t('a),
                                                     Apply.t('b)) =>
                                                     Apply.t('c);
                                                   let lift3:
                                                     (('a, 'b, 'c) => 'd,
                                                     Apply.t('a),
                                                     Apply.t('b),
                                                     Apply.t('c)) =>
                                                     Apply.t('d);
                                                   let lift4:
                                                     (('a, 'b, 'c, 'd) => 'e,
                                                     Apply.t('a),
                                                     Apply.t('b),
                                                     Apply.t('c),
                                                     Apply.t('d)) =>
                                                     Apply.t('e);
                                                   let lift5:
                                                     (('a, 'b, 'c, 'd, 'e) =>
                                                      'f,
                                                     Apply.t('a),
                                                     Apply.t('b),
                                                     Apply.t('c),
                                                     Apply.t('d),
                                                     Apply.t('e)) =>
                                                     Apply.t('f);
                                                   module Infix:
                                                     {
                                                       let ( <* ):
                                                         (Apply.t('a),
                                                         Apply.t('b)) =>
                                                         Apply.t('a);
                                                       let ( *> ):
                                                         (Apply.t('a),
                                                         Apply.t('b)) =>
                                                         Apply.t('b);
                                                     };
                                                 };
                                               let apply:
                                                 (Apply.t('a => 'b),
                                                 Apply.t('a)) => Apply.t('b);
                                               let applyFirst:
                                                 (Apply.t('a),
                                                 Apply.t('b)) => Apply.t('a);
                                               let applySecond:
                                                 (Apply.t('a),
                                                 Apply.t('b)) => Apply.t('b);
                                               let map2:
                                                 (('a, 'b) => 'c,
                                                 Apply.t('a), Apply.t('b)) =>
                                                 Apply.t('c);
                                               let map3:
                                                 (('a, 'b, 'c) => 'd,
                                                 Apply.t('a), Apply.t('b),
                                                 Apply.t('c)) => Apply.t('d);
                                               let map4:
                                                 (('a, 'b, 'c, 'd) => 'e,
                                                 Apply.t('a), Apply.t('b),
                                                 Apply.t('c), Apply.t('d)) =>
                                                 Apply.t('e);
                                               let map5:
                                                 (('a, 'b, 'c, 'd, 'e) => 'f,
                                                 Apply.t('a), Apply.t('b),
                                                 Apply.t('c), Apply.t('d),
                                                 Apply.t('e)) => Apply.t('f);
                                               let tuple2:
                                                 (Apply.t('a),
                                                 Apply.t('b)) =>
                                                 Apply.t(('a, 'b));
                                               let tuple3:
                                                 (Apply.t('a), Apply.t('b),
                                                 Apply.t('c)) =>
                                                 Apply.t(('a, 'b, 'c));
                                               let tuple4:
                                                 (Apply.t('a), Apply.t('b),
                                                 Apply.t('c), Apply.t('d)) =>
                                                 Apply.t(('a, 'b, 'c, 'd));
                                               let tuple5:
                                                 (Apply.t('a), Apply.t('b),
                                                 Apply.t('c), Apply.t('d),
                                                 Apply.t('e)) =>
                                                 Apply.t(('a, 'b, 'c, 'd, 'e));
                                             };
                                           let ( <*> ):
                                             (Apply.t('a => 'b),
                                             Apply.t('a)) => Apply.t('b);
                                           let ( <* ):
                                             (Apply.t('a), Apply.t('b)) =>
                                             Apply.t('a);
                                           let ( *> ):
                                             (Apply.t('a), Apply.t('b)) =>
                                             Apply.t('b);
                                           module MonadExtensions:
                                             {
                                               module BsMonadExtensions:
                                                 {
                                                   module I:
                                                     {
                                                       let ( <$> ):
                                                         ('a => 'b,
                                                         Monad.t('a)) =>
                                                         Monad.t('b);
                                                       let ( <#> ):
                                                         (Monad.t('a),
                                                         'a => 'b) =>
                                                         Monad.t('b);
                                                       let ( <*> ):
                                                         (Monad.t('a => 'b),
                                                         Monad.t('a)) =>
                                                         Monad.t('b);
                                                       let ( >>= ):
                                                         (Monad.t('a),
                                                         'a => Monad.t('b)) =>
                                                         Monad.t('b);
                                                       let ( =<< ):
                                                         ('a => Monad.t('b),
                                                         Monad.t('a)) =>
                                                         Monad.t('b);
                                                       let ( >=> ):
                                                         ('a => Monad.t('b),
                                                         'b => Monad.t('c),
                                                         'a) => Monad.t('c);
                                                       let ( <=< ):
                                                         ('a => Monad.t('b),
                                                         'c => Monad.t('a),
                                                         'c) => Monad.t('b);
                                                     };
                                                   module A:
                                                     {
                                                       module I:
                                                         {
                                                           let ( <$> ):
                                                             ('a => 'b,
                                                             Monad.t('a)) =>
                                                             Monad.t('b);
                                                           let ( <#> ):
                                                             (Monad.t('a),
                                                             'a => 'b) =>
                                                             Monad.t('b);
                                                           let ( <*> ):
                                                             (Monad.t(
                                                              'a => 'b),
                                                             Monad.t('a)) =>
                                                             Monad.t('b);
                                                         };
                                                       let liftA1:
                                                         ('a => 'b,
                                                         Monad.t('a)) =>
                                                         Monad.t('b);
                                                       let when_:
                                                         (bool,
                                                         Monad.t(unit)) =>
                                                         Monad.t(unit);
                                                       let unless:
                                                         (bool,
                                                         Monad.t(unit)) =>
                                                         Monad.t(unit);
                                                     };
                                                   let flatten:
                                                     Monad.t(Monad.t('a)) =>
                                                     Monad.t('a);
                                                   let compose_kliesli:
                                                     ('a => Monad.t('b),
                                                     'b => Monad.t('c),
                                                     'a) => Monad.t('c);
                                                   let compose_kliesli_flipped:
                                                     ('b => Monad.t('c),
                                                     'a => Monad.t('b),
                                                     'a) => Monad.t('c);
                                                   let if_m:
                                                     (Monad.t(bool),
                                                     Monad.t('a),
                                                     Monad.t('a)) =>
                                                     Monad.t('a);
                                                   let liftM1:
                                                     ('a => 'b,
                                                     Monad.t('a)) =>
                                                     Monad.t('b);
                                                   let ap:
                                                     (Monad.t('a => 'b),
                                                     Monad.t('a)) =>
                                                     Monad.t('b);
                                                   let when_:
                                                     (Monad.t(bool),
                                                     Monad.t(unit)) =>
                                                     Monad.t(unit);
                                                   let unless:
                                                     (Monad.t(bool),
                                                     Monad.t(unit)) =>
                                                     Monad.t(unit);
                                                 };
                                               let bind:
                                                 (Monad.t('a),
                                                 'a => Monad.t('b)) =>
                                                 Monad.t('b);
                                               let flatMap:
                                                 ('a => Monad.t('b),
                                                 Monad.t('a)) => Monad.t('b);
                                               let flatten:
                                                 Monad.t(Monad.t('a)) =>
                                                 Monad.t('a);
                                               let composeKleisli:
                                                 ('a => Monad.t('b),
                                                 'b => Monad.t('c), 'a) =>
                                                 Monad.t('c);
                                               let flipComposeKleisli:
                                                 ('b => Monad.t('c),
                                                 'a => Monad.t('b), 'a) =>
                                                 Monad.t('c);
                                               let liftM1:
                                                 ('a => 'b, Monad.t('a)) =>
                                                 Monad.t('b);
                                               let when_:
                                                 (Monad.t(bool),
                                                 Monad.t(unit)) =>
                                                 Monad.t(unit);
                                               let unless:
                                                 (Monad.t(bool),
                                                 Monad.t(unit)) =>
                                                 Monad.t(unit);
                                             };
                                           let ( >>= ):
                                             (Monad.t('a),
                                             'a => Monad.t('b)) =>
                                             Monad.t('b);
                                           let ( =<< ):
                                             ('a => Monad.t('b),
                                             Monad.t('a)) => Monad.t('b);
                                           let ( >=> ):
                                             ('a => Monad.t('b),
                                             'b => Monad.t('c), 'a) =>
                                             Monad.t('c);
                                           let ( <=< ):
                                             ('a => Monad.t('b),
                                             'c => Monad.t('a), 'c) =>
                                             Monad.t('b);
                                         };
                                     };
