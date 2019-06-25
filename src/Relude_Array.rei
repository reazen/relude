let concat: (array('a), array('a)) => array('a);
module SemigroupAny = Relude_Array_Instances.SemigroupAny;
let empty: array('a);
module MonoidAny = Relude_Array_Instances.MonoidAny;
let map: ('a => 'b, array('a)) => array('b);
module Functor = Relude_Array_Instances.Functor;
module BsFunctorExtensions = Relude_Array_Instances.BsFunctorExtensions;
let flipMap: (Functor.t('a), 'a => 'b) => Functor.t('b);
let void: Functor.t('a) => Functor.t(unit);
let voidRight: ('a, Functor.t('b)) => Functor.t('a);
let voidLeft: (Functor.t('a), 'b) => Functor.t('b);
let flap: (Functor.t('a => 'b), 'a) => Functor.t('b);
let apply: (array('a => 'b), array('a)) => array('b);
module Apply = Relude_Array_Instances.Apply;
module BsApplyExtensions = Relude_Array_Instances.BsApplyExtensions;
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
let pure: 'a => array('a);
module Applicative = Relude_Array_Instances.Applicative;
module BsApplicativeExtensions =
  Relude_Array_Instances.BsApplicativeExtensions;
let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
let bind: (array('a), 'a => array('b)) => array('b);
module Monad = Relude_Array_Instances.Monad;
module BsMonadExtensions = Relude_Array_Instances.BsMonadExtensions;
let flatMap: ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
let flatten: Monad.t(Monad.t('a)) => Monad.t('a);
let composeKleisli: ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
let flipComposeKleisli:
  ('b => Monad.t('c), 'a => Monad.t('b), 'a) => Monad.t('c);
let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
let alt: (array('a), array('a)) => array('a);
module Alt = Relude_Array_Instances.Alt;
module Plus = Relude_Array_Instances.Plus;
module Alternative = Relude_Array_Instances.Alternative;
let imap: ('a => 'b, 'b => 'a, array('a)) => array('b);
module Invariant = Relude_Array_Instances.Invariant;
module MonadZero = Relude_Array_Instances.MonadZero;
module MonadPlus = Relude_Array_Instances.MonadPlus;
let extend: (array('a) => 'b, array('a)) => array('b);
module Extend = Relude_Array_Instances.Extend;
let foldLeft: (('a, 'b) => 'a, 'a, BsAbstract.Array.Foldable.t('b)) => 'a;
let foldRight: (('a, 'b) => 'b, 'b, BsAbstract.Array.Foldable.t('a)) => 'b;
module Foldable = Relude_Array_Instances.Foldable;
module BsFoldableExtensions = Relude_Array_Instances.BsFoldableExtensions;
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
let forEach: ('a => unit, Foldable.t('a)) => unit;
let forEachWithIndex: (('a, int) => unit, Foldable.t('a)) => unit;
let find: ('a => bool, Foldable.t('a)) => option('a);
let findWithIndex: (('a, int) => bool, Foldable.t('a)) => option('a);
let toArray: Foldable.t('a) => array('a);
module FoldableSemigroupExtensions =
  Relude_Array_Instances.FoldableSemigroupExtensions;
module FoldableMonoidExtensions =
  Relude_Array_Instances.FoldableMonoidExtensions;
let foldMap:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'b => 'a,
  Foldable.t('b)) => 'a;
let foldWithMonoid:
  ((module BsAbstract.Interface.MONOID with type t = 'a), Foldable.t('a)) =>
  'a;
let intercalate:
  ((module BsAbstract.Interface.MONOID with type t = 'a), 'a,
  Foldable.t('a)) => 'a;
module FoldableApplicativeExtensions =
  Relude_Array_Instances.FoldableApplicativeExtensions;
module FoldableMonadExtensions =
  Relude_Array_Instances.FoldableMonadExtensions;
module FoldableEqExtensions = Relude_Array_Instances.FoldableEqExtensions;
module FoldableOrdExtensions = Relude_Array_Instances.FoldableOrdExtensions;
module Traversable = Relude_Array_Instances.Traversable;
let eqBy: (('a, 'a) => bool, array('a), array('a)) => bool;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a),
  array('a)) => bool;
module Eq = Relude_Array_Instances.Eq;
module Ord = Relude_Array_Instances.Ord;
let showBy: ('a => BsAbstract.String.Monoid.t, array('a)) => string;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), array('a)) => string;
module Show = Relude_Array_Instances.Show;
let fromList: list('a) => array('a);
let toList: array('a) => list('a);
module IsoList = Relude_Array_Instances.IsoList;
let cons: ('a, array('a)) => array('a);
let prepend: ('a, array('a)) => array('a);
let uncons: array('a) => option(('a, array('a)));
let append: ('a, array('a)) => array('a);
let repeat: (int, 'a) => array('a);
let makeWithIndex: (int, int => 'a) => array('a);
let mapWithIndex: (('a, int) => 'b, array('a)) => array('b);
let reverse: array('a) => array('a);
let shuffleInPlace: array('a) => array('a);
let shuffle: array('a) => array('a);
let length: array('a) => int;
let isEmpty: array('a) => bool;
let isNotEmpty: array('a) => bool;
let at: (int, array('a)) => option('a);
let setAt: (int, 'a, array('a)) => option(array('a));
let head: array('a) => option('a);
let tail: array('a) => option(array('a));
let tailOrEmpty: array('a) => array('a);
let init: array('a) => option(array('a));
let initOrEmpty: array('a) => array('a);
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
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b);
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b);
module ArrayEqExtensions = Relude_Extensions_Array.ArrayEqExtensions;
module ArrayOrdExtensions = Relude_Extensions_Array.ArrayOrdExtensions;
module ArrayMonoidExtensions = Relude_Extensions_Array.ArrayMonoidExtensions;
module String = Relude_Extensions_Array.String;
module Int = Relude_Extensions_Array.Int;
module Float = Relude_Extensions_Array.Float;
module Option = Relude_Extensions_Array.Option;
module Result = Relude_Extensions_Array.Result;
module IO = Relude_Extensions_Array.IO;
module Validation = Relude_Extensions_Array.Validation;
module Infix:
  {
    module FunctorExtensions:
      {
        module BsFunctorExtensions:
          {
            let void:
              Relude_Array_Instances.Functor.t('a) =>
              Relude_Array_Instances.Functor.t(unit);
            let void_right:
              ('a, Relude_Array_Instances.Functor.t('b)) =>
              Relude_Array_Instances.Functor.t('a);
            let void_left:
              (Relude_Array_Instances.Functor.t('a), 'b) =>
              Relude_Array_Instances.Functor.t('b);
            let flap:
              (Relude_Array_Instances.Functor.t('a => 'b), 'a) =>
              Relude_Array_Instances.Functor.t('b);
          };
        let flipMap:
          (Relude_Array_Instances.Functor.t('a), 'a => 'b) =>
          Relude_Array_Instances.Functor.t('b);
        let void:
          Relude_Array_Instances.Functor.t('a) =>
          Relude_Array_Instances.Functor.t(unit);
        let voidRight:
          ('a, Relude_Array_Instances.Functor.t('b)) =>
          Relude_Array_Instances.Functor.t('a);
        let voidLeft:
          (Relude_Array_Instances.Functor.t('a), 'b) =>
          Relude_Array_Instances.Functor.t('b);
        let flap:
          (Relude_Array_Instances.Functor.t('a => 'b), 'a) =>
          Relude_Array_Instances.Functor.t('b);
      };
    let ( <$> ):
      ('a => 'b, Relude_Array_Instances.Functor.t('a)) =>
      Relude_Array_Instances.Functor.t('b);
    let ( <#> ):
      (Relude_Array_Instances.Functor.t('a), 'a => 'b) =>
      Relude_Array_Instances.Functor.t('b);
    let ( <$ ):
      ('a, Relude_Array_Instances.Functor.t('b)) =>
      Relude_Array_Instances.Functor.t('a);
    let ( $> ):
      (Relude_Array_Instances.Functor.t('a), 'b) =>
      Relude_Array_Instances.Functor.t('b);
    let ( <@> ):
      (Relude_Array_Instances.Functor.t('a => 'b), 'a) =>
      Relude_Array_Instances.Functor.t('b);
    let ( <|> ):
      (Relude_Array_Instances.Alt.t('a), Relude_Array_Instances.Alt.t('a)) =>
      Relude_Array_Instances.Alt.t('a);
    module ApplyExtensions:
      {
        module BsApplyExtensions:
          {
            module I:
              {
                let ( <$> ):
                  ('a => 'b, Relude_Array_Instances.Apply.t('a)) =>
                  Relude_Array_Instances.Apply.t('b);
                let ( <#> ):
                  (Relude_Array_Instances.Apply.t('a), 'a => 'b) =>
                  Relude_Array_Instances.Apply.t('b);
                let ( <*> ):
                  (Relude_Array_Instances.Apply.t('a => 'b),
                  Relude_Array_Instances.Apply.t('a)) =>
                  Relude_Array_Instances.Apply.t('b);
              };
            let apply_first:
              (Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b)) =>
              Relude_Array_Instances.Apply.t('a);
            let apply_second:
              (Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b)) =>
              Relude_Array_Instances.Apply.t('b);
            let apply_both:
              (Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b)) =>
              Relude_Array_Instances.Apply.t(('a, 'b));
            let lift2:
              (('a, 'b) => 'c, Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b)) =>
              Relude_Array_Instances.Apply.t('c);
            let lift3:
              (('a, 'b, 'c) => 'd, Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b),
              Relude_Array_Instances.Apply.t('c)) =>
              Relude_Array_Instances.Apply.t('d);
            let lift4:
              (('a, 'b, 'c, 'd) => 'e, Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b),
              Relude_Array_Instances.Apply.t('c),
              Relude_Array_Instances.Apply.t('d)) =>
              Relude_Array_Instances.Apply.t('e);
            let lift5:
              (('a, 'b, 'c, 'd, 'e) => 'f,
              Relude_Array_Instances.Apply.t('a),
              Relude_Array_Instances.Apply.t('b),
              Relude_Array_Instances.Apply.t('c),
              Relude_Array_Instances.Apply.t('d),
              Relude_Array_Instances.Apply.t('e)) =>
              Relude_Array_Instances.Apply.t('f);
            module Infix:
              {
                let ( <* ):
                  (Relude_Array_Instances.Apply.t('a),
                  Relude_Array_Instances.Apply.t('b)) =>
                  Relude_Array_Instances.Apply.t('a);
                let ( *> ):
                  (Relude_Array_Instances.Apply.t('a),
                  Relude_Array_Instances.Apply.t('b)) =>
                  Relude_Array_Instances.Apply.t('b);
              };
          };
        let applyFirst:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b)) =>
          Relude_Array_Instances.Apply.t('a);
        let applySecond:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b)) =>
          Relude_Array_Instances.Apply.t('b);
        let map2:
          (('a, 'b) => 'c, Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b)) =>
          Relude_Array_Instances.Apply.t('c);
        let map3:
          (('a, 'b, 'c) => 'd, Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c)) =>
          Relude_Array_Instances.Apply.t('d);
        let map4:
          (('a, 'b, 'c, 'd) => 'e, Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c),
          Relude_Array_Instances.Apply.t('d)) =>
          Relude_Array_Instances.Apply.t('e);
        let map5:
          (('a, 'b, 'c, 'd, 'e) => 'f, Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c),
          Relude_Array_Instances.Apply.t('d),
          Relude_Array_Instances.Apply.t('e)) =>
          Relude_Array_Instances.Apply.t('f);
        let tuple2:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b)) =>
          Relude_Array_Instances.Apply.t(('a, 'b));
        let tuple3:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c)) =>
          Relude_Array_Instances.Apply.t(('a, 'b, 'c));
        let tuple4:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c),
          Relude_Array_Instances.Apply.t('d)) =>
          Relude_Array_Instances.Apply.t(('a, 'b, 'c, 'd));
        let tuple5:
          (Relude_Array_Instances.Apply.t('a),
          Relude_Array_Instances.Apply.t('b),
          Relude_Array_Instances.Apply.t('c),
          Relude_Array_Instances.Apply.t('d),
          Relude_Array_Instances.Apply.t('e)) =>
          Relude_Array_Instances.Apply.t(('a, 'b, 'c, 'd, 'e));
      };
    let ( <*> ):
      (Relude_Array_Instances.Apply.t('a => 'b),
      Relude_Array_Instances.Apply.t('a)) =>
      Relude_Array_Instances.Apply.t('b);
    let ( <* ):
      (Relude_Array_Instances.Apply.t('a),
      Relude_Array_Instances.Apply.t('b)) =>
      Relude_Array_Instances.Apply.t('a);
    let ( *> ):
      (Relude_Array_Instances.Apply.t('a),
      Relude_Array_Instances.Apply.t('b)) =>
      Relude_Array_Instances.Apply.t('b);
    module MonadExtensions:
      {
        module BsMonadExtensions:
          {
            module I:
              {
                let ( <$> ):
                  ('a => 'b, Relude_Array_Instances.Monad.t('a)) =>
                  Relude_Array_Instances.Monad.t('b);
                let ( <#> ):
                  (Relude_Array_Instances.Monad.t('a), 'a => 'b) =>
                  Relude_Array_Instances.Monad.t('b);
                let ( <*> ):
                  (Relude_Array_Instances.Monad.t('a => 'b),
                  Relude_Array_Instances.Monad.t('a)) =>
                  Relude_Array_Instances.Monad.t('b);
                let ( >>= ):
                  (Relude_Array_Instances.Monad.t('a),
                  'a => Relude_Array_Instances.Monad.t('b)) =>
                  Relude_Array_Instances.Monad.t('b);
                let ( =<< ):
                  ('a => Relude_Array_Instances.Monad.t('b),
                  Relude_Array_Instances.Monad.t('a)) =>
                  Relude_Array_Instances.Monad.t('b);
                let ( >=> ):
                  ('a => Relude_Array_Instances.Monad.t('b),
                  'b => Relude_Array_Instances.Monad.t('c), 'a) =>
                  Relude_Array_Instances.Monad.t('c);
                let ( <=< ):
                  ('a => Relude_Array_Instances.Monad.t('b),
                  'c => Relude_Array_Instances.Monad.t('a), 'c) =>
                  Relude_Array_Instances.Monad.t('b);
              };
            module A:
              {
                module I:
                  {
                    let ( <$> ):
                      ('a => 'b, Relude_Array_Instances.Monad.t('a)) =>
                      Relude_Array_Instances.Monad.t('b);
                    let ( <#> ):
                      (Relude_Array_Instances.Monad.t('a), 'a => 'b) =>
                      Relude_Array_Instances.Monad.t('b);
                    let ( <*> ):
                      (Relude_Array_Instances.Monad.t('a => 'b),
                      Relude_Array_Instances.Monad.t('a)) =>
                      Relude_Array_Instances.Monad.t('b);
                  };
                let liftA1:
                  ('a => 'b, Relude_Array_Instances.Monad.t('a)) =>
                  Relude_Array_Instances.Monad.t('b);
                let when_:
                  (bool, Relude_Array_Instances.Monad.t(unit)) =>
                  Relude_Array_Instances.Monad.t(unit);
                let unless:
                  (bool, Relude_Array_Instances.Monad.t(unit)) =>
                  Relude_Array_Instances.Monad.t(unit);
              };
            let flatten:
              Relude_Array_Instances.Monad.t(Relude_Array_Instances.Monad.t('a)) =>
              Relude_Array_Instances.Monad.t('a);
            let compose_kliesli:
              ('a => Relude_Array_Instances.Monad.t('b),
              'b => Relude_Array_Instances.Monad.t('c), 'a) =>
              Relude_Array_Instances.Monad.t('c);
            let compose_kliesli_flipped:
              ('b => Relude_Array_Instances.Monad.t('c),
              'a => Relude_Array_Instances.Monad.t('b), 'a) =>
              Relude_Array_Instances.Monad.t('c);
            let if_m:
              (Relude_Array_Instances.Monad.t(bool),
              Relude_Array_Instances.Monad.t('a),
              Relude_Array_Instances.Monad.t('a)) =>
              Relude_Array_Instances.Monad.t('a);
            let liftM1:
              ('a => 'b, Relude_Array_Instances.Monad.t('a)) =>
              Relude_Array_Instances.Monad.t('b);
            let ap:
              (Relude_Array_Instances.Monad.t('a => 'b),
              Relude_Array_Instances.Monad.t('a)) =>
              Relude_Array_Instances.Monad.t('b);
            let when_:
              (Relude_Array_Instances.Monad.t(bool),
              Relude_Array_Instances.Monad.t(unit)) =>
              Relude_Array_Instances.Monad.t(unit);
            let unless:
              (Relude_Array_Instances.Monad.t(bool),
              Relude_Array_Instances.Monad.t(unit)) =>
              Relude_Array_Instances.Monad.t(unit);
          };
        let flatMap:
          ('a => Relude_Array_Instances.Monad.t('b),
          Relude_Array_Instances.Monad.t('a)) =>
          Relude_Array_Instances.Monad.t('b);
        let flatten:
          Relude_Array_Instances.Monad.t(Relude_Array_Instances.Monad.t('a)) =>
          Relude_Array_Instances.Monad.t('a);
        let composeKleisli:
          ('a => Relude_Array_Instances.Monad.t('b),
          'b => Relude_Array_Instances.Monad.t('c), 'a) =>
          Relude_Array_Instances.Monad.t('c);
        let flipComposeKleisli:
          ('b => Relude_Array_Instances.Monad.t('c),
          'a => Relude_Array_Instances.Monad.t('b), 'a) =>
          Relude_Array_Instances.Monad.t('c);
        let liftM1:
          ('a => 'b, Relude_Array_Instances.Monad.t('a)) =>
          Relude_Array_Instances.Monad.t('b);
        let when_:
          (Relude_Array_Instances.Monad.t(bool),
          Relude_Array_Instances.Monad.t(unit)) =>
          Relude_Array_Instances.Monad.t(unit);
        let unless:
          (Relude_Array_Instances.Monad.t(bool),
          Relude_Array_Instances.Monad.t(unit)) =>
          Relude_Array_Instances.Monad.t(unit);
      };
    let ( >>= ):
      (Relude_Array_Instances.Monad.t('a),
      'a => Relude_Array_Instances.Monad.t('b)) =>
      Relude_Array_Instances.Monad.t('b);
    let ( =<< ):
      ('a => Relude_Array_Instances.Monad.t('b),
      Relude_Array_Instances.Monad.t('a)) =>
      Relude_Array_Instances.Monad.t('b);
    let ( >=> ):
      ('a => Relude_Array_Instances.Monad.t('b),
      'b => Relude_Array_Instances.Monad.t('c), 'a) =>
      Relude_Array_Instances.Monad.t('c);
    let ( <=< ):
      ('a => Relude_Array_Instances.Monad.t('b),
      'c => Relude_Array_Instances.Monad.t('a), 'c) =>
      Relude_Array_Instances.Monad.t('b);
  };
