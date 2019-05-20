let concat: (list('a), list('a)) => list('a);
module SemigroupAny = Relude_List_Instances.SemigroupAny;
let empty: list('a);
module MonoidAny = Relude_List_Instances.MonoidAny;
let map:
  ('a => 'b, BsAbstract.List.Functor.t('a)) => BsAbstract.List.Functor.t('b);
module Functor = Relude_List_Instances.Functor;
module BsFunctorExtensions = Relude_List_Instances.BsFunctorExtensions;
let flipMap: (Functor.t('a), 'a => 'b) => Functor.t('b);
let void: Functor.t('a) => Functor.t(unit);
let voidRight: ('a, Functor.t('b)) => Functor.t('a);
let voidLeft: (Functor.t('a), 'b) => Functor.t('b);
let flap: (Functor.t('a => 'b), 'a) => Functor.t('b);
let apply:
  (BsAbstract.List.Apply.t('a => 'b), BsAbstract.List.Apply.t('a)) =>
  BsAbstract.List.Apply.t('b);
module Apply = Relude_List_Instances.Apply;
module BsApplyExtensions = Relude_List_Instances.BsApplyExtensions;
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
let pure: 'a => BsAbstract.List.Applicative.t('a);
module Applicative = Relude_List_Instances.Applicative;
module BsApplicativeExtensions =
  Relude_List_Instances.BsApplicativeExtensions;
let liftA1: ('a => 'b, Applicative.t('a)) => Applicative.t('b);
let bind:
  (BsAbstract.List.Monad.t('a), 'a => BsAbstract.List.Monad.t('b)) =>
  BsAbstract.List.Monad.t('b);
module Monad = Relude_List_Instances.Monad;
module BsMonadExtensions = Relude_List_Instances.BsMonadExtensions;
let flatMap: ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
let flatten: Monad.t(Monad.t('a)) => Monad.t('a);
let composeKleisli: ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
let flipComposeKleisli:
  ('b => Monad.t('c), 'a => Monad.t('b), 'a) => Monad.t('c);
let liftM1: ('a => 'b, Monad.t('a)) => Monad.t('b);
let when_: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
let unless: (Monad.t(bool), Monad.t(unit)) => Monad.t(unit);
let alt:
  (BsAbstract.List.Alt.t('a), BsAbstract.List.Alt.t('a)) =>
  BsAbstract.List.Alt.t('a);
module Alt = Relude_List_Instances.Alt;
module Plus = Relude_List_Instances.Plus;
module Alternative = Relude_List_Instances.Alternative;
let foldLeft: (('a, 'b) => 'a, 'a, BsAbstract.List.Foldable.t('b)) => 'a;
let foldRight: (('a, 'b) => 'b, 'b, BsAbstract.List.Foldable.t('a)) => 'b;
module Foldable = Relude_List_Instances.Foldable;
module BsFoldableExtensions = Relude_List_Instances.BsFoldableExtensions;
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
  Relude_List_Instances.FoldableSemigroupExtensions;
module FoldableMonoidExtensions =
  Relude_List_Instances.FoldableMonoidExtensions;
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
  Relude_List_Instances.FoldableApplicativeExtensions;
module FoldableMonadExtensions =
  Relude_List_Instances.FoldableMonadExtensions;
module FoldableEqExtensions = Relude_List_Instances.FoldableEqExtensions;
module FoldableOrdExtensions = Relude_List_Instances.FoldableOrdExtensions;
module Traversable = Relude_List_Instances.Traversable;
let eqBy: (('a, 'a) => bool, list('a), list('a)) => bool;
let eq:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a), list('a)) =>
  bool;
module Eq = Relude_List_Instances.Eq;
let showBy: ('a => string, list('a)) => string;
let show:
  ((module BsAbstract.Interface.SHOW with type t = 'a), list('a)) => string;
module Show = Relude_List_Instances.Show;
let fromArray: array('a) => Belt.List.t('a);
let toArray: Belt.List.t('a) => array('a);
module IsoArray = Relude_List_Instances.IsoArray;
let cons: ('a, list('a)) => list('a);
let prepend: ('a, list('a)) => list('a);
let uncons: list('a) => option(('a, list('a)));
let append: ('a, list('a)) => list('a);
let repeat: (int, 'a) => list('a);
let makeWithIndex: (int, int => 'a) => list('a);
let mapWithIndex: (('a, int) => 'b, list('a)) => list('b);
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
let filterNot: ('a => bool, list('a)) => list('a);
let filterNotWithIndex: (('a, int) => bool, list('a)) => list('a);
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
let sortBy: (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a);
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
let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b);
let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b);
module ListEqExtensions = Relude_Extensions_List.ListEqExtensions;
module ListOrdExtensions = Relude_Extensions_List.ListOrdExtensions;
module ListMonoidExtensions = Relude_Extensions_List.ListMonoidExtensions;
module String = Relude_Extensions_List.String;
module Int = Relude_Extensions_List.Int;
module Float = Relude_Extensions_List.Float;
module Option = Relude_Extensions_List.Option;
module Result = Relude_Extensions_List.Result;
module Validation = Relude_Extensions_List.Validation;
module Infix:
  {
    module FunctorExtensions:
      {
        module BsFunctorExtensions:
          {
            let void:
              Relude_List_Instances.Functor.t('a) =>
              Relude_List_Instances.Functor.t(unit);
            let void_right:
              ('a, Relude_List_Instances.Functor.t('b)) =>
              Relude_List_Instances.Functor.t('a);
            let void_left:
              (Relude_List_Instances.Functor.t('a), 'b) =>
              Relude_List_Instances.Functor.t('b);
            let flap:
              (Relude_List_Instances.Functor.t('a => 'b), 'a) =>
              Relude_List_Instances.Functor.t('b);
          };
        let flipMap:
          (Relude_List_Instances.Functor.t('a), 'a => 'b) =>
          Relude_List_Instances.Functor.t('b);
        let void:
          Relude_List_Instances.Functor.t('a) =>
          Relude_List_Instances.Functor.t(unit);
        let voidRight:
          ('a, Relude_List_Instances.Functor.t('b)) =>
          Relude_List_Instances.Functor.t('a);
        let voidLeft:
          (Relude_List_Instances.Functor.t('a), 'b) =>
          Relude_List_Instances.Functor.t('b);
        let flap:
          (Relude_List_Instances.Functor.t('a => 'b), 'a) =>
          Relude_List_Instances.Functor.t('b);
      };
    let ( <$> ):
      ('a => 'b, Relude_List_Instances.Functor.t('a)) =>
      Relude_List_Instances.Functor.t('b);
    let ( <#> ):
      (Relude_List_Instances.Functor.t('a), 'a => 'b) =>
      Relude_List_Instances.Functor.t('b);
    let ( <$ ):
      ('a, Relude_List_Instances.Functor.t('b)) =>
      Relude_List_Instances.Functor.t('a);
    let ( $> ):
      (Relude_List_Instances.Functor.t('a), 'b) =>
      Relude_List_Instances.Functor.t('b);
    let ( <@> ):
      (Relude_List_Instances.Functor.t('a => 'b), 'a) =>
      Relude_List_Instances.Functor.t('b);
    let ( <|> ):
      (Relude_List_Instances.Alt.t('a), Relude_List_Instances.Alt.t('a)) =>
      Relude_List_Instances.Alt.t('a);
    module ApplyExtensions:
      {
        module BsApplyExtensions:
          {
            module I:
              {
                let ( <$> ):
                  ('a => 'b, Relude_List_Instances.Apply.t('a)) =>
                  Relude_List_Instances.Apply.t('b);
                let ( <#> ):
                  (Relude_List_Instances.Apply.t('a), 'a => 'b) =>
                  Relude_List_Instances.Apply.t('b);
                let ( <*> ):
                  (Relude_List_Instances.Apply.t('a => 'b),
                  Relude_List_Instances.Apply.t('a)) =>
                  Relude_List_Instances.Apply.t('b);
              };
            let apply_first:
              (Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b)) =>
              Relude_List_Instances.Apply.t('a);
            let apply_second:
              (Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b)) =>
              Relude_List_Instances.Apply.t('b);
            let apply_both:
              (Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b)) =>
              Relude_List_Instances.Apply.t(('a, 'b));
            let lift2:
              (('a, 'b) => 'c, Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b)) =>
              Relude_List_Instances.Apply.t('c);
            let lift3:
              (('a, 'b, 'c) => 'd, Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b),
              Relude_List_Instances.Apply.t('c)) =>
              Relude_List_Instances.Apply.t('d);
            let lift4:
              (('a, 'b, 'c, 'd) => 'e, Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b),
              Relude_List_Instances.Apply.t('c),
              Relude_List_Instances.Apply.t('d)) =>
              Relude_List_Instances.Apply.t('e);
            let lift5:
              (('a, 'b, 'c, 'd, 'e) => 'f, Relude_List_Instances.Apply.t('a),
              Relude_List_Instances.Apply.t('b),
              Relude_List_Instances.Apply.t('c),
              Relude_List_Instances.Apply.t('d),
              Relude_List_Instances.Apply.t('e)) =>
              Relude_List_Instances.Apply.t('f);
            module Infix:
              {
                let ( <* ):
                  (Relude_List_Instances.Apply.t('a),
                  Relude_List_Instances.Apply.t('b)) =>
                  Relude_List_Instances.Apply.t('a);
                let ( *> ):
                  (Relude_List_Instances.Apply.t('a),
                  Relude_List_Instances.Apply.t('b)) =>
                  Relude_List_Instances.Apply.t('b);
              };
          };
        let applyFirst:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b)) =>
          Relude_List_Instances.Apply.t('a);
        let applySecond:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b)) =>
          Relude_List_Instances.Apply.t('b);
        let map2:
          (('a, 'b) => 'c, Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b)) =>
          Relude_List_Instances.Apply.t('c);
        let map3:
          (('a, 'b, 'c) => 'd, Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c)) =>
          Relude_List_Instances.Apply.t('d);
        let map4:
          (('a, 'b, 'c, 'd) => 'e, Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c),
          Relude_List_Instances.Apply.t('d)) =>
          Relude_List_Instances.Apply.t('e);
        let map5:
          (('a, 'b, 'c, 'd, 'e) => 'f, Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c),
          Relude_List_Instances.Apply.t('d),
          Relude_List_Instances.Apply.t('e)) =>
          Relude_List_Instances.Apply.t('f);
        let tuple2:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b)) =>
          Relude_List_Instances.Apply.t(('a, 'b));
        let tuple3:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c)) =>
          Relude_List_Instances.Apply.t(('a, 'b, 'c));
        let tuple4:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c),
          Relude_List_Instances.Apply.t('d)) =>
          Relude_List_Instances.Apply.t(('a, 'b, 'c, 'd));
        let tuple5:
          (Relude_List_Instances.Apply.t('a),
          Relude_List_Instances.Apply.t('b),
          Relude_List_Instances.Apply.t('c),
          Relude_List_Instances.Apply.t('d),
          Relude_List_Instances.Apply.t('e)) =>
          Relude_List_Instances.Apply.t(('a, 'b, 'c, 'd, 'e));
      };
    let ( <*> ):
      (Relude_List_Instances.Apply.t('a => 'b),
      Relude_List_Instances.Apply.t('a)) => Relude_List_Instances.Apply.t('b);
    let ( <* ):
      (Relude_List_Instances.Apply.t('a),
      Relude_List_Instances.Apply.t('b)) => Relude_List_Instances.Apply.t('a);
    let ( *> ):
      (Relude_List_Instances.Apply.t('a),
      Relude_List_Instances.Apply.t('b)) => Relude_List_Instances.Apply.t('b);
    module MonadExtensions:
      {
        module BsMonadExtensions:
          {
            module I:
              {
                let ( <$> ):
                  ('a => 'b, Relude_List_Instances.Monad.t('a)) =>
                  Relude_List_Instances.Monad.t('b);
                let ( <#> ):
                  (Relude_List_Instances.Monad.t('a), 'a => 'b) =>
                  Relude_List_Instances.Monad.t('b);
                let ( <*> ):
                  (Relude_List_Instances.Monad.t('a => 'b),
                  Relude_List_Instances.Monad.t('a)) =>
                  Relude_List_Instances.Monad.t('b);
                let ( >>= ):
                  (Relude_List_Instances.Monad.t('a),
                  'a => Relude_List_Instances.Monad.t('b)) =>
                  Relude_List_Instances.Monad.t('b);
                let ( =<< ):
                  ('a => Relude_List_Instances.Monad.t('b),
                  Relude_List_Instances.Monad.t('a)) =>
                  Relude_List_Instances.Monad.t('b);
                let ( >=> ):
                  ('a => Relude_List_Instances.Monad.t('b),
                  'b => Relude_List_Instances.Monad.t('c), 'a) =>
                  Relude_List_Instances.Monad.t('c);
                let ( <=< ):
                  ('a => Relude_List_Instances.Monad.t('b),
                  'c => Relude_List_Instances.Monad.t('a), 'c) =>
                  Relude_List_Instances.Monad.t('b);
              };
            module A:
              {
                module I:
                  {
                    let ( <$> ):
                      ('a => 'b, Relude_List_Instances.Monad.t('a)) =>
                      Relude_List_Instances.Monad.t('b);
                    let ( <#> ):
                      (Relude_List_Instances.Monad.t('a), 'a => 'b) =>
                      Relude_List_Instances.Monad.t('b);
                    let ( <*> ):
                      (Relude_List_Instances.Monad.t('a => 'b),
                      Relude_List_Instances.Monad.t('a)) =>
                      Relude_List_Instances.Monad.t('b);
                  };
                let liftA1:
                  ('a => 'b, Relude_List_Instances.Monad.t('a)) =>
                  Relude_List_Instances.Monad.t('b);
                let when_:
                  (bool, Relude_List_Instances.Monad.t(unit)) =>
                  Relude_List_Instances.Monad.t(unit);
                let unless:
                  (bool, Relude_List_Instances.Monad.t(unit)) =>
                  Relude_List_Instances.Monad.t(unit);
              };
            let flatten:
              Relude_List_Instances.Monad.t(Relude_List_Instances.Monad.t('a)) =>
              Relude_List_Instances.Monad.t('a);
            let compose_kliesli:
              ('a => Relude_List_Instances.Monad.t('b),
              'b => Relude_List_Instances.Monad.t('c), 'a) =>
              Relude_List_Instances.Monad.t('c);
            let compose_kliesli_flipped:
              ('b => Relude_List_Instances.Monad.t('c),
              'a => Relude_List_Instances.Monad.t('b), 'a) =>
              Relude_List_Instances.Monad.t('c);
            let if_m:
              (Relude_List_Instances.Monad.t(bool),
              Relude_List_Instances.Monad.t('a),
              Relude_List_Instances.Monad.t('a)) =>
              Relude_List_Instances.Monad.t('a);
            let liftM1:
              ('a => 'b, Relude_List_Instances.Monad.t('a)) =>
              Relude_List_Instances.Monad.t('b);
            let ap:
              (Relude_List_Instances.Monad.t('a => 'b),
              Relude_List_Instances.Monad.t('a)) =>
              Relude_List_Instances.Monad.t('b);
            let when_:
              (Relude_List_Instances.Monad.t(bool),
              Relude_List_Instances.Monad.t(unit)) =>
              Relude_List_Instances.Monad.t(unit);
            let unless:
              (Relude_List_Instances.Monad.t(bool),
              Relude_List_Instances.Monad.t(unit)) =>
              Relude_List_Instances.Monad.t(unit);
          };
        let flatMap:
          ('a => Relude_List_Instances.Monad.t('b),
          Relude_List_Instances.Monad.t('a)) =>
          Relude_List_Instances.Monad.t('b);
        let flatten:
          Relude_List_Instances.Monad.t(Relude_List_Instances.Monad.t('a)) =>
          Relude_List_Instances.Monad.t('a);
        let composeKleisli:
          ('a => Relude_List_Instances.Monad.t('b),
          'b => Relude_List_Instances.Monad.t('c), 'a) =>
          Relude_List_Instances.Monad.t('c);
        let flipComposeKleisli:
          ('b => Relude_List_Instances.Monad.t('c),
          'a => Relude_List_Instances.Monad.t('b), 'a) =>
          Relude_List_Instances.Monad.t('c);
        let liftM1:
          ('a => 'b, Relude_List_Instances.Monad.t('a)) =>
          Relude_List_Instances.Monad.t('b);
        let when_:
          (Relude_List_Instances.Monad.t(bool),
          Relude_List_Instances.Monad.t(unit)) =>
          Relude_List_Instances.Monad.t(unit);
        let unless:
          (Relude_List_Instances.Monad.t(bool),
          Relude_List_Instances.Monad.t(unit)) =>
          Relude_List_Instances.Monad.t(unit);
      };
    let ( >>= ):
      (Relude_List_Instances.Monad.t('a),
      'a => Relude_List_Instances.Monad.t('b)) =>
      Relude_List_Instances.Monad.t('b);
    let ( =<< ):
      ('a => Relude_List_Instances.Monad.t('b),
      Relude_List_Instances.Monad.t('a)) => Relude_List_Instances.Monad.t('b);
    let ( >=> ):
      ('a => Relude_List_Instances.Monad.t('b),
      'b => Relude_List_Instances.Monad.t('c), 'a) =>
      Relude_List_Instances.Monad.t('c);
    let ( <=< ):
      ('a => Relude_List_Instances.Monad.t('b),
      'c => Relude_List_Instances.Monad.t('a), 'c) =>
      Relude_List_Instances.Monad.t('b);
  };
