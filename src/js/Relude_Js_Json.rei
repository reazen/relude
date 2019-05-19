module Array = Relude_Array;
module List = Relude_List;
module NonEmptyList = Relude_NonEmpty.List;
module NonEmptyArray = Relude_NonEmpty.Array;
module Option = Relude_Option;
module Result = Relude_Result;
module Validation = Relude_Validation;
let ( >> ): ('a => 'b, 'b => 'c, 'a) => 'c;
let ( << ): ('a => 'b, 'c => 'a, 'c) => 'b;
type json = Js.Json.t;
type dict = Js.Dict.t(json);
let show: (~indentSpaces: int=?, json) => string;
let isNull: json => bool;
let isBool: json => bool;
let isString: json => bool;
let isNumber: json => bool;
let isObject: json => bool;
let isArray: json => bool;
let null: json;
let fromBool: bool => json;
let fromString: string => json;
let fromInt: int => json;
let fromFloat: float => json;
let fromArrayOfJson: array(json) => json;
let fromArrayOfJsonBy: ('a => json, array('a)) => json;
let fromListOfJson: list(json) => json;
let fromListOfJsonBy: ('a => json, list('a)) => json;
let fromDictOfJson: dict => json;
let fromArrayOfDictOfJson: array(dict) => json;
let fromListOfDictOfJson: list(dict) => json;
let fromArrayOfKeyValueTuples: array((Js.Dict.key, json)) => json;
let fromListOfKeyValueTuples: list((Js.Dict.key, json)) => json;
let toNull: json => option(unit);
let toBool: json => option(bool);
let toString: json => option(string);
let toInt: json => option(int);
let toFloat: json => option(float);
let toArrayOfJson: json => option(array(json));
let toArrayOfJsonOrElse: (array(json), json) => array(json);
let toArrayOfJsonOrEmpty: json => array(json);
let toListOfJson: json => option(list(json));
let toListOfJsonOrElse: (list(json), json) => list(json);
let toListOrEmpty: json => list(json);
let toDictOfJson: json => option(dict);
let toDictOfJsonOrElse: (dict, json) => dict;
let toDictOfJsonOrEmpty: json => dict;
module Error: { type t = string; module Type: { type nonrec t = t; }; };
module Errors:
  {
    type t = NonEmptyArray.t(Error.t);
    let pure: 'a => NonEmptyArray.Applicative.t('a);
    let make: ('a, array('a)) => NonEmptyArray.t('a);
    let map:
      ('a => 'b, Validation.t('c, Relude_NonEmpty.Array.t('a))) =>
      Validation.t('c, Relude_NonEmpty.Array.t('b));
    module SemigroupAny = NonEmptyArray.SemigroupAny;
  };
module ValidationE:
  {
    module Functor:
      {
        type t('a) = Validation.t('a, Errors.SemigroupAny.t(Error.Type.t));
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
        type t('a) = Validation.t('a, Errors.SemigroupAny.t(Error.Type.t));
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
        type t('a) = Validation.t('a, Errors.SemigroupAny.t(Error.Type.t));
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
        type t('a) = Validation.t('a, Errors.SemigroupAny.t(Error.Type.t));
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
              (('a, 'b, 'c) => 'd, Apply.t('a), Apply.t('b), Apply.t('c)) =>
              Apply.t('d);
            let map4:
              (('a, 'b, 'c, 'd) => 'e, Apply.t('a), Apply.t('b), Apply.t('c),
              Apply.t('d)) => Apply.t('e);
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
module ArrayValidationE:
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
                                                   ('a => M.t, t('a)) => M.t;
                                               };
        module Fold_Map_Any:
          (M : BsAbstract.Interface.MONOID_ANY) => {
                                                     let fold_map:
                                                       ('a => M.t('a),
                                                       t('a)) => M.t('a);
                                                   };
        module Fold_Map_Plus:
          (P : BsAbstract.Interface.PLUS) => {
                                               let fold_map:
                                                 ('a => P.t('a), t('a)) =>
                                                 P.t('a);
                                             };
        type applicative_t('a) =
            Relude_Validation.WithErrors(Errors.SemigroupAny)(Error.Type).Applicative.t('a);
        let traverse:
          ('a => applicative_t('b), t('a)) => applicative_t(t('b));
        let sequence: t(applicative_t('a)) => applicative_t(t('a));
      };
  };
module TraversableE = ArrayValidationE.Traversable;
let validateNull: json => Validation.t(unit, Errors.t);
let validateBool: json => Validation.t(bool, Errors.t);
let validateString: json => Validation.t(string, Errors.t);
let validateInt: json => Validation.t(int, Errors.t);
let validateFloat: json => Validation.t(float, Errors.t);
let getJsonAtIndex: (int, json) => option(json);
let validateJsonAtIndex:
  (int, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t);
let validateNullAtIndex: (int, json) => Validation.t(unit, Errors.t);
let validateBoolAtIndex: (int, json) => Validation.t(bool, Errors.t);
let validateIntAtIndex: (int, json) => Validation.t(int, Errors.t);
let validateFloatAtIndex: (int, json) => Validation.t(float, Errors.t);
let validateStringAtIndex: (int, json) => Validation.t(string, Errors.t);
let validateArrayOfJson:
  ((int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t);
let validateArrayOfJsonAsList:
  ((int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(list('a), Errors.t);
let validateArrayAtIndex:
  (int, (int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t);
let validateObjectAtIndex:
  (int, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t);
let getJsonForKey: (string, json) => option(json);
let validateJsonForKey:
  (string, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t);
let validateNullForKey: (string, json) => Validation.t(unit, Errors.t);
let validateBoolForKey: (string, json) => Validation.t(bool, Errors.t);
let validateIntForKey: (string, json) => Validation.t(int, Errors.t);
let validateFloatForKey: (string, json) => Validation.t(float, Errors.t);
let validateStringForKey: (string, json) => Validation.t(string, Errors.t);
let validateArrayForKey:
  (string, (int, json) => Validation.t('a, Errors.t), json) =>
  Validation.t(array('a), Errors.t);
let validateObjectForKey:
  (string, json => Validation.t('a, Errors.t), json) =>
  Validation.t('a, Errors.t);
module DSL:
  {
    let ( <$> ):
      ('a => 'b, ValidationE.Functor.t('a)) => ValidationE.Functor.t('b);
    let ( <#> ):
      (ValidationE.Functor.t('a), 'a => 'b) => ValidationE.Functor.t('b);
    let ( <*> ):
      (ValidationE.Apply.t('a => 'b), ValidationE.Apply.t('a)) =>
      ValidationE.Apply.t('b);
    let ( >>= ):
      (ValidationE.Monad.t('a), 'a => ValidationE.Monad.t('b)) =>
      ValidationE.Monad.t('b);
    module JE:
      {
        let null: json;
        let bool: bool => json;
        let int: int => json;
        let float: float => json;
        let num: float => json;
        let string: string => json;
        let array: array(json) => json;
        let arrayBy: ('a => json, array('a)) => json;
        let arrayOfDict: array(dict) => json;
        let arrayOfTuples: array((Js.Dict.key, json)) => json;
        let list: list(json) => json;
        let listBy: ('a => json, list('a)) => json;
        let listOfDict: list(dict) => json;
        let listOfTuples: list((Js.Dict.key, json)) => json;
        let dict: dict => json;
      };
    module JD:
      {
        let null: json => Validation.t(unit, Errors.t);
        let bool: json => Validation.t(bool, Errors.t);
        let int: json => Validation.t(int, Errors.t);
        let float: json => Validation.t(float, Errors.t);
        let string: json => Validation.t(string, Errors.t);
        let getAt: (int, json) => option(json);
        let jsonAt:
          (int, json => Validation.t('a, Errors.t), json) =>
          Validation.t('a, Errors.t);
        let nullAt: (int, json) => Validation.t(unit, Errors.t);
        let boolAt: (int, json) => Validation.t(bool, Errors.t);
        let stringAt: (int, json) => Validation.t(string, Errors.t);
        let intAt: (int, json) => Validation.t(int, Errors.t);
        let floatAt: (int, json) => Validation.t(float, Errors.t);
        let arrayAt:
          (int, (int, json) => Validation.t('a, Errors.t), json) =>
          Validation.t(array('a), Errors.t);
        let objectAt:
          (int, json => Validation.t('a, Errors.t), json) =>
          Validation.t('a, Errors.t);
        let array:
          ((int, json) => Validation.t('a, Errors.t), json) =>
          Validation.t(array('a), Errors.t);
        let list:
          ((int, json) => Validation.t('a, Errors.t), json) =>
          Validation.t(list('a), Errors.t);
        let getFor: (string, json) => option(json);
        let jsonFor:
          (string, json => Validation.t('a, Errors.t), json) =>
          Validation.t('a, Errors.t);
        let nullFor: (string, json) => Validation.t(unit, Errors.t);
        let boolFor: (string, json) => Validation.t(bool, Errors.t);
        let stringFor: (string, json) => Validation.t(string, Errors.t);
        let intFor: (string, json) => Validation.t(int, Errors.t);
        let floatFor: (string, json) => Validation.t(float, Errors.t);
        let arrayFor:
          (string, (int, json) => Validation.t('a, Errors.t), json) =>
          Validation.t(array('a), Errors.t);
        let objectFor:
          (string, json => Validation.t('a, Errors.t), json) =>
          Validation.t('a, Errors.t);
      };
  };
