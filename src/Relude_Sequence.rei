module type SEQUENCE =
  {
    type t('a);
    let length: t('a) => int;
    let isEmpty: t('a) => bool;
    let isNotEmpty: t('a) => bool;
    let head: t('a) => option('a);
    let tail: t('a) => option(t('a));
    let tailOrEmpty: t('a) => t('a);
    let mkString: (string, t(string)) => string;
    let eqBy: (('a, 'a) => bool, t('a), t('a)) => bool;
    let showBy: ('a => string, t('a)) => string;
    module MonoidAny:
      {
        type nonrec t('a) = t('a);
        let append: (t('a), t('a)) => t('a);
        let empty: t('a);
      };
    module Monad:
      {
        type nonrec t('a) = t('a);
        let map: ('a => 'b, t('a)) => t('b);
        let apply: (t('a => 'b), t('a)) => t('b);
        let pure: 'a => t('a);
        let flat_map: (t('a), 'a => t('b)) => t('b);
      };
    module Foldable:
      {
        type nonrec t('a) = t('a);
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
      };
    module Traversable:
      (A : BsAbstract.Interface.APPLICATIVE) => {
                                                  type nonrec t('a) = t('a);
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
                                                          t('a)) => M.t;
                                                      };
                                                  module Fold_Map_Any:
                                                    (M : BsAbstract.Interface.MONOID_ANY) => 
                                                      {
                                                        let fold_map:
                                                          ('a => M.t('a),
                                                          t('a)) => M.t('a);
                                                      };
                                                  module Fold_Map_Plus:
                                                    (P : BsAbstract.Interface.PLUS) => 
                                                      {
                                                        let fold_map:
                                                          ('a => P.t('a),
                                                          t('a)) => P.t('a);
                                                      };
                                                  type applicative_t('a) =
                                                      A.t('a);
                                                  let traverse:
                                                    ('a => applicative_t('b),
                                                    t('a)) =>
                                                    applicative_t(t('b));
                                                  let sequence:
                                                    t(applicative_t('a)) =>
                                                    applicative_t(t('a));
                                                };
    module Eq:
      (EqA : BsAbstract.Interface.EQ) => {
                                           type nonrec t = t(EqA.t);
                                           let eq: (t, t) => bool;
                                         };
    module Show:
      (ShowA : BsAbstract.Interface.SHOW) => {
                                               type nonrec t = t(ShowA.t);
                                               let show: t => string;
                                             };
  };
