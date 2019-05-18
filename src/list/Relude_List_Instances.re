module SemigroupAny:
  BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = (xs, ys) => Belt.List.concat(xs, ys);
};

let concat = SemigroupAny.append;

module MonoidAny: BsAbstract.Interface.MONOID_ANY with type t('a) = list('a) = {
  include SemigroupAny;
  let empty = [];
};

let empty = MonoidAny.empty;

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = list('a) = BsAbstract.List.Functor;
include Relude_Extensions_Functor.FunctorExtensions(Functor);

module Apply: BsAbstract.Interface.APPLY with type t('a) = list('a) = BsAbstract.List.Apply;
include Relude_Extensions_Apply.ApplyExtensions(Apply);

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = list('a) = BsAbstract.List.Applicative;
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

module Monad: BsAbstract.Interface.MONAD with type t('a) = list('a) = BsAbstract.List.Monad;
include Relude_Extensions_Monad.MonadExtensions(Monad);

module Alt: BsAbstract.Interface.ALT with type t('a) = list('a) = BsAbstract.List.Alt;
include Relude_Extensions_Alt.AltExtensions(Alt);

module Plus: BsAbstract.Interface.PLUS with type t('a) = list('a) = BsAbstract.List.Plus;

module Alternative:
  BsAbstract.Interface.ALTERNATIVE with type t('a) = list('a) = BsAbstract.List.Alternative;

module Foldable: BsAbstract.Interface.FOLDABLE with type t('a) = list('a) = BsAbstract.List.Foldable;
module FoldableExtensions =
  Relude_Extensions_Foldable.FoldableExtensions(Foldable);
include FoldableExtensions;

module Traversable: BsAbstract.List.TRAVERSABLE_F = BsAbstract.List.Traversable;

let rec eqBy: (('a, 'a) => bool, list('a), list('a)) => bool =
  (innerEq, a, b) =>
    switch (a, b) {
    | ([], []) => true
    | ([x, ...xs], [y, ...ys]) when innerEq(x, y) => eqBy(innerEq, xs, ys)
    | _ => false
    };

module Eq = (EqA: BsAbstract.Interface.EQ) => {
  type t = list(EqA.t);
  let eq = (xs, ys) => eqBy(EqA.eq, xs, ys);
};

let eq =
    (type a, eqA: (module BsAbstract.Interface.EQ with type t = a), xs, ys) => {
  module EqA = (val eqA);
  eqBy(EqA.eq, xs, ys);
};

let showBy: ('a => string, list('a)) => string =
  (innerShow, xs) => {
    // TODO: change to Relude_String_Types when that exists
    let join = intercalate((module BsAbstract.String.Monoid));
    "[" ++ join(", ", map(innerShow, xs)) ++ "]";
  };

module Show = (ShowA: BsAbstract.Interface.SHOW) => {
  type t = list(ShowA.t);
  let show = xs => showBy(ShowA.show, xs);
};

let show =
    (type a, showA: (module BsAbstract.Interface.SHOW with type t = a), xs) => {
  module ShowA = (val showA);
  showBy(ShowA.show, xs);
};

module IsoArray: Relude_IsoArray.ISO_ARRAY with type t('a) = list('a) = {
  type t('a) = list('a);
  let fromArray = Belt.List.fromArray;
  let toArray = Belt.List.toArray;
};

let fromArray = IsoArray.fromArray;

let toArray = IsoArray.toArray;