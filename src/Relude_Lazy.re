
type deferred('a) = {
  f: unit => 'a,
};

type deferredU('a) = {
  f: (. unit) => 'a,
};

type t('a) = [
  | `Pure('a)
  | `Deferred(deferred('a))
  | `DeferredU(deferredU('a))
];

let force: t('a) => 'a = fun
  | `Pure(a) => a
  | `Deferred(lazyA) => lazyA.f()
  | `DeferredU(lazyA) => lazyA.f(. );

let identity: t('a) => t('a) = value => value;

let pure: 'a => t('a) = a => `Pure(a);

let defer: (unit => 'a) => t('a) = unitToA => `Deferred({f: unitToA});

let deferU: ((. unit) => 'a) => t('a) = unitToA => `DeferredU({f: unitToA});

let join: (t(t('a)) => t('a)) = force;

let map: ('a => 'b, t('a)) => t('b) = (aToB, mA) => `DeferredU({ f: (. ) => aToB(force(mA)) });

let mapU: ((. 'a) => 'b, t('a)) => t('b) = (aToB, mA) => `DeferredU({ f: (. ) => aToB(. force(mA)) });

// let flatMap: ('a => t('b), t('a)) => t('b) = (aToMB, mA) => `DeferredU((. ) => aToMB(force(mA)));

let flatMapU: ((. 'a) => t('b), t('a)) => t('b) = (aToMB, mA) => `DeferredU({ f: (. ) => join(aToMB(. force(mA))) });

let bind: (t('a), 'a => t('b)) => t('b) = (mA, aToMB) => `DeferredU({ f: (. ) => force(aToMB(force(mA))) });

let bindU: (t('a), (. 'a) => t('b)) => t('b) = (mA, aToMB) => `DeferredU({ f: (. ) => force(aToMB(. force(mA))) });

let apply: (t('a => 'b), t('a)) => t('b) = (mAtoB, mA) => `DeferredU({ f: (. ) => force(mAtoB)(force(mA)) })

let applyU: (t((. 'a) => 'b), t('a)) => t('b) = (mAtoB, mA) => `DeferredU({ f: (. ) => force(mAtoB)(. force(mA)) })

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
let map = Functor.map;
include Relude_Extensions_Functor.FunctorExtensions(Functor);

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
let apply = Apply.apply;
include Relude_Extensions_Apply.ApplyExtensions(Apply);

module Applicative:
  BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
let pure = Applicative.pure;
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
let bind = Monad.flat_map;
include Relude_Extensions_Monad.MonadExtensions(Monad);
