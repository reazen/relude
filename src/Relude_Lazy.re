
type t('a) =
  | Thunk('a)
  | Deferred(unit => 'a)
  | DeferredU((. unit) => 'a);

let pure: 'a => t('a) = (value) => Thunk(value);

let defer: (unit => 'a) => t('a) = (fn) => Deferred(fn);

let deferU: ((. unit) => 'a) => t('a) = (fn) => DeferredU(fn);

let identity: t('a) => t('a) = value => value;

let force: t('a) => 'a = fun
    | Thunk(v) => v
    | Deferred(fn) => fn()
    | DeferredU(fn) => fn(. );

let forceIdentity: t('a) => t('a) = fun
    | Thunk(v) => Thunk(v)
    | Deferred(fn) => Thunk(fn())
    | DeferredU(fn) => Thunk(fn(. ));

let join: t(t('a)) => t('a) = force;

let map: ('a => 'b, t('a)) => t('b) = (aToB, mA) => deferU((. ) => aToB(force(mA)));

let mapU: ((. 'a) => 'b, t('a)) => t('b) = (aToB, mA) => deferU((. ) => aToB(. force(mA)));

let forceMap: ('a => 'b, t('a)) => t('b) = (aToB, mA) => pure(aToB(force(mA)));

let forceMapU: ((. 'a) => 'b, t('a)) => t('b) = (aToB, mA) => pure(aToB(. force(mA)));

// let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c) = (abToC, mA, mB) => deferU((. ) => abToC(force(mA), force(mB)));

let forceMap2: (('a, 'b) => 'c, t('a), t('b)) => t('c) = (abToC, mA, mB) => pure(abToC(force(mA), force(mB)));

let apply: (t('a => 'b), t('a)) => t('b) = (mAtoB, mA) => deferU((. ) => force(mAtoB)(force(mA)));

let applyU: (t((. 'a) => 'b), t('a)) => t('b) = (mAtoB, mA) => deferU((. ) => force(mAtoB)(. force(mA)));

let forceApply: (t('a => 'b), t('a)) => t('b) = (mAtoB, mA) => pure(force(mAtoB)(force(mA)));

let forceApplyU: (t((. 'a) => 'b), t('a)) => t('b) = (mAtoB, mA) => pure(force(mAtoB)(. force(mA)));

// let flatMap: ('a => t('b), t('a)) => t('b) = (aToMB, mA) => deferU((. ) => force(aToMB(force(mA))));

let flatMapU: ((. 'a) => t('b), t('a)) => t('b) = (aToMB, mA) => deferU((. ) => force(aToMB(. force(mA))));

let forceFlatMap: ('a => t('b), t('a)) => t('b) = (aToMB, mA) => aToMB(force(mA));

let forceFlatMapU: ((. 'a) => t('b), t('a)) => t('b) = (aToMB, mA) => aToMB(. force(mA));

let bind: (t('a), 'a => t('b)) => t('b) = (mA, aToMB) => deferU((. ) => force(aToMB(force(mA))));

let bindU: (t('a), (. 'a) => t('b)) => t('b) = (mA, aToMB) => deferU((. ) => force(aToMB(. force(mA))));

let forceBind: (t('a), 'a => t('b)) => t('b) = (mA, aToMB) => aToMB(force(mA));

let forceBindU: (t('a), (. 'a) => t('b)) => t('b) = (mA, aToMB) => aToMB(. force(mA));

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
