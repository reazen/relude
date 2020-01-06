/**
 * For demonstration purposes.
 * 
 * This monadic interface could make use of OCaml's built-in Lazy module, as
 * shown below.
 * 
 * The upside is that it would interoperate with existing uses of
 * that Lazy module, which allows use of the `lazy` keyword.
 * 
 * The downside is that it doesn't really fit neatly into the Relude way
 * of doing error handling, since several functions can throw exceptions, and
 * it also handles propogated exceptions in its own unique way (exceptions
 * are rethrown each time a lazy value is forced, if it threw an exception
 * the last time it was forced). That behavior may or may not be desireable.
 */


type t('a) = Lazy.t('a);

let force: t('a) => 'a = Lazy.force;

let pure_: 'a => t('a) = Lazy.from_val;

let defer: (unit => 'a) => t('a) = Lazy.from_fun;

let identity: t('a) => t('a) = a => a;

let map: ('a => 'b, t('a)) => t('b) = (aToB, a) => lazy(aToB(force(a)));

let join: t(t('a)) => t('a) = force;

let apply: (t('a => 'b), t('a)) => t('b) = (mAtoB, mA) => defer(() => force(mAtoB)(force(mA)));

let flatMap: ('a => t('b), t('a)) => t('b) = (aToMB, mA) => defer(() => force(aToMB(force(mA))));

let bind: (t('a), 'a => t('b)) => t('b) = (mA, aToMB) => defer(() => force(aToMB(force(mA))));


