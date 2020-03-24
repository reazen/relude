open BsBastet.Interface;

/**
 * Ior is similar to result, but it has the ability to collect "non-fatal
 * warning" information during applicative validation.
 *
 * E.g. if you are doing applicative validation to construct a User model, you
 * could parse a phone number and allow it, but collect a warning that certain
 * phone number formats are deprecated.
 */
include Relude_Ior_Type;

/**
 * Constructs a This value with the given success value
 */
let this: 'a => t('a, 'b) = a => This(a);

/**
 * Constructs an That value with the given that value
 */
let that: 'b => t('a, 'b) = b => That(b);

/**
 * Constructs an Both value with the given this and that values
 */
let both: ('a, 'b) => t('a, 'b) = (a, b) => Both(a, b);

/**
 * Indicates if the Ior is This with any value
 */
let isThis: 'a 'b. t('a, 'b) => bool =
  fun
  | This(_) => true
  | That(_) => false
  | Both(_, _) => false;

/**
 * Indicates if the Ior is That with any value
 */
let isThat: 'a 'b. t('a, 'b) => bool =
  fun
  | This(_) => false
  | That(_) => true
  | Both(_, _) => false;

/**
 * Indicates if the Ior is Both with any values
 */
let isBoth: 'a 'b. t('a, 'b) => bool =
  fun
  | This(_) => false
  | That(_) => false
  | Both(_, _) => true;

/**
 * Gets the 'a value out of the Ior
 */
let getThis: 'a 'b. t('a, 'b) => option('a) =
  fun
  | This(a) => Some(a)
  | That(_) => None
  | Both(a, _) => Some(a);

/**
 * Gets the 'b value out of the Ior
 */
let getThat: 'a 'b. t('a, 'b) => option('b) =
  fun
  | This(_) => None
  | That(b) => Some(b)
  | Both(_, b) => Some(b);

/**
 * Partitions a list of Iors into lists of the values for each constructor
 */
let partition:
  'a 'b.
  list(t('a, 'b)) => (list('a), list('b), list(('a, 'b)))
 =
  iors =>
    Relude_List.foldRight(
      (ior, (as_, bs, boths)) =>
        switch (ior) {
        | This(a) => ([a, ...as_], bs, boths)
        | That(b) => (as_, [b, ...bs], boths)
        | Both(a, b) => (as_, bs, [(a, b), ...boths])
        },
      ([], [], []),
      iors,
    );

/**
 * Extracts that 'a values out of a list of Iors
 */
let catThis: 'a 'b. list(t('a, 'b)) => list('a) =
  iors =>
    Relude_List.foldRight(
      (ior, acc) =>
        switch (ior) {
        | This(a) => [a, ...acc]
        | That(_) => acc
        | Both(a, _) => [a, ...acc]
        },
      [],
      iors,
    );

/**
 * Extracts that 'b values out of a list of Iors
 */
let catThat: 'a 'b. list(t('a, 'b)) => list('b) =
  iors =>
    Relude_List.foldRight(
      (ior, acc) =>
        switch (ior) {
        | This(_) => acc
        | That(b) => [b, ...acc]
        | Both(_, b) => [b, ...acc]
        },
      [],
      iors,
    );

/**
 * Maps a pure function over the `this` channel of the Ior
 */
let mapThis: 'a 'b 'c. ('a => 'c, t('a, 'b)) => t('c, 'b) =
  (f, fa) =>
    switch (fa) {
    | That(_) as e => e
    | This(a) => This(f(a))
    | Both(a, b) => Both(f(a), b)
    };

/**
 * Alias for `mapThis`
 */
let map = mapThis;

/**
 * Maps a pure function over the `that` channel of the Ior
 */
let mapThat: 'a 'b 'c. ('b => 'c, t('a, 'b)) => t('a, 'c) =
  (f, fa) =>
    switch (fa) {
    | This(_) as a => a
    | That(b) => That(f(b))
    | Both(a, b) => Both(a, f(b))
    };

/**
 * Applies a side effect function if the value is This, That, or Both
 */
let tap:
  'a 'b.
  ('a => unit, 'b => unit, ('a, 'b) => unit, t('a, 'b)) => t('a, 'b)
 =
  (ifThis, ifThat, ifBoth, fa) =>
    switch (fa) {
    | This(a) =>
      ifThis(a);
      fa;
    | That(e) =>
      ifThat(e);
      fa;
    | Both(a, e) =>
      ifBoth(a, e);
      fa;
    };

/**
 * Applies a side-effect function if the value is This
 */
let tapThis: 'a 'b. ('a => unit, t('a, 'b)) => t('a, 'b) =
  (ifThis, fa) =>
    switch (fa) {
    | This(a) =>
      ifThis(a);
      fa;
    | That(_) => fa
    | Both(_, _) => fa
    };

/**
 * Applies a side-effect function if the value is That
 */
let tapThat: 'a 'b. ('b => unit, t('a, 'b)) => t('a, 'b) =
  (ifThat, fa) =>
    switch (fa) {
    | This(_) => fa
    | That(e) =>
      ifThat(e);
      fa;
    | Both(_, _) => fa
    };

/**
 * Applies a side-effect function if the value is Both
 */
let tapBoth: 'a 'b. (('a, 'b) => unit, t('a, 'b)) => t('a, 'b) =
  (ifBoth, fa) =>
    switch (fa) {
    | This(_) => fa
    | That(_) => fa
    | Both(a, e) =>
      ifBoth(a, e);
      fa;
    };

/**
 * Applies a side effect function to the 'a value in an This or an Both
 */
let tapThisOrBoth: 'a 'b. ('a => unit, t('a, 'b)) => t('a, 'b) =
  (ifThis, fa) =>
    switch (fa) {
    | This(a) =>
      ifThis(a);
      fa;
    | That(_) => fa
    | Both(a, _) =>
      ifThis(a);
      fa;
    };

/**
 * Applies a side effect function to the 'b value in an That or an Both
 */
let tapThatOrBoth: 'a 'b. ('b => unit, t('a, 'b)) => t('a, 'b) =
  (ifThat, fa) =>
    switch (fa) {
    | This(_) => fa
    | That(e) =>
      ifThat(e);
      fa;
    | Both(_, e) =>
      ifThat(e);
      fa;
    };

/**
 * Applies a wrapped function to the success channel, with `that` collecting semantics.
 */
let applyWithAppendThats:
  (('b, 'b) => 'b, t('a => 'c, 'b), t('a, 'b)) => t('c, 'b) =
  (appendThats, ff, fa) =>
    switch (ff, fa) {
    | (This(f), This(a)) => This(f(a))
    | (This(_), That(b)) => That(b)
    | (This(f), Both(a, b)) => Both(f(a), b)
    | (That(b), This(_)) => That(b)
    | (That(b1), That(b2)) => That(appendThats(b1, b2))
    | (That(b1), Both(_, b2)) => That(appendThats(b1, b2))
    | (Both(f, b1), This(a)) => Both(f(a), b1)
    | (Both(_, b1), That(b2)) => That(appendThats(b1, b2))
    | (Both(f, b1), Both(a, b2)) => Both(f(a), appendThats(b1, b2))
    };

/**
 * Lifts a pure value into an This
 */
let pure: 'a 'b. 'a => t('a, 'b) = a => This(a);

/**
 * Applies a monadic function to the success channel of the Ior
 */
let bind: (t('a, 'b), 'a => t('c, 'b)) => t('c, 'b) =
  (fa, f) =>
    switch (fa) {
    | This(a) => f(a)
    | That(_) as that => that
    | Both(a, _) => f(a)
    };

/**
 * Applies a monadic function to the success channel of the Ior
 */
let flatMap: ('a => t('c, 'b), t('a, 'b)) => t('c, 'b) =
  (f, fa) => bind(fa, f);

/**
 * Maps the results of 2 Ior values using the given function
 */
let map2:
  (('x, 'x) => 'x, ('a, 'b) => 'c, t('a, 'x), t('b, 'x)) => t('c, 'x) =
  (appendThats, f, fa, fb) =>
    applyWithAppendThats(appendThats, map(f, fa), fb);

/**
 * Maps the results of 3 Ior values using the given function
 */
let map3:
  (('x, 'x) => 'x, ('a, 'b, 'c) => 'd, t('a, 'x), t('b, 'x), t('c, 'x)) =>
  t('d, 'x) =
  (appendThats, f, fa, fb, fc) =>
    applyWithAppendThats(appendThats, map2(appendThats, f, fa, fb), fc);

/**
 * Maps the results of 4 Ior values using the given function
 */
let map4:
  (
    ('x, 'x) => 'x,
    ('a, 'b, 'c, 'd) => 'e,
    t('a, 'x),
    t('b, 'x),
    t('c, 'x),
    t('d, 'x)
  ) =>
  t('e, 'x) =
  (appendThats, f, fa, fb, fc, fd) =>
    applyWithAppendThats(appendThats, map3(appendThats, f, fa, fb, fc), fd);

/**
 * Maps the results of 5 Ior values using the given function
 */
let map5:
  (
    ('x, 'x) => 'x,
    ('a, 'b, 'c, 'd, 'e) => 'f,
    t('a, 'x),
    t('b, 'x),
    t('c, 'x),
    t('d, 'x),
    t('e, 'x)
  ) =>
  t('f, 'x) =
  (appendThats, f, fa, fb, fc, fd, fe) =>
    applyWithAppendThats(
      appendThats,
      map4(appendThats, f, fa, fb, fc, fd),
      fe,
    );

/**
 * Folds an Ior into a value by applying a function for each case
 */
let fold: 'a 'b 'c. ('a => 'c, 'b => 'c, ('a, 'b) => 'c, t('a, 'b)) => 'c =
  (aToC, bToC, abToC, ior) =>
    switch (ior) {
    | This(a) => aToC(a)
    | That(b) => bToC(b)
    | Both(a, b) => abToC(a, b)
    };

/**
 * Converts an Ior into a tuple by filling in default values if needed
 */
let toTuple: 'a 'b. ('a, 'b, t('a, 'b)) => ('a, 'b) =
  (defaultA, defaultB, ior) =>
    switch (ior) {
    | This(a) => (a, defaultB)
    | That(b) => (defaultA, b)
    | Both(a, b) => (a, b)
    };

/**
 * Collapses an Ior containing two same-typed values into a value of that type, combining the values if needed
 */
let merge: 'a. (('a, 'a) => 'a, t('a, 'a)) => 'a =
  (f, ior) =>
    switch (ior) {
    | This(a) => a
    | That(a) => a
    | Both(a1, a2) => f(a1, a2)
    };

/**
 * Collapses an Ior by converting each side into a same-typed value, and combining them if needed
 */
let mergeWith: 'a 'b 'c. ('a => 'c, 'b => 'c, ('c, 'c) => 'c, t('a, 'b)) => 'c =
  (aToC, bToC, ccToC, ior) =>
    switch (ior) {
    | This(a) => aToC(a)
    | That(b) => bToC(b)
    | Both(a, b) => ccToC(aToC(a), bToC(b))
    };

/**
 * Creates a module that locks in the That TYPE and SEMIGROUP_ANY modules, so
 * that we can implement the typeclass instances for the single-type-parameter typeclasses.
 */
module WithThats = (Thats: SEMIGROUP_ANY, That: TYPE) => {
  module Functor: FUNCTOR with type t('a) = t('a, Thats.t(That.t)) = {
    type nonrec t('a) = t('a, Thats.t(That.t));
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);

  module Apply: APPLY with type t('a) = t('a, Thats.t(That.t)) = {
    include Functor;
    let apply = (ff, fa) => applyWithAppendThats(Thats.append, ff, fa);
  };
  let apply = Apply.apply;
  include Relude_Extensions_Apply.ApplyExtensions(Apply);

  module Applicative: APPLICATIVE with type t('a) = t('a, Thats.t(That.t)) = {
    include Apply;
    let pure = pure;
  };
  let pure = Applicative.pure;
  include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

  module Monad: MONAD with type t('a) = t('a, Thats.t(That.t)) = {
    include Applicative;
    let flat_map = bind;
  };
  let bind = Monad.flat_map;
  include Relude_Extensions_Monad.MonadExtensions(Monad);

  module Infix = {
    include Relude_Extensions_Functor.FunctorInfix(Functor);
    include Relude_Extensions_Apply.ApplyInfix(Apply);
    include Relude_Extensions_Monad.MonadInfix(Monad);
  };
};
