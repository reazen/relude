open BsBastet.Interface;

/**
[WriterLog] contains a basic typeclass that has a log type [t] and a Monoid
for [t] for use as a log.  Not all writer typeclass instances require a
monoid, but I'm going to normalize on it here for simplicity, and to avoid
even more module functor noise.  This also contains base implementation for
List and Array, which just need to be provided the entry type in order to be
used.
*/
module WriterLog = {
  module type LOG = {
    type t;
    module Monoid: MONOID with type t = t;
  };

  /**
  Writer log that is backed by a List of entries of type Entry.t
  */
  module List = {
    module type LIST_LOG = (Entry: TYPE) => LOG with type t = list(Entry.t);

    module WithEntry: LIST_LOG =
      (Entry: TYPE) => {
        type t = list(Entry.t);
        module Monoid: MONOID with type t = list(Entry.t) = {
          type t = list(Entry.t);
          let empty = [];
          let append = Relude_List.concat;
        };
      };
  };

  /**
  Writer log that is backed by a Array of entries of type Entry.t
  */
  module Array = {
    module type ARRAY_LOG =
      (Entry: TYPE) => LOG with type t = array(Entry.t);

    module WithEntry: ARRAY_LOG =
      (Entry: TYPE) => {
        type t = array(Entry.t);
        module Monoid: MONOID with type t = array(Entry.t) = {
          type t = array(Entry.t);
          let empty = [||];
          let append = Relude_Array.concat;
        };
      };
  };
};

/**
Creates a Writer monad with the given inner monad
*/
module WithMonad = (Monad: MONAD) => {
  type t('a, 'w) =
    // 'a on the left for consistency with Result/etc.
    | WriterT(Monad.t(('a, 'w)));

  let make: 'w 'a. Monad.t(('a, 'w)) => t('a, 'w) =
    mWriter => WriterT(mWriter);

  /**
  Run the writer, and get the result value a, and the log w
  */
  let runWriterT: 'w 'a. t('a, 'w) => Monad.t(('a, 'w)) =
    (WriterT(mWriter)) => mWriter;

  /**
  Run the writer, and discard the result value a, only returning the log w
  */
  let execWriterT: 'w 'a. t('a, 'w) => Monad.t('w) =
    (WriterT(mWriter)) => Monad.map(snd, mWriter);

  /**
  Maps a inner monad-converting function over the WriterT.
  Note: this is not exactly the same as purescript/haskell in that we don't allow monad changes here
  for the sake of simplicity.
  */
  let mapWriterT:
    'w 'a 'b.
    (Monad.t(('a, 'w)) => Monad.t(('b, 'w)), t('a, 'w)) => t('b, 'w)
   =
    (mAToMB, WriterT(mA)) => WriterT(mAToMB(mA));

  /**
  Writes a single value to the log
  */
  let tell: 'w. 'w => t(unit, 'w) = w => WriterT(Monad.pure(((), w)));

  /**
  Surfaces the current value of the log w along with the current result value a in a tuple.
  */
  let listen: 'w 'a. t('a, 'w) => t(('a, 'w), 'w) =
    (WriterT(mAW)) => WriterT(Monad.map(((a, w)) => ((a, w), w), mAW));

  /**
  Applies a log-modifying function provided in the result tuple to the log
  */
  let pass: 'w 'a. t(('a, 'w => 'w), 'w) => t('a, 'w) =
    (WriterT(m)) =>
      WriterT(Monad.map((((a, wToW), w)) => (a, wToW(w)), m));

  /**
  Surfaces the current value of the log using a function to transform it.
  */
  let listens: 'w1 'w2 'a. ('w1 => 'w2, t('a, 'w1)) => t(('a, 'w2), 'w1) =
    (w1ToW2, writerTMAW1) => {
      let WriterT(m) = listen(writerTMAW1);
      WriterT(Monad.map((((a, w1), _)) => ((a, w1ToW2(w1)), w1), m));
    };

  /**
  Modifies the log
  */
  let censor: 'w 'a. ('w => 'w, t('a, 'w)) => t('a, 'w) =
    (wToW, WriterT(mAW)) => {
      pass(WriterT(Monad.map(((a, w)) => ((a, wToW), w), mAW)));
    };

  /**
  Maps a function over the result value a of the WriterT.  No change is made to the log.
  */
  let map: 'w 'a 'b. ('a => 'b, t('a, 'w)) => t('b, 'w) =
    (aToB, WriterT(mAW)) =>
      WriterT(Monad.map(((a, w)) => (aToB(a), w), mAW));

  /**
  Applies a writer-wrapped function to the result value a of the WriterT.
  The log values for the Writer-wrapped function and the Writer-wrapped value a are appended in the resulting Writer.
  */
  let applyWithAppendLog:
    'w 'a 'b.
    (('w, 'w) => 'w, t('a => 'b, 'w), t('a, 'w)) => t('b, 'w)
   =
    (appendLog, WriterT(mAToBW), WriterT(mAW)) =>
      WriterT(
        Monad.apply(
          Monad.map(
            ((aToB, w1), (a, w2)) => (aToB(a), appendLog(w1, w2)),
            mAToBW,
          ),
          mAW,
        ),
      );

  /**
  Constructs a Writer with the given value a, and an empty log
  */
  let pureWithEmptyLog: 'w 'a. ('w, 'a) => t('a, 'w) =
    (empty, a) => WriterT(Monad.pure((a, empty)));

  /**
  Applies an effectful function to the value a, and appends the original log with the new log produced
  by the effectful function.
  */
  let bindWithAppendLog:
    'w 'a 'b.
    (('w, 'w) => 'w, t('a, 'w), 'a => t('b, 'w)) => t('b, 'w)
   =
    (appendLog, WriterT(mAW), aToWriterTBW) => {
      WriterT(
        Monad.flat_map(
          mAW,
          ((a, w1)) => {
            let WriterT(mBW) = aToWriterTBW(a);
            Monad.map(((b, w2)) => (b, appendLog(w1, w2)), mBW);
          },
        ),
      );
    };

  /**
  In order to implement the common typeclasses, we need to know how to append log entries (in most cases).
  In Haskell/Purescript/Scala/etc. many of the typeclass instances depend on various other specific typeclasses,
  but here, we are just normalizing on WriterLog, which contains a type for collecting logs, and a Monoid for the type.
  */
  module WithLog = (Log: WriterLog.LOG) => {
    let make = make;
    let runWriterT = runWriterT;
    let execWriterT = execWriterT;
    let mapWriterT = mapWriterT;
    let tell = tell;
    let listen = listen;
    let listens = listens;
    let pass = pass;
    let censor = censor;

    module Functor: FUNCTOR with type t('a) = t('a, Log.t) = {
      type nonrec t('a) = t('a, Log.t);
      let map = map;
    };
    let map = Functor.map;
    include Relude_Extensions_Functor.FunctorExtensions(Functor);

    module Apply: APPLY with type t('a) = t('a, Log.t) = {
      include Functor;
      let apply = (ff, fa) => applyWithAppendLog(Log.Monoid.append, ff, fa);
    };
    let apply = Apply.apply;
    include Relude_Extensions_Apply.ApplyExtensions(Apply);

    module Applicative: APPLICATIVE with type t('a) = t('a, Log.t) = {
      include Apply;
      let pure = a => pureWithEmptyLog(Log.Monoid.empty, a);
    };
    let pure = Applicative.pure;
    include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

    module Monad: MONAD with type t('a) = t('a, Log.t) = {
      include Applicative;
      let flat_map = (fa, f) => bindWithAppendLog(Log.Monoid.append, fa, f);
    };
    let bind = Monad.flat_map;
    include Relude_Extensions_Monad.MonadExtensions(Monad);

    module Infix = {
      include Relude_Extensions_Functor.FunctorInfix(Functor);
      include Relude_Extensions_Apply.ApplyInfix(Apply);
      include Relude_Extensions_Monad.MonadInfix(Monad);
    };
  };
};

/**
Convenience constructor for when you know what Monad and Log type you want to use
*/
module WithMonadAndLog = (Monad: MONAD, Log: WriterLog.LOG) => {
  module WithMonad = WithMonad(Monad);
  include WithMonad.WithLog(Log);
};

/**
Basic Writer using the Identity monad.

In order to use this, you must still provide the Log type info,
like [module Writer = Writer.WithLog(...)]
*/
module Writer = WithMonad(Relude_Identity.Monad);
