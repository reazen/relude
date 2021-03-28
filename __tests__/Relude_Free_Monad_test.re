open Jest;
open Expect;
open Relude.Globals;

// The storage algebra
// This is the set of operations we want to allow for accessing key/value storage
module StorageF = {
  type t('k, 'v, 'next) =
    | Get('k, option('v) => 'next)
    | Put('k, 'v, 'next)
    | Delete('k, 'next);

  // Define map, because we need this type to be a functor in order to make a free monad for it
  let map = (aToB, fa) =>
    switch (fa) {
    | Get(key, getNext) => Get(key, getNext >> aToB)
    | Put(key, value, next) => Put(key, value, aToB(next))
    | Delete(key, next) => Delete(key, aToB(next))
    };

  module WithKeyAndValue =
         (K: BsBastet.Interface.TYPE, V: BsBastet.Interface.TYPE) => {
    type nonrec t('a) = t(K.t, V.t, 'a);

    // With the key/value types locked-in, we can define our functor
    module Functor: BsBastet.Interface.FUNCTOR with type t('a) = t('a) = {
      type nonrec t('a) = t('a);
      let map = map;
    };

    // With a functor, we can create a free monad
    module FreeMonad = Relude.Free.Monad.WithFunctor(Functor);

    // Create "smart constructors" to allow for easy construction of operational values for our algebra
    let get: K.t => FreeMonad.t(option(V.t)) =
      key => FreeMonad.liftF(Get(key, a => a));

    let put: (K.t, V.t) => FreeMonad.t(unit) =
      (key, value) => FreeMonad.liftF(Put(key, value, ()));

    let delete: K.t => FreeMonad.t(unit) =
      key => FreeMonad.liftF(Delete(key, ()));

    // Include the infix operators here for ease of use
    include FreeMonad.Infix;
  };
};

// A storage API using a state monad
module StorageAPI = {
  // The state monad that we'll use to interpret the StorageF operations
  // The state is a Belt.Map.String.t(int)
  module State =
    Relude.StateT.State.WithState({
      type t = Belt.Map.String.t(int);
    });

  // Low-level operations that we'll use for our interpreter
  let getState: string => State.t(option(int)) =
    key => StateT(s => (Belt.Map.String.get(s, key), s));

  let putState: (string, int) => State.t(unit) =
    (key, value) => StateT(s => ((), Belt.Map.String.set(s, key, value)));

  let deleteState: string => State.t(unit) =
    key => StateT(s => ((), Belt.Map.String.remove(s, key)));

  // Storage algebra specialized to string key and int value
  module StorageFWithKeyAndValue =
    StorageF.WithKeyAndValue(
      {
        type t = string;
      },
      {
        type t = int;
      },
    );

  // The interpreter to interpret our algebra into a state monad
  let interpreter: StorageFWithKeyAndValue.t('a) => State.t('a) =
    storage =>
      State.Infix.(
        switch (storage) {
        | Get(key, next) => getState(key) <$$> (valueOpt => next(valueOpt))
        | Put(key, value, next) => putState(key, value) <$$> (_ => next) // Map the result into our next free operation, so we can propagate the state
        | Delete(key, next) => deleteState(key) <$$> (_ => next) // Map the result into our next free operation, so we can propagate the state
        }
      );

  // Include the smart constructors and operators for our algebra
  // This gets us our get/put/delete functions and things like <$>/<$$>/>>=/<*>/etc.
  // For use when creating our programs
  include StorageFWithKeyAndValue;

  // Include foldFree function, which requires knowledge of the monad we're interpreting
  // into
  include StorageFWithKeyAndValue.FreeMonad.WithMonad(State.Monad);
};

describe("Relude_Free_Monad", () => {
  describe("StorageAPI", () => {
    test("StorageState interpreter", () => {
      // This is our monadic program stored as our free monad
      let program =
        StorageAPI.(
          put("key1", 42)
          >>= (_ => get("key1"))
          >>= (
            value1 =>
              delete("key1")
              >>= (_ => put("key2", 99))
              >>= (_ => put("key1", 5))
              >>= (_ => get("key1"))
              >>= (
                value2 =>
                  get("key2") <$$> (value3 => (value1, value2, value3))
              )
          )
        );

      // Interpret the program using our state-monad-based interpreter.
      // Then run the resulting state monad.
      let (results, _) =
        StorageAPI.foldFree(StorageAPI.interpreter, program)
        |> StorageAPI.State.runStateT(Belt.Map.String.empty);

      expect(results) |> toEqual((Some(42), Some(5), Some(99)));
    })
  })
});
