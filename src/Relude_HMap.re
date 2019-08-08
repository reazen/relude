// Inspired by https://github.com/dbuenzli/hmap

/**
 * Witness contains a type t with an existential type variable - a type that we
 * capture, but lose knowledge of once captured.
 * `t(_)` is an extensible variant type, indicated by the `..`.  This is used below in WITNESS
 * so that we can add new cases to the Witness.t(_) type on demand, so that we can create witnesses
 * for any new types we encounter going forward.
 * https://caml.inria.fr/pub/docs/manual-ocaml/manual037.html
 */
module Witness = {
  type t(_) = ..;
};

/**
 * WITNESS captures a type `t` with a `Witness` module for that type.
 * Because Witness.t(_) is an extensible variant, we use `+=` to add a constructor
 * to that type.
 * This is useful because the type t is unified with the type t hidden inside the
 * Witness module.
 */
module type WITNESS = {
  type t;
  type Witness.t(_) +=
    | Witness: Witness.t(t);
};

/**
 * witness('a) is a type which captures a WITNESS module for the given type 'a.
 */
type witness('a) = (module WITNESS with type t = 'a);

/**
 * makeWitness makes a Witness module for the given type a
 */
let makeWitness = (type a, ()): (module WITNESS with type t = a) => {
  module Witness = {
    type t = a;
    type Witness.t(_) +=
      | Witness: Witness.t(t);
  };
  ((module Witness): (module WITNESS with type t = a));
};

/**
 * typeEq('a, 'b) contains a single constructor TypeEq which can only be constructed if 'a and 'b are the same types.
 * This is used for a type-level type equality check.
 */
type typeEq('a, 'b) =
  | TypeEq: typeEq('a, 'a);

/**
 * eq checks whether the give witness types are equal
 */
let typeEq: type l r. (witness(l), witness(r)) => option(typeEq(l, r)) =
  (l, r) => {
    module L = (val l: WITNESS with type t = l);
    module R = (val r: WITNESS with type t = r);
    switch (L.Witness) {
    | R.Witness => Some(TypeEq)
    | _ => None
    };
  };

/**
 * KEY_TYPE is a module type signature which captures the type of a map key
 */
module type KEY_META = {type t('a);};

/**
 * MAP_TYPE is a module type signature which captures the types and functions exposed
 * by the HMap
 */
module type HMAP_TYPE = {
  /**
   * An abstract type for map keys
   */
  type keyImpl('a);

  /**
   * Key-related types and operations for an HMap
   */
  module Key: {
    /**
     * keyMeta('a) is the metadata we store with the key, like labels, and other fucntions for
     * operating on the otherwise existential 'a value
     */
    type keyMeta('a);

    /**
     * Creates a keyImpl (an actual map key) for the given key metadata
     */
    let create: keyMeta('a) => keyImpl('a);

    /**
     * Extracts the key metadata from a keyImpl (an actual map key)
     */
    let keyMeta: keyImpl('a) => keyMeta('a);

    /**
     * The abstract type of our map key
     */
    type t;

    /**
     * Wraps the type-aware keyImpl('a) into the abstract key type
     */
    let hideType: keyImpl('a) => t;

    /**
     * Checks if two keys are equal
     */
    let eq: (t, t) => bool;

    /**
     * Compares two keys
     */
    let compare: (t, t) => int;
  };

  type t;
  let empty: t;
  let isEmpty: t => bool;
  let hasKey: (keyImpl('a), t) => bool;
  let add: (keyImpl('a), 'a, t) => t;
  let singleton: (keyImpl('a), 'a) => t;
  let remove: (keyImpl('a), t) => t;
  let find: (keyImpl('a), t) => option('a);

  type keyValue =
    | KeyValue(keyImpl('a), 'a): keyValue;

  let forEach: (keyValue => unit, t) => unit;
  let fold: ((keyValue, 'a) => 'a, 'a, t) => 'a;
  let all: (keyValue => bool, t) => bool;
  let any: (keyValue => bool, t) => bool;
  let filter: (keyValue => bool, t) => t;
  let size: t => int;
};

/**
 * Make creates a Map module for the given KEY_META.
 *
 * KEY_META contains extra information to store with the key, like labels, and functions for
 * operating on the values contained with each key-value pair.
 */
module Make =
       (KeyMeta: KEY_META)
       : (HMAP_TYPE with type Key.keyMeta('a) = KeyMeta.t('a)) => {
  module Key = {
    /**
     * The type of metadata we store with the key
     */
    type keyMeta('a) = KeyMeta.t('a);

    /**
     * keyImpl('a) captures a unique int identifier for the given key type, a witness, and the key meta value
     */
    type keyImpl('a) = {
      intId: int,
      witness: witness('a),
      keyMeta: keyMeta('a),
    };

    /**
     * An impure function that produces monotonically increasing int values.
     */
    let uniqueInt: unit => int = {
      let id = ref(-1);
      () => {
        incr(id);
        id^;
      };
    };

    /**
     * Creates a keyImpl('a) value for the given keyMeta('a)
     *
     * This is used to create actual keys that we can use to add values to the HMap, and retrieve values
     * from the HMap.
     */
    let create: keyMeta('a) => keyImpl('a) =
      keyMeta => {
        let intId = uniqueInt();
        let witness = makeWitness();
        {intId, witness, keyMeta};
      };

    /**
     * Gets the keyType for a given keyData value
     */
    let keyMeta: keyImpl('a) => keyMeta('a) = keyData => keyData.keyMeta;

    /**
     * The abstract Key type containing the existential key type
     */
    type t =
      | Key(keyImpl('a)): t;

    /**
     * hideType wraps the keyData in a structure which hides the key type 'a using
     * an existential.
     */
    let hideType: keyImpl('a) => t = k => Key(k);

    /**
     * Compares keys for equality using the unique int ID assigned to the key type
     */
    let eq: (t, t) => bool =
      (Key(a), Key(b)) =>
        (compare: (int, int) => int)(a.intId, b.intId) == 0;

    /**
     * Compares keys using the unique int ID assigned to the key type
     */
    let compare: (t, t) => int =
      (Key(a), Key(b)) => (compare: (int, int) => int)(a.intId, b.intId);
  };

  type keyImpl('a) = Key.keyImpl('a);

  /**
   * Create our HMap with the OCaml Map type, using our Key
   */
  module HMap = Map.Make(Key);

  /**
   * A type which captures a type-aware key, and the corresponding value.
   * The type is existential, so we rely on embedded key metadata to operate on that type if needed.
   */
  type keyValue =
    | KeyValue(keyImpl('a), 'a): keyValue;

  type t = HMap.t(keyValue);

  let empty = HMap.empty;

  let isEmpty = HMap.is_empty;

  let hasKey = (k, m) => HMap.mem(Key.Key(k), m);

  let add = (k, v, m) => HMap.add(Key.Key(k), KeyValue(k, v), m);

  let singleton = (k, v) => HMap.singleton(Key.Key(k), KeyValue(k, v));

  let remove = (k, m) => HMap.remove(Key.Key(k), m);

  let find: type a. (keyImpl(a), t) => option(a) =
    (k, s) =>
      try (
        switch (HMap.find(Key.Key(k), s)) {
        | KeyValue(k', v) =>
          switch (typeEq(k.Key.witness, k'.Key.witness)) {
          | None => None
          | Some(TypeEq) => Some(v)
          }
        }
      ) {
      | Not_found => None
      };

  let forEach = (f: 'a => unit, m: HMap.t('a)) =>
    HMap.iter((_key, value) => f(value), m);

  let fold = (f: ('a, 'acc) => 'acc, acc: 'acc, m: HMap.t('a)) =>
    HMap.fold((_key, value, acc) => f(value, acc), m, acc);

  let all = (p: 'a => bool, m: HMap.t('a)): bool =>
    HMap.for_all((_, b) => p(b), m);

  let any = (p: 'a => bool, m: HMap.t('a)): bool =>
    HMap.exists((_, b) => p(b), m);

  let filter = (p: 'a => bool, m: HMap.t('a)): HMap.t('a) =>
    HMap.filter((_, b) => p(b), m);

  let size: HMap.t('a) => int = m => HMap.cardinal(m);
};

/**
 * We include a default HMap implementation which uses `unit` as it's key meta type.
 * Note that this default map type will not work in a type-safe way for functions that
 * iterate over the map with keyValue functions, like fold, all, any, etc.
 *
 * In order to use the iterating functions, use a custom HMap with the key metadata functions
 * needed to convert the existential 'a into a useful value.
 */
module WithUnitKeyMeta =
  Make({
    type t('a) = unit;
  });
include WithUnitKeyMeta;