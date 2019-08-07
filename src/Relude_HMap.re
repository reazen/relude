// Inspired by https://github.com/dbuenzli/hmap

module TypeId = {
  type t(_) = ..;
};

module type TYPE_ID = {
  type t;
  type TypeId.t(_) +=
    | TypeId: TypeId.t(t);
};

type typeId('a) = (module TYPE_ID with type t = 'a);

let typeId = ((), type s) => {
  module M = {
    type t = s;
    type TypeId.t(_) +=
      | TypeId: TypeId.t(t);
  };

  ((module M): (module TYPE_ID with type t = s));
};

type typeEq('a, 'b) =
  | TypeEq: typeEq('a, 'a);

let eq: type r s. (typeId(r), typeId(s)) => option(typeEq(r, s)) =
  (r, s) => {
    module R = (val r: TYPE_ID with type t = r);
    module S = (val s: TYPE_ID with type t = s);
    switch (R.TypeId) {
    | S.TypeId => Some(TypeEq)
    | _ => None
    };
  };

module type KEY_INFO = {type t('a);};

module type S = {
  type key('a);

  module Key: {
    type info('a);
    let create: info('a) => key('a);
    let info: key('a) => info('a);

    type t;
    let hideType: key('a) => t;
    let equal: (t, t) => bool;
    let compare: (t, t) => int;
  };

  type t;
  let empty: t;
  let isEmpty: t => bool;
  let hasKey: (key('a), t) => bool;
  let add: (key('a), 'a, t) => t;
  let pure: (key('a), 'a) => t;
  let remove: (key('a), t) => t;
  let find: (key('a), t) => option('a);

  type keyValue =
    | B(key('a), 'a): keyValue;
  let forEach: (keyValue => unit, t) => unit;
  let fold: ((keyValue, 'a) => 'a, t, 'a) => 'a;
  let all: (keyValue => bool, t) => bool;
  let any: (keyValue => bool, t) => bool;
  let filter: (keyValue => bool, t) => t;
  let size: t => int;
};

module Make =
       (KeyInfo: KEY_INFO)
       : (S with type Key.info('a) = KeyInfo.t('a)) => {
  module Key = {
    type info('a) = KeyInfo.t('a);

    type key('a) = {
      uid: int,
      typeId: typeId('a),
      info: KeyInfo.t('a),
    };

    let uid = {
      let id = ref(-1);
      () => {
        incr(id);
        id^;
      };
    };

    let create = info => {
      let uid = uid();
      let typeId = typeId();
      {uid, typeId, info};
    };

    let info = k => k.info;

    type t =
      | V(key('a)): t;
    let hideType = k => V(k);
    let equal = (V(k0), V(k1)) =>
      (compare: (int, int) => int)(k0.uid, k1.uid) == 0;
    let compare = (V(k0), V(k1)) =>
      (compare: (int, int) => int)(k0.uid, k1.uid);
  };

  type key('a) = Key.key('a);

  module M = Map.Make(Key);
  type keyValue =
    | B(key('a), 'a): keyValue;
  type t = M.t(keyValue);

  let empty = M.empty;
  let isEmpty = M.is_empty;
  let hasKey = (k, m) => M.mem(Key.V(k), m);
  let add = (k, v, m) => M.add(Key.V(k), [@implicit_arity] B(k, v), m);
  let pure = (k, v) => M.singleton(Key.V(k), [@implicit_arity] B(k, v));
  let remove = (k, m) => M.remove(Key.V(k), m);
  let find: type a. (key(a), t) => option(a) =
    (k, s) =>
      try (
        switch (M.find(Key.V(k), s)) {
        | [@implicit_arity] B(k', v) =>
          switch (eq(k.Key.typeId, k'.Key.typeId)) {
          | None => None
          | Some(TypeEq) => Some(v)
          }
        }
      ) {
      | Not_found => None
      };

  let forEach = (f, m) => M.iter((_, b) => f(b), m);
  let fold = (f, m, acc) => M.fold((_, b, acc) => f(b, acc), m, acc);
  let all = (p, m) => M.for_all((_, b) => p(b), m);
  let any = (p, m) => M.exists((_, b) => p(b), m);
  let filter = (p, m) => M.filter((_, b) => p(b), m);
  let size = m => M.cardinal(m);
};

include Make({
  type t('a) = unit;
});