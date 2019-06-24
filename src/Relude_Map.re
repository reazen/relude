module type Map = {
  type key;
  module Comparable: {
    type identity;
    type t;
  };
  type t('value) = Belt.Map.t(Comparable.t, 'value, Comparable.identity);
  let make: unit => t('value);
  let isEmpty: t('value) => bool;
  let contains: (key, t('value)) => bool;
  let compareInt: (('value, 'value) => int, t('value), t('value)) => int;
  let compare:
    (('value, 'value) => int, t('value), t('value)) =>
    BsAbstract.Interface.ordering;
  let eqBy: (('value, 'value) => bool, t('value), t('value)) => bool;
  let find: ((key, 'value) => bool, t('value)) => option((key, 'value));
  let forEach: ((key, 'value) => unit, t('value)) => unit;
  let foldLeft: (('acc, key, 'value) => 'acc, 'acc, t('value)) => 'acc;
  let all: ((key, 'value) => bool, t('value)) => bool;
  let any: ((key, 'value) => bool, t('value)) => bool;
  let length: t('value) => int;
  let toArray: t('value) => array((key, 'value));
  let fromArray: array((key, 'value)) => t('value);
  let toList: t('value) => list((key, 'value));
  let fromList: list((key, 'value)) => t('value);
  let keys: t('value) => array(key);
  let values: t('value) => array('value);
  let minKey: t('value) => option(key);
  let maxKey: t('value) => option(key);
  let min: t('value) => option((key, 'value));
  let max: t('value) => option((key, 'value));
  let get: (key, t('value)) => option('value);
  let getOrElse: (key, 'value, t('value)) => 'value;
  let remove: (key, t('value)) => t('value);
  let removeMany: (array(key), t('value)) => t('value);
  let set: (key, 'value, t('value)) => t('value);
  let update:
    (key, option('value) => option('value), t('value)) => t('value);
  let merge:
    (
      (key, option('value), option('value)) => option('value),
      t('value),
      t('value)
    ) =>
    t('value);
  let mergeMany: (array((key, 'value)), t('value)) => t('value);
  let filter: ((key, 'value) => bool, t('value)) => t('value);
  let partition:
    ((key, 'value) => bool, t('value)) => (t('value), t('value));
  let map: ('v1 => 'v2, t('v1)) => t('v2);
  let mapWithKey: ((key, 'v1) => 'v2, t('v1)) => t('v2);
};

module MakeFromOrderable =
       (M: BsAbstract.Interface.ORD)
       : (Map with type key = M.t) => {
  type key = M.t;
  module Comparable =
    Belt.Id.MakeComparable({
      type t = M.t;
      let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
    });

  type t('value) = Belt.Map.t(Comparable.t, 'value, Comparable.identity);
  let make = () => Belt.Map.make(~id=(module Comparable));
  let isEmpty = Belt.Map.isEmpty;
  let contains = key => Belt.Map.has(_, key);
  let compareInt = (comparator, a, b) => Belt.Map.cmp(a, b, comparator);
  let compare = (comparator, a, b) =>
    compareInt(comparator, a, b) |> Relude_Ordering.fromInt;
  let eqBy = (comparator, a, b) => Belt.Map.eq(a, b, comparator);
  let find = by => Belt.Map.findFirstBy(_, by);
  let forEach = fn => Belt.Map.forEach(_, fn);
  let foldLeft = (fn, acc) => Belt.Map.reduce(_, acc, fn);
  let all = cond => Belt.Map.every(_, cond);
  let any = cond => Belt.Map.some(_, cond);
  let length = Belt.Map.size;
  let toArray = Belt.Map.toArray;
  let fromArray = Belt.Map.fromArray(_, ~id=(module Comparable));
  let toList = Belt.Map.toList;
  let fromList = lst =>
    Belt.Map.fromArray(lst |> Belt.List.toArray, ~id=(module Comparable));
  let keys = Belt.Map.keysToArray;
  let values = Belt.Map.valuesToArray;
  let minKey = Belt.Map.minKey;
  let maxKey = Belt.Map.maxKey;
  let min = Belt.Map.minimum;
  let max = Belt.Map.maximum;
  let get = key => Belt.Map.get(_, key);
  let getOrElse = (key, default) => Belt.Map.getWithDefault(_, key, default);
  let remove = key => Belt.Map.remove(_, key);
  let removeMany = keys => Belt.Map.removeMany(_, keys);
  let set = (key, value) => Belt.Map.set(_, key, value);
  let update = (key, updateFn) => Belt.Map.update(_, key, updateFn);
  let merge = (mergeFn, a, b) => Belt.Map.merge(a, b, mergeFn);
  let mergeMany = arr => Belt.Map.mergeMany(_, arr);
  let filter = fn => Belt.Map.keep(_, fn);
  let partition = fn => Belt.Map.partition(_, fn);
  let map = fn => Belt.Map.map(_, fn);
  let mapWithKey = fn => Belt.Map.mapWithKey(_, fn);
};
