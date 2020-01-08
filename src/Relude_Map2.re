type t('id, 'key, 'value) = Belt.Map.t('key, 'value, 'id);

let set: ('key, 'value, t('id, 'key, 'value)) => t('id, 'key, 'value) =
  (key, value) => Belt.Map.set(_, key, value);

let isEmpty: t('id, 'key, 'value) => bool = Belt.Map.isEmpty;

let contains: ('key, t('id, 'key, 'value)) => bool = key => Belt.Map.has(_, key);

let compareInt: (('value, 'value) => int, t('id, 'key, 'value), t('id, 'key, 'value)) => int =
  (comparator, a, b) => Belt.Map.cmp(a, b, comparator);

let compareBy:
  (
    ('value, 'value) => BsAbstract.Interface.ordering,
    t('id, 'key, 'value),
    t('id, 'key, 'value)
  ) =>
  BsAbstract.Interface.ordering =
  (comparator, a, b) =>
    compareInt((a, b) => Relude_Ordering.toInt(comparator(a, b)), a, b)
    |> Relude_Ordering.fromInt;

let eqBy: (('value, 'value) => bool, t('id, 'key, 'value), t('id, 'key, 'value)) => bool =
  (comparator, a, b) => Belt.Map.eq(a, b, comparator);

let find: (('key, 'value) => bool, t('id, 'key, 'value)) => option(('key, 'value)) =
  by => Belt.Map.findFirstBy(_, by);

let forEach: (('key, 'value) => unit, t('id, 'key, 'value)) => unit =
  fn => Belt.Map.forEach(_, fn);

let foldLeft: (('acc, 'key, 'value) => 'acc, 'acc, t('id, 'key, 'value)) => 'acc =
  (fn, acc) => Belt.Map.reduce(_, acc, fn);

let all: (('key, 'value) => bool, t('id, 'key, 'value)) => bool =
  cond => Belt.Map.every(_, cond);

let any: (('key, 'value) => bool, t('id, 'key, 'value)) => bool = cond => Belt.Map.some(_, cond);

let length: t('id, 'key, 'value) => int = Belt.Map.size;

let toArray: t('id, 'key, 'value) => array(('key, 'value)) = Belt.Map.toArray;

let toList: t('id, 'key, 'value) => list(('key, 'value)) = Belt.Map.toList;

let keyArray: t('id, 'key, 'value) => array('key) = Belt.Map.keysToArray;

let keys: t('id, 'key, 'value) => list('key) = map => keyArray(map) |> Belt.List.fromArray;

let valueArray: t('id, 'key, 'value) => array('value) = Belt.Map.valuesToArray;

let values: t('id, 'key, 'value) => list('value) =
  map => valueArray(map) |> Belt.List.fromArray;

let minKey: t('id, 'key, 'value) => option('key) = Belt.Map.minKey;

let maxKey: t('id, 'key, 'value) => option('key) = Belt.Map.maxKey;

let min: t('id, 'key, 'value) => option(('key, 'value)) = Belt.Map.minimum;

let max: t('id, 'key, 'value) => option(('key, 'value)) = Belt.Map.maximum;

let get: ('key, t('id, 'key, 'value)) => option('value) = key => Belt.Map.get(_, key);

let getOrElse: ('key, 'value, t('id, 'key, 'value)) => 'value =
  (key, default) => Belt.Map.getWithDefault(_, key, default);

let remove: ('key, t('id, 'key, 'value)) => t('id, 'key, 'value) =
  key => Belt.Map.remove(_, key);

let removeMany: (array('key), t('id, 'key, 'value)) => t('id, 'key, 'value) =
  keys => Belt.Map.removeMany(_, keys);

let update:
  ('key, option('value) => option('value), t('id, 'key, 'value)) => t('id, 'key, 'value) =
  (key, updateFn) => Belt.Map.update(_, key, updateFn);

let merge:
  (
    ('key, option('value), option('value)) => option('value),
    t('id, 'key, 'value),
    t('id, 'key, 'value)
  ) =>
  t('id, 'key, 'value) =
  (mergeFn, a, b) => Belt.Map.merge(a, b, mergeFn);

let mergeMany: (array(('key, 'value)), t('id, 'key, 'value)) => t('id, 'key, 'value) =
  arr => Belt.Map.mergeMany(_, arr);

let filter: (('key, 'value) => bool, t('id, 'key, 'value)) => t('id, 'key, 'value) =
  fn => Belt.Map.keep(_, fn);

let partition:
  (('key, 'value) => bool, t('id, 'key, 'value)) =>
  (t('id, 'key, 'value), t('id, 'key, 'value)) =
  fn => Belt.Map.partition(_, fn);

let map: ('v1 => 'v2, t('id, 'key, 'v1)) => t('id, 'key, 'v2) = fn => Belt.Map.map(_, fn);

let mapWithKey: (('key, 'v1) => 'v2, t('id, 'key, 'v1)) => t('id, 'key, 'v2) =
  fn => Belt.Map.mapWithKey(_, fn);

/**
 * The following functions create new Map.t types. They require an argument of type
 * Belt.Id.comparable('key, 'id) in order to work. Using the `WithOrd` module functor
 * will eliminate this need, because it will internally generate the `Belt.Id.comparable`
 * instance.
 */

let make: Belt.Id.comparable('key, 'id) => t('id, 'key, 'value) =
  comparable => Belt.Map.make(~id=comparable);

let singleton: (Belt.Id.comparable('key, 'id), 'key, 'value) => t('id, 'key, 'value) =
  (comparable, key, value) => Belt.Map.make(~id=comparable) |> Belt.Map.set(_, key, value);

let fromArray: (Belt.Id.comparable('key, 'id), array(('key, 'value))) => t('id, 'key, 'value) =
  (comparable, arr) => Belt.Map.fromArray(~id=comparable, arr);

let fromValueArray:
  (Belt.Id.comparable('key, 'id), 'value => 'key, array('value)) => t('id, 'key, 'value) =
  (comparable, toKey) =>
    Relude_Array_Instances.foldLeft((map, v) => set(toKey(v), v, map), make(comparable));

let fromList: (Belt.Id.comparable('key, 'id), list(('key, 'value))) => t('id, 'key, 'value) =
  (comparable, lst) => Belt.Map.fromArray(lst |> Belt.List.toArray, ~id=comparable);

let fromValueList:
  (Belt.Id.comparable('key, 'id), 'value => 'key, list('value)) => t('id, 'key, 'value) =
  (comparable, toKey) =>
    Relude_List_Instances.foldLeft((map, v) => set(toKey(v), v, map), make(comparable));

let groupListBy:
  (Belt.Id.comparable('key, 'id), 'value => 'key, list('value)) => t('id, 'key, list('value)) =
  (comparable, groupBy) => {
    open Relude_Function.Infix;
    let addItemToGroup = x => getOrElse(x |> groupBy, []) >> (xs => [x, ...xs]);
    let addItemToMap = (dict, x) => dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.List.reduce(_, make(comparable), addItemToMap) >> map(Belt.List.reverse);
  };

let groupArrayBy:
  (Belt.Id.comparable('key, 'id), 'value => 'key, array('value)) => t('id, 'key, array('value)) =
  (comparable, groupBy) => {
    open Relude_Function.Infix;
    let addItemToGroup = x => getOrElse(x |> groupBy, [||]) >> Belt.Array.concat(_, [|x|]);
    let addItemToMap = (dict, x) => dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.Array.reduce(_, make(comparable), addItemToMap);
  };

module WithOrd = (M: BsAbstract.Interface.ORD) => {
  type key = M.t;

  module Comparable =
    Belt.Id.MakeComparable({
      type t = M.t;
      let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
    });

  type nonrec t('value) = t(Comparable.identity, key, 'value);

  /**
   * These functions will internally supply the `Comparable` module to the functions
   * which require it to construct new `Map` instances.
   */

  let make: unit => t('value) = () => make((module Comparable));

  let singleton: (key, 'value) => t('value) = (key, value) => make() |> set(key, value);

  let fromArray: array((key, 'value)) => t('value) =
    arr => fromArray((module Comparable), arr);

  let fromList: list((key, 'value)) => t('value) = lst => fromList((module Comparable), lst);

  let fromValueList: ('value => key, list('value)) => t('value) =
    (toKey, lst) => fromValueList((module Comparable), toKey, lst);

  let groupListBy: ('value => key, list('value)) => t(list('value)) =
    (groupBy, lst) => groupListBy((module Comparable), groupBy, lst);

  let groupArrayBy: ('value => key, array('value)) => t(array('value)) =
    (groupBy, arr) => groupArrayBy((module Comparable), groupBy, arr);

  /**
   * The functions below can just be borrowed from the outer scope, so they won't
   * allocate new stack frames when `WithOrd` is instantiated. The returned module
   * should include these functions by reference, at least in the JS output
   * via BuckleScript.
   */

  let set: (key, 'value, t('value)) => t('value) = set;

  let isEmpty: t('value) => bool = isEmpty;

  let contains: (key, t('value)) => bool = contains;

  let compareInt: (('value, 'value) => int, t('value), t('value)) => int = compareInt;

  let compareBy:
    (('value, 'value) => BsAbstract.Interface.ordering, t('value), t('value)) =>
    BsAbstract.Interface.ordering = compareBy;

  let eqBy: (('value, 'value) => bool, t('value), t('value)) => bool = eqBy;

  let find: ((key, 'value) => bool, t('value)) => option((key, 'value)) = find;

  let forEach: ((key, 'value) => unit, t('value)) => unit = forEach;

  let foldLeft: (('acc, key, 'value) => 'acc, 'acc, t('value)) => 'acc = foldLeft;

  let all: ((key, 'value) => bool, t('value)) => bool = all;

  let any: ((key, 'value) => bool, t('value)) => bool = any;

  let length: t('value) => int = length;

  let toArray: t('value) => array((key, 'value)) = toArray;

  let toList: t('value) => list((key, 'value)) = toList;

  let keyArray: t('value) => array(key) = keyArray;

  let keys: t('value) => list(key) = keys;

  let valueArray: t('value) => array('value) = valueArray;

  let values: t('value) => list('value) = values;

  let minKey: t('value) => option(key) = minKey;

  let maxKey: t('value) => option(key) = maxKey;

  let min: t('value) => option((key, 'value)) = min;

  let max: t('value) => option((key, 'value)) = max;

  let get: (key, t('value)) => option('value) = get;

  let getOrElse: (key, 'value, t('value)) => 'value = getOrElse;

  let remove: (key, t('value)) => t('value) = remove;

  let removeMany: (array(key), t('value)) => t('value) = removeMany;

  let update: (key, option('value) => option('value), t('value)) => t('value) = update;

  let merge:
    ((key, option('value), option('value)) => option('value), t('value), t('value)) =>
    t('value) = merge;

  let mergeMany: (array((key, 'value)), t('value)) => t('value) = mergeMany;

  let filter: ((key, 'value) => bool, t('value)) => t('value) = filter;

  let partition: ((key, 'value) => bool, t('value)) => (t('value), t('value)) = partition;

  let map: ('v1 => 'v2, t('v1)) => t('v2) = map;

  let mapWithKey: ((key, 'v1) => 'v2, t('v1)) => t('v2) = mapWithKey;

  module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  let map = Functor.map;
  include Relude_Extensions_Functor.FunctorExtensions(Functor);
};