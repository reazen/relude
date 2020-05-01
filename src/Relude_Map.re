open BsBastet.Interface;

type t('key, 'value, 'id) = Belt.Map.t('key, 'value, 'id);

/**
Construct a new, empty map.
*/
let make: Belt.Id.comparable('key, 'id) => t('key, 'value, 'id) =
  comparable => Belt.Map.make(~id=comparable);

/**
Set the value to the provided value at the given key in the map. This will
add a new key if the map doesn't currently have the provided key, or it
will replace the value if the key exists.

As with other operations that "change" a map, the original map is not
mutated; instead a new immutable copy is returned.
*/
let set: ('key, 'value, t('key, 'value, 'id)) => t('key, 'value, 'id) =
  (key, value) => Belt.Map.set(_, key, value);

/**
Contruct a new map from the provided key and value.
*/
let singleton:
  (Belt.Id.comparable('key, 'id), 'key, 'value) => t('key, 'value, 'id) =
  (comparable, key, value) =>
    Belt.Map.make(~id=comparable) |> Belt.Map.set(_, key, value);

/**
Determine whether a map is empty.
*/
let isEmpty: t('key, 'value, 'id) => bool = Belt.Map.isEmpty;

/**
Determine whether a map contains a given key.
*/
let contains: ('key, t('key, 'value, 'id)) => bool =
  key => Belt.Map.has(_, key);

/**
Compare the ordering of two maps, given a comparator function capable of
comparing each value in the map. [compareInt] expects the provided function
to return an int representing the comparison, and [compareInt] intself will
return an int.
*/
let compareInt:
  (('value, 'value) => int, t('key, 'value, 'id), t('key, 'value, 'id)) =>
  int =
  (comparator, a, b) => Belt.Map.cmp(a, b, comparator);

/**
Compare the ordering of two maps, given a comparator function capable of
comparing each value in the map. [compare] expects the provided function to
return an [ordering] representing the comparison, and [compare] itself will
return an [ordering] value.
*/
let compareBy:
  (
    ('value, 'value) => ordering,
    t('key, 'value, 'id),
    t('key, 'value, 'id)
  ) =>
  ordering =
  (comparator, a, b) =>
    compareInt((a, b) => Relude_Ordering.toInt(comparator(a, b)), a, b)
    |> Relude_Ordering.fromInt;

/**
Given an equality function capable of testing values for equality,
determine whether two maps are equal by checking whether they have equal
keys, and equal values at each key.
*/
let eqBy:
  (('value, 'value) => bool, t('key, 'value, 'id), t('key, 'value, 'id)) =>
  bool =
  (comparator, a, b) => Belt.Map.eq(a, b, comparator);

/**
Find (optionally) the first key/value pair in a map matching the provided
predicate function.
*/
let find:
  (('key, 'value) => bool, t('key, 'value, 'id)) => option(('key, 'value)) =
  by => Belt.Map.findFirstBy(_, by);

/**
Iterate over each key/value pair in the map, calling the provided function
that probably performs some side effect and returns [unit]. Prefer [map] or
[foldLeft] in most cases.
*/
let forEach: (('key, 'value) => unit, t('key, 'value, 'id)) => unit =
  fn => Belt.Map.forEach(_, fn);

/**
Accumulate a map of key/value pairs into a single value.
*/
let foldLeft:
  (('acc, 'key, 'value) => 'acc, 'acc, t('key, 'value, 'id)) => 'acc =
  (fn, acc) => Belt.Map.reduce(_, acc, fn);

/**
Given a predicate function to be called with key/value pairs, determine
whether every pair in the map satisfies the predicate. This will always
return [true] for empty maps.
*/
let all: (('key, 'value) => bool, t('key, 'value, 'id)) => bool =
  cond => Belt.Map.every(_, cond);

/**
Given a predicate function to be called with key/value pairs, determine
whether at least one pair in the map satisfies the predicate. This will
always return [false] for empty maps.
*/
let any: (('key, 'value) => bool, t('key, 'value, 'id)) => bool =
  cond => Belt.Map.some(_, cond);

/**
The count of keys in the map.
*/
let length: t('key, 'value, 'id) => int = Belt.Map.size;

/**
Convert a map to an array of key/value pairs. Note that the resulting array
will be sorted by the ordering of the key type, not necessarily the order
in which values were added to the map.
*/
let toArray: t('key, 'value, 'id) => array(('key, 'value)) = Belt.Map.toArray;

/**
Convert an associated array (an array of (key, value) tuples) into a map.
*/
let fromArray:
  (Belt.Id.comparable('key, 'id), array(('key, 'value))) =>
  t('key, 'value, 'id) =
  (comparable, arr) => Belt.Map.fromArray(~id=comparable, arr);

/**
Convert an array of values into a map, using the provided function from
value to key. This is useful when your value type can already be uniquely
identified (and that identifier can be ordered).
*/
let fromValueArray:
  (Belt.Id.comparable('key, 'id), 'value => 'key, array('value)) =>
  t('key, 'value, 'id) =
  (comparable, toKey) =>
    Relude_Array_Instances.foldLeft(
      (map, v) => set(toKey(v), v, map),
      make(comparable),
    );

/**
Convert a map to an associated list (a list of key/value tuples). Note that
the resulting list will be sorted according to the ordering of the key
type, not necessarily in the order in which values were added to the map.
*/
let toList: t('key, 'value, 'id) => list(('key, 'value)) = Belt.Map.toList;

/**
Convert an associated list (a list of (key, value) tuples) into a map.
*/
let fromList:
  (Belt.Id.comparable('key, 'id), list(('key, 'value))) =>
  t('key, 'value, 'id) =
  (comparable, lst) =>
    Belt.Map.fromArray(lst |> Belt.List.toArray, ~id=comparable);

/**
Convert a list of values into a map, using the provided function from
value to key. This is useful when your value type can already be uniquely
identified (and that identifier can be ordered).
*/
let fromValueList:
  (Belt.Id.comparable('key, 'id), 'value => 'key, list('value)) =>
  t('key, 'value, 'id) =
  (comparable, toKey) =>
    Relude_List_Instances.foldLeft(
      (map, v) => set(toKey(v), v, map),
      make(comparable),
    );

/**
Return a sorted array containing each key in the map.
*/
let keyArray: t('key, 'value, 'id) => array('key) = Belt.Map.keysToArray;

/**
Return a sorted list containing each key in the map
*/
let keys: t('key, 'value, 'id) => list('key) =
  map => keyArray(map) |> Belt.List.fromArray;

/**
Return an array of each value (sorted by key) in the map.
*/
let valueArray: t('key, 'value, 'id) => array('value) = Belt.Map.valuesToArray;

/**
Return a list of each value (sorted by key) in the map.
*/
let values: t('key, 'value, 'id) => list('value) =
  map => valueArray(map) |> Belt.List.fromArray;

/**
Optionally find the smallest key, using the key ordering.
*/
let minKey: t('key, 'value, 'id) => option('key) = Belt.Map.minKey;

/**
Optionally find the largest key, using the key ordering.
*/
let maxKey: t('key, 'value, 'id) => option('key) = Belt.Map.maxKey;

/**
Optionally find the smallest key/value pair, using the key ordering.
*/
let min: t('key, 'value, 'id) => option(('key, 'value)) = Belt.Map.minimum;

/**
Optionally find the largest key/value pair, using the key ordering.
*/
let max: t('key, 'value, 'id) => option(('key, 'value)) = Belt.Map.maximum;

/**
Attempt to find a value in a map, given a key.
*/
let get: ('key, t('key, 'value, 'id)) => option('value) =
  key => Belt.Map.get(_, key);

/**
Attempt to find a value in a map, given a key. Use the provided fallback
value if the key is not in the map.
*/
let getOrElse: ('key, 'value, t('key, 'value, 'id)) => 'value =
  (key, default) => Belt.Map.getWithDefault(_, key, default);

/**
Return a new copy of a map, with the provided key removed. Like other map
functions that "change" a value, this returns an immutable copy. The
original map is unchanged.
*/
let remove: ('key, t('key, 'value, 'id)) => t('key, 'value, 'id) =
  key => Belt.Map.remove(_, key);

/**
Given a list of keys, remove each from the map, returning a new copy.
*/
let removeMany: (array('key), t('key, 'value, 'id)) => t('key, 'value, 'id) =
  keys => Belt.Map.removeMany(_, keys);

/**
At the given key, set or remove the value using the provided update
function. The function will be called with the current value, if any. It
may return [Some(newValue)] which will perform a [set], or it can return
[None], which will perform a [remove].
*/
let update:
  ('key, option('value) => option('value), t('key, 'value, 'id)) =>
  t('key, 'value, 'id) =
  (key, updateFn) => Belt.Map.update(_, key, updateFn);

/**
Combine two existing maps using the provided merge function. The merge
function will be called with the key being merged as well as the two
possible values from the existing maps. The provided merge function should
return [None] to remove a key, or [Some(newValue)] to keep the key.

The value types of the two original maps don't need to be the same, nor
does the value type returned by the provided merge function.
*/
let merge:
  (
    ('key, option('value), option('value)) => option('value),
    t('key, 'value, 'id),
    t('key, 'value, 'id)
  ) =>
  t('key, 'value, 'id) =
  (mergeFn, a, b) => Belt.Map.merge(a, b, mergeFn);

/**
Given an array of key/value pairs and an existin map, add each key and
value in the array to the map.

TODO: it's not clear from the Belt docs what happens in the case of key
conflicts.
*/
let mergeMany:
  (array(('key, 'value)), t('key, 'value, 'id)) => t('key, 'value, 'id) =
  arr => Belt.Map.mergeMany(_, arr);

/**
Remove each key/value pair that doesn't pass the given predicate function.
*/
let filter:
  (('key, 'value) => bool, t('key, 'value, 'id)) => t('key, 'value, 'id) =
  fn => Belt.Map.keep(_, fn);

/**
Alias of filter
*/
let keep:
  (('key, 'value) => bool, t('key, 'value, 'id)) => t('key, 'value, 'id) = filter;

/**
Remove each key/value pair that passes the given predicate function
*/
let filterNot:
  (('key, 'value) => bool, t('key, 'value, 'id)) => t('key, 'value, 'id) =
  fn => Belt.Map.keep(_, (key, value) => !fn(key, value));
/**
Alias of filterNot
*/
let reject:
  (('key, 'value) => bool, t('key, 'value, 'id)) => t('key, 'value, 'id) = filterNot;

let partition:
  (('key, 'value) => bool, t('key, 'value, 'id)) =>
  (t('key, 'value, 'id), t('key, 'value, 'id)) =
  fn => Belt.Map.partition(_, fn);

/**
Transform each value in the map to a new value using the provided function.
*/
let map: ('v1 => 'v2, t('key, 'v1, 'id)) => t('key, 'v2, 'id) =
  fn => Belt.Map.map(_, fn);

let mapWithKey: (('key, 'v1) => 'v2, t('key, 'v1, 'id)) => t('key, 'v2, 'id) =
  fn => Belt.Map.mapWithKey(_, fn);

let groupListBy:
  (Belt.Id.comparable('key, 'id), 'value => 'key, list('value)) =>
  t('key, list('value), 'id) =
  (comparable, groupBy) => {
    open Relude_Function.Infix;
    let addItemToGroup = x =>
      getOrElse(x |> groupBy, []) >> (xs => [x, ...xs]);
    let addItemToMap = (dict, x) =>
      dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.List.reduce(_, make(comparable), addItemToMap)
    >> map(Belt.List.reverse);
  };

let groupArrayBy:
  (Belt.Id.comparable('key, 'id), 'value => 'key, array('value)) =>
  t('key, array('value), 'id) =
  (comparable, groupBy) => {
    open Relude_Function.Infix;
    let addItemToGroup = x =>
      getOrElse(x |> groupBy, [||]) >> Belt.Array.concat(_, [|x|]);
    let addItemToMap = (dict, x) =>
      dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.Array.reduce(_, make(comparable), addItemToMap);
  };

module type MAP = {
  type key;
  module Comparable: {
    type identity;
    type t;
  };
  type t('value) = Belt.Map.t(Comparable.t, 'value, Comparable.identity);
  let make: unit => t('value);
  let set: (key, 'value, t('value)) => t('value);

  let singleton: (key, 'value) => t('value);
  let isEmpty: t('value) => bool;
  let contains: (key, t('value)) => bool;
  let compareInt: (('value, 'value) => int, t('value), t('value)) => int;
  let compareBy:
    (('value, 'value) => ordering, t('value), t('value)) => ordering;

  let eqBy: (('value, 'value) => bool, t('value), t('value)) => bool;
  let find: ((key, 'value) => bool, t('value)) => option((key, 'value));
  let forEach: ((key, 'value) => unit, t('value)) => unit;
  let foldLeft: (('acc, key, 'value) => 'acc, 'acc, t('value)) => 'acc;
  let all: ((key, 'value) => bool, t('value)) => bool;
  let any: ((key, 'value) => bool, t('value)) => bool;
  let length: t('value) => int;
  let toArray: t('value) => array((key, 'value));
  let fromArray: array((key, 'value)) => t('value);
  let fromValueArray: ('value => key, array('value)) => t('value);
  let toList: t('value) => list((key, 'value));
  let fromList: list((key, 'value)) => t('value);
  let fromValueList: ('value => key, list('value)) => t('value);
  let keys: t('value) => list(key);
  let keyArray: t('value) => array(key);
  let values: t('value) => list('value);
  let valueArray: t('value) => array('value);
  let minKey: t('value) => option(key);
  let maxKey: t('value) => option(key);
  let min: t('value) => option((key, 'value));
  let max: t('value) => option((key, 'value));
  let get: (key, t('value)) => option('value);
  let getOrElse: (key, 'value, t('value)) => 'value;
  let remove: (key, t('value)) => t('value);
  let removeMany: (array(key), t('value)) => t('value);
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
  let keep: ((key, 'value) => bool, t('value)) => t('value);
  let filterNot: ((key, 'value) => bool, t('value)) => t('value);
  let reject: ((key, 'value) => bool, t('value)) => t('value);
  let partition:
    ((key, 'value) => bool, t('value)) => (t('value), t('value));
  let map: ('v1 => 'v2, t('v1)) => t('v2);
  let mapWithKey: ((key, 'v1) => 'v2, t('v1)) => t('v2);
  let groupListBy: ('a => key, list('a)) => t(list('a));
  let groupArrayBy: ('a => key, array('a)) => t(array('a));
};

module WithOrd = (M: ORD) : (MAP with type key = M.t) => {
  type key = M.t;

  module Comparable =
    Belt.Id.MakeComparable({
      type t = key;
      let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
    });

  type nonrec t('value) = t(key, 'value, Comparable.identity);

  let make: unit => t('value) = () => make((module Comparable));
  let singleton: (key, 'value) => t('value) =
    (key, value) => make() |> set(key, value);
  let fromArray: array((key, 'value)) => t('value) =
    arr => fromArray((module Comparable), arr);
  let fromValueArray: ('value => key, array('value)) => t('value) =
    (toKey, arr) => fromValueArray((module Comparable), toKey, arr);
  let fromList: list((key, 'value)) => t('value) =
    lst => fromList((module Comparable), lst);
  let fromValueList: ('value => key, list('value)) => t('value) =
    (toKey, lst) => fromValueList((module Comparable), toKey, lst);
  let groupListBy: ('value => key, list('value)) => t(list('value)) =
    (groupBy, lst) => groupListBy((module Comparable), groupBy, lst);
  let groupArrayBy: ('value => key, array('value)) => t(array('value)) =
    (groupBy, arr) => groupArrayBy((module Comparable), groupBy, arr);
  let set: (key, 'value, t('value)) => t('value) = set;
  let isEmpty: t('value) => bool = isEmpty;
  let contains: (key, t('value)) => bool = contains;
  let compareInt: (('value, 'value) => int, t('value), t('value)) => int = compareInt;
  let compareBy:
    (('value, 'value) => ordering, t('value), t('value)) => ordering = compareBy;
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
  let update:
    (key, option('value) => option('value), t('value)) => t('value) = update;
  let merge:
    (
      (key, option('value), option('value)) => option('value),
      t('value),
      t('value)
    ) =>
    t('value) = merge;
  let mergeMany: (array((key, 'value)), t('value)) => t('value) = mergeMany;
  let filter: ((key, 'value) => bool, t('value)) => t('value) = filter;
  let keep: ((key, 'value) => bool, t('value)) => t('value) = keep;
  let filterNot: ((key, 'value) => bool, t('value)) => t('value) = filterNot;
  let reject: ((key, 'value) => bool, t('value)) => t('value) = reject;
  let partition:
    ((key, 'value) => bool, t('value)) => (t('value), t('value)) = partition;
  let map: ('v1 => 'v2, t('v1)) => t('v2) = map;
  let mapWithKey: ((key, 'v1) => 'v2, t('v1)) => t('v2) = mapWithKey;

  module Functor: FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
  include Relude_Extensions_Functor.FunctorExtensions(Functor);
};
