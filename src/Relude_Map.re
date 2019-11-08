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
    (
      ('value, 'value) => BsAbstract.Interface.ordering,
      t('value),
      t('value)
    ) =>
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
  let partition:
    ((key, 'value) => bool, t('value)) => (t('value), t('value));
  let map: ('v1 => 'v2, t('v1)) => t('v2);
  let mapWithKey: ((key, 'v1) => 'v2, t('v1)) => t('v2);
  let groupListBy: ('a => key, list('a)) => t(list('a));
  let groupArrayBy: ('a => key, array('a)) => t(array('a));
};

module WithOrd = (M: BsAbstract.Interface.ORD) : (MAP with type key = M.t) => {
  open Relude_Function.Infix;
  type key = M.t;
  module Comparable =
    Belt.Id.MakeComparable({
      type t = M.t;
      let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
    });

  type t('value) = Belt.Map.t(Comparable.t, 'value, Comparable.identity);

  /**
   * Construct a new, empty map.
   */
  let make = () => Belt.Map.make(~id=(module Comparable));

  /**
   * Set the value to the provided value at the given key in the map. This will
   * add a new key if the map doesn't currently have the provided key, or it
   * will replace the value if the key exists.
   *
   * As with other operations that "change" a map, the original map is not
   * mutated; instead a new immutable copy is returned.
   */
  let set = (key, value) => Belt.Map.set(_, key, value);

  /**
   * Contruct a new map from the provided key and value.
   */
  let singleton = (key, value) => make() |> set(key, value);

  /**
   * Determine whether a map is empty.
   */
  let isEmpty = Belt.Map.isEmpty;

  /**
   * Determine whether a map contains a given key.
   */
  let contains = key => Belt.Map.has(_, key);

  /**
   * Compare the ordering of two maps, given a comparator function capable of
   * comparing each value in the map. `compareInt` expects the provided function
   * to return an int representing the comparison, and `compareInt` intself will
   * return an int.
   */
  let compareInt = (comparator, a, b) => Belt.Map.cmp(a, b, comparator);

  /**
   * Compare the ordering of two maps, given a comparator function capable of
   * comparing each value in the map. `compare` expects the provided function to
   * return an `ordering` representing the comparison, and `compare` itself will
   * return an `ordering` value.
   */
  let compareBy = (comparator, a, b) =>
    compareInt((a, b) => Relude_Ordering.toInt(comparator(a, b)), a, b)
    |> Relude_Ordering.fromInt;

  // TODO: add a `compare` that works with modules

  /**
   * Given an equality function capable of testing values for equality,
   * determine whether two maps are equal by checking whether they have equal
   * keys, and equal values at each key.
   */
  let eqBy = (comparator, a, b) => Belt.Map.eq(a, b, comparator);

  /**
   * Find (optionally) the first key/value pair in a map matching the provided
   * predicate function.
   */
  let find = by => Belt.Map.findFirstBy(_, by);

  /**
   * Iterate over each key/value pair in the map, calling the provided function
   * that probably performs some side effect and returns `unit`. Prefer `map` or
   * `foldLeft` in most cases.
   */
  let forEach = fn => Belt.Map.forEach(_, fn);

  /**
   * Accumulate a map of key/value pairs into a single value.
   */
  // TODO: the provided function is called with 3 arguments (key and value are
  // separate), which is inconsistent with FOLDABLE
  let foldLeft = (fn, acc) => Belt.Map.reduce(_, acc, fn);

  /**
   * Given a predicate function to be called with key/value pairs, determine
   * whether every pair in the map satisfies the predicate. This will always
   * return `true` for empty maps.
   */
  let all = cond => Belt.Map.every(_, cond);

  /**
   * Given a predicate function to be called with key/value pairs, determine
   * whether at least one pair in the map satisfies the predicate. This will
   * always return `false` for empty maps.
   */
  let any = cond => Belt.Map.some(_, cond);

  /**
   * The count of keys in the map.
   */
  let length = Belt.Map.size;

  /**
   * Convert a map to an array of key/value pairs. Note that the resulting array
   * will be sorted by the ordering of the key type, not necessarily the order
   * in which values were added to the map.
   */
  let toArray = Belt.Map.toArray;

  /**
   * Convert an associated array (an array of (key, value) tuples) into a map.
   */
  let fromArray = Belt.Map.fromArray(_, ~id=(module Comparable));

  /**
   * Convert an array of values into a map, using the provided function from
   * value to key. This is useful when your value type can already be uniquely
   * identified (and that identifier can be ordered).
   */
  let fromValueArray = toKey =>
    Relude_Array_Instances.foldLeft(
      (map, v) => set(toKey(v), v, map),
      make(),
    );

  /**
   * Convert a map to an associated list (a list of key/value tuples). Note that
   * the resulting list will be sorted according to the ordering of the key
   * type, not necessarily in the order in which values were added to the map.
   */
  let toList = Belt.Map.toList;

  /**
   * Convert an associated list (a list of (key, value) tuples) into a map.
   */
  let fromList = lst =>
    Belt.Map.fromArray(lst |> Belt.List.toArray, ~id=(module Comparable));

  /**
   * Convert a list of values into a map, using the provided function from
   * value to key. This is useful when your value type can already be uniquely
   * identified (and that identifier can be ordered).
   */
  let fromValueList = toKey =>
    Relude_List_Instances.foldLeft(
      (map, v) => set(toKey(v), v, map),
      make(),
    );

  /**
   * Return a sorted array containing each key in the map.
   */
  let keyArray = Belt.Map.keysToArray;

  /**
   * Return a sorted list containing each key in the map
   */
  let keys = map => keyArray(map) |> Belt.List.fromArray;

  /**
   * Return an array of each value (sorted by key) in the map.
   */
  let valueArray = Belt.Map.valuesToArray;

  /**
   * Return a list of each value (sorted by key) in the map.
   */
  let values = map => valueArray(map) |> Belt.List.fromArray;

  /**
   * Optionally find the smallest key, using the key ordering.
   */
  let minKey = Belt.Map.minKey;

  /**
   * Optionally find the largest key, using the key ordering.
   */
  let maxKey = Belt.Map.maxKey;

  /**
   * Optionally find the smallest key/value pair, using the key ordering.
   */
  let min = Belt.Map.minimum;

  /**
   * Optionally find the largest key/value pair, using the key ordering.
   */
  let max = Belt.Map.maximum;

  /**
   * Attempt to find a value in a map, given a key.
   */
  let get = key => Belt.Map.get(_, key);

  /**
   * Attempt to find a value in a map, given a key. Use the provided fallback
   * value if the key is not in the map.
   */
  let getOrElse = (key, default) => Belt.Map.getWithDefault(_, key, default);

  /**
   * Return a new copy of a map, with the provided key removed. Like other map
   * functions that "change" a value, this returns an immutable copy. The
   * original map is unchanged.
   */
  let remove = key => Belt.Map.remove(_, key);

  /**
   * Given a list of keys, remove each from the map, returning a new copy.
   */
  // TODO: we tend to prefer lists by default, but this wants an array
  let removeMany = keys => Belt.Map.removeMany(_, keys);

  /**
   * At the given key, set or remove the value using the provided update
   * function. The function will be called with the current value, if any. It
   * may return `Some(newValue)` which will perform a `set`, or it can return
   * `None`, which will perform a `remove`.
   */
  let update = (key, updateFn) => Belt.Map.update(_, key, updateFn);

  /**
   * Combine two existing maps using the provided merge function. The merge
   * function will be called with the key being merged as well as the two
   * possible values from the existing maps. The provided merge function should
   * return `None` to remove a key, or `Some(newValue)` to keep the key.
   *
   * The value types of the two original maps don't need to be the same, nor
   * does the value type returned by the provided merge function.
   */
  let merge = (mergeFn, a, b) => Belt.Map.merge(a, b, mergeFn);

  /**
   * Given an array of key/value pairs and an existin map, add each key and
   * value in the array to the map.
   *
   * TODO: it's not clear from the Belt docs what happens in the case of key
   * conflicts.
   */
  let mergeMany = arr => Belt.Map.mergeMany(_, arr);

  /**
   * Remove each key/value pair that doesn't pass the given predicate function.
   */
  let filter = fn => Belt.Map.keep(_, fn);

  let partition = fn => Belt.Map.partition(_, fn);

  /**
   * Transform each value in the map to a new value using the provided function.
   */
  let map = fn => Belt.Map.map(_, fn);

  let mapWithKey = fn => Belt.Map.mapWithKey(_, fn);

  let groupListBy = groupBy => {
    let addItemToGroup = x =>
      getOrElse(x |> groupBy, []) >> (xs => [x, ...xs]);
    let addItemToMap = (dict, x) =>
      dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.List.reduce(_, make(), addItemToMap) >> map(Belt.List.reverse);
  };

  let groupArrayBy = groupBy => {
    let addItemToGroup = x =>
      getOrElse(x |> groupBy, [||]) >> Belt.Array.concat(_, [|x|]);
    let addItemToMap = (dict, x) =>
      dict |> set(x |> groupBy, addItemToGroup(x, dict));
    Belt.Array.reduce(_, make(), addItemToMap);
  };
};
