let flip = Relude_Function.flip;
let (>>) = Relude_Function.Infix.(>>);

module type SET = {
  type value;
  module Comparable: {
    type identity;
    type t;
  };
  type t = Belt.Set.t(value, Comparable.identity);
  let empty: t;
  let fromArray: array(value) => t;
  let fromList: list(value) => t;
  let isEmpty: t => bool;
  let contains: (value, t) => bool;
  let add: (value, t) => t;
  let mergeMany: (array(value), t) => t;
  let remove: (value, t) => t;
  let removeMany: (array(value), t) => t;
  let update: (value, t) => t;
  let union: (t, t) => t;
  let intersect: (t, t) => t;
  let diff: (t, t) => t;
  let subset: (t, t) => bool;
  let compare: (t, t) => int;
  let eq: (t, t) => bool;
  let forEach: (value => unit, t) => unit;
  let foldLeft: (('a, value) => 'a, 'a, t) => 'a;
  let all: (value => bool, t) => bool;
  let any: (value => bool, t) => bool;
  let filter: (value => bool, t) => t;
  let partition: (value => bool, t) => (t, t);
  let length: t => int;
  let toArray: t => array(value);
  let toList: t => list(value);
  let minimum: t => option(value);
  let maximum: t => option(value);
  let get: (value, t) => option(value);
  let getOrElse: (value, value, t) => value;
  let split: (value, t) => ((t, t), bool);
};

module WithOrd = (M: BsAbstract.Interface.ORD) : (SET with type value = M.t) => {
  module Comparable =
    Belt.Id.MakeComparable({
      type t = M.t;
      let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
    });
  type value = M.t;
  type t = Belt.Set.t(value, Comparable.identity);
  let empty = Belt.Set.make(~id=(module Comparable));
  let fromArray = Belt.Set.fromArray(_, ~id=(module Comparable));
  let fromList = Belt.List.toArray >> fromArray;
  let isEmpty = Belt.Set.isEmpty;
  let contains = flip(Belt.Set.has);
  let add = flip(Belt.Set.add);
  let mergeMany = flip(Belt.Set.mergeMany);
  let remove = flip(Belt.Set.remove);
  let removeMany = flip(Belt.Set.removeMany);
  let update = entry => remove(entry) >> add(entry);
  let union = Belt.Set.union;
  let intersect = Belt.Set.intersect;
  let diff = Belt.Set.diff;
  let subset = Belt.Set.subset;
  let compare = Belt.Set.cmp;
  let eq = Belt.Set.eq;
  let forEach = flip(Belt.Set.forEach);
  let foldLeft = (fn, acc) => Belt.Set.reduce(_, acc, fn);
  let all = flip(Belt.Set.every);
  let any = flip(Belt.Set.some);
  let filter = flip(Belt.Set.keep);
  let partition = flip(Belt.Set.partition);
  let length = Belt.Set.size;
  let toArray = Belt.Set.toArray;
  let toList = Belt.Set.toList;
  let minimum = Belt.Set.minimum;
  let maximum = Belt.Set.maximum;
  let get = flip(Belt.Set.get);
  let getOrElse = (value, default, t) =>
    t |> get(value) |> Relude_Option_Base.getOrElse(default);
  let split = flip(Belt.Set.split);
};
