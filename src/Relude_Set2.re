type t('value, 'id) = Belt.Set.t('value, 'id);

let empty: type value id. (
    (module Belt.Id.Comparable with type t = value and type identity = id)
  ) => Belt.Set.t(value, id) = Belt.Set.make(~id=_);

let fromArray: type value id. (
    (module Belt.Id.Comparable with type t = value and type identity = id),
    array(value)
  ) => Belt.Set.t(value, id) = (id, value) => Belt.Set.fromArray(~id, value);

let fromList: type value id. (
    (module Belt.Id.Comparable with type t = value and type identity = id),
    list(value)
  ) => Belt.Set.t(value, id) = (id, value) =>  Belt.Set.fromArray(~id, Belt.List.toArray(value));

let isEmpty: type value id. t(value, id) => bool = Belt.Set.isEmpty;

let contains: type value id. (value, t(value, id)) => bool = (value, set) =>
  Belt.Set.has(set, value);

let add: type value id. (value, t(value, id)) => t(value, id) = (value, set) =>
  Belt.Set.add(set, value);

let mergeMany: type value id. (array(value), t(value, id)) => t(value, id) = (value, set) =>
  Belt.Set.mergeMany(set, value);

let remove: type value id. (value, t(value, id)) => t(value, id) = (value, set) =>
  Belt.Set.remove(set, value);

// open Relude_Function.Infix;

// module type SET = {
//   type value;
//   module Comparable: {
//     type identity;
//     type t;
//   };
//   type t = Belt.Set.t(value, Comparable.identity);
//   let empty: t;
//   let fromArray: array(value) => t;
//   let fromList: list(value) => t;
//   let isEmpty: t => bool;
//   let contains: (value, t) => bool;
//   let add: (value, t) => t;
//   let mergeMany: (array(value), t) => t;
//   let remove: (value, t) => t;
//   let removeMany: (array(value), t) => t;
//   let update: (value, t) => t;
//   let union: (t, t) => t;
//   let intersect: (t, t) => t;
//   let diff: (t, t) => t;
//   let subset: (t, t) => bool;
//   let compare: (t, t) => int;
//   let eq: (t, t) => bool;
//   let forEach: (value => unit, t) => unit;
//   let foldLeft: (('a, value) => 'a, 'a, t) => 'a;
//   let all: (value => bool, t) => bool;
//   let any: (value => bool, t) => bool;
//   let filter: (value => bool, t) => t;
//   let partition: (value => bool, t) => (t, t);
//   let length: t => int;
//   let toArray: t => array(value);
//   let toList: t => list(value);
//   let minimum: t => option(value);
//   let maximum: t => option(value);
//   let get: (value, t) => option(value);
//   let getOrElse: (value, value, t) => value;
//   let split: (value, t) => ((t, t), bool);
// };

// module WithOrd = (M: BsAbstract.Interface.ORD) : (SET with type value = M.t) => {
//   module Comparable =
//     Belt.Id.MakeComparable({
//       type t = M.t;
//       let cmp = (a, b) => M.compare(a, b) |> Relude_Ordering.toInt;
//     });
//   type value = M.t;
//   type t = Belt.Set.t(value, Comparable.identity);
//   let empty = Belt.Set.make(~id=(module Comparable));
//   let fromArray = Belt.Set.fromArray(_, ~id=(module Comparable));
//   let fromList = Belt.List.toArray >> fromArray;
//   let isEmpty = Belt.Set.isEmpty;
//   let contains = Relude_Function.flip(Belt.Set.has);
//   let add = Relude_Function.flip(Belt.Set.add);
//   let mergeMany = Relude_Function.flip(Belt.Set.mergeMany);
//   let remove = Relude_Function.flip(Belt.Set.remove);
//   let removeMany = Relude_Function.flip(Belt.Set.removeMany);
//   let update = entry => remove(entry) >> add(entry);
//   let union = Belt.Set.union;
//   let intersect = Belt.Set.intersect;
//   let diff = Belt.Set.diff;
//   let subset = Belt.Set.subset;
//   let compare = Belt.Set.cmp;
//   let eq = Belt.Set.eq;
//   let forEach = Relude_Function.flip(Belt.Set.forEach);
//   let foldLeft = (fn, acc) => Belt.Set.reduce(_, acc, fn);
//   let all = Relude_Function.flip(Belt.Set.every);
//   let any = Relude_Function.flip(Belt.Set.some);
//   let filter = Relude_Function.flip(Belt.Set.keep);
//   let partition = Relude_Function.flip(Belt.Set.partition);
//   let length = Belt.Set.size;
//   let toArray = Belt.Set.toArray;
//   let toList = Belt.Set.toList;
//   let minimum = Belt.Set.minimum;
//   let maximum = Belt.Set.maximum;
//   let get = Relude_Function.flip(Belt.Set.get);
//   let getOrElse = (value, default, t) =>
//     t |> get(value) |> Relude_Option_Base.getOrElse(default);
//   let split = Relude_Function.flip(Belt.Set.split);
// };
