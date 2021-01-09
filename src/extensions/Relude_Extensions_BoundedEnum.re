let listAppend = (value, list) => List.concat([list, [value]]);
//let arrayAppend = (value, array) => Array.concat([array, [|value|]]);

module BoundedEnumExtensions = (E: Relude_Interface.BOUNDED_ENUM) => {
  include Relude_Extensions_Enum.EnumExtensions(E);

  /**
  Generates a list of enum values starting at [~start], then going to [~next],
  then using that step size to generate the rest of the list.
  */
  let fromThenToAsList: (~start: E.t, ~next: E.t, ~finish: E.t) => list(E.t) =
    (~start, ~next, ~finish) => {
      let startInt = E.fromEnum(start);
      let nextInt = E.fromEnum(next);
      let stepInt = nextInt - startInt; // Can be negative for backwards
      let rec go =
              (acc: list(E.t), current: E.t, currentInt: int): list(E.t) => {
        switch (E.compare(current, finish)) {
        | `equal_to => listAppend(current, acc)
        | `less_than
        | `greater_than =>
          /**
        Since step can be positive or negative, we handle less_than
        and greater_than the same way by just adding the step
        */
          let nextInt = currentInt + stepInt;
          switch (E.toEnum(nextInt)) {
          | Some(next) => go(listAppend(current, acc), next, nextInt)
          | None => listAppend(current, acc)
          };
        };
      };
      go([], start, startInt);
    };

  /**
   [BoundedEnumExtensions.inverseMapEqBy] takes an equality function of type
   [(a, a) => bool] along with a function of type [t => a], where each [a]
   is a unique value, and returns a new function of type [a => option(t)].

   [inverseMapEqBy] performs the same operations as [inverseMapEq], but
   it accepts an equality function for [a], instead of a first-class
   [Eq] module.

   Note: since there is no way to determine the ordering of [a] without
   a comparator function, any call to the returned function must iterate
   through the entire list of possible values until it finds a match or
   reaches the end of the list. Therefore, the worst-case time complexity
   of the returned function is **linear**, and failed lookups will always
   be worst-case.

   See {!val:inverseMapOrd} or {!val: inverseMapOrdBy} for a similar function
   that accepts a comparator for [a], and returns a reverse-lookup function
   with a worst-case running time of O(log(n)).

   Running time for staging: O(n)
   Running time of the returned lookup function: O(n)
   */
  let inverseMapEqBy: type a. ((a, a) => bool, E.t => a, a) => option(E.t) =
    (eqA, eToA) => {
      let rec loop = (lst, maybeE, getNextE) => {
        switch (maybeE) {
        | None =>
          // generate the closure to pass back to the caller:
          let arr = Belt.List.toArray(lst);
          let len = Belt.Array.length(arr);
          let result = a => {
            let rec find = (arr, i, limit) =>
              if (i < limit) {
                let (e__, a__) = Belt.Array.getUnsafe(arr, i);
                if (eqA(a__, a)) {
                  Some(e__);
                } else {
                  find(arr, succ(i), limit);
                };
              } else {
                None;
              };
            find(arr, 0, len);
          };
          result;
        | Some(e) =>
          // Continue building the associative list:
          loop([(e, eToA(e)), ...lst], getNextE(e), getNextE)
        };
      };

      // Build associative list and pass a lookup function to the caller:
      loop([], Some(E.bottom), E.succ);
    };

  let inverseMapEqBy2: type a. ((a, a) => bool, E.t => a, a) => option(E.t) =
    (eqA, eToA) => {
      // create an associative array for reverse lookups:
      let arr =
        upFromIncludingAsList(E.bottom)
        |> Belt.List.mapU(_, (. e) => (e, eToA(e)))
        |> Belt.List.toArray;
      let len = Belt.Array.length(arr);
      // generate the closure to pass back to the caller:
      let result = a => {
        let rec find = (arr, i, limit) =>
          if (i < limit) {
            let (e__, a__) = Belt.Array.getUnsafe(arr, i);
            if (eqA(a__, a)) {
              Some(e__);
            } else {
              find(arr, succ(i), limit);
            };
          } else {
            None;
          };
        find(arr, 0, len);
      };
      result;
    };

  /**
   [BoundedEnumExtensions.inverseMapEq] takes a first-class [EQ] module for
   type [a], along with a function of type [t => a] where each [a] is a
   unique value, and returns a new function of type [a => option(t)].

   This is useful for generating parser utilities, reverse-lookup functions,
   etc.

   Note: since there is no way to determine the ordering of [a] without
   a comparator function, any call to the returned function must iterate
   through the entire list of possible values until it finds a match or
   reaches the end of the list. Therefore, the worst-case time complexity
   of the returned function is **linear**, and failed lookups will always
   be worst-case.

   See {!val:inverseMapToOrd} for a similar function that accepts a
   comparator for [a], and returns a reverse-lookup function with a
   worst-case running time of O(log(n)).

   Running time for staging: O(n)
   Running time of the returned lookup function: O(n)
   */
  let inverseMapEq:
    type a.
      (~eqA: (module BsBastet.Interface.EQ with type t = a), E.t => a, a) =>
      option(E.t) =
    (~eqA, eToA) => {
      let (module EqA) = (eqA: (module BsBastet.Interface.EQ with type t = a));
      inverseMapEqBy(EqA.eq, eToA);
    };

  /**
   [BoundedEnumExtensions.inverseMapOrdBy] takes a comparator function of
   type [(a, a) => Relude.Ordering.t], along with a mapping function of type
   [t => a], where each [a] is a unique value, and returns a new function
   of type [a => option(t)].

   [inverseMapOrdBy] performs the same operations as [inverseMapOrd], but
   it accepts a comparator function for [a], instead of a first-class
   [Ord] module.

   Running time for staging: O(n)
   Running time for returned lookup function: O(log(n))
   */
  let inverseMapOrdBy:
    type a.
      ((a, a) => BsBastet.Interface.ordering, E.t => a, a) => option(E.t) =
    (cmp, eToA) => {
      // Generate the Map data structure:
      let (module M) = (
        (module
         Map.Make({
           type t = a;
           let compare = (a, b) =>
             switch (cmp(a, b)) {
             | `equal_to => 0
             | `less_than => (-1)
             | `greater_than => 1
             };
         })): (module Map.S with type key = a)
      );
      /**
       Produce an 'a' value for each 'enum' value that exists,
       then insert each of them into the Map.
       */
      let rec loop = (store, current) => {
        switch (current) {
        | None => M.find_opt(_, store)
        | Some(e) =>
          let a = eToA(e);
          let st = M.add(a, e, store);
          loop(st, E.succ(e));
        };
      };

      loop(M.empty, Some(E.bottom));
    };

  let inverseMapOrdBy2:
    type a.
      ((a, a) => BsBastet.Interface.ordering, E.t => a, a) => option(E.t) =
    (cmp, eToA) => {
      // Generate the Map data structure:
      let (module M) = (
        (module
         Map.Make({
           type t = a;
           let compare = (a, b) =>
             switch (cmp(a, b)) {
             | `equal_to => 0
             | `less_than => (-1)
             | `greater_than => 1
             };
         })): (module Map.S with type key = a)
      );
      let store =
        upFromIncludingAsList(E.bottom)
        ->Belt.List.reduceU(
            M.empty,
            (. acc, e) => {
              let a = eToA(e);
              M.add(a, e, acc);
            },
          );

      M.find_opt(_, store);
    };

  /**
   [BoundedEnumExtensions.inverseMapOrd] takes a first-class [ORD] module
   for type [a], along with a function of type [t => a] where each [a] is
   a unique value, and returns a new function of type [a => option(t)].

   This is useful for generating parser utilities, reverse-lookup functions,
   etc.

   Running time for staging: O(n)
   Running time for returned lookup function: O(log(n))
   */
  let inverseMapOrd:
    type a.
      (~ordA: (module BsBastet.Interface.ORD with type t = a), E.t => a, a) =>
      option(E.t) =
    (~ordA, eToA) => {
      let (module OrdA) = (
        ordA: (module BsBastet.Interface.ORD with type t = a)
      );
      inverseMapOrdBy(OrdA.compare, eToA);
    };

  /**
   [BoundedEnumExtensions.inverseMapString] is specialized version of
   [inverseMapOrd] that is optimized for [string] types. It takes function
   of type [t => string], where each value of [t] maps to a unique [string]
   value. It returns a new function of type [string => option(t)], which inverts
   that mapping.

   Running time for staging: O(n)
   Running time for returned lookup function: O(log(n))
   */
  let inverseMapString: (E.t => string, string) => option(E.t) =
    (eToString: E.t => string) => {
      let strMap = Belt.MutableMap.String.make();
      let rec loop = (store, maybeE) => {
        switch (maybeE) {
        | None => (string => Belt.MutableMap.String.get(store, string))
        | Some(e) =>
          Belt.MutableMap.String.set(store, eToString(e), e);
          loop(store, E.succ(e));
        };
      };
      loop(strMap, Some(E.bottom));
    };
};
