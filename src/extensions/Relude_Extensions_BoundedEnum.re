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
      // Create a list of tuples [a, E.t] used for doign lookup
      let lookupList: list((a, E.t)) =
        upFromIncludingAsList(E.bottom)
        |> Relude_List_Instances.map(e => (eToA(e), e))

      // Create the lookup function which closes over the lookupList
      let lookup = (a) => {
        lookupList |> Relude_List_Instances.find(((a', _)) => eqA(a', a)) |> Relude_Option_Instances.map(snd)
      }

      lookup
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
      type a. ((a, a) => BsBastet.Interface.ordering, E.t => a, a) => option(E.t) =
    (compareA, eToA) => {
      // Create the Map module used for doing lookups of a => E.t
      // This is necessary because of how OCaml maps work
      let (module M) = (
        (module
         Map.Make({
           type t = a;
           let compare = (a, b) =>
             switch (compareA(a, b)) {
             | `equal_to => 0
             | `less_than => (-1)
             | `greater_than => 1
             };
         })): (module Map.S with type key = a)
      );

      // Enumerate the E.t values and create the lookup map
      let lookupMap =
        upFromIncludingAsList(E.bottom) |>
        Relude_List_Instances.foldRight((e, acc) => M.add(eToA(e), e, acc), M.empty)

      // Lookup function which closes over the lookup map
      let lookup = (a) => M.find_opt(a, lookupMap)

      lookup
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
      inverseMapOrdBy(BsBastet.String.Ord.compare, eToString)
    };
};
