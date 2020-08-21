let listAppend = (value, list) => List.concat([list, [value]]);
//let arrayAppend = (value, array) => Array.concat([array, [|value|]]);

module BoundedEnumExtensions = (E: Relude_Interface.BOUNDED_ENUM) => {
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
          // Since step can be positive or negative, we handle less_than and greater_than the same way by just adding the step
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
   [BoundedEnumExtensions.inverseMap] takes a function of type [t => a],
   where each [a] is a unique value, and returns a new function of type
   [a => option(t)].

   This is useful for generating parser utilities, reverse-lookup functions,
   etc.

   Note: since there is no way to determine the ordering of [a] without
   a comparator function, any call to the returned function must iterate
   through the entire list of possible values until it finds a match or
   reaches the end of the list. Therefore, the worst-case time complexity
   of the returned function is necessarily **linear**, and failed lookups
   will always be worst-case.

   See {!val:inverseMapToOrd} for a similar function that accepts a
   comparator for [a], and returns a reverse-lookup function with a
   worst-case running time of O(log(n)).

   Running time for staging: O(n)
   Running time of the returned lookup function: O(n)
   */
  let inverseMap: (E.t => 'a, 'a) => option(E.t) =
    map => {
      let rec loop = (lst, maybeE, getNextE) => {
        switch (maybeE) {
        | None =>
          // generate the closure to pass back to the caller:
          let arr = Belt.List.toArray(lst);
          let len = Belt.Array.length(arr);
          let return = a => {
            let rec find = (arr, i, limit) =>
              if (i < limit) {
                let (e__, a__) = Belt.Array.getUnsafe(arr, i);
                if (a__ === a) {
                  Some(e__);
                } else {
                  find(arr, succ(i), limit);
                };
              } else {
                None;
              };
            find(arr, 0, len);
          };
          return;
        | Some(e) =>
          // Continue building the associative list:
          loop([(e, map(e)), ...lst], getNextE(e), getNextE)
        };
      };

      // Build associative list and pass a lookup function to the caller:
      loop([], Some(E.bottom), E.succ);
    };

  /**
   [BoundedEnum.inverseMapWithComparator] takes a [~compare] function of
   type [(a, a) => Relude.Ordering.t], along with a mapping function of type
   [t => a], where each [a] is a unique value, and returns a new function
   of type [a => option(t)].

   It works similarly to `inverseMap`, but uses a comparator function for
   the ordered type [a]. This enables the use of an internal Map structure
   for O(log(n)) lookups. This might yield a slightly higher constant
   factor, but scales favorably for large enums.

   Running time for staging: O(n)
   Running time for returned lookup function: O(log(n))
   */
  let inverseMapWithComparator:
    type a.
      (~compare: (a, a) => BsBastet.Interface.ordering, E.t => a, a) =>
      option(E.t) =
    (~compare, map) => {
      // Generate the Map data structure:
      let (module M_) = (
        (module
         Map.Make({
           type t = a;
           let compare = (a, b) =>
             switch (compare(a, b)) {
             | `equal_to => 0
             | `less_than => (-1)
             | `greater_than => 1
             };
         })): (module Map.S with type key = a)
      );

      /**
       * Produce an 'a' value for each 'enum' value that exists,
       * then insert each of them into the Map.
       * */
      let rec loop = (insert, store, current) => {
        switch (current) {
        | None => M_.find_opt(_, store)
        | Some(e) =>
          let a = map(e);
          let st = insert(a, e, store);
          loop(insert, st, E.succ(e));
        };
      };

      loop(M_.add, M_.empty, Some(E.bottom));
    };
};
