let listAppend = (value, list) => List.concat([list, [value]]);
//let arrayAppend = (value, array) => Array.concat([array, [|value|]]);

module EnumExtensions = (E: Relude_Interface.ENUM) => {
  // These would be better implemented in terms of Unfoldable rather than list,
  // but I'll just use list for simplicity.
  // We can change/add this when needed/requested.

  /**
  Generates a list of values of the Enum between the given start and finish
  values (inclusive)
  */
  let fromToAsList: (~start: E.t, ~finish: E.t) => list(E.t) =
    (~start, ~finish) => {
      let rec go = (acc, current) =>
        switch (E.compare(current, finish)) {
        | `equal_to => listAppend(current, acc)
        | `less_than =>
          switch (E.succ(current)) {
          | Some(next) => go(listAppend(current, acc), next)
          | None => listAppend(current, acc)
          }
        | `greater_than =>
          switch (E.pred(current)) {
          | Some(prev) => go(listAppend(current, acc), prev)
          | None => listAppend(current, acc)
          }
        };
      go([], start);
    };

  /**
  Generates a list of enum values that come after the given start value (not
  including the start). Warning: the resulting list could be unbounded here.
  */
  let upFromAsList: E.t => list(E.t) =
    start => {
      let rec go: (list(E.t), E.t) => list(E.t) =
        (acc, current) =>
          switch (E.succ(current)) {
          | Some(next) => go(listAppend(current, acc), next)
          | None => listAppend(current, acc)
          };
      switch (E.succ(start)) {
      | Some(next) => go([], next)
      | None => []
      };
    };

  /**
  Generates a list of enum values that come after the given start value
  (including the start). Warning: the resulting list could be unbounded here.
  */
  let upFromIncludingAsList: E.t => list(E.t) =
    start => {
      let rec go: (list(E.t), E.t) => list(E.t) =
        (acc, current) =>
          switch (E.succ(current)) {
          | Some(next) => go(listAppend(current, acc), next)
          | None => listAppend(current, acc)
          };
      go([], start);
    };

  /**
  Generates a list of enum values that come before the given start value (not
  including the start). Warning: the resulting list could be unbounded here.
  */
  let downFromAsList: E.t => list(E.t) =
    start => {
      let rec go: (list(E.t), E.t) => list(E.t) =
        (acc, current) =>
          switch (E.pred(current)) {
          | Some(prev) => go(listAppend(current, acc), prev)
          | None => listAppend(current, acc)
          };
      switch (E.pred(start)) {
      | Some(next) => go([], next)
      | None => []
      };
    };

  /**
  Generates a list of enum values that come before the given start value
  (including the start). Warning: the resulting list could be unbounded here.
  */
  let downFromIncludingAsList: E.t => list(E.t) =
    start => {
      let rec go: (list(E.t), E.t) => list(E.t) =
        (acc, current) =>
          switch (E.pred(current)) {
          | Some(prev) => go(listAppend(current, acc), prev)
          | None => listAppend(current, acc)
          };
      go([], start);
    };
};
