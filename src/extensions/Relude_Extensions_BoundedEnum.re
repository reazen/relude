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
};
