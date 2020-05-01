module IO = {
  /**
  Suspends a Js.log side effect in an IO
  */
  let log: 'a. 'a => Relude_IO.t(unit, 'e) =
    a => Relude_IO.suspend(() => Js.Console.log(a));

  /**
  Suspends a Js.log2 side effect in an IO
  */
  let log2: 'a 'b. ('a, 'b) => Relude_IO.t(unit, 'e) =
    (a, b) => Relude_IO.suspend(() => Js.Console.log2(a, b));

  /**
  Suspends a Js.log3 side effect in an IO
  */
  let log3: 'a 'b 'c. ('a, 'b, 'c) => Relude_IO.t(unit, 'e) =
    (a, b, c) => Relude_IO.suspend(() => Js.Console.log3(a, b, c));

  /**
  Suspends a Js.log4 side effect in an IO
  */
  let log4: 'a 'b 'c 'd. ('a, 'b, 'c, 'd) => Relude_IO.t(unit, 'e) =
    (a, b, c, d) => Relude_IO.suspend(() => Js.Console.log4(a, b, c, d));

  /**
  Suspends a Js.logMany side effect in an IO
  */
  let logMany: 'a. array('a) => Relude_IO.t(unit, 'e) =
    xs => Relude_IO.suspend(() => Js.Console.logMany(xs));

  /**
  Suspends a Js.warn side effect in an IO
  */
  let warn: 'a. 'a => Relude_IO.t(unit, 'e) =
    a => Relude_IO.suspend(() => Js.Console.warn(a));
  // TODO: warn, error, info, time, etc.
};
