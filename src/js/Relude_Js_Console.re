module IO = {
  let log: string => Relude_IO.t(unit, 'e) =
    message => Relude_IO.suspend(() => Js.Console.log(message));

  let warn: string => Relude_IO.t(unit, 'e) =
    message => Relude_IO.suspend(() => Js.Console.warn(message));
};