
module IO {
  module IO = Relude_IO;

  let log: string => IO.t(unit, 'e) = message => IO.suspend(() => {
    Js.Console.log(message);
  });

  let warn: string => IO.t(unit, 'e) = message => IO.suspend(() => {
    Js.Console.warn(message);
  });
}
