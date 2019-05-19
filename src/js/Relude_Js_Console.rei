module IO:
  {
    let log: string => Relude_IO.t(unit, 'e);
    let warn: string => Relude_IO.t(unit, 'e);
  };
