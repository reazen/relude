module ShowExtensions = (S: Bastet.Interface.SHOW) => {
  let logShow: S.t => unit = a => print_endline(S.show(a));

  let infoShow: S.t => unit = a =>  print_endline(S.show(a));

  let warnShow: S.t => unit = a =>  print_endline(S.show(a));

  let errorShow: S.t => unit = a => print_endline(S.show(a));
};

module ShowInfix = (S: Bastet.Interface.SHOW) => {};
