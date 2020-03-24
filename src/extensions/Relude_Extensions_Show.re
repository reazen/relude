module ShowExtensions = (S: BsBastet.Interface.SHOW) => {
  let logShow: S.t => unit = a => Js.log(S.show(a));

  let infoShow: S.t => unit = a => Js.Console.info(S.show(a));

  let warnShow: S.t => unit = a => Js.Console.warn(S.show(a));

  let errorShow: S.t => unit = a => Js.Console.error(S.show(a));
};

module ShowInfix = (S: BsBastet.Interface.SHOW) => {};
