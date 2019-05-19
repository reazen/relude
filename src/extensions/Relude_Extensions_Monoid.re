module MonoidExtensions = (M: BsAbstract.Interface.MONOID) => {
  module BsMonoidExtensions = BsAbstract.Functions.Monoid(M);

  //let empty = M.empty;

  let guard: (bool, M.t) => M.t = BsMonoidExtensions.guard;

  let power: (M.t, int) => M.t = BsMonoidExtensions.power;
};