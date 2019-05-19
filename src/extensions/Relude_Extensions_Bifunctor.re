module BifunctorExtensions = (B: BsAbstract.Interface.BIFUNCTOR) => {
  //let bimap = B.bimap;
  // leftMap/rightMap? - these are confusing with the ReasonML convention of putting the "success value" on the left in many bi-functors
};

module BifunctorInfix = (B: BsAbstract.Interface.BIFUNCTOR) => {
  let (<<$>>) = B.bimap;
};