module BifoldableExtensions = (B: BsAbstract.Interface.BIFOLDABLE) => {
  let bifoldLeft = B.bifold_left;

  let bifoldRight = B.bifold_right;
  // ???
};