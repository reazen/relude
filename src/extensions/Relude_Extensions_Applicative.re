module ApplicativeExtensions = (A: BsAbstract.Interface.APPLICATIVE) => {
  module BsApplicativeExtensions = BsAbstract.Functions.Applicative(A);

  let liftA1 = BsApplicativeExtensions.liftA1;

  let when_ = BsApplicativeExtensions.when_;

  let unless = BsApplicativeExtensions.unless;
};