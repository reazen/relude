module BifunctorExtensions = (B: BsAbstract.Interface.BIFUNCTOR) => {
  let mapLeft: 'a 'b 'c. ('a => 'c, B.t('a, 'b)) => B.t('c, 'b) =
    (aToC, fab) => B.bimap(aToC, b => b, fab);

  let mapRight: 'a 'b 'd. ('b => 'd, B.t('a, 'b)) => B.t('a, 'd) =
    (bToD, fab) => B.bimap(a => a, bToD, fab);

  // This makes the assumption that the error type is on the right, which is true
  // for things like Result, IO, Validation, Ior, etc.
  let mapError = mapRight;
};

module BifunctorInfix = (B: BsAbstract.Interface.BIFUNCTOR) => {
  let (<<$>>) = B.bimap;
};