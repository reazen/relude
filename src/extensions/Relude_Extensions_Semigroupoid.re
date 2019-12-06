module SemigroupoidExtensions = (S: BsAbstract.Interface.SEMIGROUPOID) => {
  let andThen = (aToB, bToC) => S.compose(bToC, aToB);
};

module SemigroupoidInfix = (S: BsAbstract.Interface.SEMIGROUPOID) => {
  module SE = SemigroupoidExtensions(S);
  let (<<<) = S.compose;
  let (>>>) = SE.andThen;
};