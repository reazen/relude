module SemigroupoidExtensions = (S: Bastet.Interface.SEMIGROUPOID) => {
  let andThen = (aToB, bToC) => S.compose(bToC, aToB);
};

module SemigroupoidInfix = (S: Bastet.Interface.SEMIGROUPOID) => {
  module SE = SemigroupoidExtensions(S);
  let (<<<) = S.compose;
  let (>>>) = SE.andThen;
};
