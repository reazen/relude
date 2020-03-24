module SemigroupoidExtensions = (S: BsBastet.Interface.SEMIGROUPOID) => {
  let andThen = (aToB, bToC) => S.compose(bToC, aToB);
};

module SemigroupoidInfix = (S: BsBastet.Interface.SEMIGROUPOID) => {
  module SE = SemigroupoidExtensions(S);
  let (<<<) = S.compose;
  let (>>>) = SE.andThen;
};
