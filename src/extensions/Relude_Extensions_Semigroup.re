module SemigroupExtensions = (S: BsAbstract.Interface.SEMIGROUP) => {
  //let append = S.append;
};

module SemigroupInfix = (S: BsAbstract.Interface.SEMIGROUP) => {
  let (|+|) = S.append;
};