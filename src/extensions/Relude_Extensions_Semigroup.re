module SemigroupExtensions = (S: BsAbstract.Interface.SEMIGROUP) => {};

module SemigroupInfix = (S: BsAbstract.Interface.SEMIGROUP) => {
  let (|+|) = S.append;
};