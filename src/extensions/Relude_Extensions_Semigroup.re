/**
 * Extensions for any SEMIGROUP
 */
module SemigroupExtensions = (S: BsAbstract.Interface.SEMIGROUP) => {
  let concatNamed = (~prefix: S.t, suffix: S.t) => S.append(prefix, suffix);
};

/**
 * Infix operator extensions for any SEMIGROUP
 */
module SemigroupInfix = (S: BsAbstract.Interface.SEMIGROUP) => {
  let (|+|) = S.append;
};