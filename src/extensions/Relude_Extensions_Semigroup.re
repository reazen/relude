/**
 * Extensions for any SEMIGROUP
 */
module SemigroupExtensions = (S: BsAbstract.Interface.SEMIGROUP) => {};

/**
 * Infix operator extensions for any SEMIGROUP
 */
module SemigroupInfix = (S: BsAbstract.Interface.SEMIGROUP) => {
  let (|+|) = S.append;
};