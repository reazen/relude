/**
 * Extensions for any SEMIGROUP_ANY
 */
module SemigroupAnyExtensions = (S: BsBastet.Interface.SEMIGROUP_ANY) => {
  let concatNamed = (~prefix: S.t('a), suffix: S.t('a)): S.t('a) =>
    S.append(prefix, suffix);
};

/**
 * Infix operator extensions for any SEMIGROUP_ANY
 */
module SemigroupAnyInfix = (S: BsBastet.Interface.SEMIGROUP_ANY) => {
  let (|+|) = S.append;
};
