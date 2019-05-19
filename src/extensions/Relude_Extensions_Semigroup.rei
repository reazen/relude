module SemigroupExtensions:
  (S : BsAbstract.Interface.SEMIGROUP) => { let append: (S.t, S.t) => S.t; };
module SemigroupInfix:
  (S : BsAbstract.Interface.SEMIGROUP) => { let ( |+| ): (S.t, S.t) => S.t; };
