module BifunctorExtensions:
  (B : BsAbstract.Interface.BIFUNCTOR) => {
                                            let bimap:
                                              ('a => 'b, 'c => 'd,
                                              B.t('a, 'c)) => B.t('b, 'd);
                                          };
module BifunctorInfix:
  (B : BsAbstract.Interface.BIFUNCTOR) => {
                                            let ( <<$>> ):
                                              ('a => 'b, 'c => 'd,
                                              B.t('a, 'c)) => B.t('b, 'd);
                                          };
