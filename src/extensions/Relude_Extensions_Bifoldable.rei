module BifoldableExtensions:
  (B : BsAbstract.Interface.BIFOLDABLE) => {
                                             let bifoldLeft:
                                               (('a, 'b) => 'a,
                                               ('a, 'c) => 'a, 'a,
                                               B.t('b, 'c)) => 'a;
                                             let bifoldRight:
                                               (('a, 'b) => 'b,
                                               ('c, 'b) => 'b, 'b,
                                               B.t('a, 'c)) => 'b;
                                           };
