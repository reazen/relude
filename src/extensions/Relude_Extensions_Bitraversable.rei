module BitraversableExtensions:
  (B : BsAbstract.Interface.BITRAVERSABLE) => {
                                                let bitraverse:
                                                  ('a => B.applicative_t('b),
                                                  'c => B.applicative_t('d),
                                                  B.t('a, 'c)) =>
                                                  B.applicative_t(B.t('b, 'd));
                                                let bisequence:
                                                  B.t(B.applicative_t('a),
                                                       B.applicative_t('b)) =>
                                                  B.applicative_t(B.t('a, 'b));
                                              };
