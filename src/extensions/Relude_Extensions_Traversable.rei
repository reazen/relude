module TraversableExtensions:
  (T : BsAbstract.Interface.TRAVERSABLE) => {
                                              let traverse:
                                                ('a => T.applicative_t('b),
                                                T.t('a)) =>
                                                T.applicative_t(T.t('b));
                                              let sequence:
                                                T.t(T.applicative_t('a)) =>
                                                T.applicative_t(T.t('a));
                                            };
