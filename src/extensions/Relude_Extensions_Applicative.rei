module ApplicativeExtensions:
  (A : BsAbstract.Interface.APPLICATIVE) => {
                                              module BsApplicativeExtensions:
                                                {
                                                  module I:
                                                    {
                                                      let ( <$> ):
                                                        ('a => 'b,
                                                        A.t('a)) => A.t('b);
                                                      let ( <#> ):
                                                        (A.t('a),
                                                        'a => 'b) => 
                                                        A.t('b);
                                                      let ( <*> ):
                                                        (A.t('a => 'b),
                                                        A.t('a)) => A.t('b);
                                                    };
                                                  let liftA1:
                                                    ('a => 'b, A.t('a)) =>
                                                    A.t('b);
                                                  let when_:
                                                    (bool, A.t(unit)) =>
                                                    A.t(unit);
                                                  let unless:
                                                    (bool, A.t(unit)) =>
                                                    A.t(unit);
                                                };
                                              let pure: 'a => A.t('a);
                                              let liftA1:
                                                ('a => 'b, A.t('a)) =>
                                                A.t('b);
                                              let when_:
                                                (bool, A.t(unit)) =>
                                                A.t(unit);
                                              let unless:
                                                (bool, A.t(unit)) =>
                                                A.t(unit);
                                            };
