module FunctorExtensions:
  (F : BsAbstract.Interface.FUNCTOR) => {
                                          module BsFunctorExtensions:
                                            {
                                              let void: F.t('a) => F.t(unit);
                                              let void_right:
                                                ('a, F.t('b)) => F.t('a);
                                              let void_left:
                                                (F.t('a), 'b) => F.t('b);
                                              let flap:
                                                (F.t('a => 'b), 'a) =>
                                                F.t('b);
                                            };
                                          let map:
                                            ('a => 'b, F.t('a)) => F.t('b);
                                          let flipMap:
                                            (F.t('a), 'a => 'b) => F.t('b);
                                          let void: F.t('a) => F.t(unit);
                                          let voidRight:
                                            ('a, F.t('b)) => F.t('a);
                                          let voidLeft:
                                            (F.t('a), 'b) => F.t('b);
                                          let flap:
                                            (F.t('a => 'b), 'a) => F.t('b);
                                        };
module FunctorInfix:
  (F : BsAbstract.Interface.FUNCTOR) => {
                                          module FunctorExtensions:
                                            {
                                              module BsFunctorExtensions:
                                                {
                                                  let void:
                                                    F.t('a) => F.t(unit);
                                                  let void_right:
                                                    ('a, F.t('b)) => F.t('a);
                                                  let void_left:
                                                    (F.t('a), 'b) => F.t('b);
                                                  let flap:
                                                    (F.t('a => 'b), 'a) =>
                                                    F.t('b);
                                                };
                                              let map:
                                                ('a => 'b, F.t('a)) =>
                                                F.t('b);
                                              let flipMap:
                                                (F.t('a), 'a => 'b) =>
                                                F.t('b);
                                              let void: F.t('a) => F.t(unit);
                                              let voidRight:
                                                ('a, F.t('b)) => F.t('a);
                                              let voidLeft:
                                                (F.t('a), 'b) => F.t('b);
                                              let flap:
                                                (F.t('a => 'b), 'a) =>
                                                F.t('b);
                                            };
                                          let ( <$> ):
                                            ('a => 'b, F.t('a)) => F.t('b);
                                          let ( <#> ):
                                            (F.t('a), 'a => 'b) => F.t('b);
                                          let ( <$ ):
                                            ('a, F.t('b)) => F.t('a);
                                          let ( $> ):
                                            (F.t('a), 'b) => F.t('b);
                                          let ( <@> ):
                                            (F.t('a => 'b), 'a) => F.t('b);
                                        };
