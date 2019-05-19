module MonadExtensions:
  (M : BsAbstract.Interface.MONAD) => {
                                        module BsMonadExtensions:
                                          {
                                            module I:
                                              {
                                                let ( <$> ):
                                                  ('a => 'b, M.t('a)) =>
                                                  M.t('b);
                                                let ( <#> ):
                                                  (M.t('a), 'a => 'b) =>
                                                  M.t('b);
                                                let ( <*> ):
                                                  (M.t('a => 'b), M.t('a)) =>
                                                  M.t('b);
                                                let ( >>= ):
                                                  (M.t('a), 'a => M.t('b)) =>
                                                  M.t('b);
                                                let ( =<< ):
                                                  ('a => M.t('b), M.t('a)) =>
                                                  M.t('b);
                                                let ( >=> ):
                                                  ('a => M.t('b),
                                                  'b => M.t('c), 'a) =>
                                                  M.t('c);
                                                let ( <=< ):
                                                  ('a => M.t('b),
                                                  'c => M.t('a), 'c) =>
                                                  M.t('b);
                                              };
                                            module A:
                                              {
                                                module I:
                                                  {
                                                    let ( <$> ):
                                                      ('a => 'b, M.t('a)) =>
                                                      M.t('b);
                                                    let ( <#> ):
                                                      (M.t('a), 'a => 'b) =>
                                                      M.t('b);
                                                    let ( <*> ):
                                                      (M.t('a => 'b),
                                                      M.t('a)) => M.t('b);
                                                  };
                                                let liftA1:
                                                  ('a => 'b, M.t('a)) =>
                                                  M.t('b);
                                                let when_:
                                                  (bool, M.t(unit)) =>
                                                  M.t(unit);
                                                let unless:
                                                  (bool, M.t(unit)) =>
                                                  M.t(unit);
                                              };
                                            let flatten:
                                              M.t(M.t('a)) => M.t('a);
                                            let compose_kliesli:
                                              ('a => M.t('b), 'b => M.t('c),
                                              'a) => M.t('c);
                                            let compose_kliesli_flipped:
                                              ('b => M.t('c), 'a => M.t('b),
                                              'a) => M.t('c);
                                            let if_m:
                                              (M.t(bool), M.t('a),
                                              M.t('a)) => M.t('a);
                                            let liftM1:
                                              ('a => 'b, M.t('a)) => M.t('b);
                                            let ap:
                                              (M.t('a => 'b), M.t('a)) =>
                                              M.t('b);
                                            let when_:
                                              (M.t(bool), M.t(unit)) =>
                                              M.t(unit);
                                            let unless:
                                              (M.t(bool), M.t(unit)) =>
                                              M.t(unit);
                                          };
                                        let bind:
                                          (M.t('a), 'a => M.t('b)) => M.t('b);
                                        let flatMap:
                                          ('a => M.t('b), M.t('a)) => M.t('b);
                                        let flatten: M.t(M.t('a)) => M.t('a);
                                        let composeKleisli:
                                          ('a => M.t('b), 'b => M.t('c),
                                          'a) => M.t('c);
                                        let flipComposeKleisli:
                                          ('b => M.t('c), 'a => M.t('b),
                                          'a) => M.t('c);
                                        let liftM1:
                                          ('a => 'b, M.t('a)) => M.t('b);
                                        let when_:
                                          (M.t(bool), M.t(unit)) => M.t(unit);
                                        let unless:
                                          (M.t(bool), M.t(unit)) => M.t(unit);
                                      };
module MonadInfix:
  (M : BsAbstract.Interface.MONAD) => {
                                        module MonadExtensions:
                                          {
                                            module BsMonadExtensions:
                                              {
                                                module I:
                                                  {
                                                    let ( <$> ):
                                                      ('a => 'b, M.t('a)) =>
                                                      M.t('b);
                                                    let ( <#> ):
                                                      (M.t('a), 'a => 'b) =>
                                                      M.t('b);
                                                    let ( <*> ):
                                                      (M.t('a => 'b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >>= ):
                                                      (M.t('a),
                                                      'a => M.t('b)) =>
                                                      M.t('b);
                                                    let ( =<< ):
                                                      ('a => M.t('b),
                                                      M.t('a)) => M.t('b);
                                                    let ( >=> ):
                                                      ('a => M.t('b),
                                                      'b => M.t('c), 'a) =>
                                                      M.t('c);
                                                    let ( <=< ):
                                                      ('a => M.t('b),
                                                      'c => M.t('a), 'c) =>
                                                      M.t('b);
                                                  };
                                                module A:
                                                  {
                                                    module I:
                                                      {
                                                        let ( <$> ):
                                                          ('a => 'b,
                                                          M.t('a)) => 
                                                          M.t('b);
                                                        let ( <#> ):
                                                          (M.t('a),
                                                          'a => 'b) =>
                                                          M.t('b);
                                                        let ( <*> ):
                                                          (M.t('a => 'b),
                                                          M.t('a)) => 
                                                          M.t('b);
                                                      };
                                                    let liftA1:
                                                      ('a => 'b, M.t('a)) =>
                                                      M.t('b);
                                                    let when_:
                                                      (bool, M.t(unit)) =>
                                                      M.t(unit);
                                                    let unless:
                                                      (bool, M.t(unit)) =>
                                                      M.t(unit);
                                                  };
                                                let flatten:
                                                  M.t(M.t('a)) => M.t('a);
                                                let compose_kliesli:
                                                  ('a => M.t('b),
                                                  'b => M.t('c), 'a) =>
                                                  M.t('c);
                                                let compose_kliesli_flipped:
                                                  ('b => M.t('c),
                                                  'a => M.t('b), 'a) =>
                                                  M.t('c);
                                                let if_m:
                                                  (M.t(bool), M.t('a),
                                                  M.t('a)) => M.t('a);
                                                let liftM1:
                                                  ('a => 'b, M.t('a)) =>
                                                  M.t('b);
                                                let ap:
                                                  (M.t('a => 'b), M.t('a)) =>
                                                  M.t('b);
                                                let when_:
                                                  (M.t(bool), M.t(unit)) =>
                                                  M.t(unit);
                                                let unless:
                                                  (M.t(bool), M.t(unit)) =>
                                                  M.t(unit);
                                              };
                                            let bind:
                                              (M.t('a), 'a => M.t('b)) =>
                                              M.t('b);
                                            let flatMap:
                                              ('a => M.t('b), M.t('a)) =>
                                              M.t('b);
                                            let flatten:
                                              M.t(M.t('a)) => M.t('a);
                                            let composeKleisli:
                                              ('a => M.t('b), 'b => M.t('c),
                                              'a) => M.t('c);
                                            let flipComposeKleisli:
                                              ('b => M.t('c), 'a => M.t('b),
                                              'a) => M.t('c);
                                            let liftM1:
                                              ('a => 'b, M.t('a)) => M.t('b);
                                            let when_:
                                              (M.t(bool), M.t(unit)) =>
                                              M.t(unit);
                                            let unless:
                                              (M.t(bool), M.t(unit)) =>
                                              M.t(unit);
                                          };
                                        let ( >>= ):
                                          (M.t('a), 'a => M.t('b)) => M.t('b);
                                        let ( =<< ):
                                          ('a => M.t('b), M.t('a)) => M.t('b);
                                        let ( >=> ):
                                          ('a => M.t('b), 'b => M.t('c),
                                          'a) => M.t('c);
                                        let ( <=< ):
                                          ('a => M.t('b), 'c => M.t('a),
                                          'c) => M.t('b);
                                      };
