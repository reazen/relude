module ApplyExtensions:
  (A : BsAbstract.Interface.APPLY) => {
                                        module BsApplyExtensions:
                                          {
                                            module I:
                                              {
                                                let ( <$> ):
                                                  ('a => 'b, A.t('a)) =>
                                                  A.t('b);
                                                let ( <#> ):
                                                  (A.t('a), 'a => 'b) =>
                                                  A.t('b);
                                                let ( <*> ):
                                                  (A.t('a => 'b), A.t('a)) =>
                                                  A.t('b);
                                              };
                                            let apply_first:
                                              (A.t('a), A.t('b)) => A.t('a);
                                            let apply_second:
                                              (A.t('a), A.t('b)) => A.t('b);
                                            let apply_both:
                                              (A.t('a), A.t('b)) =>
                                              A.t(('a, 'b));
                                            let lift2:
                                              (('a, 'b) => 'c, A.t('a),
                                              A.t('b)) => A.t('c);
                                            let lift3:
                                              (('a, 'b, 'c) => 'd, A.t('a),
                                              A.t('b), A.t('c)) => A.t('d);
                                            let lift4:
                                              (('a, 'b, 'c, 'd) => 'e,
                                              A.t('a), A.t('b), A.t('c),
                                              A.t('d)) => A.t('e);
                                            let lift5:
                                              (('a, 'b, 'c, 'd, 'e) => 'f,
                                              A.t('a), A.t('b), A.t('c),
                                              A.t('d), A.t('e)) => A.t('f);
                                            module Infix:
                                              {
                                                let ( <* ):
                                                  (A.t('a), A.t('b)) =>
                                                  A.t('a);
                                                let ( *> ):
                                                  (A.t('a), A.t('b)) =>
                                                  A.t('b);
                                              };
                                          };
                                        let apply:
                                          (A.t('a => 'b), A.t('a)) => A.t('b);
                                        let applyFirst:
                                          (A.t('a), A.t('b)) => A.t('a);
                                        let applySecond:
                                          (A.t('a), A.t('b)) => A.t('b);
                                        let map2:
                                          (('a, 'b) => 'c, A.t('a),
                                          A.t('b)) => A.t('c);
                                        let map3:
                                          (('a, 'b, 'c) => 'd, A.t('a),
                                          A.t('b), A.t('c)) => A.t('d);
                                        let map4:
                                          (('a, 'b, 'c, 'd) => 'e, A.t('a),
                                          A.t('b), A.t('c), A.t('d)) =>
                                          A.t('e);
                                        let map5:
                                          (('a, 'b, 'c, 'd, 'e) => 'f,
                                          A.t('a), A.t('b), A.t('c), 
                                          A.t('d), A.t('e)) => A.t('f);
                                        let tuple2:
                                          (A.t('a), A.t('b)) => A.t(('a, 'b));
                                        let tuple3:
                                          (A.t('a), A.t('b), A.t('c)) =>
                                          A.t(('a, 'b, 'c));
                                        let tuple4:
                                          (A.t('a), A.t('b), A.t('c),
                                          A.t('d)) => A.t(('a, 'b, 'c, 'd));
                                        let tuple5:
                                          (A.t('a), A.t('b), A.t('c),
                                          A.t('d), A.t('e)) =>
                                          A.t(('a, 'b, 'c, 'd, 'e));
                                      };
module ApplyInfix:
  (A : BsAbstract.Interface.APPLY) => {
                                        module ApplyExtensions:
                                          {
                                            module BsApplyExtensions:
                                              {
                                                module I:
                                                  {
                                                    let ( <$> ):
                                                      ('a => 'b, A.t('a)) =>
                                                      A.t('b);
                                                    let ( <#> ):
                                                      (A.t('a), 'a => 'b) =>
                                                      A.t('b);
                                                    let ( <*> ):
                                                      (A.t('a => 'b),
                                                      A.t('a)) => A.t('b);
                                                  };
                                                let apply_first:
                                                  (A.t('a), A.t('b)) =>
                                                  A.t('a);
                                                let apply_second:
                                                  (A.t('a), A.t('b)) =>
                                                  A.t('b);
                                                let apply_both:
                                                  (A.t('a), A.t('b)) =>
                                                  A.t(('a, 'b));
                                                let lift2:
                                                  (('a, 'b) => 'c, A.t('a),
                                                  A.t('b)) => A.t('c);
                                                let lift3:
                                                  (('a, 'b, 'c) => 'd,
                                                  A.t('a), A.t('b),
                                                  A.t('c)) => A.t('d);
                                                let lift4:
                                                  (('a, 'b, 'c, 'd) => 'e,
                                                  A.t('a), A.t('b), A.t('c),
                                                  A.t('d)) => A.t('e);
                                                let lift5:
                                                  (('a, 'b, 'c, 'd, 'e) => 'f,
                                                  A.t('a), A.t('b), A.t('c),
                                                  A.t('d), A.t('e)) =>
                                                  A.t('f);
                                                module Infix:
                                                  {
                                                    let ( <* ):
                                                      (A.t('a), A.t('b)) =>
                                                      A.t('a);
                                                    let ( *> ):
                                                      (A.t('a), A.t('b)) =>
                                                      A.t('b);
                                                  };
                                              };
                                            let apply:
                                              (A.t('a => 'b), A.t('a)) =>
                                              A.t('b);
                                            let applyFirst:
                                              (A.t('a), A.t('b)) => A.t('a);
                                            let applySecond:
                                              (A.t('a), A.t('b)) => A.t('b);
                                            let map2:
                                              (('a, 'b) => 'c, A.t('a),
                                              A.t('b)) => A.t('c);
                                            let map3:
                                              (('a, 'b, 'c) => 'd, A.t('a),
                                              A.t('b), A.t('c)) => A.t('d);
                                            let map4:
                                              (('a, 'b, 'c, 'd) => 'e,
                                              A.t('a), A.t('b), A.t('c),
                                              A.t('d)) => A.t('e);
                                            let map5:
                                              (('a, 'b, 'c, 'd, 'e) => 'f,
                                              A.t('a), A.t('b), A.t('c),
                                              A.t('d), A.t('e)) => A.t('f);
                                            let tuple2:
                                              (A.t('a), A.t('b)) =>
                                              A.t(('a, 'b));
                                            let tuple3:
                                              (A.t('a), A.t('b), A.t('c)) =>
                                              A.t(('a, 'b, 'c));
                                            let tuple4:
                                              (A.t('a), A.t('b), A.t('c),
                                              A.t('d)) =>
                                              A.t(('a, 'b, 'c, 'd));
                                            let tuple5:
                                              (A.t('a), A.t('b), A.t('c),
                                              A.t('d), A.t('e)) =>
                                              A.t(('a, 'b, 'c, 'd, 'e));
                                          };
                                        let ( <*> ):
                                          (A.t('a => 'b), A.t('a)) => A.t('b);
                                        let ( <* ):
                                          (A.t('a), A.t('b)) => A.t('a);
                                        let ( *> ):
                                          (A.t('a), A.t('b)) => A.t('b);
                                      };
