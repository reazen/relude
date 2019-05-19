module AltExtensions:
  (A : BsAbstract.Interface.ALT) => {
                                      let alt: (A.t('a), A.t('a)) => A.t('a);
                                    };
module AltInfix:
  (A : BsAbstract.Interface.ALT) => {
                                      let ( <|> ):
                                        (A.t('a), A.t('a)) => A.t('a);
                                    };
