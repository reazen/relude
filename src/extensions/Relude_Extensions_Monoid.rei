module MonoidExtensions:
  (M : BsAbstract.Interface.MONOID) => {
                                         module BsMonoidExtensions:
                                           {
                                             module I:
                                               {
                                                 let ( <:> ):
                                                   (M.t, M.t) => M.t;
                                               };
                                             let power: (M.t, int) => M.t;
                                             let guard: (bool, M.t) => M.t;
                                           };
                                         let empty: M.t;
                                         let guard: (bool, M.t) => M.t;
                                         let power: (M.t, int) => M.t;
                                       };
