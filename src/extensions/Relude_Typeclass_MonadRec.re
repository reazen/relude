type step('a, 'b) = [
   | `Loop('a)
   | `Done('b)
  ];

let tailRec: 'a 'b. ('a => step('a, 'b), 'a) => 'b =
  (f, v) => {
    let rec go =
      fun
      | `Loop(a) => go(f(a))
      | `Done(b) => b;

    go(f(v));
  };

module type MONAD_REC = {
  include BsAbstract.Interface.MONAD;
  let tailRecM: ('a => t(step('a, 'b)), 'a) => t('b);
};

module MonadRecExtensions = (M: MONAD_REC) => {
  let tailRecM2:
    'a 'b 'c.
    (('a, 'b) => M.t(step(('a, 'b), 'c)), 'a, 'b) => M.t('c)
   =
    (f, a, b) => M.tailRecM(((a, b)) => f(a, b), (a, b));
};
