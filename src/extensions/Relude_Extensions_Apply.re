module ApplyExtensions = (A: BsAbstract.Interface.APPLY) => {
  module BsApplyExtensions = BsAbstract.Functions.Apply(A);

  //let apply: 'a 'b. (A.t('a => 'b), A.t('a)) => A.t('b) = A.apply;

  let applyFirst: 'a 'b. (A.t('a), A.t('b)) => A.t('a) = BsApplyExtensions.apply_first;

  let applySecond: 'a 'b. (A.t('a), A.t('b)) => A.t('b) = BsApplyExtensions.apply_second;

  let map2: 'a 'b 'c. (('a, 'b) => 'c, A.t('a), A.t('b)) => A.t('c) = BsApplyExtensions.lift2;

  let map3:
    'a 'b 'c 'd.
    (('a, 'b, 'c) => 'd, A.t('a), A.t('b), A.t('c)) => A.t('d)
   = BsApplyExtensions.lift3;

  let map4 = BsApplyExtensions.lift4;

  let map5 = BsApplyExtensions.lift5;

  let tuple2: 'a 'b. (A.t('a), A.t('b)) => A.t(('a, 'b)) = BsApplyExtensions.apply_both;

  let tuple3: 'a 'b 'c. (A.t('a), A.t('b), A.t('c)) => A.t(('a, 'b, 'c)) =
    (fa, fb, fc) => map3((a, b, c) => (a, b, c), fa, fb, fc);

  let tuple4:
    'a 'b 'c 'd.
    (A.t('a), A.t('b), A.t('c), A.t('d)) => A.t(('a, 'b, 'c, 'd))
   =
    (fa, fb, fc, fd) => map4((a, b, c, d) => (a, b, c, d), fa, fb, fc, fd);

  let tuple5:
    'a 'b 'c 'd 'e.
    (A.t('a), A.t('b), A.t('c), A.t('d), A.t('e)) =>
    A.t(('a, 'b, 'c, 'd, 'e))
   =
    (fa, fb, fc, fd, fe) =>
      map5((a, b, c, d, e) => (a, b, c, d, e), fa, fb, fc, fd, fe);
};

module ApplyInfix = (A: BsAbstract.Interface.APPLY) => {
  module ApplyExtensions = ApplyExtensions(A);

  let (<*>) = A.apply;

  let ( <* ) = ApplyExtensions.applyFirst;

  let ( *> ) = ApplyExtensions.applySecond;
};