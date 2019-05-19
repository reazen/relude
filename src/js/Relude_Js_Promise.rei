let toIO: Js.Promise.t('a) => Relude_IO.t('a, Js.Promise.error);
let toIOLazy: (unit => Js.Promise.t('a)) => Relude_IO.t('a, Js.Promise.error);
let fromIO: Relude_IO.t('a, 'e) => Js.Promise.t('a);
let fromIOExn: Relude_IO.t('a, exn) => Js.Promise.t('a);
let fromIOJsExn: Relude_IO.t('a, Js.Exn.t) => Js.Promise.t('a);
