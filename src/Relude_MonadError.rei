module type MONAD_THROW =
  {
    type t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
    type e;
    let throwError: e => t('a);
  };
module type MONAD_ERROR =
  {
    type t('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
    type e;
    let throwError: e => t('a);
    let catchError: (e => t('a), t('a)) => t('a);
  };
