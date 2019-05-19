module type ISO_ARRAY =
  {
    type t('a);
    let fromArray: array('a) => t('a);
    let toArray: t('a) => array('a);
  };
