/*
Interface

This module is intended to hold our common module types (typeclass-like interfaces),
similar to the bs-abstract Interface module.
*/

module type SEQUENCE = {
  type t('a);
  let length: t('a) => int;
  let isEmpty: t('a) => bool;
  let isNonEmpty: t('a) => bool;
  let head: t('a) => option('a);
  let tail: t('a) => option(t('a));
  let tailOrEmpty: t('a) => t('a);
  let toList: t('a) => list('a);
  let toArray: t('a) => array('a);
};
