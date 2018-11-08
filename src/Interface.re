/*
Interface

This module is intended to hold our common module types (typeclass-like interfaces),
similar to the bs-abstract Interface module.
*/

/**
SEQUENCE

Common operations for types that contain a sequence of values (e.g. list or array)
TODO: not sure what exact functions are appropriate to have here.
 */
module type SEQUENCE = {
  type t('a);
  let length: t('a) => int;
  let isEmpty: t('a) => bool;
  let isNotEmpty: t('a) => bool;
  let head: t('a) => option('a);
  let tail: t('a) => option(t('a));
  let tailOrEmpty: t('a) => t('a);
}

module type ISO_LIST = {
  type t('a);
  let fromList: list('a) => t('a);
  let toList: t('a) => list('a);
}

module type ISO_ARRAY = {
  type t('a);
  let fromArray: array('a) => t('a);
  let toArray: t('a) => array('a);
};
