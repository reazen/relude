let mapWithIndex: (('a, int) => 'b, array('a)) => array('b);
let cons: ('a, array('a)) => array('a);
let prepend: ('a, array('a)) => array('a);
let uncons: array('a) => option(('a, array('a)));
let append: ('a, array('a)) => array('a);
let repeat: (int, 'a) => array('a);
let makeWithIndex: (int, int => 'a) => array('a);
let reverse: array('a) => array('a);
let shuffleInPlace: array('a) => array('a);
let shuffle: array('a) => array('a);
let length: array('a) => int;
let isEmpty: array('a) => bool;
let isNotEmpty: array('a) => bool;
let at: (int, array('a)) => option('a);
let setAt: (int, 'a, array('a)) => option(array('a));
let head: array('a) => option('a);
let tail: array('a) => option(array('a));
let tailOrEmpty: array('a) => array('a);
let init: array('a) => option(array('a));
let last: array('a) => option('a);
let take: (int, array('a)) => array('a);
let takeExactly: (int, array('a)) => option(array('a));
let takeWhile: ('a => bool, array('a)) => array('a);
let drop: (int, array('a)) => array('a);
let dropExactly: (int, array('a)) => option(array('a));
let dropWhile: ('a => bool, array('a)) => array('a);
let filter: ('a => bool, array('a)) => array('a);
let filterWithIndex: (('a, int) => bool, array('a)) => array('a);
let filterNot: ('a => bool, array('a)) => array('a);
let filterNotWithIndex: (('a, int) => bool, array('a)) => array('a);
let partition: ('a => bool, array('a)) => (array('a), array('a));
let splitAt: (int, array('a)) => option((array('a), array('a)));
let prependToAll:
  ('a, array('a)) => Relude_Array_Instances.SemigroupAny.t('a);
let intersperse: ('a, array('a)) => array('a);
let replicate: (int, array('a)) => array('a);
let zip: (array('a), array('b)) => array(('a, 'b));
let zipWith: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let zipWithIndex: array('a) => array(('a, int));
let unzip: array(('a, 'b)) => (array('a), array('b));
let sortWithInt: (('a, 'a) => int, array('a)) => array('a);
let sortBy:
  (('a, 'a) => BsAbstract.Interface.ordering, array('a)) => array('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), array('a)) =>
  array('a);
let distinctBy: (('a, 'a) => bool, array('a)) => array('a);
let removeFirstBy: (('a, 'a) => bool, 'a, array('a)) => array('a);
let removeEachBy: (('a, 'a) => bool, 'a, array('a)) => array('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), array('a)) => array('a);
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, array('a)) =>
  array('a);
let scanLeft: (('b, 'a) => 'b, 'b, array('a)) => array('b);
let scanRight: (('a, 'b) => 'b, 'b, array('a)) => array('b);
