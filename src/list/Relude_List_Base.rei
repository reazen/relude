let cons: ('a, list('a)) => list('a);
let prepend: ('a, list('a)) => list('a);
let uncons: list('a) => option(('a, list('a)));
let append: ('a, list('a)) => list('a);
let repeat: (int, 'a) => list('a);
let makeWithIndex: (int, int => 'a) => list('a);
let mapWithIndex: (('a, int) => 'b, list('a)) => list('b);
let reverse: list('a) => list('a);
let shuffle: list('a) => list('a);
let isEmpty: list('a) => bool;
let isNotEmpty: list('a) => bool;
let at: (int, list('a)) => option('a);
let head: list('a) => option('a);
let tail: list('a) => option(list('a));
let tailOrEmpty: list('a) => list('a);
let init: list('a) => option(list('a));
let last: list('a) => option('a);
let take: (int, list('a)) => list('a);
let takeExactly: (int, list('a)) => option(list('a));
let takeWhile: ('a => bool, list('a)) => list('a);
let drop: (int, list('a)) => list('a);
let dropExactly: (int, list('a)) => option(list('a));
let dropWhile: ('a => bool, list('a)) => list('a);
let filter: ('a => bool, list('a)) => list('a);
let filterWithIndex: (('a, int) => bool, list('a)) => list('a);
let filterNot: ('a => bool, list('a)) => list('a);
let filterNotWithIndex: (('a, int) => bool, list('a)) => list('a);
let mapOption: ('a => option('b), list('a)) => list('b);
let catOptions: list(option('a)) => list('a);
let partition: ('a => bool, list('a)) => (list('a), list('a));
let splitAt: (int, list('a)) => option((list('a), list('a)));
let prependToAll: ('a, list('a)) => list('a);
let intersperse: ('a, list('a)) => list('a);
let replicate: (int, list('a)) => list('a);
let zip: (list('a), list('b)) => list(('a, 'b));
let zipWith: (('a, 'b) => 'c, list('a), list('b)) => list('c);
let zipWithIndex: list('a) => list(('a, int));
let unzip: list(('a, 'b)) => (list('a), list('b));
let sortWithInt: (('a, 'a) => int, list('a)) => list('a);
let sortBy: (('a, 'a) => BsAbstract.Interface.ordering, list('a)) => list('a);
let sort:
  ((module BsAbstract.Interface.ORD with type t = 'a), list('a)) => list('a);
let distinctBy: (('a, 'a) => bool, list('a)) => list('a);
let removeFirstBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let removeEachBy: (('a, 'a) => bool, 'a, list('a)) => list('a);
let distinct:
  ((module BsAbstract.Interface.EQ with type t = 'a), list('a)) => list('a);
let removeFirst:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);
let removeEach:
  ((module BsAbstract.Interface.EQ with type t = 'a), 'a, list('a)) =>
  list('a);
let scanLeft: (('b, 'a) => 'b, 'b, list('a)) => list('b);
let scanRight: (('a, 'b) => 'b, 'b, list('a)) => list('b);
