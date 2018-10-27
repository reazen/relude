let isEmpty: list('a) => bool = l => Belt.List.length(l) == 0;

let nonEmpty: list('a) => bool = l => Belt.List.length(l) > 0;

let map: ('a => 'b, list('a)) => list('b) = BsAbstract.List.Functor.map;

let flatMap: (list('a), 'a => list('b)) => list('b) = BsAbstract.List.Monad.flat_map;

let pure: 'a => list('a) = BsAbstract.List.Applicative.pure;

let one = pure;

let single = pure;

let apply: (list('a => 'b), list('a)) => list('b) = BsAbstract.List.Applicative.apply;

let foldLeft: (('b, 'a) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_left;

let foldRight: (('a, 'b) => 'b, 'b, list('a)) => 'b = BsAbstract.List.Foldable.fold_right;

let traverseOption: ('a => option('a), list('a)) => option(list('a)) = BsAbstract.Functors.ListF.Option.Traversable.traverse;

let sequenceOption: list(option('a)) => option(list('a)) = BsAbstract.Functors.ListF.Option.Traversable.sequence;

/*
let traverseResult: ('a => Belt.Result.t('a, 'b), list('a)) => Belt.Result.t(list('a), 'b) = BsAbstract.List.Traversable(Result.Applicative);
 */
