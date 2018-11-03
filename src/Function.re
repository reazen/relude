let identity: 'a => 'a = a => a;

let id: 'a => 'a = identity;

let const: ('a, 'b) => 'a = (a, _) => a;

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

let compose: ('b => 'c, 'a => 'b, 'a) => 'c = (f, g, a) => f(g(a));

let flipCompose: ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, a) => g(f(a));

let andThen: ('a => 'b, 'b => 'c, 'a) => 'c = flipCompose;

module Functor = BsAbstract.Function.Functor;

module Apply = BsAbstract.Function.Apply;

module Infix = {
  let (<<) = compose;
  let (>>) = flipCompose;
};
