let identity: 'a => 'a = a => a;

let id: 'a => 'a = identity;

let const: ('a, 'b) => 'a = (a, _) => a;

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

let compose = (f: 'b => 'c, g: 'a => 'b, a: 'a) => f(g(a));

let flipCompose = (f: 'a => 'b, g: 'b => 'c, a: 'a) => g(f(a));

let andThen = flipCompose;

module Infix {
  let (<<) = compose;
  let (>>) = flipCompose;
}
