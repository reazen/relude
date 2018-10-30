let identity: 'a => 'a = a => a;

let id = identity;

let const: ('a, 'b) => 'a = (a, _) => a;

let compose = (f: 'b => 'c, g: 'a => 'b, a: 'a) => f(g(a));
let (<<) = compose;

let flipCompose = (f: 'a => 'b, g: 'b => 'c, a: 'a) => g(f(a));
let andThen = flipCompose;
let (>>) = flipCompose;

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);
