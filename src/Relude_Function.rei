let identity: 'a => 'a;
let id: 'a => 'a;
let const: ('a, 'b) => 'a;
let flip: (('a, 'b) => 'c, 'b, 'a) => 'c;
let compose: ('b => 'c, 'a => 'b, 'a) => 'c;
let flipCompose: ('a => 'b, 'b => 'c, 'a) => 'c;
let andThen: ('a => 'b, 'b => 'c, 'a) => 'c;
let pure: ('a, 'r) => 'a;
let map: ('a => 'b, 'r => 'a, 'r) => 'b;
let apply: (('r, 'a) => 'b, 'r => 'a, 'r) => 'b;
let bind: ('r => 'a, ('a, 'r) => 'b, 'r) => 'b;
let flatMap: (('a, 'r) => 'b, 'r => 'a, 'r) => 'b;
module Functor = BsAbstract.Function.Functor;
module Apply = BsAbstract.Function.Apply;
module Infix:
  {
    let ( << ): ('a => 'b, 'c => 'a, 'c) => 'b;
    let ( >> ): ('a => 'b, 'b => 'c, 'a) => 'c;
  };
