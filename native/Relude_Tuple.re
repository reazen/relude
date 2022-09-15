module Tuple2 = Relude_Tuple2;
module Tuple3 = Relude_Tuple3;
module Tuple4 = Relude_Tuple4;
module Tuple5 = Relude_Tuple5;

// Aliases for backwards compat/convenience
// This also helps to make sure our implementations of all the tuples are in sync
// We might be able to make a module type for that, but it could be tricky with some
// of the module shenanigans going on with EQ/ORD/etc.

let make = Tuple2.make;
let make2 = Tuple2.make;
let make3 = Tuple3.make;
let make4 = Tuple4.make;
let make5 = Tuple5.make;

let fromArray = Tuple2.fromArray;
let fromArray2 = Tuple2.fromArray;
let fromArray3 = Tuple3.fromArray;
let fromArray4 = Tuple4.fromArray;
let fromArray5 = Tuple5.fromArray;

let fromArrayAtLeast = Tuple2.fromArrayAtLeast;
let fromArrayAtLeast2 = Tuple2.fromArrayAtLeast;
let fromArrayAtLeast3 = Tuple3.fromArrayAtLeast;
let fromArrayAtLeast4 = Tuple4.fromArrayAtLeast;
let fromArrayAtLeast5 = Tuple5.fromArrayAtLeast;

let fromList = Tuple2.fromList;
let fromList2 = Tuple2.fromList;
let fromList3 = Tuple3.fromList;
let fromList4 = Tuple4.fromList;
let fromList5 = Tuple5.fromList;

let fromListAtLeast = Tuple2.fromListAtLeast;
let fromListAtLeast2 = Tuple2.fromListAtLeast;
let fromListAtLeast3 = Tuple3.fromListAtLeast;
let fromListAtLeast4 = Tuple4.fromListAtLeast;
let fromListAtLeast5 = Tuple5.fromListAtLeast;

// These were re-definitions of uncurry, so leaving them here as aliases for now
let apply = Relude_Function.uncurry2;
let apply2 = Relude_Function.uncurry2;
let apply3 = Relude_Function.uncurry3;
let apply4 = Relude_Function.uncurry4;
let apply5 = Relude_Function.uncurry5;

let first = Tuple2.first;
let first2 = Tuple2.first;
let first3 = Tuple3.first;
let first4 = Tuple4.first;
let first5 = Tuple5.first;

let second = Tuple2.second;
let second2 = Tuple2.second;
let second3 = Tuple3.second;
let second4 = Tuple4.second;
let second5 = Tuple5.second;

let third = Tuple3.third;
let third3 = Tuple3.third;
let third4 = Tuple4.third;
let third5 = Tuple5.third;

let fourth = Tuple4.fourth;
let fourth4 = Tuple4.fourth;
let fourth5 = Tuple5.fourth;

let fifth = Tuple5.fifth;
let fifth5 = Tuple5.fifth;

let showBy = Tuple2.showBy;
let showBy2 = Tuple2.showBy;
let showBy3 = Tuple3.showBy;
let showBy4 = Tuple4.showBy;
let showBy5 = Tuple5.showBy;

let eqBy = Tuple2.eqBy;
let eqBy2 = Tuple2.eqBy;
let eqBy3 = Tuple3.eqBy;
let eqBy4 = Tuple4.eqBy;
let eqBy5 = Tuple5.eqBy;
module WithEqs2 = Tuple2.WithEqs;
module WithEqs3 = Tuple3.WithEqs;
module WithEqs4 = Tuple4.WithEqs;
module WithEqs5 = Tuple5.WithEqs;
module EqBy2 = Tuple2.EqBy;
module EqBy3 = Tuple3.EqBy;
module EqBy4 = Tuple4.EqBy;
module EqBy5 = Tuple5.EqBy;

let compareBy = Tuple2.compareBy;
let compareBy2 = Tuple2.compareBy;
let compareBy3 = Tuple3.compareBy;
let compareBy4 = Tuple4.compareBy;
let compareBy5 = Tuple5.compareBy;
module WithOrds2 = Tuple2.WithOrds;
module WithOrds3 = Tuple3.WithOrds;
module WithOrds4 = Tuple4.WithOrds;
module WithOrds5 = Tuple5.WithOrds;
module OrdBy2 = Tuple2.OrdBy;
module OrdBy3 = Tuple3.OrdBy;
module OrdBy4 = Tuple4.OrdBy;
module OrdBy5 = Tuple5.OrdBy;
