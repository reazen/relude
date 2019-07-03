let make = (a, b) => (a, b);
let make2 = make;
let make3 = (a, b, c) => (a, b, c);
let make4 = (a, b, c, d) => (a, b, c, d);
let make5 = (a, b, c, d, e) => (a, b, c, d, e);

let fromArray =
  fun
  | [|a, b|] => Some((a, b))
  | _ => None;

let fromArray2 = fromArray;

let fromArray3 =
  fun
  | [|a, b, c|] => Some((a, b, c))
  | _ => None;

let fromArray4 =
  fun
  | [|a, b, c, d|] => Some((a, b, c, d))
  | _ => None;

let fromArray5 =
  fun
  | [|a, b, c, d, e|] => Some((a, b, c, d, e))
  | _ => None;

let fromArrayAtLeast2 = xs => Relude_Array.take(2, xs) |> fromArray;
let fromArrayAtLeast3 = xs => Relude_Array.take(3, xs) |> fromArray3;
let fromArrayAtLeast4 = xs => Relude_Array.take(4, xs) |> fromArray4;
let fromArrayAtLeast5 = xs => Relude_Array.take(5, xs) |> fromArray5;

// Pattern matching on lists leads to deeply nested conditional branches in the
// JS code, which is hard to read and painful to test exhaustively. Instead, we
// implement fromList* in terms of fromArray*. We avoid an O(n) conversion to
// Array by first taking only enough elements to make sure the tuple is OK. That
// count is the size of the tuple + 1 to ensure that lists larger than the
// target tuple size will be caught and rejected.

let fromList = xs => Relude_List.(take(3, xs) |> toArray) |> fromArray;
let fromList2 = fromList;
let fromList3 = xs => Relude_List.(take(4, xs) |> toArray) |> fromArray3;
let fromList4 = xs => Relude_List.(take(5, xs) |> toArray) |> fromArray4;
let fromList5 = xs => Relude_List.(take(6, xs) |> toArray) |> fromArray5;

let fromListAtLeast2 = xs => Relude_List.take(2, xs) |> fromList;
let fromListAtLeast3 = xs => Relude_List.take(3, xs) |> fromList3;
let fromListAtLeast4 = xs => Relude_List.take(4, xs) |> fromList4;
let fromListAtLeast5 = xs => Relude_List.take(5, xs) |> fromList5;

let apply2: 'a 'b 'c. (('a, 'b) => 'c, ('a, 'b)) => 'c =
  (f, (a, b)) => f(a, b);

let apply3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd =
  (f, (a, b, c)) => f(a, b, c);

let apply4: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd) => 'e, ('a, 'b, 'c, 'd)) => 'e =
  (f, (a, b, c, d)) => f(a, b, c, d);

let apply5:
  'a 'b 'c 'd 'e 'f.
  (('a, 'b, 'c, 'd, 'e) => 'f, ('a, 'b, 'c, 'd, 'e)) => 'f
 =
  (f, (a, b, c, d, e)) => f(a, b, c, d, e);