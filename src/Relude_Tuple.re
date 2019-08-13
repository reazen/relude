/**
 * Constructs a tuple-2 from 2 values
 */
let make = (a, b) => (a, b);

/**
 * Constructs a tuple-2 from 2 values
 * 
 * Alias of `make`
 */
let make2 = make;

/**
 * Constructs a tuple-3 from 3 values
 */
let make3 = (a, b, c) => (a, b, c);

/**
 * Constructs a tuple-4 from 4 values
 */
let make4 = (a, b, c, d) => (a, b, c, d);

/**
 * Constructs a tuple-5 from 5 values
 */
let make5 = (a, b, c, d, e) => (a, b, c, d, e);

/**
 * Constructs a tuple-2 from an array of exactly 2 values
 */
let fromArray =
  fun
  | [|a, b|] => Some((a, b))
  | _ => None;

/**
 * Constructs a tuple-2 from an array of exactly 2 values
 * 
 * Alias of `fromArray`
 */
let fromArray2 = fromArray;

/**
 * Constructs a tuple-3 from an array of exactly 3 values
 */
let fromArray3 =
  fun
  | [|a, b, c|] => Some((a, b, c))
  | _ => None;

/**
 * Constructs a tuple-4 from an array of exactly 4 values
 */
let fromArray4 =
  fun
  | [|a, b, c, d|] => Some((a, b, c, d))
  | _ => None;

/**
 * Constructs a tuple-5 from an array of exactly 5 values
 */
let fromArray5 =
  fun
  | [|a, b, c, d, e|] => Some((a, b, c, d, e))
  | _ => None;

/**
 * Constructs a tuple-2 from an array of at least 2 values
 */
let fromArrayAtLeast2 = xs => Relude_Array.take(2, xs) |> fromArray;

/**
 * Constructs a tuple-3 from an array of at least 3 values
 */
let fromArrayAtLeast3 = xs => Relude_Array.take(3, xs) |> fromArray3;

/**
 * Constructs a tuple-4 from an array of at least 4 values
 */
let fromArrayAtLeast4 = xs => Relude_Array.take(4, xs) |> fromArray4;

/**
 * Constructs a tuple-5 from an array of at least 5 values
 */
let fromArrayAtLeast5 = xs => Relude_Array.take(5, xs) |> fromArray5;

// Pattern matching on lists leads to deeply nested conditional branches in the
// JS code, which is hard to read and painful to test exhaustively. Instead, we
// implement fromList* in terms of fromArray*. We avoid an O(n) conversion to
// Array by first taking only enough elements to make sure the tuple is OK. That
// count is the size of the tuple + 1 to ensure that lists larger than the
// target tuple size will be caught and rejected.

/**
 * Constructs a tuple-2 from a list of exactly 2 values
 */
let fromList = xs => Relude_List.(take(3, xs) |> toArray) |> fromArray;

/**
 * Constructs a tuple-2 from a list of exactly 2 values
 * 
 * Alias of `fromList`
 */
let fromList2 = fromList;

/**
 * Constructs a tuple-3 from a list of exactly 3 values
 */
let fromList3 = xs => Relude_List.(take(4, xs) |> toArray) |> fromArray3;

/**
 * Constructs a tuple-4 from a list of exactly 4 values
 */
let fromList4 = xs => Relude_List.(take(5, xs) |> toArray) |> fromArray4;

/**
 * Constructs a tuple-5 from a list of exactly 5 values
 */
let fromList5 = xs => Relude_List.(take(6, xs) |> toArray) |> fromArray5;

/**
 * Constructs a tuple-2 from a list of at least 2 values
 */
let fromListAtLeast2 = xs => Relude_List.take(2, xs) |> fromList;

/**
 * Constructs a tuple-3 from a list of at least 3 values
 */
let fromListAtLeast3 = xs => Relude_List.take(3, xs) |> fromList3;

/**
 * Constructs a tuple-4 from a list of at least 4 values
 */
let fromListAtLeast4 = xs => Relude_List.take(4, xs) |> fromList4;

/**
 * Constructs a tuple-5 from a list of at least 5 values
 */
let fromListAtLeast5 = xs => Relude_List.take(5, xs) |> fromList5;

/**
 * Applies a normal 2-argument function to arguments contained in a tuple-2
 */
let apply2: 'a 'b 'c. (('a, 'b) => 'c, ('a, 'b)) => 'c =
  (f, (a, b)) => f(a, b);

/**
 * Applies a normal 3-argument function to arguments contained in a tuple-3
 */
let apply3: 'a 'b 'c 'd. (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd =
  (f, (a, b, c)) => f(a, b, c);

/**
 * Applies a normal 4-argument function to arguments contained in a tuple-4
 */
let apply4: 'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd) => 'e, ('a, 'b, 'c, 'd)) => 'e =
  (f, (a, b, c, d)) => f(a, b, c, d);

/**
 * Applies a normal 5-argument function to arguments contained in a tuple-5
 */
let apply5:
  'a 'b 'c 'd 'e 'f.
  (('a, 'b, 'c, 'd, 'e) => 'f, ('a, 'b, 'c, 'd, 'e)) => 'f
 =
  (f, (a, b, c, d, e)) => f(a, b, c, d, e);