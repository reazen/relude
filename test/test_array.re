open Alcotest;

module Array = Relude.Array;
module Int = Relude.Int;

let test_length_empty_array () =
  [||] |> Array.length |> check int "empty array length" 0;

let test_length_non_empty_array () =
  [|1, 2, 3|] |> Array.length |> check int "non-empty array length" 3;

let test_isEmpty_true_for_empty_array () =
  [||] |> Array.isEmpty |> check bool "isEmpty true for empty array" true;

let test_isEmpty_false_for_non_empty_array () =
  [|1|] |> Array.isEmpty |> check bool "isEmpty false for non-empty array" false;

let test_isNotEmpty_false_for_empty_array () =
  [||] |> Array.isNotEmpty |> check bool "isNotEmpty false for empty array" false;

let test_isNotEmpty_true_for_non_empty_array () =
  [|1|] |> Array.isNotEmpty |> check bool "isNotEmpty true for non-empty array" true;

let test_pure_creates_one_item_array () =
  123 |> Array.pure |> check (array int) "pure creates one-item array" [|123|];

let test_repeat_creates_array_of_n_items () =
  Array.repeat(3, "a") |> check (array string) "repeat creates array of n items" [|"a", "a", "a"|];

let test_makeWithIndex_creates_array_of_n_items () =
  Array.makeWithIndex(4, i => i + 2) |> check (array int) "makeWithIndex creates array" [|2, 3, 4, 5|];

let test_makeWithIndex_with_pattern_matching () =
  Array.makeWithIndex(
    3,
    fun
    | 0 => "a"
    | _ => "b",
  )
  |> check (array string) "makeWithIndex with pattern matching" [|"a", "b", "b"|];

let test_makeWithIndex_negative_count () =
  Array.makeWithIndex(-1, i => i + 2) |> check (array int) "makeWithIndex negative count" [||];

let suite = [
  ("length empty array", `Quick, test_length_empty_array);
  ("length non-empty array", `Quick, test_length_non_empty_array);
  ("isEmpty true for empty array", `Quick, test_isEmpty_true_for_empty_array);
  ("isEmpty false for non-empty array", `Quick, test_isEmpty_false_for_non_empty_array);
  ("isNotEmpty false for empty array", `Quick, test_isNotEmpty_false_for_empty_array);
  ("isNotEmpty true for non-empty array", `Quick, test_isNotEmpty_true_for_non_empty_array);
  ("pure creates one-item array", `Quick, test_pure_creates_one_item_array);
  ("repeat creates array of n items", `Quick, test_repeat_creates_array_of_n_items);
  ("makeWithIndex creates array", `Quick, test_makeWithIndex_creates_array_of_n_items);
  ("makeWithIndex with pattern matching", `Quick, test_makeWithIndex_with_pattern_matching);
  ("makeWithIndex negative count", `Quick, test_makeWithIndex_negative_count);
];