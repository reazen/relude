let () =
  Alcotest.run "Relude" [
    ("Array", Test_array.suite);
  ]