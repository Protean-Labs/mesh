open OUnit2

let suite = test_list([
  Test_parser.suite
]);

let () = run_test_tt_main(suite);