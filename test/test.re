open OUnit2;

Common.Logging.set_logging_level(NoLevel);

let suite = test_list([
  Test_parser.suite,
  Test_infer.suite,
  Test_eval.suite
]);

let () = run_test_tt_main(suite);