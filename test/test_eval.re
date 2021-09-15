open OUnit2;
open Rresult;
open R.Infix;

open Mesh.Eval;

let test_cases = [
  // Literals
  ("1;",                                    [VInt(1)]),
  ("\"hello\";",                            [VString("hello")]),

  // Simple identity function
  ("let f = a => a; f(0);",                 [VInt(0)]),

  // Function with let binding
  ("let f = (x) => {
      let y = 0;
      y
    };
    f(\"hello\");",                         [VInt(0)]),

  // Variable overshadowing
  ("let x = true;
    let x = 10;
    x;",                                    [VInt(10)]),

  // Destructuring
  ("let (a, b) = (0, 1); a;",               [VInt(0)]),
  
  // Argument pattern
  ("let first = ((a, b)) => a;
    first((0, 1));
    first((\"hello\", 10));",               [VInt(0), VString("hello")])
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected)));

let pp_value = (values) => 
  switch (values) {
  | Error(`Msg(msg)) => msg
  | Ok(values) =>
    List.map(string_of_value, values)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  };

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~printer=pp_value, expected, Mesh.parse_file(mesh_src) >>= Mesh.Eval.eval);

let suite = 
  "test_eval" >::: List.map(make_single_test, test_cases);