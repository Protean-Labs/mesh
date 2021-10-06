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

  ("let f = (x) => {
      let x = 0;
      x
    };
    f(\"hello\");",                         [VInt(0)]),

  // Destructuring
  ("let (a, b) = (0, 1); a;",               [VInt(0)]),
  
  // Argument pattern
  ("let first = ((a, b)) => a;
    first((0, 1));
    first((\"hello\", 10));",               [VInt(0), VString("hello")]),


  // Modules
  ("module M = {
      let x = 2;
    };
    M.x;",                                  [VInt(2)]),
  ("module M = {
      let x = 2;
      let y = 5;
      let f = (a, b) => b;
    };
    M.f(M.x, \"hello\");",                  [VString("hello")]),
  ("module M = {
      let x = 2;
      let y = 5;
      let f = (a, b) => b;
    };
    open M;
    f(x, \"hello\");",                      [VString("hello")]),
    
  // External functions
  ("external f = \"list_cons\";
    f(1, [2, 3]);",                         [VList([VInt(1), VInt(2), VInt(3)])]),
  ("external f = \"int_add\";
    f(1, 2);",                              [VInt(3)]),
  ("external f = \"int_sub\";
    f(1, 2);",                              [VInt(-1)]),
  ("external f = \"int_mul\";
    f(1, 2);",                              [VInt(2)]),
  ("external f = \"int_div\";
    f(4, 2);",                              [VInt(2)]),
  ("external f = \"float_add\";
    f(1., 2.);",                            [VFloat(3.)]),
  ("external f = \"float_sub\";
    f(1., 2.);",                            [VFloat(-1.)]),
  ("external f = \"float_mul\";
    f(1., 2.);",                            [VFloat(2.)]),
  ("external f = \"float_div\";
    f(4., 2.);",                            [VFloat(2.)]),

  // Operator binding
  ("external add = \"int_add\";
    let (+) = add;
    1 + 2;",                                [VInt(3)]),
  ("external add = \"int_add\";
    let (.++) = add(1);
    ++2;",                                  [VInt(3)]),

  // Records
  ("{};",                                   [VRecord([])]),
  ("{a: 1, b: 2, c: 3};",                   [VRecord([("c", VInt(3)), ("b", VInt(2)), ("a", VInt(1))])]),
  ("let r = {a: 1, b: 2};
    {...r, c: 3};",                         [VRecord([("c", VInt(3)), ("b", VInt(2)), ("a", VInt(1))])]),
  ("{a: 1, b: 2, a: 3};",                   [VRecord([("a", VInt(3)), ("b", VInt(2))])]),
  ("let r = {a: 1, b: 2};
    {...r, a: 3};",                         [VRecord([("a", VInt(3)), ("b", VInt(2))])]),
  ("let r = {a: 1, b: 2};
    r.a;",                                  [VInt(1)]),
  ("let r = {a: 1, b: 2};
    let r2 = {...r, a: 3};
    r2.a;",                                 [VInt(3)]),

  // Stdlib
  ("1 + 2;",                                [VInt(3)]),
  ("3 - 2;",                                [VInt(1)]),
  ("3 * 2;",                                [VInt(6)]),
  ("4 / 2;",                                [VInt(2)]),

  ("1.0 +. 2.0;",                           [VFloat(3.0)]),
  ("3.0 -. 2.0;",                           [VFloat(1.0)]),
  ("3.0 *. 2.0;",                           [VFloat(6.0)]),
  ("3.0 /. 2.0;",                           [VFloat(1.5)]),

] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected)));

let pp_value = (values) => 
  switch (values) {
  | Error(`Msg(msg)) => msg
  | Ok(values) =>
    List.map(string_of_value, values)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  };

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~printer=pp_value, expected, Mesh.parse_eval(mesh_src) >>| fst);

let suite = 
  "test_eval" >::: List.map(make_single_test, test_cases);