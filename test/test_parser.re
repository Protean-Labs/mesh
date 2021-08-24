open OUnit2;
open Rresult;

open Mesh.Syntax;

// True positive test cases
let test_cases = [
  ("x",           Expr(Var("x"))),
  ("x_asd",       Expr(Var("x_asd"))),
  ("x0",          Expr(Var("x0"))),
  ("2",           Expr(int_lit(2))),
  ("-2",          Expr(int_lit(-2))),
  ("2.1",         Expr(float_lit(2.1))),
  ("0.2",         Expr(float_lit(0.2))),
  ("\"abc\"",     Expr(string_lit("abc"))),
  ("\"\"",        Expr(string_lit(""))),
  ("true",        Expr(bool_lit(true))),
  ("false",       Expr(bool_lit(false))),

  ("let x = x",           Let("x", Var("x"))),
  ("let x = x_asd",       Let("x", Var("x_asd"))),
  ("let x = x0",          Let("x", Var("x0"))),
  ("let x = 2",           Let("x", int_lit(2))),
  ("let x = -2",          Let("x", int_lit(-2))),
  ("let x = 2.1",         Let("x", float_lit(2.1))),
  ("let x = 0.2",         Let("x", float_lit(0.2))),
  ("let x = \"abc\"",     Let("x", string_lit("abc"))),
  ("let x = \"\"",        Let("x", string_lit(""))),
  ("let x = true",        Let("x", bool_lit(true))),
  ("let x = false",       Let("x", bool_lit(false))),
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected)));

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(Mesh.parse_toplevel(mesh_src), expected);

let suite = 
  "test_parsing" >::: List.map(make_single_test, test_cases);