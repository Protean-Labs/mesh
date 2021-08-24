open OUnit2;
open Rresult;

open Mesh.Syntax;

// True positive test cases
let test_cases = [
  ("x;",                      Expr(EVar("x"))),
  ("x_asd;",                  Expr(EVar("x_asd"))),
  ("x0;",                     Expr(EVar("x0"))),
  ("2;",                      Expr(int_lit(2))),
  ("-2;",                     Expr(int_lit(-2))),
  ("2.1;",                    Expr(float_lit(2.1))),
  ("0.2;",                    Expr(float_lit(0.2))),
  ("\"abc\";",                Expr(string_lit("abc"))),
  ("\"\";",                   Expr(string_lit(""))),
  ("true;",                   Expr(bool_lit(true))),
  ("false;",                  Expr(bool_lit(false))),

  ("[];",                     Expr(EList([]))),
  ("[1,2];",                  Expr(EList([intv(1),intv(2)]))),
  ("[\"hello\",\"world\"];",  Expr(EList([stringv("hello"),stringv("world")]))),
  ("();",                     Expr(ETuple([]))),
  ("(1,\"hello\");",          Expr(ETuple([int_lit(1), string_lit("hello")]))),


  ("let x = x;",                      Let("x", EVar("x"))),
  ("let x = x_asd;",                  Let("x", EVar("x_asd"))),
  ("let x = x0;",                     Let("x", EVar("x0"))),
  ("let x = 2;",                      Let("x", int_lit(2))),
  ("let x = -2;",                     Let("x", int_lit(-2))),
  ("let x = 2.1;",                    Let("x", float_lit(2.1))),
  ("let x = 0.2;",                    Let("x", float_lit(0.2))),
  ("let x = \"abc\";",                Let("x", string_lit("abc"))),
  ("let x = \"\";",                   Let("x", string_lit(""))),
  ("let x = true;",                   Let("x", bool_lit(true))),
  ("let x = false;",                  Let("x", bool_lit(false))),

  ("let x = [];",                     Let("x", EList([]))),
  ("let x = [1,2];",                  Let("x", EList([intv(1),intv(2)]))),
  ("let x = [\"hello\",\"world\"];",  Let("x", EList([stringv("hello"),stringv("world")]))),
  ("let x = ();",                     Let("x", ETuple([]))),
  ("let x = (1,\"hello\");",          Let("x", ETuple([int_lit(1),string_lit("hello")])))
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok([expected])));

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(Mesh.parse_file(mesh_src), expected);

let suite = 
  "test_parsing" >::: List.map(make_single_test, test_cases);