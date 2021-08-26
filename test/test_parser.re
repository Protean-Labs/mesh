open OUnit2;
open Rresult;

open Mesh.Syntax;

// True positive test cases
let test_cases = [
  // Literals
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
  ("[1,2];",                  Expr(EList([int_lit(1),int_lit(2)]))),
  ("[\"hello\",\"world\"];",  Expr(EList([string_lit("hello"),string_lit("world")]))),
  ("();",                     Expr(ETuple([]))),
  ("(1,\"hello\");",          Expr(ETuple([int_lit(1), string_lit("hello")]))),


  // Let bindings
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
  ("let x = [1,2];",                  Let("x", EList([int_lit(1),int_lit(2)]))),
  ("let x = [\"hello\",\"world\"];",  Let("x", EList([string_lit("hello"),string_lit("world")]))),
  ("let x = ();",                     Let("x", ETuple([]))),
  ("let x = (1,\"hello\");",          Let("x", ETuple([int_lit(1),string_lit("hello")]))),

  // Infix operators
  ("a + b;",                          Expr(EApp(EApp(EVar("+"), EVar("a")), EVar("b")))),
  ("a &* b;",                         Expr(EApp(EApp(EVar("&*"), EVar("a")), EVar("b")))),
  ("a <<= b;",                        Expr(EApp(EApp(EVar("<<="), EVar("a")), EVar("b")))),
  ("a ** b;",                         Expr(EApp(EApp(EVar("**"), EVar("a")), EVar("b")))),

  // Prefix operators
  ("!a;",                             Expr(EApp(EVar("!"), EVar("a")))),
  ("&@a;",                            Expr(EApp(EVar("&@"), EVar("a")))),
  ("-a;",                             Expr(EApp(EVar("-"), EVar("a")))),
  
  // Prefix and Infix
  ("++a / ++b;",                      Expr(EApp(EApp(EVar("/"), EApp(EVar("++"), EVar("a"))), EApp(EVar("++"), EVar("b"))))),

  // Anonymous functions
  ("(a, b) => a;",                    Expr(EFun(EVar("a"), EFun(EVar("b"), EVar("a")))))
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok([expected])));

let pp_ast = (ast) => 
  switch (ast) {
  | Error(`Msg(msg)) => msg
  | Ok(ast) =>
    List.map(string_of_top, ast)   |> (s) =>
    String.concat(",\n", s);
  }

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~printer=pp_ast, Mesh.parse_file(mesh_src), expected);

let suite = 
  "test_parsing" >::: List.map(make_single_test, test_cases);