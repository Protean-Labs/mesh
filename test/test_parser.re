open OUnit2;
open Rresult;

open Mesh.Syntax;

// True positive test cases
let test_cases = [
  // Literals
  ("x;",                      EVar("x")),
  ("x_asd;",                  EVar("x_asd")),
  ("x0;",                     EVar("x0")),
  ("2;",                      int_lit(2)),
  ("-2;",                     int_lit(-2)),
  ("2.1;",                    float_lit(2.1)),
  ("0.2;",                    float_lit(0.2)),
  ("\"abc\";",                string_lit("abc")),
  ("\"\";",                   string_lit("")),
  ("true;",                   bool_lit(true)),
  ("false;",                  bool_lit(false)),

  ("[];",                     EList([])),
  ("[1,2];",                  EList([int_lit(1), int_lit(2)])),
  ("[\"hello\",\"world\"];",  EList([string_lit("hello"), string_lit("world")])),
  ("();",                     unit_lit()),
  ("(1,\"hello\");",          ETuple([int_lit(1), string_lit("hello")])),


  // Let bindings
  ("let x = x;",                      ELet(PVar("x"), EVar("x"))),
  ("let x = x_asd;",                  ELet(PVar("x"), EVar("x_asd"))),
  ("let x = x0;",                     ELet(PVar("x"), EVar("x0"))),
  ("let x = 2;",                      ELet(PVar("x"), int_lit(2))),
  ("let x = -2;",                     ELet(PVar("x"), int_lit(-2))),
  ("let x = 2.1;",                    ELet(PVar("x"), float_lit(2.1))),
  ("let x = 0.2;",                    ELet(PVar("x"), float_lit(0.2))),
  ("let x = \"abc\";",                ELet(PVar("x"), string_lit("abc"))),
  ("let x = \"\";",                   ELet(PVar("x"), string_lit(""))),
  ("let x = true;",                   ELet(PVar("x"), bool_lit(true))),
  ("let x = false;",                  ELet(PVar("x"), bool_lit(false))),

  ("let x = [];",                     ELet(PVar("x"), EList([]))),
  ("let x = [1,2];",                  ELet(PVar("x"), EList([int_lit(1), int_lit(2)]))),
  ("let x = [\"hello\",\"world\"];",  ELet(PVar("x"), EList([string_lit("hello"), string_lit("world")]))),
  ("let x = ();",                     ELet(PVar("x"), unit_lit())),
  ("let x = (1,\"hello\");",          ELet(PVar("x"), ETuple([int_lit(1), string_lit("hello")]))),

  // Infix operators
  ("a + b;",                          EApp(EApp(EVar("+"), EVar("a")), EVar("b"))),
  ("a &* b;",                         EApp(EApp(EVar("&*"), EVar("a")), EVar("b"))),
  ("a <<= b;",                        EApp(EApp(EVar("<<="), EVar("a")), EVar("b"))),
  ("a ** b;",                         EApp(EApp(EVar("**"), EVar("a")), EVar("b"))),

  // Prefix operators
  ("!a;",                             EApp(EVar("!"), EVar("a"))),
  ("&@a;",                            EApp(EVar("&@"), EVar("a"))),
  ("-a;",                             EApp(EVar("-"), EVar("a"))),
  
  // Prefix and Infix
  ("++a / ++b;",                      EApp(EApp(EVar("/"), EApp(EVar("++"), EVar("a"))), EApp(EVar("++"), EVar("b")))),

  // Anonymous functions
  ("(a, b) => a;",                    EFun(PVar("a"), EFun(PVar("b"), EVar("a")))),
  ("(a) => (b) => a;",                EFun(PVar("a"), EFun(PVar("b"), EVar("a")))),
  ("() => 10;",                       EFun(PLit(Unit), int_lit(10))),

  // Function binding
  ("let f = (a, b) => a;",            ELet(PVar("f"), EFun(PVar("a"), EFun(PVar("b"), EVar("a"))))),
  ("let f = (a) => (b) => a;",        ELet(PVar("f"), EFun(PVar("a"), EFun(PVar("b"), EVar("a"))))),
  ("let f = () => 10;",               ELet(PVar("f"), EFun(PLit(Unit), int_lit(10)))),
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok([expected])));

let pp_ast = (ast) => 
  switch (ast) {
  | Error(`Msg(msg)) => msg
  | Ok(ast) =>
    List.map(string_of_expr(0), ast)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  }

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~printer=pp_ast, expected, Mesh.parse_file(mesh_src));

let suite = 
  "test_parsing" >::: List.map(make_single_test, test_cases);