open OUnit2;
open Rresult;

open Mesh.Syntax;

// Helpers
let cons = (e, l) => EApp(EApp(var("cons"), e), l);

// True positive test cases
let test_cases = [
  // Literals
  ("x;",                      var("x")),
  ("x_asd;",                  var("x_asd")),
  ("x0;",                     var("x0")),
  ("2;",                      int_lit(2)),
  ("-2;",                     int_lit(-2)),
  ("2.1;",                    float_lit(2.1)),
  ("0.2;",                    float_lit(0.2)),
  ("\"abc\";",                string_lit("abc")),
  ("\"\";",                   string_lit("")),
  ("true;",                   bool_lit(true)),
  ("false;",                  bool_lit(false)),

  // Lists
  ("[];",                     EList([])),
  ("[1,2];",                  EList([int_lit(1), int_lit(2)])),
  ("[\"hello\",\"world\"];",  EList([string_lit("hello"), string_lit("world")])),
  ("[1, 2, ...l];",           cons(int_lit(1), cons(int_lit(2), var("l")))),
  ("[1, 2, ...[3, 4]];",      cons(int_lit(1), cons(int_lit(2), EList([int_lit(3), int_lit(4)])))),

  // Tuples
  ("();",                     unit_lit()),
  ("(1, \"hello\");",         ETuple([int_lit(1), string_lit("hello")])),
  ("(1, (\"hello\", 0));",    ETuple([int_lit(1), ETuple([string_lit("hello"), int_lit(0)])])),

  // Let bindings
  ("let x = x;",                      ELet(PVar("x"), var("x"))),
  ("let (x) = x;",                    ELet(PVar("x"), var("x"))),
  ("let x = x_asd;",                  ELet(PVar("x"), var("x_asd"))),
  ("let x = x0;",                     ELet(PVar("x"), var("x0"))),
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
  ("let x = (1, (\"hello\", 0));",    ELet(PVar("x"), ETuple([int_lit(1), ETuple([string_lit("hello"), int_lit(0)])]))),

  // Infix operators
  ("a + b;",                          EApp(EApp(var("+"), var("a")), var("b"))),
  ("a &* b;",                         EApp(EApp(var("&*"), var("a")), var("b"))),
  ("a <<= b;",                        EApp(EApp(var("<<="), var("a")), var("b"))),
  ("a ** b;",                         EApp(EApp(var("**"), var("a")), var("b"))),

  // Prefix operators
  ("!a;",                             EApp(var("!"), var("a"))),
  ("&@a;",                            EApp(var("&@"), var("a"))),
  ("-a;",                             EApp(var("-"), var("a"))),
  
  // Prefix and Infix
  ("++a / ++b;",                      EApp(EApp(var("/"), EApp(var("++"), var("a"))), EApp(var("++"), var("b")))),

  // Anonymous functions
  ("(a, b) => a;",                    EFun(PVar("a"), EFun(PVar("b"), var("a")))),
  ("(a) => (b) => a;",                EFun(PVar("a"), EFun(PVar("b"), var("a")))),
  ("() => 10;",                       EFun(PLit(Unit), int_lit(10))),
  ("() => f();",                      EFun(PLit(Unit), EApp(var("f"), unit_lit()))),
  ("(a, b) => f(a + b);",             EFun(PVar("a"), EFun(PVar("b"), EApp(var("f"), EApp(EApp(var("+"), var("a")), var("b")))))),
  ("(a, b) => {
    foo(a);
    foo(b);
  };",                                EFun(PVar("a"), EFun(PVar("b"), ESeq(EApp(var("foo"), var("a")), EApp(var("foo"), var("b")))))),

  // Function binding
  ("let f = (a, b) => a;",            ELet(PVar("f"), EFun(PVar("a"), EFun(PVar("b"), var("a"))))),
  ("let f = ((a, b)) => a;",          ELet(PVar("f"), EFun(PTuple([PVar("a"), PVar("b")]), var("a")))),
  ("let f = (a) => (b) => a;",        ELet(PVar("f"), EFun(PVar("a"), EFun(PVar("b"), var("a"))))),
  ("let f = a => a;",                 ELet(PVar("f"), EFun(PVar("a"), var("a")))),
  ("let f = () => 10;",               ELet(PVar("f"), EFun(PLit(Unit), int_lit(10)))),
  ("let f = () => {
      foo(a);
      true
    };",                              ELet(PVar("f"), EFun(PLit(Unit), ESeq(EApp(var("foo"), var("a")), bool_lit(true))))),

  // Function application
  ("f(a, b);",                        EApp(EApp(var("f"), var("a")), var("b"))),
  ("f(a)(b);",                        EApp(EApp(var("f"), var("a")), var("b"))),
  ("f((a, b));",                      EApp(var("f"), ETuple([var("a"), var("b")]))),
  ("f(1);",                           EApp(var("f"), int_lit(1))),

  // Function partial application
  ("let g = f(a);",                   ELet(PVar("g"), EApp(var("f"), var("a")))),

  // Let bindings with patterns
  ("let (a, b) = (0, \"hello\");",                  ELet(PTuple([PVar("a"), PVar("b")]), ETuple([int_lit(0), string_lit("hello")]))),
  ("let (a, b) = (0, (\"hello\", 1.0));",           ELet(PTuple([PVar("a"), PVar("b")]), ETuple([int_lit(0), ETuple([string_lit("hello"), float_lit(1.0)])]))),
  ("let (a, (b, c)) = (0, (\"hello\", 1.0));",      ELet(PTuple([PVar("a"), PTuple([PVar("b"), PVar("c")])]), ETuple([int_lit(0), ETuple([string_lit("hello"), float_lit(1.0)])]))),

  // Expressions wrapped in parantheses
  ("(a => a);",                                     EFun(PVar("a"), var("a"))),
  ("((a, b));",                                     ETuple([var("a"), var("b")])),
  ("(1);",                                          int_lit(1)),

  // Modules
  ("module M = {};",                                EMod("M", [])),
  ("module M = {
      let x = 2;
    };",                                            EMod("M", [ELet(PVar("x"), int_lit(2))])),
  ("M.x;",                                          EVar(["M"], "x")),
  ("M1.M2.x;",                                      EVar(["M1", "M2"], "x")),
  ("open M;",                                       EOpen([], "M")),
  ("open M1.M2;",                                   EOpen(["M1"], "M2")),
  
  // External functions
  ("external f = \"int_add\";",                     ELet(PVar("f"), EFun(PVar("a"), EFun(PVar("b"), EPrim(PIntAdd(var("a"), var("b"))))))),

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
  "test_parser" >::: List.map(make_single_test, test_cases);