open OUnit2;
open Rresult;

open Mesh.Syntax;
open Mesh.Syntax_util;

// Helpers
let cons = (e, l) => mk_expr(EApp(mk_expr(EApp(mk_evar("cons"), e)), l));

// True positive test cases
let test_cases = [
  // Literals
  ("x;",                      mk_evar("x")),
  ("x_asd;",                  mk_evar("x_asd")),
  ("x0;",                     mk_evar("x0")),
  ("2;",                      mk_elit_int(2)),
  ("-2;",                     mk_elit_int(-2)),
  ("2.1;",                    mk_elit_float(2.1)),
  ("0.2;",                    mk_elit_float(0.2)),
  ("\"abc\";",                mk_elit_string("abc")),
  ("\"\";",                   mk_elit_string("")),
  ("true;",                   mk_elit_bool(true)),
  ("false;",                  mk_elit_bool(false)),

  // Lists
  ("[];",                     mk_expr(EList([]))),
  ("[1,2];",                  mk_expr(EList([mk_elit_int(1), mk_elit_int(2)]))),
  ("[\"hello\",\"world\"];",  mk_expr(EList([mk_elit_string("hello"), mk_elit_string("world")]))),
  ("[1, 2, ...l];",           cons(mk_elit_int(1), cons(mk_elit_int(2), mk_evar("l")))),
  ("[1, 2, ...[3, 4]];",      cons(mk_elit_int(1), cons(mk_elit_int(2), mk_expr(EList([mk_elit_int(3), mk_elit_int(4)]))))),

  // Tuples
  ("();",                     mk_elit_unit()),
  ("(1, \"hello\");",         mk_expr(ETuple([mk_elit_int(1), mk_elit_string("hello")]))),
  ("(1, (\"hello\", 0));",    mk_expr(ETuple([mk_elit_int(1), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_int(0)]))]))),

  // Let bindings
  ("let x = x;",                      mk_expr(ELet(mk_pvar("x"), mk_evar("x")))),
  ("let (x) = x;",                    mk_expr(ELet(mk_pvar("x"), mk_evar("x")))),
  ("let x = x_asd;",                  mk_expr(ELet(mk_pvar("x"), mk_evar("x_asd")))),
  ("let x = x0;",                     mk_expr(ELet(mk_pvar("x"), mk_evar("x0")))),
  ("let x = 2;",                      mk_expr(ELet(mk_pvar("x"), mk_elit_int(2)))),
  ("let x = -2;",                     mk_expr(ELet(mk_pvar("x"), mk_elit_int(-2)))),
  ("let x = 2.1;",                    mk_expr(ELet(mk_pvar("x"), mk_elit_float(2.1)))),
  ("let x = 0.2;",                    mk_expr(ELet(mk_pvar("x"), mk_elit_float(0.2)))),
  ("let x = \"abc\";",                mk_expr(ELet(mk_pvar("x"), mk_elit_string("abc")))),
  ("let x = \"\";",                   mk_expr(ELet(mk_pvar("x"), mk_elit_string("")))),
  ("let x = true;",                   mk_expr(ELet(mk_pvar("x"), mk_elit_bool(true)))),
  ("let x = false;",                  mk_expr(ELet(mk_pvar("x"), mk_elit_bool(false)))),

  ("let x = [];",                     mk_expr(ELet(mk_pvar("x"), mk_expr(EList([]))))),
  ("let x = [1,2];",                  mk_expr(ELet(mk_pvar("x"), mk_expr(EList([mk_elit_int(1), mk_elit_int(2)]))))),
  ("let x = [\"hello\",\"world\"];",  mk_expr(ELet(mk_pvar("x"), mk_expr(EList([mk_elit_string("hello"), mk_elit_string("world")]))))),
  ("let x = ();",                     mk_expr(ELet(mk_pvar("x"), mk_elit_unit()))),
  ("let x = (1,\"hello\");",          mk_expr(ELet(mk_pvar("x"), mk_expr(ETuple([mk_elit_int(1), mk_elit_string("hello")]))))),
  ("let x = (1, (\"hello\", 0));",    mk_expr(ELet(mk_pvar("x"), mk_expr(ETuple([mk_elit_int(1), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_int(0)]))]))))),

  // Infix operators
  ("a + b;",                          mk_expr(EApp(mk_expr(EApp(mk_evar("+"), mk_evar("a"))), mk_evar("b")))),
  ("a &* b;",                         mk_expr(EApp(mk_expr(EApp(mk_evar("&*"), mk_evar("a"))), mk_evar("b")))),
  ("a <<= b;",                        mk_expr(EApp(mk_expr(EApp(mk_evar("<<="), mk_evar("a"))), mk_evar("b")))),
  ("a ** b;",                         mk_expr(EApp(mk_expr(EApp(mk_evar("**"), mk_evar("a"))), mk_evar("b")))),

  // Prefix operators
  ("!a;",                             mk_expr(EApp(mk_evar("!"), mk_evar("a")))),
  ("&@a;",                            mk_expr(EApp(mk_evar("&@"), mk_evar("a")))),
  ("-a;",                             mk_expr(EApp(mk_evar("-"), mk_evar("a")))),
  
  // Prefix and Infix
  ("++a / ++b;",                      mk_expr(EApp(mk_expr(EApp(mk_evar("/"), mk_expr(EApp(mk_evar("++"), mk_evar("a"))))), mk_expr(EApp(mk_evar("++"), mk_evar("b")))))),

  // Anonymous functions
  ("(a, b) => a;",                    mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))),
  ("(a) => (b) => a;",                mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))),
  ("() => 10;",                       mk_expr(EFun(mk_pat(PLit(Unit)), mk_elit_int(10)))),
  ("() => f();",                      mk_expr(EFun(mk_pat(PLit(Unit)), mk_expr(EApp(mk_evar("f"), mk_elit_unit()))))),
  ("(a, b) => f(a + b);",             mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_expr(EApp(mk_evar("f"), mk_expr(EApp(mk_expr(EApp(mk_evar("+"), mk_evar("a"))), mk_evar("b")))))))))),
  ("(a, b) => {
    foo(a);
    foo(b);
  };",                                mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_expr(ESeq(mk_expr(EApp(mk_evar("foo"), mk_evar("a"))), mk_expr(EApp(mk_evar("foo"), mk_evar("b")))))))))),

  // // Function binding
  ("let f = (a, b) => a;",            mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))))),
  ("let f = ((a, b)) => a;",          mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_evar("a")))))),
  ("let f = (a) => (b) => a;",        mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))))),
  ("let f = a => a;",                 mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_evar("a")))))),
  ("let f = () => 10;",               mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PLit(Unit)), mk_elit_int(10)))))),
  ("let f = () => {
      foo(a);
      true
    };",                              mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PLit(Unit)), mk_expr(ESeq(mk_expr(EApp(mk_evar("foo"), mk_evar("a"))), mk_elit_bool(true)))))))),

  // // Function application
  ("f(a, b);",                        mk_expr(EApp(mk_expr(EApp(mk_evar("f"), mk_evar("a"))), mk_evar("b")))),
  ("f(a)(b);",                        mk_expr(EApp(mk_expr(EApp(mk_evar("f"), mk_evar("a"))), mk_evar("b")))),
  ("f((a, b));",                      mk_expr(EApp(mk_evar("f"), mk_expr(ETuple([mk_evar("a"), mk_evar("b")]))))),
  ("f(1);",                           mk_expr(EApp(mk_evar("f"), mk_elit_int(1)))),

  // // Function partial application
  ("let g = f(a);",                   mk_expr(ELet(mk_pat(PVar("g")), mk_expr(EApp(mk_evar("f"), mk_evar("a")))))),

  // // Let bindings with patterns
  ("let (a, b) = (0, \"hello\");",                  mk_expr(ELet(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_expr(ETuple([mk_elit_int(0), mk_elit_string("hello")]))))),
  ("let (a, b) = (0, (\"hello\", 1.0));",           
    mk_expr(ELet(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_expr(ETuple([mk_elit_int(0), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_float(1.0)]))]))))),
  ("let (a, (b, c)) = (0, (\"hello\", 1.0));",      
    mk_expr(ELet(
      mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PTuple([mk_pat(PVar("b")), mk_pat(PVar("c"))]))])), 
      mk_expr(ETuple([mk_elit_int(0), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_float(1.0)]))]))
  ))),

  // // Expressions wrapped in parantheses
  ("(a => a);",                                     mk_expr(EFun(mk_pat(PVar("a")), mk_evar("a")))),
  ("((a, b));",                                     mk_expr(ETuple([mk_evar("a"), mk_evar("b")]))),
  ("(1);",                                          mk_elit_int(1)),

  // // Modules
  ("module M = {};",                                mk_expr(EMod("M", []))),
  ("module M = {
      let x = 2;
    };",                                            mk_expr(EMod("M", [mk_expr(ELet(mk_pvar("x"), mk_elit_int(2)))]))),
  ("M.x;",                                          mk_expr(EVar(["M"], "x"))),
  ("M1.M2.x;",                                      mk_expr(EVar(["M1", "M2"], "x"))),
  
  // // External functions
  ("external f = \"int_add\";",                     mk_expr(ELet(mk_pvar("f"), mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), mk_expr(EPrim(PIntAdd(mk_evar("a"), mk_evar("b"))))))))))),
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok([expected])));

let pp_ast = (ast) => 
  switch (ast) {
  | Error(`Msg(msg)) => msg
  | Ok(ast) =>
    List.map(string_of_expr(~print_loc=true), ast)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  }

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~printer=pp_ast, expected, Mesh.parse_file(mesh_src));

let suite = 
  "test_parser" >::: List.map(make_single_test, test_cases);