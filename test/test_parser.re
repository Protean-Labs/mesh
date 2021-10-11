open OUnit2;
open Rresult;

open Mesh.Syntax;
open Mesh.Syntax_util;

exception Missing_test(string);

let rec assert_expr_equal = (expr, expr') => 
  switch (expr.pexpr_desc, expr'.pexpr_desc) {
  | (ELit(lit), ELit(lit'))                   => lit == lit' 
  | (EVar(path, name), EVar(path', name'))    => (path == path') && (name == name')
  | (EList(l), EList(l'))                     => List.fold_left2((acc, ele, ele') => acc && assert_expr_equal(ele, ele'), true, l, l')
  | (ETuple(l), ETuple(l'))                   => List.fold_left2((acc, ele, ele') => acc && assert_expr_equal(ele, ele'), true, l, l')
  | (EApp(e1, e2), EApp(e1', e2'))            => assert_expr_equal(e1, e1') && assert_expr_equal(e2, e2')
  | (EFun(pat, e), EFun(pat', e'))            => assert_pat_equal(pat, pat') && assert_expr_equal(e, e')
  | (ELet(pat, e), ELet(pat', e'))            => assert_pat_equal(pat, pat') && assert_expr_equal(e, e')
  | (ESeq(e, rest), ESeq(e', rest'))          => assert_expr_equal(e, e') && assert_expr_equal(rest, rest')
  | (EMod(name, body), EMod(name', body'))    => (name == name') && List.fold_left2((acc, ele, ele') => acc && assert_expr_equal(ele, ele'), true, body, body')
  | (EPrim(prim), EPrim(prim'))               => assert_prim_equal(prim, prim')
  | (EOpen(path, modname), EOpen(path', modname')) => (path == path') && (modname == modname')
  | (ERecExtend(name, e1, e2), ERecExtend(name', e1', e2')) => 
    (name == name') && assert_expr_equal(e1, e1') && assert_expr_equal(e2, e2')
  | (ERecSelect(e, name), ERecSelect(e', name')) => (name == name') && assert_expr_equal(e, e')
  | (ERecEmpty, ERecEmpty)                    => true
  | (EGraphql(_), EGraphql(_))                => true
  | _                                         => false
  }
and assert_prim_equal = (prim, prim') =>
  switch (prim, prim') {
  | (PListCons(e1, e2), PListCons(e1', e2'))    
  | (PIntAdd(e1, e2), PIntAdd(e1', e2'))        
  | (PIntSub(e1, e2), PIntSub(e1', e2'))        
  | (PIntMul(e1, e2), PIntMul(e1', e2'))        
  | (PIntDiv(e1, e2), PIntDiv(e1', e2'))        
  | (PFloatAdd(e1, e2), PFloatAdd(e1', e2'))    
  | (PFloatSub(e1, e2), PFloatSub(e1', e2'))    
  | (PFloatMul(e1, e2), PFloatMul(e1', e2'))    
  | (PFloatDiv(e1, e2), PFloatDiv(e1', e2'))  => assert_expr_equal(e1, e1') && assert_expr_equal(e2, e2')
  | _                                         => false 
  }
and assert_pat_equal = (pat, pat') =>
  switch (pat.ppat_desc, pat'.ppat_desc) {
  | (PAny, PAny)              => true
  | (PVar(name), PVar(name')) => name == name'
  | (PLit(lit), PLit(lit'))   => lit == lit'
  | (PTuple(l), PTuple(l'))   => List.fold_left2((acc, ele, ele') => acc && assert_pat_equal(ele, ele'), true, l, l')
  | _                         => false
  }
;

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

  // Function binding
  ("let f = (a, b) => a;",            mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))))),
  ("let f = ((a, b)) => a;",          mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_evar("a")))))),
  ("let f = (a) => (b) => a;",        mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_expr(EFun(mk_pat(PVar("b")), mk_evar("a")))))))),
  ("let f = a => a;",                 mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PVar("a")), mk_evar("a")))))),
  ("let f = () => 10;",               mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PLit(Unit)), mk_elit_int(10)))))),
  ("let f = () => {
      foo(a);
      true
    };",                              mk_expr(ELet(mk_pat(PVar("f")), mk_expr(EFun(mk_pat(PLit(Unit)), mk_expr(ESeq(mk_expr(EApp(mk_evar("foo"), mk_evar("a"))), mk_elit_bool(true)))))))),

  // Function application
  ("f(a, b);",                        mk_expr(EApp(mk_expr(EApp(mk_evar("f"), mk_evar("a"))), mk_evar("b")))),
  ("f(a)(b);",                        mk_expr(EApp(mk_expr(EApp(mk_evar("f"), mk_evar("a"))), mk_evar("b")))),
  ("f((a, b));",                      mk_expr(EApp(mk_evar("f"), mk_expr(ETuple([mk_evar("a"), mk_evar("b")]))))),
  ("f(1);",                           mk_expr(EApp(mk_evar("f"), mk_elit_int(1)))),

  // Function partial application
  ("let g = f(a);",                   mk_expr(ELet(mk_pat(PVar("g")), mk_expr(EApp(mk_evar("f"), mk_evar("a")))))),

  // // Let bindings with patterns
  ("let (a, b) = (0, \"hello\");",                  
    mk_expr(ELet(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_expr(ETuple([mk_elit_int(0), mk_elit_string("hello")]))))),
  
  ("let (a, b) = (0, (\"hello\", 1.0));",           
    mk_expr(ELet(mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PVar("b"))])), mk_expr(ETuple([mk_elit_int(0), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_float(1.0)]))]))))),
  
  ("let (a, (b, c)) = (0, (\"hello\", 1.0));",      
    mk_expr(ELet(
      mk_pat(PTuple([mk_pat(PVar("a")), mk_pat(PTuple([mk_pat(PVar("b")), mk_pat(PVar("c"))]))])), 
      mk_expr(ETuple([mk_elit_int(0), mk_expr(ETuple([mk_elit_string("hello"), mk_elit_float(1.0)]))]))
    ))
  ),

  // Expressions wrapped in parantheses
  ("(a => a);",                                     mk_expr(EFun(mk_pat(PVar("a")), mk_evar("a")))),
  ("((a, b));",                                     mk_expr(ETuple([mk_evar("a"), mk_evar("b")]))),
  ("(1);",                                          mk_elit_int(1)),

  // // Modules
  // TODO: Figure out if we should re-support empty modules
  // ("module M = {};",                                mk_expr(EMod("M", []))),
  ("module M = {
      let x = 2;
    };",                                            mk_expr(EMod("M", [mk_expr(ELet(mk_pvar("x"), mk_elit_int(2)))]))),
  ("M.x;",                                          mk_expr(EVar(["M"], "x"))),
  ("M1.M2.x;",                                      mk_expr(EVar(["M1", "M2"], "x"))),
  ("open M;",                                       mk_expr(EOpen([], "M"))),
  ("open M1.M2;",                                   mk_expr(EOpen(["M1"], "M2"))),
  
  // External functions
  ("external f = \"int_add\";",                     mk_expr(ELet(mk_pvar("f"), mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), mk_expr(EPrim(PIntAdd(mk_evar("a"), mk_evar("b"))))))))))),

  // Operator definition
  ("let (*) = f;",                                  mk_expr(ELet(mk_pvar("*"), mk_evar("f")))),
  ("let (+) = (a, b) => a;",                        mk_expr(ELet(mk_pvar("+"), mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), mk_evar("a")))))))),
  ("let (.~) = (a) => f(a);",                       mk_expr(ELet(mk_pvar("~"), mk_expr(EFun(mk_pvar("a"), mk_expr(EApp(mk_evar("f"), mk_evar("a")))))))),
  
  // Records
  ("{};",                                           mk_expr(ERecEmpty)),
  ("{a: 1, b: 2};",                                 mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_elit_int(1), mk_expr(ERecEmpty)))))),
  ("{...x, a: 1, b: 2};",                           mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_elit_int(1), mk_evar("x")))))),

  ("let r = {a: 1, b: 2};",                         mk_expr(ELet(mk_pvar("r"), mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_elit_int(1), mk_expr(ERecEmpty)))))))),
  ("let r = {...x, a: 1, b: 2};",                   mk_expr(ELet(mk_pvar("r"), mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_elit_int(1), mk_evar("x")))))))),

  ("let f = (x) => {a: x, b: 2};",                  
    mk_expr(ELet(mk_pvar("f"), mk_expr(EFun(mk_pvar("x"), mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_evar("x"), mk_expr(ERecEmpty)))))))))),
  
  ("let f = (x) => {...x, a: 1, b: 2};",            
    mk_expr(ELet(mk_pvar("f"), mk_expr(EFun(mk_pvar("x"), mk_expr(ERecExtend("b", mk_elit_int(2), mk_expr(ERecExtend("a", mk_elit_int(1), mk_evar("x")))))))))),
  
  ("r.a;",                                          mk_expr(ERecSelect(mk_evar("r"), "a"))),

  // Extensions
  // TODO: Make test compare query 
  // ("let query = ```graphql
  //     query {
  //       country(code: \"BR\") {
  //         name
  //       }
  //     }
  //   ```;
    
  //   Graphql.execute: string => graphql_query => 'b;

  //   let data = Graphql.execute(\"http://www.endpoint.com/graphql\", query);",
  //   mk_expr(EGraphql([])))

] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok([expected])));

let pp_ast = (ast) => 
  switch (ast) {
  | Error(`Msg(msg)) => msg
  | Ok(ast) =>
    List.map(string_of_expr(~print_loc=true), ast)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  }

let cmp_ast = (ast, ast') =>
  switch (
    ast   >>= (ast) =>
    ast'  >>| (ast') =>
    List.fold_left2((acc, e, e') => acc && assert_expr_equal(e, e'), true, ast, ast')
  ) {
  | Ok(true) => true
  | _ => false
  };

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: (_) => assert_equal(~cmp=cmp_ast, ~printer=pp_ast, expected, Lwt_main.run @@ Mesh.parse(mesh_src));

let suite = 
  "test_parser" >::: List.map(make_single_test, test_cases);