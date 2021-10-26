open OUnit2;
open Rresult;
open Lwt.Infix;

open Mesh.Typetree;
open Mesh.Typetree_util;

let rec assert_type_equal = (typ, typ') => 
  switch (typ, typ') {
  | (TConst(t), TConst(t'))         => t == t'
  | (TFun(t1, t2), TFun(t1', t2'))
  | (TApp(t1, t2), TApp(t1', t2'))  => assert_type_equal(t1, t1') && assert_type_equal(t2, t2')
  | (TTuple(typs), TTuple(typs'))   => List.fold_left2((acc, typ, typ') => acc && assert_type_equal(typ, typ'), true, typs, typs')
  | (TList(t), TList(t'))           => assert_type_equal(t, t')
  | (TVar(tvar), TVar(tvar'))       => assert_tvar_equal(tvar^, tvar'^)
  | (TRec(t), TRec(t'))             => assert_type_equal(t, t')
  | (TRowEmpty, TRowEmpty)          => true
  | (TRowExtend(name, t1, t2), TRowExtend(name', t1', t2')) =>
    name == name' && assert_type_equal(t1, t1') && assert_type_equal(t2, t2')
  | (TOpt(t), TOpt(t'))             => assert_type_equal(t, t')
  | (TTag(t), TTag(t'))             => assert_type_equal(t, t')
  | (TMod(tenv), TMod(tenv'))       => List.fold_left2((acc, (_, typ), (_, typ')) => acc && assert_type_equal(typ, typ'), true, tenv, tenv')
  | _ => false
  }
and assert_tvar_equal = (tvar, tvar') =>
  switch (tvar, tvar') {
  | (Free(_), Free(_))                => false
  | (Constrained(t), Constrained(t')) => assert_type_equal(t, t')
  | (Quantified(id), Quantified(id')) => id == id'
  | _ => false
  };

let test_cases = [
  ("1;",                 [TConst("int")]),
  ("1.0;",                [TConst("float")]),
  ("\"hello\";",         [TConst("string")]),
  ("true;",              [TConst("bool")]),
  ("();",                [TConst("unit")]),

  // bindings
  // let x = 1;
  ("let x = 1; x;", [TConst("unit"),TConst("int")]),
  ( 
    "let x = (1,\"hello\"); x;",
    [TConst("unit"), TTuple([TConst("int"),TConst("string")])]
  ),
  (// let a = [1,2];
    "let a = [1,2]; a;",
    [TConst("unit"), TList(TConst("int"))]
  ),

  // Function bindings
  // (// let f = x => x;
  //   ELet(PVar("f"), EFun(PVar("x"), var("x"))),
  //   TFun(TVar({contents: Quantified(0)}), TVar({contents: Quantified(0)}))
  // ),
  (
    "let f = x => x; let x = f(1); x;",
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),
  (
    "let f = _ => 1; let x = f(\"hello\"); x;",
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),
  (
    "let f = a => [a]; let x = f(\"hello\"); x;",
    [TConst("unit"), TConst("unit"), TList(TConst("string"))]
  ),
  (
    "let f = () => (); let x = f(); x;",
    [TConst("unit"), TConst("unit"), TConst("unit")]
  ),
  // (// let f = () => {let a = 1; let b = a; b };
  //   ELet(PVar("f"), EFun(PLit(unitv()), ESeq(ELet(PVar("a"), int_lit(1)), ESeq(ELet(PVar("b"), var("a")), var("b"))))),
  //   TFun(TConst("unit"), TConst("int"))
  // ),
  (
    "let f = () => {let a = 1; let b = a; b }; let x = f(); x;",
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),

  // Destructuring
  // (
  //   "let (a, b) = (1, \"hello\");",
  //   [TConst("int"), TConst("string")]
  // ),
  // (
  //   "let (a, b) = (1, (1,2));",
  //   [TConst("int"),TTuple([TConst("int"), TConst("int")])]
  // ),
  // (
  //   "let (a, (b, c)) = (1, (1, 1));",
  //   [TConst("int"),TConst("int"),TConst("int")]
  // ),

  // Closures
  (// need to fix diagnose variable inversion after application
    "let f = (a, (b, c)) => {let a = (); let b = 2; (a, b, c);}; let x = f(1, ((), \"hello\")); x;",
    [TConst("unit"), TConst("unit"), TTuple([TConst("unit"), TConst("int"), TConst("string")])]
  ),

  // External
  (
    "external f = \"list_cons\"; let f1 = f(1); let f2 = f(()); f1; f2;",
    [
      TConst("unit"), TConst("unit"), TConst("unit"), 
      TFun(TList(TConst("int")), TList(TConst("int"))),
      TFun(TList(TConst("unit")), TList(TConst("unit"))) 
    ]
  ),
  (
    "external f = \"int_add\"; let f1 = f(1); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("int"), TConst("int"))]
  ),
  (
    "external f = \"int_sub\"; let f1 = f(1); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("int"), TConst("int"))]
  ),
  (
    "external f = \"int_mul\"; let f1 = f(1); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("int"), TConst("int"))]
  ),
  (
    "external f = \"int_div\"; let f1 = f(1); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("int"), TConst("int"))]
  ),
  (
    "external f = \"float_add\"; let f1 = f(1.0); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("float"), TConst("float"))]
  ),
  (
    "external f = \"float_sub\"; let f1 = f(1.0); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("float"), TConst("float"))]
  ),
  (
    "external f = \"float_mul\"; let f1 = f(1.0); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("float"), TConst("float"))]
  ),
  (
    "external f = \"float_div\"; let f1 = f(1.0); f1;",
    [TConst("unit"), TConst("unit"), TFun(TConst("float"), TConst("float"))]
  ),

  // modules
  (
    "module M = {
      let x = 2;
    };
    M.x;",
    [TConst("unit"), TConst("int")]
  ),
  (
    "module M = {
      let f = (a, b) => a;
    };
    let f2 = M.f(1);
    f2(\"hello\");",
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),
  ("external add = \"int_add\";
    let (+) = add;
    1 + 2;",                                
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),
  ("external add = \"int_add\";
    let (.++) = add(1);
    ++2;",                                  
    [TConst("unit"), TConst("unit"), TConst("int")]
  ),
  (
    "external f = \"int_add\"; 
    let x = 1; 
    module M = { 
      let y = x; 
    }; 
    f(x, M.y);",
    [TConst("unit"), TConst("unit"), TConst("unit"), TConst("int")]
  ),
  (
    "external f = \"int_add\";
    let x = 1; 
    module M = { 
      let x = (); 
    }; 
    open M; 
    x;",
    [TConst("unit"), TConst("unit"), TConst("unit"), TConst("unit"), TConst("unit")]
  ),
  (
    "let x = {a:1, b:\"hello\"}; x;",
    [TConst("unit"), TRec((TRowExtend("b", TConst("string"), TRowExtend("a", TConst("int"), TRowEmpty))))]
  ),
  (
    "let x = {a:1, b:\"hello\"}; let y = {...x, c:x}; y;",
    [TConst("unit"), TConst("unit"), 
      TRec((TRowExtend("c", TRec((
        TRowExtend("b", TConst("string"), TRowExtend("a", TConst("int"), TRowEmpty))
      )), TRowExtend("b", TConst("string"), TRowExtend("a", TConst("int"), TRowEmpty)))))
    ]
  ),
  (
    "let f = (y) => y;  let x = {a:f(1)}; let y = {b:f(\"hello\")}; (x,y);",
    [TConst("unit"), TConst("unit"), TConst("unit"),
      TTuple([
        TRec((TRowExtend("a", TConst("int"), TRowEmpty))),
        TRec((TRowExtend("b", TConst("string"), TRowEmpty)))
      ])
    ]
  ),
  // Stdlib
  ("let l = [1, 2, 3];
    let f = (x) => x + 1;
    let x = List.map(f, l);
    x;",                       [TConst("unit"), TConst("unit"), TConst("unit"), TList(TConst("int"))]),

  ("let l = [1, 2, 3];
    let x = List.map((x) => x + 1, l);
    x;",            [TConst("unit"), TConst("unit"), TList(TConst("int"))]),

  ("let l = [1, 2, 3];
    let f = (i, x) => x + i;
    let x = List.mapi(f, l);
    x;",                      [TConst("unit"), TConst("unit"), TConst("unit"), TList(TConst("int"))]),

  ("let l = [1, 2, 3];
    let x = List.foldl((acc, x) => acc + x, 0, l);
    x;",                      
    [TConst("unit"), TConst("unit"), TConst("int")]),

  ("let l = [1, 2, 3];
    let x = List.foldr((x, acc) => acc + x, l, 0);
    x;",                      
    [TConst("unit"), TConst("unit"), TConst("int")]),

  // Options
  ("let x = Option.some(1);
    x;", 
    [TConst("unit"), TOpt(TConst("int"))]),

  // TODO: Figure out how to make tests with free/quantified variables
  // ("let x = Option.none;
  //   x;",
  //   [TConst("unit"), TOpt(TConst("int"))]),
  
  ("let maybe_int = Option.some(1);
    let x = Option.get(10, maybe_int);
    x;", 
    [TConst("unit"), TConst("unit"), TConst("int")]),

  ("let maybe_int = Option.none;
    let x = Option.get(10, maybe_int);
    x;", 
    [TConst("unit"), TConst("unit"), TConst("int")]),

  ("let data = Graphql.execute(```graphql(https://countries.trevorblades.com/)
      query {
        country(code: \"BR\") {
          name
        }
      }
    ```);
    data;", [TConst("unit"), TRec(TRowExtend("country", TOpt(TRec(TRowExtend("name", TConst("string"), TRowEmpty))), TRowEmpty))])
]|> List.map(((mesh_expr, expected)) => (mesh_expr, R.ok(expected)));

let pp_typ_signatures = (typs) => 
  switch (typs) {
  | Error(`Msg(msg)) => msg
  | Ok(l) => List.map(string_of_typ, l) |> String.concat(",");
  };

let cmp_type = (typs, typs') =>
  R.Infix.(switch (
    typs   >>= (typs) =>
    typs'  >>| (typs') =>
    List.fold_left2((acc, typ, typ') => acc && assert_type_equal(typ, typ'), true, typs, typs')
  ) {
  | Ok(true) => true
  | _ => false
  });


let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: OUnitLwt.lwt_wrapper((_) => 
    Lwt_result.map(fst, Mesh.parse_infer(mesh_src)) >|= (typs) => 
    assert_equal(~cmp=cmp_type, ~printer=pp_typ_signatures, expected, typs)
  )

let suite = 
  "test_infer" >::: List.map(make_single_test, test_cases);