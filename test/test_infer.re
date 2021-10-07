open OUnit2;
open Rresult;

open Mesh.Infer;

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
]|> List.map(((mesh_expr, expected)) => (mesh_expr, R.ok(expected)));

let pp_typ_signatures = (typs) => 
  switch (typs) {
  | Error(`Msg(msg)) => msg
  | Ok(l) => List.map(string_of_typ, l) |> String.concat(",");
  }


let make_single_test = ((mesh_expr, expected)) =>
  (mesh_expr) >:: (_) => {
    // assert_equal(~printer=pp_typ_signatures, expected, infer(Env.empty, 0, [mesh_expr]));
    assert_equal(~printer=pp_typ_signatures, expected, Mesh.parse_infer(mesh_expr) >>| ((typs, _)) => typs);
  }

let suite = 
  "test_infer" >::: List.map(make_single_test, test_cases);