open OUnit2;
open Rresult;

open Mesh.Syntax;
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
  )


]|> List.map(((mesh_expr, expected)) => (mesh_expr, R.ok(expected)));

let pp_typ_signatures = (typs) => 
  switch (typs) {
  | Error(`Msg(msg)) => msg
  | Ok(l) => List.map(string_of_typ, l) |> List.hd
  }


let make_single_test = ((mesh_expr, expected)) =>
  (mesh_expr) >:: (_) => {
    // assert_equal(~printer=pp_typ_signatures, expected, infer(Env.empty, 0, [mesh_expr]));
    assert_equal(~printer=pp_typ_signatures, expected, Mesh.parse_file(mesh_expr) >>= infer(Env.empty, 0) >>| ((typs, new_env)) => {Env.reset_id(new_env); typs});
  }

let suite = 
  "test_infering" >::: List.map(make_single_test, test_cases);