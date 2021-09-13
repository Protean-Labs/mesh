open OUnit2;
open Rresult;

open Mesh.Syntax;
open Mesh.Infer;

let test_cases = [
  (int_lit(1),                 TConst("int")),
  (float_lit(1.),              TConst("float")),
  (string_lit("hello"),        TConst("string")),
  (bool_lit(true),             TConst("bool")),
  (unit_lit(),                 TConst("unit")),

  // bindings
  // let x = 1;
  (ELet(PVar("x"),int_lit(1)), TConst("int")),
  ( // let x = (1,"hello");
    ELet(PVar("x"), ETuple([int_lit(1), string_lit("hello")])),
    TTuple([TConst("int"),TConst("string")])
  ),
  (// let a = [1,2];
    ELet(PVar("a"), EList([int_lit(1), int_lit(2)])),
    TList(TConst("int"))
  ),

  // Function bindings
  (// let f = x => x;
    ELet(PVar("f"), EFun(PVar("x"),EVar("x"))),
    TFun(TVar({contents: Quantified(0)}), TVar({contents: Quantified(0)}))
  ),
  (// let f = _ => 1;
    ELet(PVar("f"), EFun(PAny, int_lit(1))),
    TFun(TVar({contents: Quantified(0)}), TConst("int"))
  ),
  (// let f = _ => [];
    ELet(PVar("f"), EFun(PAny, EList([]))),
    TFun(TVar({contents: Quantified(0)}), TList(TVar({contents: Quantified(1)})))
  ),
  (// let f = () => ();
    ELet(PVar("f"), EFun(PLit(unitv()), unit_lit())),
    TFun(TConst("unit"), TConst("unit"))
  )

]|> List.map(((mesh_expr, expected)) => (mesh_expr, R.ok([expected])));

let pp_typ_signatures = (typs) => 
  switch (typs) {
  | Error(`Msg(msg)) => msg
  | Ok(l) => List.map(string_of_typ, l) |> List.nth(_,0)
  }


let make_single_test = ((mesh_expr, expected)) =>
  (mesh_expr |> string_of_expr(0)) >:: (_) => {
    assert_equal(~printer=pp_typ_signatures, expected, infer(Env.empty, 0, [mesh_expr]));
  }

let suite = 
  "test_infering" >::: List.map(make_single_test, test_cases);