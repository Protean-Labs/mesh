open OUnit2;
open Rresult;

open Mesh.Syntax;

let test_cases = [
  (int_lit(1),                 TConst("int")),
  (float_lit(1.),              TConst("float")),
  (string_lit("hello"),        TConst("string")),
  (bool_lit(true),             TConst("bool")),
  (unit_lit(),                 TConst("unit")),
]|> List.map(((mesh_expr, expected)) => (mesh_expr, R.ok(expected)));

let pp_typ_signatures = (typ) => 
  switch (typ) {
  | Error(`Msg(msg)) => msg
  | Ok(typ) => string_of_typ(typ)
  }

let make_single_test = ((mesh_expr, expected)) =>
  (mesh_expr |> string_of_expr(0)) >:: (_) => assert_equal(~printer=pp_typ_signatures, expected, Mesh.Infer.infer(mesh_expr));

let suite = 
  "test_infering" >::: List.map(make_single_test, test_cases);