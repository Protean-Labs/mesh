open Syntax;
open Syntax_util;

exception Invalid_primitive(string);

let mk_2args_primitive = (expr) =>
  mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), expr))));

let primitive_of_name = fun
  | "list_cons" => mk_2args_primitive(mk_expr(EPrim(PListCons(mk_evar("a"), mk_evar("b")))))
  | "int_add"   => mk_2args_primitive(mk_expr(EPrim(PIntAdd(mk_evar("a"), mk_evar("b")))))
  | "int_sub"   => mk_2args_primitive(mk_expr(EPrim(PIntSub(mk_evar("a"), mk_evar("b")))))
  | "int_mul"   => mk_2args_primitive(mk_expr(EPrim(PIntMul(mk_evar("a"), mk_evar("b")))))
  | "int_div"   => mk_2args_primitive(mk_expr(EPrim(PIntDiv(mk_evar("a"), mk_evar("b")))))
  | "float_add" => mk_2args_primitive(mk_expr(EPrim(PFloatAdd(mk_evar("a"), mk_evar("b")))))
  | "float_sub" => mk_2args_primitive(mk_expr(EPrim(PFloatSub(mk_evar("a"), mk_evar("b")))))
  | "float_mul" => mk_2args_primitive(mk_expr(EPrim(PFloatMul(mk_evar("a"), mk_evar("b")))))
  | "float_div" => mk_2args_primitive(mk_expr(EPrim(PFloatDiv(mk_evar("a"), mk_evar("b")))))
  | "graphql"   => mk_2args_primitive(mk_expr(EPrim(PGraphQL(mk_evar("a"), mk_evar("b")))))
  | name        => raise(Invalid_primitive(name))
;