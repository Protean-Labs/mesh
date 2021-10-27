open Parsetree;
open Parsetree_util;

exception Invalid_primitive(string);

let rec mk_narg_primitive = (n, expr) =>
  switch (n) {
  | 0 => mk_expr(EFun(mk_pvar("arg0"), expr))
  | n => mk_expr(EFun(mk_pvar([%string "arg%{string_of_int n}"]), mk_narg_primitive(n-1, expr)))
  };

let mk_1arg_primitive = (expr) =>
  mk_expr(EFun(mk_pvar("a"), expr));

let mk_2args_primitive = (expr) =>
  mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), expr))));

let mk_3args_primitive = (expr) =>
  mk_expr(EFun(mk_pvar("a"), mk_expr(EFun(mk_pvar("b"), mk_expr(EFun(mk_pvar("c"), expr))))));

let primitive_of_name = fun
  // Int primitive functions
  | "int_add"     => mk_2args_primitive(mk_expr(EPrim(PIntAdd(mk_evar("a"), mk_evar("b")))))
  | "int_sub"     => mk_2args_primitive(mk_expr(EPrim(PIntSub(mk_evar("a"), mk_evar("b")))))
  | "int_mul"     => mk_2args_primitive(mk_expr(EPrim(PIntMul(mk_evar("a"), mk_evar("b")))))
  | "int_div"     => mk_2args_primitive(mk_expr(EPrim(PIntDiv(mk_evar("a"), mk_evar("b")))))
  | "int_neg"     => mk_1arg_primitive(mk_expr(EPrim(PIntNeg(mk_evar("a")))))

  // Float primitive functions
  | "float_add"   => mk_2args_primitive(mk_expr(EPrim(PFloatAdd(mk_evar("a"), mk_evar("b")))))
  | "float_sub"   => mk_2args_primitive(mk_expr(EPrim(PFloatSub(mk_evar("a"), mk_evar("b")))))
  | "float_mul"   => mk_2args_primitive(mk_expr(EPrim(PFloatMul(mk_evar("a"), mk_evar("b")))))
  | "float_div"   => mk_2args_primitive(mk_expr(EPrim(PFloatDiv(mk_evar("a"), mk_evar("b")))))
  | "float_neg"   => mk_1arg_primitive(mk_expr(EPrim(PFloatNeg(mk_evar("a")))))

  // List primitive functions
  | "list_cons"   => mk_2args_primitive(mk_expr(EPrim(PListCons(mk_evar("a"), mk_evar("b")))))
  | "list_map"    => mk_2args_primitive(mk_expr(EPrim(PListMap(mk_evar("a"), mk_evar("b")))))
  | "list_mapi"   => mk_2args_primitive(mk_expr(EPrim(PListMapi(mk_evar("a"), mk_evar("b")))))
  | "list_foldl"  => mk_3args_primitive(mk_expr(EPrim(PListFoldl(mk_evar("a"), mk_evar("b"), mk_evar("c")))))
  | "list_foldr"  => mk_3args_primitive(mk_expr(EPrim(PListFoldr(mk_evar("a"), mk_evar("b"), mk_evar("c")))))

  // Option primitive functions
  | "option_some"  => mk_1arg_primitive(mk_expr(EPrim(POptionSome(mk_evar("a")))))
  | "option_none"  => mk_expr(EPrim(POptionNone))
  | "option_get"  => mk_2args_primitive(mk_expr(EPrim(POptionGet(mk_evar("a"), mk_evar("b")))))

  // GraphQL
  // | "graphql_execute" => mk_2args_primitive(mk_expr(EPrim(PGraphqlExec(mk_evar("a"), mk_evar("b")))))
  // TODO: Revisit graphql_execute with URI
  | "graphql_execute" => mk_1arg_primitive(mk_expr(EPrim(PGraphqlExec(mk_evar("a")))))
  
  | name          => raise(Invalid_primitive(name))
;