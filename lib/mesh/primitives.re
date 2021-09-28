open Syntax;

exception InvalidPrimitive(string);

let primitive_of_name = fun
  | "list_cons" => EFun(PVar("a"), EFun(PVar("b"), EPrim(PListCons(EVar([], "a"), EVar([], "b")))))
  | "int_add"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PIntAdd(EVar([], "a"), EVar([], "b")))))
  | "int_sub"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PIntSub(EVar([], "a"), EVar([], "b")))))
  | "int_mul"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PIntMul(EVar([], "a"), EVar([], "b")))))
  | "int_div"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PIntDiv(EVar([], "a"), EVar([], "b")))))
  | "float_add"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PFloatAdd(EVar([], "a"), EVar([], "b")))))
  | "float_sub"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PFloatSub(EVar([], "a"), EVar([], "b")))))
  | "float_mul"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PFloatMul(EVar([], "a"), EVar([], "b")))))
  | "float_div"   => EFun(PVar("a"), EFun(PVar("b"), EPrim(PFloatDiv(EVar([], "a"), EVar([], "b")))))
  | name          => raise(InvalidPrimitive(name))
;