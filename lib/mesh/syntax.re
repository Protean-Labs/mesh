// open Compiler_libs;

// ================================================================
// Expressions
// ================================================================
type literal = 
  | Int(int)
  | Float(float)
  | String(string)
  | Bool(bool)
  | Unit;

type name = string;

type pattern = {
  ppat_desc: pattern_desc,
  ppat_loc: Location.t
}
and pattern_desc = 
  | PAny                    // _        E.g.: let f = (_) => 10;
  | PVar(name)              // x        E.g.: let f = (x) => 1;
  | PLit(literal)           // true     E.g.: let f = (true) => false;
  | PTuple(list(pattern));  // (a, b)   E.g.: let f = ((a, b)) => a;

type expr = {
  pexpr_desc: expr_desc,
  pexpr_loc: Location.t
}
and expr_desc =
  | ELit(literal)
  | EVar(list(name), name)
  | EList(list(expr))
  | ETuple(list(expr))
  | EApp(expr, expr)
  | EFun(pattern, expr)
  | ELet(pattern, expr)
  | ERecSelect(expr, name)
  | ERecExtend(name, expr, expr)
  | ERecEmpty
  | ESeq(expr, expr)
  | EMod(name, list(expr))
  | EPrim(primitive)
  | EGraphql(Extensions.Graphql.t)
and primitive = 
  | PListCons(expr, expr)
  | PIntAdd(expr, expr)
  | PIntSub(expr, expr)
  | PIntMul(expr, expr)
  | PIntDiv(expr, expr)
  | PFloatAdd(expr, expr)
  | PFloatSub(expr, expr)
  | PFloatMul(expr, expr)
  | PFloatDiv(expr, expr)
  | PGraphQL(expr, expr);
