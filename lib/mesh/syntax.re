type literal = 
  | Int(int)
  | Float(float)
  | String(string)
  | Bool(bool)
;

let intv    = (v) => Int(v);
let floatv  = (v) => Float(v);
let stringv = (v) => String(v);
let boolv   = (v) => Bool(v);

let string_of_literal = fun
  | Int(v)    => string_of_int(v)
  | Float(v)  => string_of_float(v)
  | String(v) => v
  | Bool(v)   => string_of_bool(v)
 // | List(v)   => List.map(string_of_literal, v) |> String.concat(",")
;

type name = string;

type expr = 
  | ELit(literal)
  | EVar(name)
  | EList(list(expr))
  | ETuple(list(expr))
  | EApp(expr, expr)
  | EFun(expr, expr)
;

let rec string_repeat = (s,n) => n == 0 ? "" : s ++ string_repeat(s, n-1);
let dspace_repeat = string_repeat("  ");

let rec string_of_expr = (level, e) =>
  dspace_repeat(level)  |> (indent) =>
  switch (e) {
  | ELit(lit)           => [%string "%{indent}(ELit %{string_of_literal lit})"]
  | EVar(name)          => [%string "%{indent}(EVar %{name})"]
  | EList([])           => [%string "%{indent}(EList [])"]
  | EList(l)            => 
    List.map((ele) => string_of_expr(level + 1, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(EList \n%{elements})"]
  | ETuple([])          => [%string "%{indent}(ETuple ())"] 
  | ETuple(l)           => 
    List.map((ele) => string_of_expr(level + 1, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(ETuple \n%{elements})"]
  | EApp(e1, e2)        => [%string "%{indent}(EApp %{string_of_expr 0 e1}\n%{string_of_expr (level + 1) e2})"]
  | EFun(e1, e2)        => [%string "%{indent}(EFun %{string_of_expr 0 e1} =>\n%{string_of_expr (level + 1) e2})"]
  }

type toplevel_cmd = 
  | Expr(expr)
  | Let(string, expr)
;

let string_of_top = fun
  | Expr(e) => string_of_expr(0, e)
  | Let(vname,e) => "Let(" ++ "\n  " ++ vname ++ "\n" ++ string_of_expr(1, e) ++ "\n)"
;

let int_lit    = (v) => ELit(Int(v));
let float_lit  = (v) => ELit(Float(v));
let string_lit = (v) => ELit(String(v));
let bool_lit   = (v) => ELit(Bool(v));