// ================================================================
// Expressions
// ================================================================
type literal = 
  | Int(int)
  | Float(float)
  | String(string)
  | Bool(bool)
  | Unit
;

let intv    = (v) => Int(v);
let floatv  = (v) => Float(v);
let stringv = (v) => String(v);
let boolv   = (v) => Bool(v);
let unitv   = () => Unit;

let string_of_literal = fun
  | Int(v)    => string_of_int(v)
  | Float(v)  => string_of_float(v)
  | String(v) => v
  | Bool(v)   => string_of_bool(v)
  | Unit      => "()"
;

type name = string;

type pattern = 
  | PAny                    // _        E.g.: let f = (_) => 10;
  | PVar(name)              // x        E.g.: let f = (x) => 1;
  | PLit(literal)           // true     E.g.: let f = (true) => false;
  | PTuple(list(pattern))   // (a, b)   E.g.: let f = ((a, b)) => a;
;

let rec string_of_pattern = fun
  | PAny          => "PAny"
  | PVar(name)    => [%string "PVar %{name}"]
  | PLit(lit)     => [%string "PLit %{string_of_literal lit}"]
  | PTuple(pats)  => 
    List.map(string_of_pattern, pats) |> String.concat(", ") |> (inner) =>
    [%string "(PTuple %{inner})"]
;

type expr = 
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
;

let int_lit    = (v) => ELit(Int(v));
let float_lit  = (v) => ELit(Float(v));
let string_lit = (v) => ELit(String(v));
let bool_lit   = (v) => ELit(Bool(v));
let unit_lit   = () => ELit(Unit);

let var = (~path=[], varname) => EVar(path, varname);

let rec string_repeat = (s,n) => n == 0 ? "" : s ++ string_repeat(s, n-1);

let dspace_repeat = string_repeat("  ");


let rec string_of_expr = (level, e) =>
  dspace_repeat(level)  |> (indent) =>
  switch (e) {
  | ELit(lit)                 => [%string "%{indent}(ELit %{string_of_literal lit})"]
  | EVar([], name)            => [%string "%{indent}(EVar %{name})"]
  | EVar(path, name)          => 
    String.concat(".", path)  |> (path) =>
    [%string "%{indent}(EVar %{path}.%{name})"]
  | EList([])                 => [%string "%{indent}(EList [])"]
  | EList(l)                  => 
    List.map((ele) => string_of_expr(level + 1, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(EList \n%{elements})"]
  | ETuple([])                => [%string "%{indent}(ETuple ())"] 
  | ETuple(l)                 => 
    List.map((ele) => string_of_expr(level + 1, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(ETuple \n%{elements})"]
  | EApp(e1, e2)              => [%string "%{indent}(EApp %{string_of_expr 0 e1}\n%{string_of_expr (level + 1) e2})"]
  | EFun(pat, e)              => [%string "%{indent}(EFun %{string_of_pattern pat} =>\n%{string_of_expr (level + 1) e})"]
  | ELet(pat, e)              => [%string "%{indent}(ELet %{string_of_pattern pat} =\n%{string_of_expr (level + 1) e})"]
  | ERecSelect(e, name)       => [%string "%{indent}(ERecSelect %{string_of_expr 0 e} %{name})"]
  | ERecExtend(name, e1, e2)  => [%string "%{indent}(ERecExtend %{name}\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2})"]
  | ERecEmpty                 => [%string "%{indent}ERecEmpty"]
  | ESeq(e, rest)             => [%string "%{indent}(ESeq \n%{string_of_expr (level + 1) e}\n%{string_of_expr (level + 1) rest})"]
  | EMod(name, body)          => 
    List.map((ele) => string_of_expr(level + 1, ele), body) |> String.concat("\n") |> (elements) =>    
    [%string "%{indent}(EMod %{name}\n%{elements})"]
  | EPrim(prim)               => string_of_primitive(level, prim)
  }
and string_of_primitive = (level, prim) =>
  dspace_repeat(level)  |> (indent) =>
  switch (prim) {
  | PListCons(e1, e2)   => [%string "%{indent}(list_cons\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PIntAdd(e1, e2)     => [%string "%{indent}(int_add\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PIntSub(e1, e2)     => [%string "%{indent}(int_sub\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PIntMul(e1, e2)     => [%string "%{indent}(int_mul\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PIntDiv(e1, e2)     => [%string "%{indent}(int_div\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PFloatAdd(e1, e2)   => [%string "%{indent}(float_add\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PFloatSub(e1, e2)   => [%string "%{indent}(float_sub\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PFloatMul(e1, e2)   => [%string "%{indent}(float_mul\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  | PFloatDiv(e1, e2)   => [%string "%{indent}(float_div\n%{string_of_expr (level + 1) e1}\n%{string_of_expr (level + 1) e2}"]
  };
  
