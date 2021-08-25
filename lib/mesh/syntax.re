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
;

type toplevel_cmd = 
  | Expr(expr)
  | Let(string, expr)
;

let int_lit    = (v) => ELit(Int(v));
let float_lit  = (v) => ELit(Float(v));
let string_lit = (v) => ELit(String(v));
let bool_lit   = (v) => ELit(Bool(v));