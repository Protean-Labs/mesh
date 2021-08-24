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
;

type name = string;

type expr = 
  | Lit(literal)
  | Var(name)
;

type toplevel_cmd = 
  | Expr(expr)
  | Let(string, expr)
;

let int_lit    = (v) => Lit(Int(v));
let float_lit  = (v) => Lit(Float(v));
let string_lit = (v) => Lit(String(v));
let bool_lit   = (v) => Lit(Bool(v));