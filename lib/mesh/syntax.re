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

type name = string;

type expr = 
  | Lit(literal)
  | Var(name)
  | Let(name, expr, expr)
;
