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
;

let rec string_repeat = (s,n) => n == 0 ? "" : s ++ string_repeat(s, n-1);
let dspace_repeat = string_repeat("  ");

let rec string_of_expr = (level, e) => {
  switch (e) {
    | ELit(lit) => dspace_repeat(level) ++ string_of_literal(lit)
    | EVar(name) => dspace_repeat(level) ++ name
    | EList([]) => dspace_repeat(level) ++ "[]"
    | EList(l) => {
        dspace_repeat(level) 
        ++ "[\n" ++ (List.map((x => string_of_expr(level + 1, x)),l) |> String.concat(",\n")) 
        ++ "\n" ++ dspace_repeat(level) ++ "]"
      }
    | ETuple([]) => dspace_repeat(level) ++ "()"
    | ETuple(l) => {
        dspace_repeat(level) 
        ++ "(\n" ++ (List.map((x => string_of_expr(level + 1, x)),l) |> String.concat(",\n")) 
        ++ "\n" ++  dspace_repeat(level) ++ ")"
      }
  }
};


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