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

type expr = 
  | ELit(literal)
  | EVar(name)
  | EList(list(expr))
  | ETuple(list(expr))
  | EApp(expr, expr)
  | EFun(expr, expr)
;

let int_lit    = (v) => ELit(Int(v));
let float_lit  = (v) => ELit(Float(v));
let string_lit = (v) => ELit(String(v));
let bool_lit   = (v) => ELit(Bool(v));
let unit_lit   = () => ELit(Unit);

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

// ================================================================
// Types
// ================================================================
type id = int;
type level = int;


type typ =
  | TConst(name)
  | TFun(list(typ),typ)
  | TApp(typ, list(typ))
  | TTuple(list(typ))
  | TList(typ)
  | TVar(ref(tvar))
and tvar = 
  | Free(id,level)
  | Constrained(typ)
  | Quantified(id)
;

let string_of_typ = (typ) => {
  let id_name_map = Hashtbl.create(10);
  let count = ref(0);

  let next_name = () => {
    let i = count^;
    incr(count);
    let tvar_char = String.make(1,(Char.chr(97 + i mod 26)));
    let tvar_index = i > 26 ? string_of_int(1 / 26) : ""; 
    [%string "%{tvar_char}%{tvar_index}"];
  }

  let concat_typ_strings = (f,typ_list) => List.map(f(false), typ_list) |>  String.concat(", ");

  let rec f = (is_simple, typ) => 
    switch(typ) {
      | TConst(name) => name
      | TApp(ftyp, arg_typs) => [%string "%{f true ftyp}[%{concat_typ_strings f arg_typs}]"]
      | TFun(arg_typs, rtyp) => {
        let arrow_typ_string = switch (arg_typs) {
          | [arg_typ] => [%string "%{f true arg_typ} => %{f false rtyp}"]
          | _ => [%string "(%{concat_typ_strings f arg_typs}) => %{f false rtyp}"]
        };
        is_simple ? [%string "(%{arrow_typ_string})"] : arrow_typ_string;
      }
      | TTuple(l) => [%string "(%{concat_typ_strings f l})"]
      | TList(typ) => [%string "list(%{f false typ})"]
      | TVar({contents: Quantified(id)}) => {
          try (Hashtbl.find(id_name_map,id)){
          | Not_found => {
            let name = next_name();
            Hashtbl.add(id_name_map, id, name);
            name; 
          }
        }
      }
      | TVar( {contents: Free(id, _)}) => [%string "_%{string_of_int(id)}"]
      | TVar( {contents: Constrained(typ)}) => f(is_simple,typ)
    };
  f(false,typ);
 }