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
    [%string "(%{inner})"]
;

type expr = 
  | ELit(literal)
  | EVar(name)
  | EList(list(expr))
  | ETuple(list(expr))
  | EApp(expr, expr)
  | EFun(pattern, expr)
  | ELet(pattern, expr)
  | ESeq(expr, expr)
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
  | EFun(pat, e)        => [%string "%{indent}(EFun %{string_of_pattern pat} =>\n%{string_of_expr (level + 1) e})"]
  | ELet(pat, e)        => [%string "%{indent}(ELet %{string_of_pattern pat} =\n%{string_of_expr (level + 1) e})"]
  | ESeq(e, rest)       => [%string "%{indent}(ESeq \n%{string_of_expr (level + 1) e}\n%{string_of_expr (level + 1) rest})"]
  };

// ================================================================
// Types
// ================================================================
type id = int;
type level = int;


type typ =
  | TConst(name)
  | TFun(typ, typ)
  | TApp(typ, typ)
  | TTuple(list(typ))
  | TList(typ)
  | TVar(ref(tvar))
and tvar = 
  | Free(id, level)
  | Constrained(typ)
  | Quantified(id)
;

let string_of_typ = (typ) => {
  let id_name_map = Hashtbl.create(10);
  let count = ref(0);

  let next_name = () => {
    let i = count^;
    incr(count);
    let tvar_char = String.make(1, (Char.chr(97 + i mod 26)));
    let tvar_index = i > 26 ? string_of_int(1 / 26) : ""; 
    [%string "%{tvar_char}%{tvar_index}"];
  }

  let concat_typ_strings = (f, typ_list) => List.map(f(false), typ_list) |>  String.concat(", ");

  let rec f = (is_simple, typ) => 
    switch(typ) {
    | TConst(name) => name
    | TApp(ftyp, param_typ) => [%string "%{f true ftyp}[%{f false param_typ}]"]
    | TFun(param_typ, rtyp) => {
      let arrow_typ_string = [%string "%{f true param_typ} => %{f false rtyp}"]
      is_simple ? [%string "(%{arrow_typ_string})"] : arrow_typ_string;
    }
    | TTuple(l) => [%string "(%{concat_typ_strings f l})"]
    | TList(typ) => [%string "list(%{f false typ})"]
    | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_name_map, id)) {
        | Not_found => {
          let name = next_name();
          Hashtbl.add(id_name_map, id, name);
          name; 
        }
      }
    }
    | TVar({contents: Free(id, _)}) => [%string "_%{string_of_int(id)}"]
    | TVar({contents: Constrained(typ)}) => f(is_simple, typ)
    };
  
  f(false, typ);
}
