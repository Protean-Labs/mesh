open Syntax;

// ================================================================
// Literals helpers
// ================================================================
let mk_lit_int    = (v) => Int(v);
let mk_lit_float  = (v) => Float(v);
let mk_lit_string = (v) => String(v);
let mk_lit_bool   = (v) => Bool(v);
let mk_lit_unit   = () => Unit;

let string_of_literal = fun
  | Int(v)    => string_of_int(v)
  | Float(v)  => string_of_float(v)
  | String(v) => v
  | Bool(v)   => string_of_bool(v)
  | Unit      => "()"
;

// ================================================================
// Expression helpers
// ================================================================

let mk_expr = (~loc=Location.none, pexpr_desc) => {pexpr_desc, pexpr_loc: loc}; 

let mk_elit = (~loc=Location.none, lit) => mk_expr(~loc, ELit(lit));
let mk_evar = (~loc=Location.none, ~path=[], varname) => mk_expr(~loc, EVar(path, varname));

let mk_elit_int    = (~loc=Location.none, v) => mk_expr(~loc, ELit(Int(v)));
let mk_elit_float  = (~loc=Location.none, v) => mk_expr(~loc, ELit(Float(v)));
let mk_elit_string = (~loc=Location.none, v) => mk_expr(~loc, ELit(String(v)));
let mk_elit_bool   = (~loc=Location.none, v) => mk_expr(~loc, ELit(Bool(v)));
let mk_elit_unit   = (~loc=Location.none, ()) => mk_expr(~loc, ELit(Unit));


let rec string_repeat = (s,n) => n == 0 ? "" : s ++ string_repeat(s, n-1);
let indent = string_repeat("  ");
let maybe_print_loc = (print_loc, expr) => print_loc ? Format.asprintf("%a", Location.print_loc, expr.pexpr_loc) : "";

let rec string_of_pattern = (pat) =>
  switch (pat.ppat_desc) {
  | PAny          => "PAny"
  | PVar(name)    => [%string "PVar %{name}"]
  | PLit(lit)     => [%string "PLit %{string_of_literal lit}"]
  | PTuple(pats)  => 
    List.map(string_of_pattern, pats) |> String.concat(", ") |> (inner) =>
    [%string "(PTuple %{inner})"]
  };

let rec string_of_expr = (~level=0, ~print_loc=false, expr) => {
  let indent = indent(level);
  switch (expr.pexpr_desc) {
  | ELit(lit)           => [%string "%{indent}(ELit %{maybe_print_loc print_loc expr} %{string_of_literal lit})"]
  | EVar([], name)      => [%string "%{indent}(EVar %{maybe_print_loc print_loc expr} %{name})"]
  | EVar(path, name)    => 
    String.concat(".", path)  |> (path) =>
    [%string "%{indent}(EVar %{maybe_print_loc print_loc expr} %{path}.%{name})"]
  | EList([])           => [%string "%{indent}(EList %{maybe_print_loc print_loc expr} [])"]
  | EList(l)            => 
    List.map((ele) => string_of_expr(~level=level + 1, ~print_loc, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(EList %{maybe_print_loc print_loc expr} \n%{elements})"]
  | ETuple([])          => [%string "%{indent}(ETuple %{maybe_print_loc print_loc expr} ())"] 
  | ETuple(l)           => 
    List.map((ele) => string_of_expr(~level=level + 1, ~print_loc, ele), l) |> String.concat("\n") |> (elements) =>
    [%string "%{indent}(ETuple %{maybe_print_loc print_loc expr} \n%{elements})"]
  | EApp(e1, e2)        => [%string "%{indent}(EApp %{maybe_print_loc print_loc expr} %{string_of_expr ~print_loc e1}\n%{string_of_expr ~level:(level + 1) ~print_loc e2})"]
  | EFun(pat, e)        => [%string "%{indent}(EFun %{maybe_print_loc print_loc expr} %{string_of_pattern pat} =>\n%{string_of_expr ~level:(level + 1) ~print_loc e})"]
  | ELet(pat, e)        => [%string "%{indent}(ELet %{maybe_print_loc print_loc expr} %{string_of_pattern pat} =\n%{string_of_expr ~level:(level + 1) ~print_loc e})"]
  | ERecSelect(e, name)       => [%string "%{indent}(ERecSelect %{string_of_expr ~print_loc e} %{name})"]
  | ERecExtend(name, e1, e2)  => [%string "%{indent}(ERecExtend %{name}\n%{string_of_expr ~level:(level + 1) ~print_loc e1}\n%{string_of_expr ~level:(level + 1) ~print_loc e2})"]
  | ERecEmpty                 => [%string "%{indent}ERecEmpty"]  | ESeq(e, rest)       => [%string "%{indent}(ESeq %{maybe_print_loc print_loc expr} \n%{string_of_expr ~level:(level + 1) ~print_loc e}\n%{string_of_expr ~level:(level + 1) ~print_loc rest})"]
  | EMod(name, body)    => 
    List.map((ele) => string_of_expr(~level=level + 1, ~print_loc, ele), body) |> String.concat("\n") |> (elements) =>    
    [%string "%{indent}(EMod %{maybe_print_loc print_loc expr} %{name}\n%{elements})"]
  | EOpen([], name)      => [%string "%{indent}(EOpen %{maybe_print_loc print_loc expr} %{name})"]
  | EOpen(path, name)    => 
    String.concat(".", path)  |> (path) =>
    [%string "%{indent}(EOpen %{maybe_print_loc print_loc expr} %{path}.%{name})"]
  | EPrim(prim)         => string_of_primitive(~level, prim)
  }
}
and string_of_primitive = (~level=0, prim) =>
  indent(level)  |> (indent) =>
  switch (prim) {
  // Int primitive functions
  | PIntAdd(e1, e2)     => [%string "%{indent}(int_add\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PIntSub(e1, e2)     => [%string "%{indent}(int_sub\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PIntMul(e1, e2)     => [%string "%{indent}(int_mul\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PIntDiv(e1, e2)     => [%string "%{indent}(int_div\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PIntNeg(e)          => [%string "%{indent}(int_neg\n%{string_of_expr ~level:(level + 1) e})"]
  // Float primitive functions
  | PFloatAdd(e1, e2)   => [%string "%{indent}(float_add\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PFloatSub(e1, e2)   => [%string "%{indent}(float_sub\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PFloatMul(e1, e2)   => [%string "%{indent}(float_mul\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PFloatDiv(e1, e2)   => [%string "%{indent}(float_div\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PFloatNeg(e)        => [%string "%{indent}(float_neg\n%{string_of_expr ~level:(level + 1) e}"]
  // List primitive functions
  | PListCons(e1, e2)   => [%string "%{indent}(list_cons\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PListMap(e1, e2)    => [%string "%{indent}(list_map\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PListMapi(e1, e2)   => [%string "%{indent}(list_mapi\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  | PListFoldl(e1, e2, e3)  => [%string "%{indent}(list_foldl\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}\n%{string_of_expr ~level:(level + 1) e3}"]
  | PListFoldr(e1, e2, e3)  => [%string "%{indent}(list_foldr\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}\n%{string_of_expr ~level:(level + 1) e3}"]
  // GraphQL primitive functions
  // | PGraphqlExec(e1, e2) => [%string "%{indent}(graphql_exec\n%{string_of_expr ~level:(level + 1) e1}\n%{string_of_expr ~level:(level + 1) e2}"]
  };

// ================================================================
// Pattern helpers
// ================================================================

let mk_pat = (~loc=Location.none, ppat_desc) => {ppat_desc, ppat_loc: loc}; 

let mk_pany   = (~loc=Location.none, ()) => mk_pat(~loc, PAny);
let mk_pvar   = (~loc=Location.none, varname) => mk_pat(~loc, PVar(varname));
let mk_plit   = (~loc=Location.none, lit) => mk_pat(~loc, PLit(lit));
let mk_ptuple = (~loc=Location.none, pats) => mk_pat(~loc, PTuple(pats));



