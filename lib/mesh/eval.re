open Rresult;
open Lwt.Infix;

open Syntax;
open Syntax_util;

exception Runtime_error(string);

type environment = list((name, value))
and value = 
  | VInt(int)
  | VFloat(float)
  | VString(string)
  | VBool(bool)
  | VUnit
  | VList(list(value))
  | VTuple(list(value))
  | VClosure(environment, expr_desc)
  | VMod(environment)
  | VRecord(list((name, value)))
  | VOpt(option(value))
;

let value_of_lit = fun
  | Int(v)    => VInt(v)
  | Float(v)  => VFloat(v)
  | String(v) => VString(v)
  | Bool(v)   => VBool(v)
  | Unit      => VUnit
;

let rec string_of_value = (~level=0, value) => {
  let indent = indent(level);
  switch (value) {
  | VInt(v)       => [%string "%{indent}%{string_of_int v}"]
  | VFloat(v)     => [%string "%{indent}%{string_of_float v}"]
  | VString(v)    => [%string "%{indent}%{v}"]
  | VBool(v)      => [%string "%{indent}%{string_of_bool v}"]
  | VUnit         => [%string "%{indent}()"]
  | VList([])     => [%string "%{indent}[]"]
  | VList(l)      => 
    let elements = List.map((ele) => string_of_value(~level=level+1, ele), l) |> String.concat(",\n");
    [%string "%{indent}[\n%{elements}]"]
  | VTuple([])    => "()"
  | VTuple(l)     => 
    let elements = List.map((ele) => string_of_value(ele), l) |> String.concat(",\n");
    [%string "%{indent}(\n%{elements})"]
  | VClosure(_)   => [%string "%{indent}closure"]
  | VMod(env)  => 
    let elements = List.map(((name, ele)) => [%string "%{indent}%{name}: %{string_of_value ele}"], env) |> String.concat("\n");
    [%string "%{indent}{\n%{elements}\n}"]
  | VRecord(fields) =>
    let fields = List.map(((name, ele)) => [%string "%{indent}\"%{name}\": %{string_of_value ele}"], fields) |> String.concat(",\n");
    [%string "%{indent}{\n%{fields}\n}"];
  | VOpt(maybe_value) =>
    switch (maybe_value) {
    | Some(value) =>  [%string "%{indent}Some(%{string_of_value value})"]
    | None =>         [%string "%{indent}None"]
    }
  };
};

let value_of_var = (env, path, name, loc) => {
  // Attempt to find module namespace based on [path]
  let mod_ns = 
    List.fold_left((acc, modname) => 
      switch (List.assoc_opt(modname, acc)) {
      | Some(VMod(env)) => env
      | Some(_)         => raise(Runtime_error([%string "%{modname} is not a module!"]))
      | None            => raise(Runtime_error([%string "Unbound module %{modname}"]))
      }
    , env, path);

  // Attempt to find [name] in module namespace
  switch (List.assoc_opt(name, mod_ns)) {
  | Some(v) => v
  | None    => raise(Runtime_error([%string "Unbound value %{string_of_expr ~print_loc:true (mk_expr ~loc (EVar(path, name)))}"]))
  }; 
};

let rec value_of_yojson = (json) => 
  switch (json) {
  | `Null         => VUnit
  | `Bool(v)      => VBool(v)
  | `Int(v)       => VInt(v)
  | `Intlit(v)    => VInt(int_of_string(v))
  | `Float(v)     => VFloat(v)
  | `Floatlit(v)  => VFloat(float_of_string(v))
  | `String(v)    => VString(v)
  | `Stringlit(v) => VString(v)
  | `List(l)      => VList(List.map(value_of_yojson, l))
  | `Tuple(l)     => VTuple(List.map(value_of_yojson, l))
  | `Assoc(l)     => VRecord(List.map(((name, v)) => (name, value_of_yojson(v)), l))
  };

let rec value_to_yojson = (record) =>
  switch (record) {
  | VInt(v)       => `Int(v)
  | VFloat(v)     => `Float(v)
  | VString(v)    => `String(v)
  | VBool(v)      => `Bool(v)
  | VUnit         => `Null
  | VList(l)      => `List(List.map(value_to_yojson, l))
  | VTuple(l)     => `Tuple(List.map(value_to_yojson, l))
  | VClosure(_)   => `String("closure")
  | VMod(_)       => `String("module")
  | VRecord(l)    => `Assoc(List.map(((name, v)) => (name, value_to_yojson(v)), l))
  | VOpt(v)       => 
    switch (v) {
    | Some(v) => value_to_yojson(v)
    | None    => `Null
    }
  };

let eval_graphql = (uri, query) =>{
  let uri = Uri.of_string(uri);

  Data_source.Graphql.Client.query(uri, query) >|= (data) =>
  switch (R.map(value_of_yojson, data)) {
  | Error(`Msg(msg)) => raise(Runtime_error(msg))
  | Ok(r) => r
  };
};

let rm_record_duplicates = (l) =>
  List.rev @@ List.fold_left((acc, (name, _) as ele) => List.assoc_opt(name, acc) == None ? [ele, ...acc] : acc, [], l);

/** [bind_pat_value(pat, v)] returns a list of tuples of type [(name, value)] containing 
    the bindings to be added to the environment where each variable in the {pattern} [pat] 
    is binded to a value in the {value} [v] when possible. When [v]'s structure does not 
    match the pattern [pat], a {Runtime_error} exception is raised.
    
    E.g.: Given the mesh expression [let (a, b) = (0, 1);], the LHS of the expression 
    is the {pattern} [pat = PTuple([PVar("a"), PVar("b")])] and the RHS of the expression is
    the {value} [v = VTuple([VInt(0), VInt(1)])]. Calling [bind_pat_value(pat, v)] would return
    [[("a", VInt(0)), ("b", VInt(1))]]. */
let rec bind_pat_value = (pat, v) =>
  switch (pat.ppat_desc, v) {
  | (PAny, _)                       => []
  | (PVar(name), v)                 => [(name, v)]
  | (PLit(lit), v)                  => 
    value_of_lit(lit) == v ? [] : raise(Runtime_error([%string "argument does not match literal! %{string_of_value v} != %{string_of_literal lit}"]))
  | (PTuple(t_pat), VTuple(t_val))  => 
    List.fold_left2((acc, pat, v) => bind_pat_value(pat, v) @ acc, [], t_pat, t_val)
  | (_, _)                          => raise(Runtime_error(""))
  };

/** [eval_exn(ret, env, e)] 

  [eval_exn] is made up of two subfunctions: [eval_value] and [eval_env].
  [ELet] {expr} values update the environment and evaluate to [unit]. All the 
  other {expr} values evaluate to some value and do not change the environment.
  Although it would be possible to have only one recursive function, breaking it 
  into two makes it more manageable in terms of return values (i.e.: [eval_value]
  returns a value whereas [eval_env] returns an updated environment).

  TODO: Refactor into single elegant function.
*/
let rec eval_exn = (ret: list(value), env, e: list(expr)) => {
  let rec eval_value = (env, expr) => 
    switch (expr.pexpr_desc) {
    | ELit(lit)           => Lwt.return @@ value_of_lit(lit)
    | EVar(path, varname) => Lwt.return @@ value_of_var(env, path, varname, expr.pexpr_loc)
    | EList(l)            => Lwt_list.map_s(eval_value(env), l) >|= (l) => VList(l)
    | ETuple(t)           => Lwt_list.map_s(eval_value(env), t) >|= (t) => VTuple(t)
    | EApp(e_fun, e_arg)  => 
      Lwt.both(eval_value(env, e_fun), eval_value(env, e_arg)) >>= (e) =>
      switch (e) {
      | (VClosure(env', EFun(pat, e)), argv)  => eval_value(bind_pat_value(pat, argv) @ env', e)
      | _                                     => raise(Runtime_error("EApp: LHS is not a function!"))
      };
    | EFun(_, _) as e     => Lwt.return @@ VClosure(env, e)
    | ELet(_)             => raise(Runtime_error("Unexpected ELet in eval_value"))
    | EMod(_)             => raise(Runtime_error("Unexpected EMod in eval_value"))
    | EOpen(_)            => raise(Runtime_error("Unexpected EOpen in eval_value"))
    | ESeq(e, rest)       => 
      switch (e) {
      | {pexpr_desc: ELet(pat, e), _} => 
        eval_value(env, e)              >>= (value) =>
        bind_pat_value(pat, value) @ env  |> (env') =>
        eval_value(env', rest)
      | e => 
        eval_value(env, e) |> (_) => eval_value(env, rest)
      }
    | EPrim(prim) => eval_prim(env, prim)
    | ERecSelect(e, name) =>
      eval_value(env, e) >>= (value) =>
      switch (value) {
      | VRecord(fields) => Lwt.return @@ List.assoc(name, fields)
      | _ => raise(Runtime_error("ERecSelect: base is not a record"))
      }
    | ERecExtend(name, e, base) =>
      eval_value(env, base) >>= (basev) =>
      switch (basev) {
      | VRecord(fields) => 
        eval_value(env, e) >|= (value) =>
        VRecord(rm_record_duplicates @@ [(name, value), ...fields])
      | _ => raise(Runtime_error("ERecExtend: base is not a record"))
      }
    | ERecEmpty => Lwt.return @@ VRecord([])
    | EOpt(maybe_e) => 
      switch (maybe_e) {
      | Some(e) => eval_value(env, e) >|= (v) => VOpt(Some(v))
      | None => Lwt.return @@ VOpt(None)
      }
    | _ => raise(Runtime_error("eval not implemented"))
    }
  and eval_prim = (env, prim) =>
    switch (prim) {
    | PListCons(e1, e2)   => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (v, VList(l)) => Lwt.return @@ VList([v, ...l]) | _ => raise(Runtime_error([%string "PListCons: Unexpected types"]))}
    | PIntAdd(e1, e2)     => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VInt(a), VInt(b)) => Lwt.return @@ VInt(a + b) | _ => raise(Runtime_error([%string "PIntAdd: Unexpected types"]))}
    | PIntSub(e1, e2)     => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VInt(a), VInt(b)) => Lwt.return @@ VInt(a - b) | _ => raise(Runtime_error([%string "PIntSub: Unexpected types"]))}
    | PIntMul(e1, e2)     => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VInt(a), VInt(b)) => Lwt.return @@ VInt(a * b) | _ => raise(Runtime_error([%string "PIntMul: Unexpected types"]))}
    | PIntDiv(e1, e2)     => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VInt(a), VInt(b)) => Lwt.return @@ VInt(a / b) | _ => raise(Runtime_error([%string "PIntDiv: Unexpected types"]))}
    | PFloatAdd(e1, e2)   => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VFloat(a), VFloat(b)) => Lwt.return @@ VFloat(a +. b) | _ => raise(Runtime_error([%string "PFloatAdd: Unexpected types"]))}
    | PFloatSub(e1, e2)   => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VFloat(a), VFloat(b)) => Lwt.return @@ VFloat(a -. b) | _ => raise(Runtime_error([%string "PFloatSub: Unexpected types"]))}
    | PFloatMul(e1, e2)   => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VFloat(a), VFloat(b)) => Lwt.return @@ VFloat(a *. b) | _ => raise(Runtime_error([%string "PFloatMul: Unexpected types"]))}
    | PFloatDiv(e1, e2)   => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VFloat(a), VFloat(b)) => Lwt.return @@ VFloat(a /. b) | _ => raise(Runtime_error([%string "PFloatDiv: Unexpected types"]))}
    | PGraphQL(e1, e2)    => Lwt.both(eval_value(env, e1), eval_value(env, e2)) >>= (e) => switch (e) { | (VString(uri), VString(query)) => eval_graphql(uri, query) | _ => raise(Runtime_error([%string "PFloatDiv: Unexpected types"]))}
    };

  let eval_env = (env, expr) =>
    switch (expr.pexpr_desc) {
    | ELet(pat, e) => 
      eval_value(env, e)              >|= (value) =>
      bind_pat_value(pat, value) @ env
    | EMod(name, body)    => 
      eval_exn([], env, body) >|= ((_, mod_env)) =>
      [(name, VMod(mod_env)), ...env]
    | EOpen(path, modname) =>
      switch (value_of_var(env, path, modname, expr.pexpr_loc)) {
      | VMod(env') => Lwt.return @@  env' @ env
      | _ => raise(Runtime_error([%string "open %{modname}: %{modname} is not a module!"]))
      }
    | _            => raise(Runtime_error("Unexpected expr in eval_env"))
    };

  switch (e) {
  | [{pexpr_desc: (ELet(_) | EMod(_) | EOpen(_)), _} as e, ...rest] => 
    eval_env(env, e)     >>= (env') => eval_exn(ret, env', rest)
  | [e, ...rest]                              => eval_value(env, e)   >>= (value) => eval_exn([value, ...ret], env, rest)
  | []                                        => Lwt.return @@ (List.rev(ret), env)
  };
};

let eval = (e) => 
  try (eval_exn([], [], e) >|= R.ok) {
  | Runtime_error(msg) => Lwt.return @@ R.error_msg(msg)
  };