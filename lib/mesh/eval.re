open Rresult;

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
    | ELit(lit)           => value_of_lit(lit)
    | EVar(path, varname) => value_of_var(env, path, varname, expr.pexpr_loc)
    | EList(l)            => VList(List.map(eval_value(env), l))
    | ETuple(t)           => VTuple(List.map(eval_value(env), t))
    | EApp(e_fun, e_arg)  => 
      switch (eval_value(env, e_fun), eval_value(env, e_arg)) {
      | (VClosure(env', EFun(pat, e)), argv)  => eval_value(bind_pat_value(pat, argv) @ env', e)
      | _                                     => raise(Runtime_error("EApp: LHS is not a function!"))
      };
    | EFun(_, _) as e     => VClosure(env, e)
    | ELet(_)             => raise(Runtime_error("Unexpected ELet in eval_nonlet"))
    | EMod(_)             => raise(Runtime_error("Unexpected EMod in eval_nonlet"))
    | EOpen(_)            => raise(Runtime_error("Unexpected EOpen in eval_nonlet"))
    | ESeq(e, rest)       => 
      switch (e) {
      | {pexpr_desc: ELet(pat, e), _} => 
        eval_value(env, e)              |> (value) =>
        bind_pat_value(pat, value) @ env  |> (env') =>
        eval_value(env', rest)
      | e => 
        eval_value(env, e) |> (_) => eval_value(env, rest)
      }
    | EPrim(prim) => eval_prim(env, prim)
    | ERecSelect(e, name) =>
      switch (eval_value(env, e)) {
      | VRecord(fields) => List.assoc(name, fields)
      | _ => raise(Runtime_error("ERecSelect: base is not a record"))
      }
    | ERecExtend(name, e, base) =>
      switch (eval_value(env, base)) {
      | VRecord(fields) => VRecord(rm_record_duplicates @@ [(name, eval_value(env, e)), ...fields])
      | _ => raise(Runtime_error("ERecExtend: base is not a record"))
      }
    | ERecEmpty => VRecord([])
    }
  and eval_prim = (env, prim) =>
    switch (prim) {
    // Int primitive functions
    | PIntAdd(e1, e2)     => switch (eval_value(env, e1), eval_value(env, e2)) { | (VInt(a), VInt(b)) => VInt(a + b) | _ => raise(Runtime_error([%string "PIntAdd: Unexpected types"]))}
    | PIntSub(e1, e2)     => switch (eval_value(env, e1), eval_value(env, e2)) { | (VInt(a), VInt(b)) => VInt(a - b) | _ => raise(Runtime_error([%string "PIntSub: Unexpected types"]))}
    | PIntMul(e1, e2)     => switch (eval_value(env, e1), eval_value(env, e2)) { | (VInt(a), VInt(b)) => VInt(a * b) | _ => raise(Runtime_error([%string "PIntMul: Unexpected types"]))}
    | PIntDiv(e1, e2)     => switch (eval_value(env, e1), eval_value(env, e2)) { | (VInt(a), VInt(b)) => VInt(a / b) | _ => raise(Runtime_error([%string "PIntDiv: Unexpected types"]))}
    | PIntNeg(e)          => switch (eval_value(env, e)) { | VInt(a) => VInt(-a) | _ => raise(Runtime_error([%string "PIntNeg: Unexpected types"])) }
    // Float primitive functions
    | PFloatAdd(e1, e2)   => switch (eval_value(env, e1), eval_value(env, e2)) { | (VFloat(a), VFloat(b)) => VFloat(a +. b) | _ => raise(Runtime_error([%string "PFloatAdd: Unexpected types"]))}
    | PFloatSub(e1, e2)   => switch (eval_value(env, e1), eval_value(env, e2)) { | (VFloat(a), VFloat(b)) => VFloat(a -. b) | _ => raise(Runtime_error([%string "PFloatSub: Unexpected types"]))}
    | PFloatMul(e1, e2)   => switch (eval_value(env, e1), eval_value(env, e2)) { | (VFloat(a), VFloat(b)) => VFloat(a *. b) | _ => raise(Runtime_error([%string "PFloatMul: Unexpected types"]))}
    | PFloatDiv(e1, e2)   => switch (eval_value(env, e1), eval_value(env, e2)) { | (VFloat(a), VFloat(b)) => VFloat(a /. b) | _ => raise(Runtime_error([%string "PFloatDiv: Unexpected types"]))}
    | PFloatNeg(e)        => switch (eval_value(env, e)) { | VFloat(a) => VFloat(-.a) | _ => raise(Runtime_error([%string "PIntNeg: Unexpected types"])) }
    // List primitive functions
    | PListCons(e1, e2)   => switch (eval_value(env, e1), eval_value(env, e2)) { | (v, VList(l)) => VList([v, ...l]) | _ => raise(Runtime_error([%string "PListCons: Unexpected types"]))}
    | PListMap(e1, e2)    => 
      switch (eval_value(env, e1), eval_value(env, e2)) {
      | (VClosure(env', EFun(pat, e)), VList(l)) => VList(List.map((argv) => eval_value(bind_pat_value(pat, argv) @ env', e), l))
      | _ => raise(Runtime_error([%string "PListMap: Unexpected types"]))
      }
    | PListMapi(e1, e2)   => 
      switch (eval_value(env, e1), eval_value(env, e2)) {
      | (VClosure(env', EFun(pat, e)), VList(l)) => 
        VList(List.mapi((i, argv) => 
          switch (eval_value(bind_pat_value(pat, VInt(i)) @ env', e)) {
          | VClosure(env', EFun(pat, e)) => eval_value(bind_pat_value(pat, argv) @ env', e)
          | _ => raise(Runtime_error([%string "PListMapi: Unexpected types"]))
          },
          l)
        )
      | _ => raise(Runtime_error([%string "PListMapi: Unexpected types"]))
      }
    // | PListFoldl(e1, e2, e3)  => 
    //   switch (eval_non_let(env, e1), eval_non_let(env, e2), eval_non_let(env, e3)) {
    //   | (VClosure(env', EFun(pat1, EFun(pat2, body))), v, VList(l)) => VList(List.fold_left((acc, argv) => eval_non_let(bind_pat_value(pat1, acc) @ bind_pat_value(pat2, argv) @ env', body), v, l))
    //   | _ => raise(Runtime_error([%string "PListMap: Unexpected types"]))
    //   }  
    // | PListFoldr(e1, e2, e3)  => 
    //   switch (eval_non_let(env, e1), eval_non_let(env, e2), eval_non_let(env, e3)) { 
    //   | (VClosure(env', EFun(pat, e)), VList(l)) => VList(List.map((argv) => eval_non_let(bind_pat_value(pat, argv) @ env', e), l))
    //   | _ => raise(Runtime_error([%string "PListMap: Unexpected types"]))
    //   }  
    // GraphQL primitive functions
    // TODO: Graphql query execution
    // | PGraphqlExec(e1, e2) => switch (eval_non_let(env, e1), eval_non_let(env, e2)) { | (VString(a), VGraphqlQuery(b)) =>  | _ => raise(Runtime_error([%string "PFloatDiv: Unexpected types"]))}
    };

  let eval_env = (env, expr) =>
    switch (expr.pexpr_desc) {
    | ELet(pat, e) => 
      eval_value(env, e)              |> (value) =>
      bind_pat_value(pat, value) @ env
    | EMod(name, body)    => 
      eval_exn([], env, body) |> ((_, mod_env)) =>
      [(name, VMod(mod_env)), ...env]
    | EOpen(path, modname) =>
      switch (value_of_var(env, path, modname, expr.pexpr_loc)) {
      | VMod(env') => env' @ env
      | _ => raise(Runtime_error([%string "open %{modname}: %{modname} is not a module!"]))
      }
    | _            => raise(Runtime_error("Unexpected expr in eval_env"))
    };

  switch (e) {
  | [{pexpr_desc: (ELet(_) | EMod(_) | EOpen(_)), _} as e, ...rest] => 
    eval_env(env, e)     |> (env') => eval_exn(ret, env', rest)
  | [e, ...rest]            => eval_value(env, e) |> (value) => eval_exn([value, ...ret], env, rest)
  | []                      => (List.rev(ret), env)
  };
};

let eval = (~env=[], e) => 
  try (R.ok @@ eval_exn([], env, e)) {
  | Runtime_error(msg) => R.error_msg(msg)
  };