open Rresult;

open Syntax;

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
  | VClosure(environment, expr)
;

let value_of_lit = fun
  | Int(v)    => VInt(v)
  | Float(v)  => VFloat(v)
  | String(v) => VString(v)
  | Bool(v)   => VBool(v)
  | Unit      => VUnit
;

let rec string_of_value = fun
  | VInt(v)       => string_of_int(v)
  | VFloat(v)     => string_of_float(v)
  | VString(v)    => v
  | VBool(v)      => string_of_bool(v)
  | VUnit         => "()"
  | VList([])     => "[]"
  | VList(l)      => 
    List.map((ele) => string_of_value(ele), l) |> String.concat(", ") |> (elements) =>
    [%string "[%{elements}]"]
  | VTuple([])    => "()"
  | VTuple(l)     => 
    List.map((ele) => string_of_value(ele), l) |> String.concat(", ") |> (elements) =>
    [%string "(%{elements})"]
  | VClosure(_)   => "closure"
;

/** [bind_pat_value(pat, v)] returns a list of tuples of type [(name, value)] containing 
    the bindings to be added to the environment where each variable in the {pattern} [pat] 
    is binded to a value in the {value} [v] when possible. When [v]'s structure does not 
    match the pattern [pat], a {Runtime_error} exception is raised.
    
    E.g.: Given the mesh expression [let (a, b) = (0, 1);], the LHS of the expression 
    is the {pattern} [pat = PTuple([PVar("a"), PVar("b")])] and the RHS of the expression is
    the {value} [v = VTuple([VInt(0), VInt(1)])]. Calling [bind_pat_value(pat, v)] would return
    [[("a", VInt(0)), ("b", VInt(1))]]. */
let rec bind_pat_value = (pat, v) =>
  switch (pat, v) {
  | (PAny, _)                       => []
  | (PVar(name), v)                 => [(name, v)]
  | (PLit(lit), v)                  => 
    value_of_lit(lit) == v ? [] : raise(Runtime_error([%string "argument does not match literal! %{string_of_value v} != %{string_of_literal lit}"]))
  | (PTuple(t_pat), VTuple(t_val))  => 
    List.fold_left2((acc, pat, v) => bind_pat_value(pat, v) @ acc, [], t_pat, t_val)
  | (_, _)                          => raise(Runtime_error(""))
  };

let rec eval_exn = (ret: list(value), env, e: list(expr)) => {
  let rec eval_non_let = (env, e) => 
    switch (e) {
    | ELit(lit)           => value_of_lit(lit)
    | EVar(varname)       => List.assoc(varname, env)
    | EList(l)            => VList(List.map(eval_non_let(env), l))
    | ETuple(t)           => VTuple(List.map(eval_non_let(env), t))
    | EApp(e_fun, e_arg)  => 
      switch (eval_non_let(env, e_fun), eval_non_let(env, e_arg)) {
      | (VClosure(env', EFun(pat, e)), argv)  => eval_non_let(bind_pat_value(pat, argv) @ env', e)
      | _                                     => raise(Runtime_error("EApp: LHS is not a function!"))
      };
    | EFun(_, _) as e     => VClosure(env, e)
    | ELet(_)             => raise(Runtime_error("Unexpected ELet in eval_nonlet"))
    | ESeq(e, rest)       => 
      switch (e) {
      | ELet(pat, e) => 
        eval_non_let(env, e)              |> (value) =>
        bind_pat_value(pat, value) @ env  |> (env') =>
        eval_non_let(env', rest)
      | e => 
        eval_non_let(env, e) |> (_) => eval_non_let(env, rest)
      }
    };

  let eval_let = (env, e) =>
    switch (e) {
    | ELet(pat, e) => 
      eval_non_let(env, e)              |> (value) =>
      bind_pat_value(pat, value) @ env
    | _            => raise(Runtime_error("Unexpected expr in eval_let"))     
    };

  switch (e) {
  | [ELet(_) as e, ...rest] => eval_let(env, e)     |> (env') => eval_exn(ret, env', rest)
  | [e, ...rest]            => eval_non_let(env, e) |> (value) => eval_exn([value, ...ret], env, rest)
  | []                      => List.rev(ret)
  };
};

let eval = (e) => 
  try (R.ok @@ eval_exn([], [], e)) {
  | Runtime_error(msg) => R.error_msg(msg)
  };