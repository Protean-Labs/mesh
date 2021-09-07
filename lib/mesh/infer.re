open Syntax;
open Rresult;

exception TypeError(string);


let current_id = ref(0);
let next_id = () => {
  let id = current_id^;
  incr(current_id);
  id;
};

let reset_id = () => current_id := 0;

let new_var = (lvl) => TVar(ref(Free(next_id(), lvl)));
let new_quantified_var = () => TVar(ref(Quantified(next_id())));

module Env = {
  module StringMap = Map.Make(String);
  type env = StringMap.t(typ);

  let empty: env = StringMap.empty;
  let extend = (env, name, typ) => StringMap.add(name, typ, env);
  let lookup = (env, name) => StringMap.find(name, env);
};

// (other_id == tvar_id)? raise(TypeError("recursive types")):(other_level > tvar_level)? other_tvar := Free(other_id, tvar_level): ();
let occurs_check_adjust_levels = (tvar_id, tvar_level, typ) => {
  let rec f = fun
    | TVar({contents: Constrained(typ)}) => f(typ)
    | TVar({contents: Quantified(_)}) => raise(TypeError("Cant check quantified variable"))
    | TVar({contents: Free(other_id, other_level)} as other_tvar) => {
      (other_id == tvar_id)? 
      raise(TypeError("Recursive types")): 
      (other_level > tvar_level)? 
            other_tvar := Free(other_id, tvar_level):
            ();
    }
    | TApp(fun_typ, param_typ) => {f(fun_typ); f(param_typ)}
    | TFun(param_typ, return_typ) => {f(param_typ); f(return_typ)}
    | TConst(_) => ()
    | _ => raise(TypeError("Not implemented"))
  ;
  f(typ);
};

let rec unify = (typ1, typ2) => {
  typ1 == typ2 ? () :
  switch (typ1, typ2) {
    | (TConst(name1), TConst(name2)) when name1 == name2 => ()
    | (TApp(fun_typ1, param_typ1), TApp(fun_typ2, param_typ2)) => {
      unify(fun_typ1, fun_typ2); unify(param_typ1, param_typ2);
    };
    | (TFun(param_typ1, return_typ1), TFun(param_typ2, return_typ2)) => {
      unify(param_typ1, param_typ2); unify(return_typ1, return_typ2);
    };
    | (TVar({contents: Constrained(typ1)}), typ2) => unify(typ1, typ2)
    | (typ1, TVar({contents: Constrained(typ2)})) => unify(typ1, typ2)
    | (TVar({contents: Free(id1, _)}), TVar({contents: Free(id2, _)})) when id1 == id2 =>
      raise(TypeError("Can only have one instance of a type variable"))
    | (TVar({contents: Free(id, level)} as tvar), typ2) => {
      occurs_check_adjust_levels(id, level, typ2);
      tvar := Constrained(typ2);
    }
    | (typ1, TVar({contents: Free(id, level)} as tvar)) => {
      occurs_check_adjust_levels(id, level, typ1);
      tvar := Constrained(typ1);
    } 
    | (_, _) => raise(TypeError([%string "Cannot unify %{string_of_typ(typ1)} and %{string_of_typ(typ2)}"]))
  }
};

let rec generalize = (level) => fun
  | TVar({contents: Free(id, other_level)}) when other_level > level => 
    TVar(ref(Quantified(id)))
  | TApp(fun_typ, param_typ) => 
    TApp(generalize(level, fun_typ), generalize(level, param_typ))
  | TFun(param_typ, return_typ) => 
    TFun(generalize(level, param_typ), generalize(level, return_typ))
  | TVar({contents: Constrained(typ)}) => generalize(level, typ)
  | TVar({contents: Quantified(_)}) as typ => typ
  | TVar({contents: Free(_)}) as typ => typ
  | TConst(_) as typ => typ
  | _ => raise(TypeError("not implemented"))
;

let instantiate = (level, typ) => {
  let id_var_map = Hashtbl.create(10);
  let rec f = (typ) => {
    switch(typ) {
      | TConst(_) => typ
      | TVar({contents: Constrained(typ)}) => f(typ)
      | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_var_map, id)){
          | Not_found => {
            let var = new_var(level);
            Hashtbl.add(id_var_map, id, var);
            var;
            };
          };
      };
      | TVar({contents: Free(_)}) => typ
      | TApp(fun_typ, param_typ) => TApp(f(fun_typ), f(param_typ))
      | TFun(param_typ, return_typ) => TFun(f(param_typ), f(return_typ))
      | _ => raise(TypeError("not implemented"))
    };
  };
  f(typ);
};

let rec match_fun_typ = fun
  | TFun(param_typ, return_typ)              => (param_typ, return_typ)
  | TVar({contents: Constrained(typ)})       => match_fun_typ(typ)
  | TVar({contents: Free(_, level)} as tvar) => {
    let param_typ = new_var(level);
    let return_typ = new_var(level);
    tvar := Constrained(TFun(param_typ, return_typ));
    (param_typ, return_typ);
  }
  | _ => raise(TypeError("Expected a function"))
;


let type_const_of_literal = fun 
  | Int(_) => TConst("int")
  | Float(_) => TConst("float")
  | String(_) => TConst("string")
  | Bool(_) => TConst("bool")
  | Unit => TConst("unit")
; 

let infer_exn = (env, level, exprs) => {
  let get_typ = (x => List.nth(x,0));
  let rec f = (env, level, typs) => fun
  | [ELit(lit), ...rest] => [type_const_of_literal(lit), ...typs] |> f(env, level, _, rest)
  | [EVar(name), ...rest] => {
      let typ = try (instantiate(level, Env.lookup(env, name))) {
        | Not_found => raise(TypeError([%string "variable %{name}"]))
      }
      f(env, level, [typ, ...typs], rest)
    }
  | [EFun(param_pat, body_expr), ...rest] => {
      let (param_name,param_typ) = param_pat |> fun 
        | PVar(name) => (name, new_var(level)) 
        | PLit(l)    => ("", type_const_of_literal(l))
        | PAny       => ("", new_var(level))
        | PTuple(_)  => raise(TypeError("Tuple pattern as arg"))
      ;
      let fn_env = Env.extend(env, param_name, param_typ);
      let return_typ = f(fn_env, level,[] ,[body_expr]) |> get_typ;
      f(env, level, [TFun(param_typ, return_typ), ...typs], rest);
    }
  // let expression scope?
  // match pattern binding to value expr type
  // | ELet(pattern, expr) => {
  //     let var_names = pattern |> var_names_of_pattern;

  //   }
  | [EApp(fn_expr, param_expr), ...rest] => {
      let (param_typ, return_typ) = 
        match_fun_typ(f(env, level, [], [fn_expr]) |> get_typ);
      unify(param_typ, f(env, level, [], [param_expr]) |> get_typ);
      f(env, level, [return_typ,...typs], rest);
    }
  | [] => typs
  | _ => raise(TypeError("Not Implemented"))
  ;
  f(env, level, [], exprs);
};

let infer = (env, level, e) =>
  try (R.ok @@ infer_exn(env, level, e)) {
  | TypeError(msg) => R.error_msg(msg)
  };