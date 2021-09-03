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
    | TApp(fun_typ, param_typs) => {
        f(fun_typ); 
        List.iter(f, param_typs);
      }
    | TFun(param_typs, return_typ) => {
        List.iter(f, param_typs); 
        f(return_typ);
      }
    | TConst(_) => ()
    | _ => raise(TypeError("Not implemented"))
  ;
  f(typ);
};

let rec unify = (typ1, typ2) => {
  typ1 == typ2 ? () :
  switch (typ1, typ2) {
    | (TConst(name1), TConst(name2)) when name1 == name2 => ()
    | (TApp(fun_typ1, param_typs1), TApp(fun_typ2, param_typs2)) => {
      unify(fun_typ1, fun_typ2); List.iter2(unify, param_typs1, param_typs2);
    };
    | (TFun(param_typs1, return_typ1), TFun(param_typs2, return_typ2)) => {
      List.iter2(unify, param_typs1, param_typs2); unify(return_typ1, return_typ2);
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
  | TApp(fun_typ, param_typs) => 
    TApp(generalize(level, fun_typ), List.map(generalize(level), param_typs))
  | TFun(param_typs, return_typ) => 
    TFun(List.map(generalize(level), param_typs), generalize(level, return_typ))
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
      | TApp(fun_typ, param_typs) => TApp(f(fun_typ), List.map(f, param_typs))
      | TFun(param_typs, return_typ) => TFun(List.map(f, param_typs), f(return_typ))
      | _ => raise(TypeError("not implemented"))
    };
  };
  f(typ);
};

let rec match_fun_typ = (num_params) => fun
  | TFun(param_typs, return_typ) => {
      (List.length(param_typs) != num_params)? 
      raise(TypeError("Unexpected number of arguments")):
      (param_typs, return_typ)
    }
  | TVar({contents: Constrained(typ)}) => match_fun_typ(num_params, typ)
  | TVar({contents: Free(_, level)} as tvar) => {
    let param_typs = {
      let rec f = fun
        | 0 => []
        | n => [new_var(level), ...f(n-1)]
      ;
      f(num_params);
    };
    let return_typ = new_var(level);
    tvar := Constrained(TFun(param_typs, return_typ));
    (param_typs, return_typ);
  }
  | _ => raise(TypeError("Expected a function"))
;


let type_name_of_literal = fun 
  | Int(_) => TConst("int")
  | Float(_) => TConst("float")
  | String(_) => TConst("string")
  | Bool(_) => TConst("bool")
  | Unit => TConst("unit")
; 

// Keep tuple structure when unpacking pattern?
let expr_list_of_pattern = (p) => {
  let rec f = (p, acc) => switch (p) {
  | PTuple(l) => (List.map(f(_,[]), l) |> List.concat)@acc
  | PLit(lit) => [ELit(lit), ...acc]
  | PVar(name) => [EVar(name), ...acc]
  | _ => acc
  };
  f(p, []);
};

let var_names_of_pattern = (pattern) => {
  (pattern) 
  |> expr_list_of_pattern 
  |> List.filter((fun | EVar(_) => true | _ => false ))
  |> List.map((fun | EVar(name) => name | _ => ""))
};

let rec infer_exn = (env, level) => fun
  | ELit(lit) => type_name_of_literal(lit)
  | EVar(name) => {
      try (instantiate(level, Env.lookup(env, name))) {
        | Not_found => raise(TypeError([%string "variable %{name}"]))
      }
    }
  | EFun(pattern, body_expr) => {
      let param_names = pattern |> var_names_of_pattern;
      let param_typs = param_names |> List.map((_) => new_var(level));
      let fn_env = List.fold_left2(
        (env, param_name, param_typ) => Env.extend(env, param_name, param_typ),
        env,
        param_names,
        param_typs
      );
      let return_typ = infer_exn(fn_env, level, body_expr);
      TFun(param_typs, return_typ);
    }
  // let expression scope?
  // match pattern binding to value expr type
  // | ELet(pattern, expr) => {
  //     let var_names = pattern |> var_names_of_pattern;

  //   }
  | _ => raise(TypeError("Not Implemented"))
  ;


let infer = (env, level, e) =>
  try (R.ok @@ infer_exn(env, level, e)) {
  | TypeError(msg) => R.error_msg(msg)
  };