open Syntax;
open Rresult;

exception TypeError(string);


module Env = {
  module StringMap = Map.Make(String);
  type t = {
    tvars: StringMap.t(typ),
    mutable current_id: int
  };

  let empty = {
    tvars: StringMap.empty,
    current_id: 0
  };
  let extend = (env, name, typ) => {...env, tvars:StringMap.add(name, typ, env.tvars)};
  let lookup = (env, name) => StringMap.find(name, env.tvars);

  let next_id = (env) => {
    let id = env.current_id;
    // print_endline([%string "id update %{string_of_int(id)}"]);
    env.current_id = env.current_id + 1;
    id;
  }

  let get_string_id = (env) => string_of_int(env.current_id);

  let get_bindings = (env) => StringMap.bindings(env.tvars);

  let reset_id = (env) => env.current_id = 0;

};


let new_var = (env, lvl) => TVar(ref(Free(Env.next_id(env), lvl)));

let new_quantified_var = (env) => TVar(ref(Quantified(Env.next_id(env))));

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
    | TList(list_typ) => f(list_typ)
    | TConst(_) => ()
    | _ => raise(TypeError("occurs check not implemented"))
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
    | (TList(typ1), TList(typ2)) => unify(typ1, typ2)
    | (TTuple(typs1), TTuple(typs2)) => {
        try(List.iter2((typ1, typ2) => unify(typ1,typ2), typs1, typs2)){
          | Invalid_argument(_) => raise(TypeError("Cant unify tuples of different lengths"))
        }
      }
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
  | TList(typ) => TList(generalize(level, typ))
  | TTuple(l) => TTuple(List.map(generalize(level), l))
  | TVar({contents: Constrained(typ)}) => generalize(level, typ)
  | TVar({contents: Quantified(_)}) as typ => typ
  | TVar({contents: Free(_)}) as typ => typ
  | TConst(_) as typ => typ
  // | _ => raise(TypeError("generalize not implemented"))
;

let instantiate = (env, level, typ) => {
  let id_var_map = Hashtbl.create(10);
  let rec f = (typ) => {
    switch(typ) {
      | TConst(_) => typ
      | TVar({contents: Constrained(typ)}) => f(typ)
      | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_var_map, id)){
          | Not_found => {
            let var = new_var(env, level);
            Hashtbl.add(id_var_map, id, var);
            var;
            };
          };
      };
      | TVar({contents: Free(_)}) => typ
      | TApp(fun_typ, param_typ) => TApp(f(fun_typ), f(param_typ))
      | TFun(param_typ, return_typ) => TFun(f(param_typ), f(return_typ))
      | TList(typ1) => TList(f(typ1))
      | TTuple(l) => TTuple(List.map(f, l))
     // | _ => raise(TypeError("instantiate not implemented"))
    };
  };
  (f(typ), env);
};

let rec match_fun_typ = (env, fun_typ) => switch (fun_typ) {
  | TFun(param_typ, return_typ)              => (param_typ, return_typ)
  | TVar({contents: Constrained(typ)})       => match_fun_typ(env, typ)
  | TVar({contents: Free(_, level)} as tvar) => {
    let param_typ = new_var(env, level);
    let return_typ = new_var(env, level);
    tvar := Constrained(TFun(param_typ, return_typ));
    (param_typ, return_typ);
  }
  | _ => raise(TypeError("Expected a function"))
};



let type_const_of_literal = fun 
  | Int(_) => TConst("int")
  | Float(_) => TConst("float")
  | String(_) => TConst("string")
  | Bool(_) => TConst("bool")
  | Unit => TConst("unit")
; 


let rec bind_pat_typ = (pat, typ) => switch (pat, typ) {
  | (PAny, _) => []
  | (PVar(name), e) => [(name, e)]
  | (PLit(_), _) => []
  | (PTuple(lpat), TTuple(lt)) => 
    List.fold_left2((acc, pat, e) => acc @ bind_pat_typ(pat, e), [], lpat, lt)
  | _ => raise(TypeError("Cannot destructure pattern"))
};


let rec infer_exn = (env, level, exprs, typs) => {
  
  let update_env = (env, names_typs) => List.fold_right(
        ((var_name, var_typ), old_env) => var_name == ""? 
          old_env:
          Env.extend(old_env, var_name, var_typ),
        names_typs, env
        ); 
  
  let inherit_id = (env1: Env.t, env2: Env.t) => {...env2, current_id: env1.current_id};

  let rec f = (env, level, expr) => 
    switch (expr) {
    | ELit(lit) => (type_const_of_literal(lit), env)
    | EVar(name) => {
        let (typ, new_env) = try (instantiate(env, level, Env.lookup(env, name))) {
          | Not_found => raise(TypeError([%string "variable %{name}"]))
        };
        (typ, inherit_id(new_env, env))
      }
    | EFun(param_pat, body_expr) => {
        let rec bind_pat_param = (p) => switch (p) {
          | PAny => [("", new_var(env, level))]
          | PVar(name) =>  [(name, new_var(env, level))]
          | PLit(lit) => [("", type_const_of_literal(lit))]
          | PTuple(lpat) => 
            List.fold_left((acc, pat) => acc @ bind_pat_param(pat), [], lpat);
        };
        let names_typs = bind_pat_param(param_pat);
        let param_typ = names_typs |> fun
          | [] => TConst("unit")
          | [(_, typ)] => typ
          | l => {
            let (_, typs) = List.split(l);
            TTuple(typs)
          }
        ;
        let fn_env = update_env(env, names_typs);
        let (return_typ,new_env) = f(fn_env, level, body_expr);
        (TFun(param_typ, return_typ), inherit_id(new_env, env));
      }
    | ELet(pattern, expr) => {
        let (gen_typ, new_env) = f(env, level + 1, expr) 
        |> ((typ, new_env1)) => (generalize(level,typ), new_env1);
        let names_typs = bind_pat_typ(pattern, gen_typ);
        let new_env2 = update_env(inherit_id(new_env, env), names_typs);
        (TConst("unit"), new_env2);
      }
    | EApp(fn_expr, param_expr) => {
        let (fn_typ, new_env) = f(env, level, fn_expr);
        let (param_typ, return_typ) = match_fun_typ(inherit_id(new_env,env), fn_typ);
        let (param_typ1, new_env1) = f(inherit_id(new_env,env), level, param_expr);
        unify(param_typ, param_typ1);
        (return_typ, inherit_id(new_env1,env));
      }
    | EList(l) => {
        let (list_typ, new_env1) = infer_exn(env, level, l, []) 
        |> ((typs, new_env)) => switch (typs) {
          | [] => (TList(new_var(new_env, level)), new_env)
          | [lone] => (TList(lone), new_env)
          | [first, ...rest] => 
            try (List.iter(unify(first), rest)) {
              | TypeError(msg) => raise(TypeError("List" ++ msg))
            };
            (TList(first), new_env);
        };
        (list_typ, inherit_id(new_env1, env));
      }
    | ETuple(l) => {
        let (tuple_typs, new_env) = infer_exn(env, level, l, []);
        (TTuple(tuple_typs), inherit_id(new_env, env));
      }
    | ESeq(expr1, expr2) => 
      let (typs, new_env) = infer_exn(env, level, [expr1, expr2], []);
      (typs |> List.rev |> List.hd, inherit_id(new_env, env));
    | EPrim(prim) =>
      typ_of_primitive(prim, env)
    }
    and typ_of_primitive = (prim, env) => switch (prim) {
      | PListCons(el_expr, list_expr) =>
        let (list_typ, new_env) = f(env, level, list_expr);
        let (typ, new_env1) = f(new_env, level, el_expr);
        unify(list_typ, TList(typ));
        (list_typ, inherit_id(new_env1, env));
      | PIntAdd(a_expr, b_expr)       => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("int")), typs);
        (TConst("int"), inherit_id(new_env, env))
      | PIntSub(a_expr, b_expr)                 =>
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("int")), typs); 
        (TConst("int"), inherit_id(new_env, env))
      | PIntMul(a_expr, b_expr)                 => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("int")), typs);
        (TConst("int"), inherit_id(new_env, env))
      | PIntDiv(a_expr, b_expr)                 =>
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("int")), typs); 
        (TConst("int"), inherit_id(new_env, env))
      | PFloatAdd(a_expr, b_expr)               => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("float")), typs);
        (TConst("float"), inherit_id(new_env, env))
      | PFloatSub(a_expr, b_expr)               => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("float")), typs);
        (TConst("float"), inherit_id(new_env, env))
      | PFloatMul(a_expr, b_expr)               => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("float")), typs);
        (TConst("float"), inherit_id(new_env, env))
      | PFloatDiv(a_expr, b_expr)               => 
        let (typs, new_env) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(TConst("float")), typs);
        (TConst("float"), inherit_id(new_env, env))
    };

  switch (exprs) {
  | [e, ...rest] => 
    f(env, level, e) |> ((typ, new_env)) =>
    infer_exn(new_env, level, rest, typs@[typ])
  | [] => (typs, env)
  }
};


let infer = (env, level, e) =>
  try (R.ok @@ infer_exn(env, level, e, [])) {
  | TypeError(msg) => R.error_msg(msg)
  };