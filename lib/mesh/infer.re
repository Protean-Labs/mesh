open Rresult;
// open Lwt.Infix;

open Syntax;
open Syntax_util;

exception Type_error(string);

type id = int;
type level = int;

type typ =
  | TConst(name)
  | TFun(typ, typ)
  | TApp(typ, typ)
  | TTuple(list(typ))
  | TList(typ)
  | TVar(ref(tvar))
  | TRec(typ)
  | TRowEmpty
  | TRowExtend(name, typ, typ)
  | TMod(tenv)
and tvar = 
  | Free(id, level)
  | Constrained(typ)
  | Quantified(id)
and tenv = list((name, typ));

let typ_of_graphql_query = (uri, query) => { 
  open Graphql_ppx_base.Result_structure;
  
  let rec typ_of_result_structure = (op) => 
    switch (op) {
    | Res_nullable(_) =>                    raise(Type_error("GraphQL: Res_nullable not implemented"))
    | Res_array({inner, _}) =>              TList(typ_of_result_structure(inner))
    | Res_id(_) =>                          raise(Type_error("GraphQL: Res_id not implemented"))
    | Res_string(_) =>                      TConst("string")
    | Res_int(_) =>                         TConst("int")
    | Res_float(_) =>                       TConst("float")
    | Res_boolean(_) =>                     TConst("bool") 
    | Res_raw_scalar(_) =>                  raise(Type_error("GraphQL: Res_raw_scalar not implemented"))
    | Res_poly_enum(_) =>                   raise(Type_error("GraphQL: Res_poly_enum not implemented"))
    | Res_custom_decoder(_) =>              raise(Type_error("GraphQL: Res_custom_decoder not implemented"))
    | Res_record({fields, _})
    | Res_object({fields, _}) =>
      let fields = List.fold_left((acc, field) => typ_of_field_result(field) |> ((name, typ)) => TRowExtend(name, typ, acc), TRowEmpty, fields);
      TRec(fields)
    | Res_poly_variant_selection_set(_) =>  raise(Type_error("GraphQL: Res_poly_variant_selection_set not implemented"))
    | Res_poly_variant_union(_) =>          raise(Type_error("GraphQL: Res_poly_variant_union not implemented"))
    | Res_poly_variant_interface(_) =>      raise(Type_error("GraphQL: Res_poly_variant_interface not implemented"))
    | Res_solo_fragment_spread(_) =>        raise(Type_error("GraphQL: Res_solo_fragment_spread not implemented"))
    | Res_error(_) =>                       raise(Type_error("GraphQL: Res_error not implemented"))
    }
  and typ_of_field_result = (field) =>
    switch (field) {
    | Fr_named_field({name, type_, _}) => (name, typ_of_result_structure(type_));
    | Fr_fragment_spread(_) => raise(Type_error("GraphQL: Fragments not supported"))
    };

  Lwt_result.map((ops) => 
    switch (ops) {
    | [(Def_operation({inner, _}), _)] => typ_of_result_structure(inner);
    | [(Def_fragment(_), _)] => raise(Type_error("GraphQL: Fragments not supported"))
    | _ => raise(Type_error("GraphQL: Only single query supported"))
    },
    Data_source.Graphql.Client.validate(Uri.of_string(uri), query)
  );
};

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

  // let rec_to_string = (f,row:tenv) => List.map(((name, typ)) => [%string "%{name}: %{f false typ}"], row) |>  String.concat(", ");

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
    | TMod(_) => [%string "module"]
    | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_name_map, id)) {
        | Not_found => {
          let name = next_name();
          Hashtbl.add(id_name_map, id, name);
          [%string "%{name}%{string_of_int(id)}"]; 
        }
      }
    }
    | TVar({contents: Free(id, _)}) => [%string "_%{string_of_int(id)}"]
    | TVar({contents: Constrained(typ)}) => f(is_simple, typ)
    | TRec(row) => [%string "{%{f false row}}"]
    | TRowEmpty => "TRowEmpty"
    | TRowExtend(_) => "TRow extend"
    };
  
  f(false, typ);
}

module Env = {
  let counter = () => {
    let c = ref(0);
    fun () => {
      let id = c^;
      incr(c);
      id;
    };
  };

  let new_var = (counter) => {
    let current_id = counter();
    fun (lvl) => TVar(ref(Free(current_id(), lvl)));
  }

  type t = {
    tvars: tenv,
    new_var: level => typ
  };

  let empty = {
    tvars: [],
    new_var: new_var(counter)
  };
  let extend = (env, path, name, typ) => {
    ...env, 
    tvars: List.fold_left((acc, modname) => 
    switch (List.assoc_opt(modname, acc)) {
    | Some(TMod(tenv)) => tenv
    | Some(_)         => raise(Type_error([%string "%{modname} is not a module!"]))
    | None            => raise(Type_error([%string "Unbound module %{modname}"]))
    }
    , env.tvars, path)  |> (mod_ns) => [(name, typ), ...mod_ns]
  };

  let extend_fold = (env, path, names_typs) => List.fold_right(
    ((var_name, var_typ), old_env) => var_name == ""? 
      old_env:
      extend(old_env, path, var_name, var_typ),
    names_typs, env
    ); 

  let lookup = (env, path, name, loc) =>
    // Attempt to find module namespace based on [path]
    List.fold_left((acc, modname) => 
      switch (List.assoc_opt(modname, acc)) {
      | Some(TMod(tenv)) => tenv
      | Some(_)         => raise(Type_error([%string "%{modname} is not a module!"]))
      | None            => raise(Type_error([%string "Unbound module %{modname}"]))
      }
    , env.tvars, path)  |> (mod_ns) =>
    // Attempt to find [name] in module namespace
    switch (List.assoc_opt(name, mod_ns)) {
    | Some(t) => t
    | None    => raise(Type_error([%string "Unbound value %{string_of_expr (mk_expr ~loc (EVar(path, name)))}"]))
    }; 

  let reset_id = (env) => {...env, new_var :new_var(counter)};
};

let occurs_check_adjust_levels = (tvar_id, tvar_level, typ) => {
  let rec f = fun
    | TVar({contents: Constrained(typ)}) => f(typ)
    | TVar({contents: Quantified(_)}) => raise(Type_error("Cant check quantified variable"))
    | TVar({contents: Free(other_id, other_level)} as other_tvar) => {
      (other_id == tvar_id)? 
      raise(Type_error("Recursive types")): 
      (other_level > tvar_level)? 
            other_tvar := Free(other_id, tvar_level):
            ();
    }
    | TApp(fun_typ, param_typ) => {f(fun_typ); f(param_typ)}
    | TFun(param_typ, return_typ) => {f(param_typ); f(return_typ)}
    | TList(list_typ) => f(list_typ)
    | TRec(row) => f(row)
    | TRowExtend(_, field_typ, row) => { f(field_typ); f(row);}
    | TRowEmpty => ()
    | TConst(_) => ()
    | _ => raise(Type_error("occurs check not implemented"))
  ;
  f(typ);
};

let rec unify = (new_var, typ1, typ2) => {
  typ1 == typ2 ? () :
  switch (typ1, typ2) {
    | (TConst(name1), TConst(name2)) when name1 == name2 => ()
    | (TApp(fun_typ1, param_typ1), TApp(fun_typ2, param_typ2)) => {
      unify(new_var, fun_typ1, fun_typ2); unify(new_var, param_typ1, param_typ2);
    };
    | (TFun(param_typ1, return_typ1), TFun(param_typ2, return_typ2)) => {
      unify(new_var, param_typ1, param_typ2); unify(new_var, return_typ1, return_typ2);
    };
    | (TList(typ1), TList(typ2)) => unify(new_var, typ1, typ2)
    | (TTuple(typs1), TTuple(typs2)) => {
        try(List.iter2((typ1, typ2) => unify(new_var, typ1,typ2), typs1, typs2)){
          | Invalid_argument(_) => raise(Type_error("Cant unify new_var, tuples of different lengths"))
        }
      }
    | (TVar({contents: Constrained(typ1)}), typ2) => unify(new_var, typ1, typ2)
    | (typ1, TVar({contents: Constrained(typ2)})) => unify(new_var, typ1, typ2)
    | (TVar({contents: Free(id1, _)}), TVar({contents: Free(id2, _)})) when id1 == id2 =>
      raise(Type_error("Can only have one instance of a type variable"))
    | (TVar({contents: Free(id, level)} as tvar), typ2) => {
      occurs_check_adjust_levels(id, level, typ2);
      tvar := Constrained(typ2);
    }
    | (typ1, TVar({contents: Free(id, level)} as tvar)) => {
      occurs_check_adjust_levels(id, level, typ1);
      tvar := Constrained(typ1);
    }
    | (TRec(row1), TRec(row2)) => unify(new_var, row1, row2)
    | (TRowEmpty, TRowEmpty) => ()
    | (TRowExtend(label1, field_typ1, rest_row1), TRowExtend(_) as row2) =>
      let rest_row_tvar_ref_option = switch (rest_row1) {
        | TVar({contents: Free(_)} as tvar_ref) => Some(tvar_ref)
        | _ => None
      };
      let rest_row2 = rewrite_row(new_var, row2, label1, field_typ1);
      switch (rest_row_tvar_ref_option) {
        | Some({contents: Constrained(_)}) => raise(Type_error("Recursive row types"))
        | _ => ()
      }
      unify(new_var, rest_row1, rest_row2)
    | (_, _) => raise(Type_error([%string "Cannot unify %{string_of_typ(typ1)} and %{string_of_typ(typ2)}"]))
  }
}
and rewrite_row = (new_var, row2, label1, field_typ1) => switch (row2) {
  | TRowEmpty => raise(Type_error([%string "rec does not contain label %{label1}"]))
  | TRowExtend(label2, field_typ2, rest_row2) when label2 == label1 =>
    unify(new_var, field_typ1, field_typ2);
    rest_row2;
  | TRowExtend(label2, field_typ2, rest_row2) => 
    TRowExtend(label2, field_typ2, rewrite_row(new_var, rest_row2, label1, field_typ1))
  | TVar({contents: Constrained(row2)}) => rewrite_row(new_var, row2, label1, field_typ1)
  | TVar({contents: Free(_, level)} as tvar) => 
    let rest_row2 = new_var(level);
    let typ2 = TRowExtend(label1, field_typ1, rest_row2);
    tvar := Constrained(typ2);
    rest_row2
  | _ => raise(Type_error("rewrite requires rec type"))
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
  // | TMod(tenv) => TMod(List.split(tenv) |> ((names, typs)) => List.combine(names, List.map(generalize(level), typs)))
  | TRec(row) => TRec(generalize(level,row))
  | TMod(_) => raise(Type_error("Cannot generalize module"))
  | TRowExtend(label, field_typ, row) =>  TRowExtend(label, generalize(level,field_typ), generalize(level,row))
  | TVar({contents: Constrained(typ)}) => generalize(level, typ)
  | TVar({contents: Quantified(_)}) as typ => typ
  | TVar({contents: Free(_)}) as typ => typ
  | TConst(_) as typ => typ
  | TRowEmpty as typ => typ
  // | _ => raise(Type_error("generalize not implemented"))
;

let instantiate = (new_var, level:level, typ) => {
  let id_var_map = Hashtbl.create(10);
  let rec f = (typ) =>
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
      | TList(typ1) => TList(f(typ1))
      | TTuple(l) => TTuple(List.map(f, l))
      | TRec(row) => TRec(f(row))
      | TMod(_) => raise(Type_error("Cannot instantiate module"))
      | TRowEmpty => typ 
      | TRowExtend(label, field_typ, row) => TRowExtend(label, f(field_typ), f(row))
     // | _ => raise(Type_error("instantiate not implemented"))
    };

  f(typ);
};

let rec match_fun_typ = (new_var, fun_typ) => switch (fun_typ) {
  | TFun(param_typ, return_typ)              => (param_typ, return_typ)
  | TVar({contents: Constrained(typ)})       => match_fun_typ(new_var, typ)
  | TVar({contents: Free(_, level)} as tvar) => {
    let param_typ = new_var(level);
    let return_typ = new_var(level);
    tvar := Constrained(TFun(param_typ, return_typ));
    (param_typ, return_typ);
  }
  | _ => raise(Type_error("Expected a function"))
};


let type_const_of_literal = fun 
  | Int(_) => TConst("int")
  | Float(_) => TConst("float")
  | String(_) => TConst("string")
  | Bool(_) => TConst("bool")
  | Unit => TConst("unit")
; 


let rec bind_pat_typ = (pat, typ) => 
  switch (pat.ppat_desc, typ) {
  | (PAny, _) => []
  | (PVar(name), e) => [(name, e)]
  | (PLit(_), _) => []
  | (PTuple(lpat), TTuple(lt)) => 
    List.fold_left2((acc, pat, e) => acc @ bind_pat_typ(pat, e), [], lpat, lt)
  | _ => raise(Type_error("Cannot destructure pattern"))
  };


let rec infer_exn = (env, level, exprs, typs) => {

  let rec f = (env: Env.t, level, expr) => 
    switch (expr.pexpr_desc) {
    | ELit(lit) => (type_const_of_literal(lit), env)
    | EVar(path, name) => {
        let typ = try (instantiate(env.new_var, level, Env.lookup(env, path, name, expr.pexpr_loc))) {
          | Not_found => raise(Type_error([%string "variable %{name}"]))
        };
        (typ, env)
      }
    | EFun(param_pat, body_expr) => {
        let rec bind_pat_param = (pat) => 
          switch (pat.ppat_desc) {
          | PAny => [("", env.new_var(level))]
          | PVar(name) =>  [(name, env.new_var(level))]
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
        let fn_env = Env.extend_fold(env, [],names_typs);
        let (return_typ,_) = f(fn_env, level, body_expr);
        (TFun(param_typ, return_typ), env);
      }
    | ELet(pattern, expr) => {
        let (gen_typ, _) = f(env, level + 1, expr) 
        |> ((typ, new_env1)) => (generalize(level,typ), new_env1);
        let names_typs = bind_pat_typ(pattern, gen_typ);
        let new_env2 = Env.extend_fold(env, [], names_typs);
        (TConst("unit"), new_env2);
      }
    | EApp(fn_expr, param_expr) => {
        let (fn_typ, _) = f(env, level, fn_expr);
        let (param_typ, return_typ) = match_fun_typ(env.new_var, fn_typ);
        let (param_typ1, _) = f(env, level, param_expr);
        unify(env.new_var, param_typ, param_typ1);
        (return_typ, env);
      }
    | EList(l) => {
        let (list_typ, _) = infer_exn(env, level, l, []) 
        |> ((typs, new_env)) => switch (typs) {
          | [] => (TList(env.new_var(level)), new_env)
          | [lone] => (TList(lone), new_env)
          | [first, ...rest] => 
            try (List.iter(unify(env.new_var,first), rest)) {
              | Type_error(msg) => raise(Type_error("List" ++ msg))
            };
            (TList(first), new_env);
        };
        (list_typ, env);
      }
    | ETuple(l) => {
        let (tuple_typs, _) = infer_exn(env, level, l, []);
        (TTuple(tuple_typs), env);
      }
    | ERecEmpty => (TRec(TRowEmpty), env)
    | ERecSelect(rec_expr, label)=>
      let rest_row_typ = env.new_var(level);
      let field_typ = env.new_var(level);
      let param_typ = TRec(TRowExtend(label, field_typ, rest_row_typ));
      let (rec_typ, _) = f(env, level, rec_expr);
      unify(env.new_var, param_typ, rec_typ);
      (field_typ, env); 
    | ERecExtend(label, expr, rec_expr) =>
      let rest_row_typ = env.new_var(level);
      let field_typ = env.new_var(level);
      let param_typ1 = field_typ;
      let param_typ2 = TRec(rest_row_typ);
      let return_typ = TRec(TRowExtend(label, field_typ, rest_row_typ));
      f(env, level, expr) |> ((expr_typ, _)) => unify(env.new_var, param_typ1, expr_typ);
      f(env, level, rec_expr) |> ((rec_typ, _)) => unify(env.new_var, param_typ2, rec_typ);
      (return_typ, env)
    | ESeq(expr1, expr2) => 
      let (typs, _) = infer_exn(env, level, [expr1, expr2], []);
      (typs |> List.rev |> List.hd, env);
    | EPrim(prim) =>
      typ_of_primitive(prim, env)
    | EMod(name, exprs) => 
      let (_, new_env:Env.t) = infer_exn({...env, tvars:[]}, level, exprs, []);
      (TConst("unit"), Env.extend(env,[],name ,TMod(new_env.tvars)))
    // | _ => raise(Type_error("infer not implmented"))
    }
    and typ_of_primitive = (prim, env) => switch (prim) {
      | PListCons(el_expr, list_expr) =>
        let (typs, _) = infer_exn(env, level, [el_expr, list_expr], []) |> ((inferred, nenv)) =>
        switch (inferred) {
          | [_, _] as l => (l, nenv)
          | _ => raise(Type_error("cons_list wrong number of arguments"))
        };
        let typ = List.hd(typs);
        List.iter2(
          unify(env.new_var), 
          typs,
          [typ, TList(typ)]
        );
        (TList(typ), env);
      | PIntAdd(a_expr, b_expr)       => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("int")), typs);
        (TConst("int"), env)
      | PIntSub(a_expr, b_expr)                 =>
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("int")), typs); 
        (TConst("int"), env)
      | PIntMul(a_expr, b_expr)                 => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("int")), typs);
        (TConst("int"), env)
      | PIntDiv(a_expr, b_expr)                 =>
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("int")), typs); 
        (TConst("int"), env)
      | PFloatAdd(a_expr, b_expr)               => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("float")), typs);
        (TConst("float"), env)
      | PFloatSub(a_expr, b_expr)               => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("float")), typs);
        (TConst("float"), env)
      | PFloatMul(a_expr, b_expr)               => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("float")), typs);
        (TConst("float"), env)
      | PFloatDiv(a_expr, b_expr)               => 
        let (typs, _) = infer_exn(env, level, [a_expr,b_expr], []);
        List.iter(unify(env.new_var, TConst("float")), typs);
        (TConst("float"), env)
      | PGraphQL(_) => raise(Type_error("graphql not implemented"))
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
  | Type_error(msg) => R.error_msg(msg)
  };