open Rresult;
open Lwt.Infix;

open Parsetree;
open Parsetree_util;

exception Runtime_error(string);

// let logger = Easy_logging.Logging.make_logger("Mesh.Eval", Debug, [Cli(Debug)]);

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
  | VGraphqlQuery(string, string, Extensions.Graphql.t);

type eval_result = Lwt_result.t((list(value), environment), Rresult.R.msg);

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
  | VGraphqlQuery(uri, query, _) => [%string "%{indent}%{uri}\n%{indent}%{query}"]
  };
};

let string_of_env = (env) =>
  List.map(((name, value)) => [%string "%{name}: %{string_of_value value}"], env)
  |> String.concat("\n");

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
  | VTuple(l)     => `List(List.map(value_to_yojson, l))
  | VClosure(_)   => `String("closure")
  | VMod(_)       => `String("module")
  | VRecord(l)    => `Assoc(List.map(((name, v)) => (name, value_to_yojson(v)), l))
  | VOpt(v)       => 
    switch (v) {
    | Some(v) => value_to_yojson(v)
    | None    => `Null
    }
  | VGraphqlQuery(_) => `String("graphql_query")
  };

let eval_graphql = (uri, raw_query, query) => {
  open Typetree;

  let rec gql_to_value = (typ, data) => 
    switch (typ, data) {
    | (TConst("bool"), `Bool(v))      => VBool(v)
    | (TConst("int"), `Int(v))        => VInt(v)
    | (TConst("float"), `Float(v))    => VFloat(v)
    | (TConst("string"), `String(v))  => VString(v)
    | (TList(typ'), `List(v))         => VList(List.map(gql_to_value(typ'), v))
    | (TRec(typ'), `Assoc(l))         => VRecord(gql_assoc_to_value(typ', l))
    | (TOpt(_), `Null)                => VOpt(None)
    | (TOpt(typ'), v)                 => VOpt(Some(gql_to_value(typ', v)))
    | (typ, value)                    => 
      raise(Runtime_error([%string "gql_to_value: Non-matching type-value %{Typetree_util.string_of_typ typ} %{Yojson.Basic.to_string value}"]))
    }
  and gql_assoc_to_value = (typ, data) =>
    switch (typ, data) {
    | (TRowExtend(_, typ', typ_rest), [(name, v), ...rest]) => 
      [(name, gql_to_value(typ', v)), ...gql_assoc_to_value(typ_rest, rest)]
    | (TRowEmpty, []) => []
    | _ => raise(Runtime_error("gql_assoc_to_value: Not records"))
    };


  Infer.typ_of_graphql_query(uri, query)                          >>= (typ) =>
  Data_source.Graphql.Client.query(Uri.of_string(uri), raw_query) >|= (data) =>
  switch (R.map(gql_to_value(typ), data)) {
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

/** [eval_exn(ret, env, e)] */
let rec eval_exn = (ret: list(value), env, e: list(expr)) => {
  let rec eval = (env, expr) => 
    switch (expr.pexpr_desc) {
    | ELit(lit)           => Lwt.return @@ (env, value_of_lit(lit))
    | EVar(path, varname) => Lwt.return @@ (env, value_of_var(env, path, varname, expr.pexpr_loc))
    | EList(l)            => Lwt_list.map_s(eval(env), l) >|= (l) => (env, VList(List.map(snd, l)))
    | ETuple(t)           => Lwt_list.map_s(eval(env), t) >|= (t) => (env, VTuple(List.map(snd, t)))
    | EApp(e_fun, e_arg)  => 
      Lwt.both(eval(env, e_fun), eval(env, e_arg)) >>= ((e1, e2)) =>
      switch (snd(e1), snd(e2)) {
      | (VClosure(env', EFun(pat, e)), argv)  => 
        eval(bind_pat_value(pat, argv) @ env', e) >|= ((_, value)) =>
        (env, value)
      | _ => raise(Runtime_error("EApp: LHS is not a function!"))
      };
    | EFun(_, _) as e     => Lwt.return @@ (env, VClosure(env, e))
    | ELet(pat, e) => 
      eval(env, e)              >|= (value) =>
      (bind_pat_value(pat, snd(value)) @ env, VUnit)
    | EMod(name, body)    => 
      eval_exn([], env, body) >|= ((_, mod_env)) =>
      ([(name, VMod(mod_env)), ...env], VUnit)
    | EOpen(path, modname) =>
      switch (value_of_var(env, path, modname, expr.pexpr_loc)) {
      | VMod(env') => Lwt.return @@ (env' @ env, VUnit)
      | _ => raise(Runtime_error([%string "open %{modname}: %{modname} is not a module!"]))
      }
    | ESeq(e, rest)       => 
      switch (e) {
      | {pexpr_desc: ELet(pat, e), _} => 
        eval(env, e)              >>= (value) =>
        bind_pat_value(pat, snd(value)) @ env  |> (env') =>
        eval(env', rest)
      | e => 
        eval(env, e) |> (_) => eval(env, rest)
      }
    | EPrim(prim) => eval_prim(env, prim)
    | ERecSelect(e, name) =>
      eval(env, e) >>= (value) =>
      switch (snd(value)) {
      | VRecord(fields) => Lwt.return @@ (env, List.assoc(name, fields))
      | _ => raise(Runtime_error("ERecSelect: base is not a record"))
      }
    | ERecExtend(name, e, base) =>
      eval(env, base) >>= (basev) =>
      switch (snd(basev)) {
      | VRecord(fields) => 
        eval(env, e) >|= (value) =>
        (env, VRecord(rm_record_duplicates @@ [(name, snd(value)), ...fields]))
      | _ => raise(Runtime_error("ERecExtend: base is not a record"))
      }
    | ERecEmpty => Lwt.return @@ (env, VRecord([]))
    | EOpt(maybe_e) => 
      switch (maybe_e) {
      | Some(e) => eval(env, e) >|= (v) => (env, VOpt(Some(snd(v))))
      | None => Lwt.return @@ (env, VOpt(None))
      }
    | EGraphql(uri, raw_query, query) => Lwt.return @@ (env, VGraphqlQuery(uri, raw_query, query))
    | _ => raise(Runtime_error("eval not implemented"))
    }
  and eval_prim = (env, prim) =>
    switch (prim) {
    // Int primitive functions
    | PIntAdd(e1, e2)     => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VInt(a), VInt(b)) => Lwt.return @@ (env, VInt(a + b)) | _ => raise(Runtime_error([%string "PIntAdd: Unexpected types"]))}
    | PIntSub(e1, e2)     => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VInt(a), VInt(b)) => Lwt.return @@ (env, VInt(a - b)) | _ => raise(Runtime_error([%string "PIntSub: Unexpected types"]))}
    | PIntMul(e1, e2)     => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VInt(a), VInt(b)) => Lwt.return @@ (env, VInt(a * b)) | _ => raise(Runtime_error([%string "PIntMul: Unexpected types"]))}
    | PIntDiv(e1, e2)     => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VInt(a), VInt(b)) => Lwt.return @@ (env, VInt(a / b)) | _ => raise(Runtime_error([%string "PIntDiv: Unexpected types"]))}
    | PIntNeg(e)          => eval(env, e) >>= (e) => switch (snd(e)) { | VInt(a) => Lwt.return @@ (env, VInt(-a)) | _ => raise(Runtime_error([%string "PIntNeg: Unexpected types"])) }
    // Float primitive functions
    | PFloatAdd(e1, e2)   => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VFloat(a), VFloat(b)) => Lwt.return @@ (env, VFloat(a +. b)) | _ => raise(Runtime_error([%string "PFloatAdd: Unexpected types"]))}
    | PFloatSub(e1, e2)   => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VFloat(a), VFloat(b)) => Lwt.return @@ (env, VFloat(a -. b)) | _ => raise(Runtime_error([%string "PFloatSub: Unexpected types"]))}
    | PFloatMul(e1, e2)   => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VFloat(a), VFloat(b)) => Lwt.return @@ (env, VFloat(a *. b)) | _ => raise(Runtime_error([%string "PFloatMul: Unexpected types"]))}
    | PFloatDiv(e1, e2)   => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (VFloat(a), VFloat(b)) => Lwt.return @@ (env, VFloat(a /. b)) | _ => raise(Runtime_error([%string "PFloatDiv: Unexpected types"]))}
    | PFloatNeg(e)        => eval(env, e) >>= (e) => switch (snd(e)) { | VFloat(a) => Lwt.return @@ (env,  VFloat(-.a)) | _ => raise(Runtime_error([%string "PIntNeg: Unexpected types"])) }
    // List primitive functions
    | PListCons(e1, e2)   => Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => switch (snd(e1), snd(e2)) { | (v, VList(l)) => Lwt.return @@ (env, VList([v, ...l])) | _ => raise(Runtime_error([%string "PListCons: Unexpected types"]))}
    | PListMap(e1, e2)    => 
      Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => 
      switch (snd(e1), snd(e2)) {
      | (VClosure(env', EFun(pat, e)), VList(l)) => Lwt_list.map_s((argv) => eval(bind_pat_value(pat, argv) @ env', e), l) >|= (l) => (env, VList(List.map(snd, l)))
      | _ => raise(Runtime_error([%string "PListMap: Unexpected types"]))
      }
    | PListMapi(e1, e2)   => 
      Lwt.both(eval(env, e1), eval(env, e2)) >>= ((e1, e2)) => 
      switch (snd(e1), snd(e2)) {
      | (VClosure(env', EFun(pat, e)), VList(l)) => 
        Lwt_list.mapi_s((i, argv) => 
          eval(bind_pat_value(pat, VInt(i)) @ env', e) >>= (v) =>
          switch (snd(v)) {
          | VClosure(env', EFun(pat, e)) => eval(bind_pat_value(pat, argv) @ env', e)
          | _ => raise(Runtime_error([%string "PListMapi: Unexpected types"]))
          },
          l
        )
        >|= (v) => (env, VList(List.map(snd, v)))
      | _ => raise(Runtime_error([%string "PListMapi: Unexpected types"]))
      }

    | PListFoldl(e1, e2, e3)  => 
      Lwt.both(Lwt.both(eval(env, e1), eval(env, e2)), eval(env, e3)) >>= (((e1, e2), e3)) =>
      switch (snd(e1), snd(e2), snd(e3)) {
      | (VClosure(env', EFun(pat, e)), acc, VList(l)) => 
        Lwt_list.fold_left_s(((_, acc), argv) => 
          eval(bind_pat_value(pat, acc) @ env', e) >>= (v) =>
          switch (snd(v)) {
          | VClosure(env', EFun(pat, e)) => eval(bind_pat_value(pat, argv) @ env', e)
          | _ => raise(Runtime_error([%string "PListFoldl: Unexpected types"]))
          },
          (env, acc),
          l
        )
      | _ => raise(Runtime_error([%string "PListFoldl: Unexpected types"]))
      }

    | PListFoldr(e1, e2, e3)  => 
      Lwt.both(Lwt.both(eval(env, e1), eval(env, e2)), eval(env, e3)) >>= (((e1, e2), e3)) =>
      switch (snd(e1), snd(e2), snd(e3)) {
      | (VClosure(env', EFun(pat, e)), VList(l), acc) => 
        Lwt_list.fold_right_s((argv, (_, acc)) => 
          eval(bind_pat_value(pat, argv) @ env', e) >>= (v) =>
          switch (snd(v)) {
          | VClosure(env', EFun(pat, e)) => eval(bind_pat_value(pat, acc) @ env', e)
          | _ => raise(Runtime_error([%string "PListFoldl: Unexpected types"]))
          },
          l,
          (env, acc)
        )
      | _ => raise(Runtime_error([%string "PListFoldl: Unexpected types"]))
      }

    // Option primitive functions
    | POptionSome(e) =>
      eval_value(env, e) >|= (value) => VOpt(Some(value))

    | POptionNone => Lwt.return @@ VOpt(None)

    | POptionGet(e1, e2) =>
      Lwt.both(eval_value(env, e1), eval_value(env, e2)) >|= ((e1, e2)) =>
      switch (e1, e2) {
      | (v, VOpt(None))
      | (_, VOpt(Some(v))) => v
      | _ => raise(Runtime_error([%string "POptionGet: Unexpected types"]))
      }

    // GraphQL primitive functions
    // TODO: Revisit graphql_execute with URI
    // | PGraphqlExec(e1, e2) => Lwt.both(eval(env, e1), eval(env, e2)) >>= (e) => switch (e) { | (VString(uri), VGraphqlQuery(query)) => eval_graphql(uri, query) | _ => raise(Runtime_error([%string "PGraphqlExec: Unexpected types"]))}
    | PGraphqlExec(e) => 
      eval(env, e) >>= (e) => 
      switch (snd(e)) { 
      | VGraphqlQuery(uri, raw_query, query) => eval_graphql(uri, raw_query, query) >|= (v) => (env, v) 
      | _ => raise(Runtime_error([%string "PGraphqlExec: Unexpected types"])) 
      }
    };

  Lwt_list.fold_left_s(
    ((env, ret), expr) => eval(env, expr) >|= ((env', value)) => (env', [value, ...ret]),
    (env, ret),
    e
  )
  >|= ((env, ret)) => (List.rev @@ List.filter(fun | VUnit => false | _ => true, ret), env)
};

let eval = (~env=[], e) => 
  try (Lwt_result.ok @@ eval_exn([], env, e)) {
  | Runtime_error(msg) => Lwt.return @@ Rresult.R.error_msg(msg)
  };
