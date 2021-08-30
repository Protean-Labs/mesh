open Syntax;
open Rresult;

exception TypeError(string);


let current_id = ref(0)
let next_id = () => {
  let id = current_id^;
  incr(current_id);
  id;
}

let reset_id = () => current_id := 0;

let new_var = (lvl) => TVar(ref(Free(next_id(), lvl)));
let new_quantified_var = () => TVar(ref(Quantified(next_id())));

module Env = {
  module StringMap = Map.Make(String);
  type env = StringMap.t(typ);

  let empty: env = StringMap.empty;
  let extend = (env, name, typ) => StringMap.add(name, typ, env);
  let lookup = (env, name) => StringMap.find(name, env)
}

let type_name_of_literal = fun 
  | Int(_) => TConst("int")
  | Float(_) => TConst("float")
  | String(_) => TConst("string")
  | Bool(_) => TConst("bool")
  | Unit => TConst("unit")
; 


let infer_exn = (e) =>
  switch (e) {
  | ELit(lit) => type_name_of_literal(lit)
  // | ELit(...) => 
  | _ => raise(TypeError("Not Implemented"))
  };


let infer = (e) =>
  try (R.ok @@ infer_exn(e)) {
  | TypeError(msg) => R.error_msg(msg)
  };