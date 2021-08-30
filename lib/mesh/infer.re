open Rresult;

open Syntax;

exception TypeError(string);

let infer_exn = (e) => 
  switch (e) {
  | ELit(Int(_)) => TInt
  // | ELit(...) => 
  | _ => raise(TypeError("Not Implemented"))
  };

let infer = (e) =>
  try (R.ok @@ infer_exn(e)) {
  | TypeError(msg) => R.error_msg(msg)
  };