exception Type_error(string);

module Env: {
  type t;

  let empty: t;
  let reset_id: t => t;
};

type infer_result = Lwt_result.t((list(Typetree.typ), Env.t), Rresult.R.msg);

let infer: (~env: Env.t=?) => (~level:int=?) => list(Parsetree.expr) => infer_result;