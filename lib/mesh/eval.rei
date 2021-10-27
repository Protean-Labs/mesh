open Parsetree;

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
  | VOpt(option(value))
  | VGraphqlQuery(string, string, Extensions.Graphql.t);

type eval_result = Lwt_result.t((list(value), environment), Rresult.R.msg);

let eval: (~env:environment=?) => list(expr) => eval_result;

// ================================================================
// Values helpers
// ================================================================
let string_of_value: (~level:int=?) => value => string;
let string_of_env: environment => string;

let value_of_yojson: Yojson.Basic.t => value;
let value_to_yojson: value => Yojson.Basic.t;