module Syntax = Syntax;
module Infer = Infer;

open Rresult;

let parse_file = (source) => 
  try (Result.ok @@ Parser.file(Lexer.token, Lexing.from_string(source))) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

let string_of_ast = (source) =>
  parse_file(source)
  |> fun
    | Ok(ast) => List.map(Syntax.string_of_expr(0), ast) |> String.concat("\n")
    | Error(`Msg(msg) ) => msg
  ;
