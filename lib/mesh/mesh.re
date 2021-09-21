module Syntax = Syntax;
module Infer = Infer;
module Eval = Eval;

open Rresult;

let token = (lexbuf) => 
  Lexer.token(lexbuf)                               |> (token) =>
  print_endline(Mesh_lexer.string_of_token(token))  |> () => 
  token;

let parse_file = (source) => 
  try (Result.ok @@ Parser.file(Mesh_lexer.token, Lexing.from_string(source))) {
  // try (Result.ok @@ Parser.file(token, Lexing.from_string(source))) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

let string_of_ast = (source) =>
  parse_file(source)
  |> fun
    | Ok(ast) => List.map(Syntax.string_of_expr(0), ast) |> String.concat("\n")
    | Error(`Msg(msg) ) => msg
  ;
