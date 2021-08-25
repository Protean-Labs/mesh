module Syntax = Syntax;

open Rresult;

let parse_toplevel = (source) => 
  try (Result.ok @@ Parser.toplevel(Lexer.token, Lexing.from_string(source))) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

let parse_file = (source) => 
  try (Result.ok @@ Parser.file(Lexer.token, Lexing.from_string(source))) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

let print_ast = (source) => {
  parse_toplevel(source)
  |> fun
    | Ok(ast) => Syntax.string_of_top(ast) |> print_endline
    | Error(_) => print_endline("error")
}