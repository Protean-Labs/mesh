module Syntax = Syntax;

open Rresult;

let parse_toplevel = (source) => 
  try (Result.ok @@ Parser.toplevel(Lexer.token, Lexing.from_string(source))) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };