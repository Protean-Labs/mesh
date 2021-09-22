module Syntax = Syntax;
module Infer = Infer;
module Eval = Eval;

open Rresult;

let parse_file = (source) => {
  let lexer = Mesh_lexer.init(Lexing.from_string(source));
  let parser = Parser.Incremental.file(Mesh_lexer.lexbuf(lexer).Lexing.lex_curr_p);

  // NOTE: Alternative lexer token supplier that prints each token that is being parsed.
  // Useful for debugging
  // let verbose_supplier = () => 
  //   Mesh_lexer.token(lexer)                         |> ((tok, _, _) as token) =>
  //   print_endline(Mesh_lexer.string_of_token(tok))  |> () => 
  //   token;

  let supplier = () => Mesh_lexer.token(lexer);

  try(R.ok @@ Parser.MenhirInterpreter.loop(supplier, parser)) {
  | Lexer.SyntaxError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  }
};

let string_of_ast = (source) =>
  parse_file(source)
  |> fun
    | Ok(ast) => List.map(Syntax.string_of_expr(0), ast) |> String.concat("\n")
    | Error(`Msg(msg) ) => msg
  ;
