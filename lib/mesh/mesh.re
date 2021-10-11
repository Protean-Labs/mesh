module Parsetree = Parsetree;
module Parsetree_util = Parsetree_util;
module Typetree = Typetree;
module Infer = Infer;
module Eval = Eval;

open Rresult;
open Lwt_result.Infix;

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

let logger = Easy_logging.Logging.make_logger("Mesh", Debug, [Cli(Debug)]);

let read_file = (path) =>{
  let path = Fpath.v(path);
  logger#debug("Trying path %s", Fpath.filename(path));
  
  switch (File.read(path)) {
  | Ok(source) => source
  | Error(`Msg(msg)) => raise(Eval.Runtime_error(msg))
  };
};

let parse = (source) => {
  let lexer = Mesh_lexer.init(Lexing.from_string(source));
  let parser = Parser.Incremental.file(Mesh_lexer.lexbuf(lexer).Lexing.lex_curr_p);

  // NOTE: Alternative lexer token supplier that prints each token that is being parsed.
  // Useful for debugging
  // let supplier = () => 
  //   Mesh_lexer.token(lexer)                         |> ((tok, _, _) as token) =>
  //   print_endline(Mesh_lexer.string_of_token(tok))  |> () => 
  //   token;

  let supplier = () => Mesh_lexer.token(lexer);

  try(Lwt_result.return @@ Parser.MenhirInterpreter.loop(supplier, parser)) {
  | Lexer_util.Parsetree_error(err) => Lwt.return @@ R.error_msg(err)
  | exn => Lwt.return @@ R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  }
};

let parse_file = (path) => parse(read_file(path));

let string_of_ast = (source) => 
  Lwt.map(
    fun
    | Ok(ast) => List.map(Parsetree_util.string_of_expr, ast) |> String.concat("\n")
    | Error(`Msg(msg) ) => msg,
    parse(source)
  );

// Stdlib loading

let existing_path = (paths) =>
  List.fold_left((acc, path) => 
    logger#debug("Trying path %s", path) |> () =>
    switch (acc) {
    | Error(_) => 
      switch (Bos.OS.Dir.exists(Fpath.v(path))) {
      | Error(_) as err => err
      | Ok(false) => R.error_msg("Directory not found")
      | Ok(true) => Ok(path)
      }
    | Ok(_) as path => path
    },
    R.error_msg("Directory not found"),
    paths
  );

let std_env = {
  logger#debug("Location: %s", Sys.executable_name);

  let paths = [
    "../../../stdlib",
    "stdlib",
    [%string "%{Unix.getenv \"HOME\"}/.local/share/jupyter/kernels/mesh/stdlib"]
  ];

  // TODO: Fix stdlib path  
  Lwt.return @@ existing_path(paths)          >>= (path) =>
  parse_file([%string "%{path}/stdlib.mesh"]) >>= (ast) =>
  Lwt_result.both(
    Eval.eval(ast) >|= snd, 
    Infer.infer(ast) >|= snd
  );
};

let parse_eval = (source) => 
  std_env                                             >>= ((std_env, _)) =>
  parse(source)                                       >>= (ast) =>
  Parsetree_util.[mk_expr(EOpen([], "Stdlib")), ...ast]  |> (ast') =>
  Eval.eval(~env=std_env, ast')

  // R.bind(
  //   R.map((ast) => 
  //     Parsetree_util.[mk_expr(EOpen([], "Stdlib")), ...ast], 
  //     parse_file(source)
  //   ), 
  //   Eval.eval(~env=std_env)
  // );

let parse_infer = (source) =>
  std_env                                             >>= ((_, std_tenv)) =>
  parse(source)                                       >>= (ast) =>
  Parsetree_util.[mk_expr(EOpen([], "Stdlib")), ...ast]  |> (ast') =>
  Infer.infer(~env=std_tenv, ast')                    >|= ((tvars, tenv)) =>
  // Pop first type in [typs] since it will be unit due to the implicit
  // EOpen([], "Stdlib") prepended to mesh expressions
  switch (tvars) {
  | [_, ...rest] => (rest, tenv)
  | _ => (tvars, tenv)
  };

  // R.map(
  //   ((typs, env)) => 
  //     switch (typs) {
  //     | [_, ...rest] => (rest, env)
  //     | _ => (typs, env)
  //     },
  //   R.bind(
  //     R.map((ast) => 
  //       Parsetree_util.[mk_expr(EOpen([], "Stdlib")), ...ast],
  //       // ast,
  //       parse_file(source)
  //     ), 
  //     Infer.infer(~env=std_tenv)
  //   )
  // );

  // parse_file(source) >>= Infer.infer(Infer.Env.empty,0) |> (result) =>
  // switch (result) {
  // | Error(`Msg(msg)) => {print_endline(msg); ([], Infer.Env.empty)}
  // | Ok((typs, env)) => (typs, env)
  // }
