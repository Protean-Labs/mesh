module Syntax = Syntax;
module Syntax_util = Syntax_util;
module Infer = Infer;
module Eval = Eval;

open Rresult;

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

let parse_file = (source) => {
  let lexer = Mesh_lexer.init(Lexing.from_string(source));
  let parser = Parser.Incremental.file(Mesh_lexer.lexbuf(lexer).Lexing.lex_curr_p);

  // NOTE: Alternative lexer token supplier that prints each token that is being parsed.
  // Useful for debugging
  // let supplier = () => 
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
    | Ok(ast) => List.map(Syntax_util.string_of_expr, ast) |> String.concat("\n")
    | Error(`Msg(msg) ) => msg
  ;

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

let (std_env, std_tenv) = {
  logger#debug("Location: %s", Sys.executable_name);

  let paths = [
    "../../../stdlib",
    "stdlib",
    [%string "%{Unix.getenv \"HOME\"}/.local/share/jupyter/kernels/mesh/stdlib"]
  ];

  // TODO: Fix stdlib path

  (
    switch (
      R.bind(
        R.bind(existing_path(paths), (path) => parse_file(read_file([%string "%{path}/stdlib.mesh"]))), 
        Eval.eval
      )
    ) {
    | Ok((_, env)) => env
    | Error(`Msg(msg)) => raise(Eval.Runtime_error(msg))
    },
    // switch (
    //   R.bind(
    //     R.bind(existing_path(paths), (path) => parse_file(read_file([%string "%{path}/stdlib.mesh"]))), 
    //     Infer.infer
    //   )
    // ) {
    // | Ok((_, env)) => env
    // | Error(`Msg(msg)) => raise(Eval.Runtime_error(msg))
    // }
    Infer.Env.empty
  )
};

let parse_eval = (source) => 
  R.bind(
    R.map((ast) => 
      Syntax_util.[mk_expr(EOpen([], "Stdlib")), ...ast], 
      parse_file(source)
    ), 
    Eval.eval(~env=std_env)
  );

let parse_infer = (source) =>
  R.bind(
    R.map((ast) => 
      // Syntax_util.[mk_expr(EOpen([], "Stdlib")), ...ast], 
      ast,
      parse_file(source)
    ), 
    Infer.infer(~env=std_tenv)
  );

  // parse_file(source) >>= Infer.infer(Infer.Env.empty,0) |> (result) =>
  // switch (result) {
  // | Error(`Msg(msg)) => {print_endline(msg); ([], Infer.Env.empty)}
  // | Ok((typs, env)) => (typs, env)
  // }
