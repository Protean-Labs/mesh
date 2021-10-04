open Jupyter_kernel;
module K = Client.Kernel;

open Lwt.Infix;
open Rresult;

// open Mesh.Syntax_util;
open Mesh;

// let exec = (~count, src) => 
//   Lwt.return @@ switch (Mesh.parse_file(src)) {
//   | Error(`Msg(msg)) => Result.error(msg)
//   | Ok(ast) =>
//     List.map(string_of_expr, ast)    |> (s) =>
//     [%string "\n%{string_of_int count}:"] ++ String.concat(",\n", s)     |> (pp_ast) =>
//     Result.ok @@ K.{msg: Option.some(pp_ast), actions: []}
//   };

let exec = (~count, src) => {
  Lwt_result.bind(Lwt.return @@ Mesh.parse_file(src), Eval.eval) >|= (result) =>
  switch (result) {
  | Error(`Msg(msg)) => Result.error(msg)
  | Ok(result) =>
    List.map(Eval.string_of_value, fst(result))    |> (s) =>
    [%string "\n%{string_of_int count}:"] ++ String.concat(",\n", s)     |> (pp_ast) =>
    Result.ok @@ K.{msg: Option.some(pp_ast), actions: []}
  };
};

let kernel = K.make(
  ~language_version=[0,0,1],
  ~language="mesh",
  ~exec,
  ()
);

let config = Client_main.mk_config(~usage="Mesh language", ());

let () = Lwt_main.run(Client_main.main(~config, ~kernel));