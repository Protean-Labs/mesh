open Jupyter_kernel;
module K = Client.Kernel;

open Mesh.Syntax;

let exec = (~count, src) => 
  Lwt.return @@ switch (Mesh.parse_file(src)) {
  | Error(`Msg(msg)) => Result.error(msg)
  | Ok(ast) =>
    List.map(string_of_expr(0), ast)    |> (s) =>
    [%string "\n%{string_of_int count}:"] ++ String.concat(",\n", s)     |> (pp_ast) =>
    Result.ok @@ K.{msg: Option.some(pp_ast), actions: []}
  };

let kernel = K.make(
  ~language_version=[0,0,1],
  ~language="mesh",
  ~exec,
  ()
);

let config = Client_main.mk_config(~usage="Mesh language", ());

let () = Lwt_main.run(Client_main.main(~config, ~kernel));