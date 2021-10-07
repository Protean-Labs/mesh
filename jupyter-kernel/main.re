open Jupyter_kernel;
module K = Client.Kernel;

// open Mesh.Syntax_util;

let exec = (~count, src) => 
  Lwt.return @@ switch (
    Mesh.parse_eval(src)
  ) {
  | Error(`Msg(msg)) => Result.error(msg)
  | Ok((values, _)) =>
    List.map(Mesh.Eval.string_of_value, values)    |> (s) =>
    [%string "\n%{string_of_int count}:"] ++ String.concat(",\n", s)     |> (pp_values) =>
    Result.ok @@ K.{msg: Option.some(pp_values), actions: []}
  };

let kernel = K.make(
  ~language_version=[0,0,1],
  ~language="mesh",
  ~exec,
  ()
);

let config = Client_main.mk_config(~usage="Mesh language", ());

let () = Lwt_main.run(Client_main.main(~config, ~kernel));