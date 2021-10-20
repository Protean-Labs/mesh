open Jupyter_kernel;
module K = Client.Kernel;

open Lwt_result.Infix;
open Mesh;

let exec = (~count, src) =>
  Mesh.parse_eval(src)                                              |> (result) =>
  Lwt_result.bind_lwt_err(result, (`Msg(msg)) => Lwt.return @@ msg) >|= ((values, _)) => {
    let str_values = List.map(Eval.string_of_value, values);
    let pp_result = [%string "\n%{string_of_int count}:"] ++ String.concat(",\n", str_values);
    K.{msg: Option.some(pp_result), actions: []}
  };
  
let kernel = K.make(
  ~language_version=[0,0,1],
  ~language="mesh",
  ~exec,
  ()
);

let config = Client_main.mk_config(~usage="Mesh language", ());

let () = Lwt_main.run(Client_main.main(~config, ~kernel));