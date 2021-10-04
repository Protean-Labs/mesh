open Rresult;
open R.Infix;

let () = {
  switch (
    Mesh.parse_file(Sys.argv[1])        >>= (ast) => 
    Lwt_main.run @@ Mesh.Eval.eval(ast) >>| fst    
  ) {
  | Error(`Msg(msg))  => print_endline(msg)
  | Ok(v)             => List.iter((v) => print_endline(Mesh.Eval.string_of_value(v)), v)
  }
};