open Rresult;
open Lwt_result.Infix;

let () = {
  switch (
    Lwt_main.run @@ (
      Mesh.parse(Sys.argv[1])        >>= (ast) => 
      Mesh.Eval.eval(ast) >|= fst
    )    
  ) {
  | Error(`Msg(msg))  => print_endline(msg)
  | Ok(v)             => List.iter((v) => print_endline(Mesh.Eval.string_of_value(v)), v)
  }
};