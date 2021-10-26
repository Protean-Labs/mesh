open Rresult;
open Lwt_result.Infix;

let () = {
  print_endline([%string {|$TEST_DIR: %{Sys.getenv("TEST_DIR")}|}])
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