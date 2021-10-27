open Rresult;
open Lwt_result.Infix;

// let source = {|
// let first = ((a, b)) => a;
// first((0, 1));
// first(("hello", 10));
// |};

let () = {
  switch (
    Lwt_main.run @@ (
      // Mesh.parse(source)        >>= (ast) => 
      Mesh.parse(Sys.argv[1])        >>= (ast) => 
      Mesh.Eval.eval(ast) >|= fst
    )    
  ) {
  | Error(`Msg(msg))  => print_endline(msg)
  | Ok(v)             => List.iter((v) => print_endline(Mesh.Eval.string_of_value(v)), v)
  }
};