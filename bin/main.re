open Rresult;
open R.Infix;

let () = {
  Mesh.parse_file(Sys.argv[1]) >>= Mesh.Eval.eval >>| fst |> (result) =>
  switch (result) {
  | Error(`Msg(msg))  => print_endline(msg)
  | Ok(v)             => List.iter((v) => print_endline(Mesh.Eval.string_of_value(v)), v)
  }
};