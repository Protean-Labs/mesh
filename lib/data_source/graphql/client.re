open Rresult;
open Lwt.Infix;

open Piaf;

let query = (uri, query) => {
  let body = 
    `Assoc([("query", `String(query))])
    |> Yojson.Basic.to_string
    |> Body.of_string;

  Client.Oneshot.request(~body, ~meth=`POST, ~headers=[("Content-Type", "application/json")], uri) >>= (resp) =>
  switch (resp) {
  | Error(msg)  => Lwt.return @@ R.error_msg(Error.to_string(msg))
  | Ok(resp)    => 
    Body.to_string(resp.body) >|= (body) =>
    switch (body) {
    | Error(msg) => R.error_msg(Error.to_string(msg))
    | Ok(body) => R.ok @@ Yojson.Basic.from_string(body)
    }
  };
};