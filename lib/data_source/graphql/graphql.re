open Rresult;
open Lwt.Infix;

open Piaf;
// open Graphql_ppx_base;

module type QUERY = {
  type t;
  type t_vars;

  module Raw: {
    type t;
    type t_vars;
  };

  let vars_to_json: Raw.t_vars => Yojson.Basic.t;
  let serialize_vars: t_vars => Raw.t_vars;

  let unsafe_from_json: Yojson.Basic.t => Raw.t;
  let parse: Raw.t => t;
  let query: string;
};

let make_request = (type vars, type ret, module Q: QUERY with type t_vars = vars and type t = ret, vars, url) => {
  let vars = 
    Q.serialize_vars(vars) 
    |> Q.vars_to_json;

  let body = 
    `Assoc([("query", `String(Q.query)), ("variables", vars)])
    |> Yojson.Basic.to_string
    |> Body.of_string;

  Client.Oneshot.request(~body, ~meth=`POST, ~headers=[("Content-Type", "application/json")], url) >>= (resp) =>
  switch (resp) {
  | Error(msg)  => Lwt.return @@ R.error_msg(Error.to_string(msg))
  | Ok(resp)    => 
    Body.to_string(resp.body) >|= (body) =>
    switch (body) {
    | Error(msg) => R.error_msg(Error.to_string(msg))
    | Ok(body) =>
      switch (Yojson.Basic.from_string(body)) {
      | `Assoc([("data", data)])  => Q.unsafe_from_json(data) |> Q.parse |> R.ok;
      | _                         => R.error_msg("Bad graphgl response")
      }
    }
  };
};



exception Graphql_error(string);

// let parse_query = (query, loc, delim) => {
//   let lexer = Graphql_lexer.make(query);
//   let delimLength =
//     switch (delim) {
//     | Some(s) => 2 + String.length(s)
//     | None => 1
//     };

//   switch (Graphql_lexer.consume(lexer)) {
//   | Result.Error(e) =>
//     Location.raise_errorf(
//       ~loc=Location.none,
//       "%s",
//       fmt_lex_err(e.item),
//     )
//     raise(Graphql_error("Parser error"))

//   | Result.Ok(tokens) =>
//     let parser = Graphql_parser.make(tokens);
//     switch (Graphql_parser_document.parse_document(parser)) {
//     | Result.Error(_) =>
//       // Location.raise_errorf(
//       //   ~loc=Location.none,
//       //   "%s",
//       //   fmt_parse_err(e.item),
//       // )
//       raise(Graphql_error("Parser error"))
//     | Result.Ok(document) =>
//       document
//       // let document_with_config =
//       //   Result_decoder.generate_config(
//       //     ~map_loc=((_, _)) => Location.none,
//       //     ~delimiter=None,
//       //     ~initial_query_config=empty_query_config,
//       //     document,
//       //   );

//       // document_with_config
//       // |> Result_decoder.unify_document_schema
//       // |> Output_bucklescript_module.generate_module_interfaces(module_name);
//     };
//   };};

include Validation;
include Schema;

open Rresult;
open Lwt.Infix;

let query = (query, uri) =>
  Schema.get_schema(uri) >|= (schema) =>
  R.map(
    (schema) => validate(schema, Location.none, None, query, ()),
    schema
  );

let test = query({|
  query {
    country(code: "BR") {
      name
    }
  }
|}, Uri.of_string("https://countries.trevorblades.com/"));


/**
  test output:

  toplevel:

  Def_operation({
    variable_definitions:
      option(Source_pos.spanning(Graphql_ast.variable_definitions)),
    has_error: bool,
    operation: Source_pos.spanning(Graphql_ast.operation),
    inner: t,
  });

  inner:
    Res_object({
      name: "Query"
      fields: [
        Fr_named_field({
          name: "country"
          type_: Res_nullable({
            inner: Res_object({
              name: "Country"
              fields: [
                Fr_named_field({
                  name: "name"
                  type_: Res_string({...})
                })
              ]
            })
          })
        })
      ]
    })

 */