open Rresult;
open Lwt.Infix;

open Piaf;
open Graphql_ppx_base;

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

let introspection = (url) => {
  let introspection_query = {|
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }
    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }
    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }
    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }  
  |};

  let body = 
    `Assoc([("query", `String(introspection_query))])
    |> Yojson.Basic.to_string
    |> Body.of_string;

  Client.Oneshot.request(~body, ~meth=`POST, ~headers=[("Content-Type", "application/json")], url) >>= (resp) =>
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

let make_schema = (schema) => {  
  open Yojson.Basic.Util;
  open Schema;
  
  let schema =
    schema
    |> member("data")
    |> to_option(json => json |> member("__schema"))
    |> (
      fun
      | Some(json) => json
      | None => schema |> member("__schema")
    );  

  Read_schema.({
    meta: make_schema_meta(schema),
    type_map:
      schema |> member("types") |> to_list |> Array.of_list |> make_type_map,
    directive_map:
      schema
      |> member("directives")
      |> to_list
      |> Array.of_list
      |> make_directive_map,
  });
};

exception Graphql_error(string);

let parse_query = (query) => {
  let lexer = Graphql_lexer.make(query);
  // let delimLength =
  //   switch (delim) {
  //   | Some(s) => 2 + String.length(s)
  //   | None => 1
  //   };

  switch (Graphql_lexer.consume(lexer)) {
  | Result.Error(_) =>
    // Location.raise_errorf(
    //   ~loc=Location.none,
    //   "%s",
    //   fmt_lex_err(e.item),
    // )
    raise(Graphql_error("Parser error"))

  | Result.Ok(tokens) =>
    let parser = Graphql_parser.make(tokens);
    switch (Graphql_parser_document.parse_document(parser)) {
    | Result.Error(_) =>
      // Location.raise_errorf(
      //   ~loc=Location.none,
      //   "%s",
      //   fmt_parse_err(e.item),
      // )
      raise(Graphql_error("Parser error"))
    | Result.Ok(document) =>
      document
      // let document_with_config =
      //   Result_decoder.generate_config(
      //     ~map_loc=((_, _)) => Location.none,
      //     ~delimiter=None,
      //     ~initial_query_config=empty_query_config,
      //     document,
      //   );

      // document_with_config
      // |> Result_decoder.unify_document_schema
      // |> Output_bucklescript_module.generate_module_interfaces(module_name);
    };
  };};