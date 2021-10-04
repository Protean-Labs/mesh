open Rresult;
open Lwt.Infix;

// open Piaf;
open Graphql_ppx_base;

let introspection = (uri) => {
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

  Client.query(uri, introspection_query)
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
      schema 
      |> member("types") 
      |> to_list 
      |> Array.of_list 
      |> make_type_map,
    directive_map:
      schema
      |> member("directives")
      |> to_list
      |> Array.of_list
      |> make_directive_map,
  });
};

let get_schema = (uri) => 
  introspection(uri) >|= (raw_schema) =>
  R.map(make_schema, raw_schema);