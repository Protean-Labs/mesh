exception Graphql_error(string);

module Client = Client;

let test = Client.validate(Uri.of_string("https://countries.trevorblades.com/"), {|
  query {
    country(code: "BR") {
      name
    }
  }
|});


/**
  test output:

  toplevel:

  Def_operation({
    variable_definitions: option(Source_pos.spanning(Graphql_ast.variable_definitions)),
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