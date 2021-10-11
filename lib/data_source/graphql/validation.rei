open Graphql_ppx_base;
open Source_pos;

let validate: (Schema.t, ast_location, option(string), Graphql_ast.document, unit) =>
list((Result_structure.definition, Generator_utils.output_config));