open Graphql_ppx_base;
open Source_pos;

let validate: (Schema.t, ast_location, option(string), string, unit) =>
list((Result_structure.definition, Generator_utils.output_config));