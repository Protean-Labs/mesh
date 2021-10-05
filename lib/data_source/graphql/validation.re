open Graphql_ppx_base;
open Source_pos;

let add_loc = (delimLength, base, span) => {
  let (_, _, col) = Ocaml_common.Location.get_pos_info(base.loc_start);
  let pos_bol_start =
    base.loc_start.pos_bol
    + col
    + delimLength
    + fst(span).index
    - fst(span).col;
  let pos_bol_end =
    base.loc_start.pos_bol
    + col
    + delimLength
    + snd(span).index
    - snd(span).col;
  let start = pos_bol_start + fst(span).col;
  let end_ = pos_bol_end + snd(span).col;
  {
    loc_start: {
      pos_fname: base.loc_start.pos_fname,
      pos_lnum: base.loc_start.pos_lnum + fst(span).line,
      pos_bol: pos_bol_start,
      pos_cnum: start,
    },
    loc_end: {
      pos_fname: base.loc_start.pos_fname,
      pos_lnum: base.loc_start.pos_lnum + snd(span).line,
      pos_bol: pos_bol_end,
      pos_cnum: end_,
    },
    loc_ghost: false,
  };
};

let fmt_lex_err = err =>
  Graphql_lexer.(
    switch (err) {
    | Unknown_character(ch) => Printf.sprintf("Unknown character %c", ch)
    | Unexpected_character(ch) =>
      Printf.sprintf("Unexpected character %c", ch)
    | Unterminated_string => Printf.sprintf("Unterminated string literal")
    | Unknown_character_in_string(ch) =>
      Printf.sprintf("Unknown character in string literal: %c", ch)
    | Unknown_escape_sequence(s) =>
      Printf.sprintf("Unknown escape sequence in string literal: %s", s)
    | Unexpected_end_of_file => Printf.sprintf("Unexpected end of query")
    | Invalid_number => Printf.sprintf("Invalid number")
    }
  );

let fmt_parse_err = err =>
  Graphql_parser.(
    switch (err) {
    | Unexpected_token(t) =>
      Printf.sprintf("Unexpected token %s", Graphql_lexer.string_of_token(t))
    | Unexpected_end_of_file => "Unexpected end of query"
    | Lexer_error(err) => fmt_lex_err(err)
    }
  );

// let make_error_expr = (loc, message) => {
//   let error = Ocaml_common.Location.error(~loc, message);
//   let ext = Ocaml_common.Ast_mapper.extension_of_error(error);
//   let extension = Ocaml_common.Ast_helper.Exp.extension(~loc, ext);

//   To_ppxlib.copy_expression(extension);
// };

// let run_validations = (config, definition) => {
//   switch (Validations.run_validators(config, [definition])) {
//   | (Some(errs), _) =>
//     Some(
//       errs
//       |> List.rev
//       |> List.map(((loc, msg)) => {
//            let loc = conv_loc(loc);
//            %stri
//            [%e make_error_expr(loc, msg)];
//          }),
//     )
//   | (None, warnings) =>
//     warnings
//     |> List.iter(((loc, message)) => {
//          let loc = conv_loc(loc);
//          Ocaml_common.Location.print_warning(
//            loc,
//            Ocaml_common.Location.formatter_for_warnings^,
//            Ocaml_common.Warnings.Preprocessor(message),
//          );
//        });
//     None;
//   };
// };

let validate = (schema, loc, delim, query, ()) => {
  let lexer = Graphql_lexer.make(query);
  let delimLength =
    switch (delim) {
    | Some(s) => 2 + String.length(s)
    | None => 1
    };

  switch (Graphql_lexer.consume(lexer)) {
  | Result.Error(e) =>
    Location.raise_errorf(
      ~loc=add_loc(delimLength, loc, e.span),
      "%s",
      fmt_lex_err(e.item),
    )

  | Result.Ok(tokens) =>
    let parser = Graphql_parser.make(tokens);
    switch (Graphql_parser_document.parse_document(parser)) {
    | Result.Error(e) =>
      Location.raise_errorf(
        ~loc=add_loc(delimLength, loc, e.span),
        "%s",
        fmt_parse_err(e.item),
      )

    | Result.Ok(document) =>
      // let document_with_config =
      //   Result_decoder.generate_config(
      //     ~map_loc=add_loc(delimLength, loc),
      //     ~delimiter=delim,
      //     ~query_config=empty_query_config(schema),
      //     document,
      //   );

      let document_with_config =
        List.map(
          (def) => (def, Generator_utils.{
            map_loc: add_loc(delimLength, loc),
            delimiter: delim,
            schema,
            full_document: document,
            template_tag: (None, None, None),
            template_tag_return_type: None,
            template_tag_is_function: None,
            inline: false,
            future_added_value: false,
            extend: None,
            fragment_in_query: Include,
            native: true,
          }),
          document
        );

      document_with_config
      |> Result_decoder.unify_document_schema
      // |> Output_bucklescript_module.generate_module_interfaces(module_name);
    };
  };
};

// let rewrite_definition =
//     (
//       ~query_config: query_config,
//       ~loc,
//       ~delim,
//       ~query,
//       ~module_name,
//       ~module_type,
//       (),
//     ) => {
//   let lexer = Graphql_lexer.make(query);
//   let delimLength =
//     switch (delim) {
//     | Some(s) => 2 + String.length(s)
//     | None => 1
//     };

//   switch (Graphql_lexer.consume(lexer)) {
//   | Result.Error(e) =>
//     Location.raise_errorf(
//       ~loc=add_loc(delimLength, loc, e.span),
//       "%s",
//       fmt_lex_err(e.item),
//     )
//   | Result.Ok(tokens) =>
//     let parser = Graphql_parser.make(tokens);
//     switch (Graphql_parser_document.parse_document(parser)) {
//     | Result.Error(e) =>
//       Location.raise_errorf(
//         ~loc=add_loc(delimLength, loc, e.span),
//         "%s",
//         fmt_parse_err(e.item),
//       )
//     | Result.Ok(document) =>
//       let document_with_config =
//         Result_decoder.generate_config(
//           ~map_loc=add_loc(delimLength, loc),
//           ~delimiter=delim,
//           ~initial_query_config=query_config,
//           document,
//         );

//       let errors =
//         document_with_config
//         |> List.fold_left(
//              (acc, (definition, config)) => {
//                switch (run_validations(config, definition)) {
//                | Some(error) => [error, ...acc]
//                | None => acc
//                }
//              },
//              [],
//            )
//         |> List.rev;

//       switch (errors) {
//       | [] =>
//         try(
//           document_with_config
//           |> Result_decoder.unify_document_schema
//           |> Output_bucklescript_module.generate_modules(
//                module_name,
//                module_type,
//              )
//         ) {
//         | Output_bucklescript_module.Cant_find_fragment_type_with_loc(
//             location,
//             fragment_type,
//           ) => [
//             [%stri
//               [%e
//                 make_error_expr(
//                   Output_bucklescript_utils.conv_loc(location),
//                   "Can't find fragment type: " ++ fragment_type,
//                 )
//               ]
//             ],
//           ]
//         }
//       | errors => errors |> List.concat
//       };
//     };
//   };
// };