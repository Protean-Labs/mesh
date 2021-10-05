open Graphql_ppx_base;
open Source_pos;

exception Graphql_syntax_error(string);
exception Graphql_parsing_error(string);

type t = Graphql_ast.document;

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

let lex = (~delim=None, ~loc=Location.none, query) => {
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
  | Result.Ok(tokens) => tokens
  }
};

let parse = (~delim=None, ~loc=Location.none, tokens) => {
  let parser = Graphql_parser.make(tokens);
  let delimLength =
    switch (delim) {
    | Some(s) => 2 + String.length(s)
    | None => 1
    };

  switch (Graphql_parser_document.parse_document(parser)) {
  | Result.Error(e) =>
    Location.raise_errorf(
      ~loc=add_loc(delimLength, loc, e.span),
      "%s",
      fmt_parse_err(e.item),
    )
  | Result.Ok(document) => document
  }
};