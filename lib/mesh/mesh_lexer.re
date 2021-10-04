open Parser;

/** Module containing intermediate functions used between lexing and parsing.
    Adapted from Reason's `reason_lexer.ml` (https://github.com/reasonml/reason) */ 

type positioned('a) = ('a, Lexing.position, Lexing.position);

type t = {
  lexbuf: Lexing.lexbuf,
  // mutable comments: list((string, Location.t)),
  mutable queued_tokens: list(positioned(token)),
  mutable queued_exn: option(exn),
  mutable last_cnum: int,
  mutable completion_ident_offset: int,
  completion_ident_pos: Lexing.position,
};

let init = (~insert_completion_ident=?, lexbuf) => {
  let (completion_ident_offset, completion_ident_pos) =
    switch (insert_completion_ident) {
    | None => (min_int, Lexing.dummy_pos)
    | Some(pos) => (pos.Lexing.pos_cnum, pos)
    };

  {
    lexbuf,
    // comments: [],
    queued_tokens: [],
    queued_exn: None,
    last_cnum: (-1),
    completion_ident_offset,
    completion_ident_pos,
  };
};

let lexbuf = state => state.lexbuf;

let token = state =>
  switch (Lexer.token(state.lexbuf)) {
  // | [@implicit_arity] COMMENT(s, comment_loc) =>
  //   state.comments = [(s, comment_loc), ...state.comments];
  //   token(state);
  | tok => tok
  };

/* Routines for manipulating lexer state */

let save_triple = (lexbuf, tok) => (
  tok,
  lexbuf.Lexing.lex_start_p,
  lexbuf.Lexing.lex_curr_p,
);

let fake_triple = (t, (_, pos, _)) => (t, pos, pos);

/* insert ES6_FUN */

exception Lex_balanced_failed(list(positioned(token)), option(exn));

let closing_of = fun
  | LPAREN => RPAREN
  | LBRACE => RBRACE
  | _ => assert(false)
;

let inject_es6_fun = fun
  | [tok, ...acc] => [tok, fake_triple(ES6_FUN, tok), ...acc]
  | _ => assert(false)
;

// TODO: Make sure removing COLON from trigerring token list doesn't break anything
let is_triggering_token = fun
  // | ARROW | COLON => true
  | ARROW  => true
  | _      => false
;

let rec lex_balanced_step = (state, closing, acc, tok) => {
  let lexbuf = state.lexbuf;
  let acc = [save_triple(lexbuf, tok), ...acc];
  switch (tok, closing) {
  | (RPAREN, RPAREN)     => acc
  | (RBRACE, RBRACE)
  | (RBRACK, RBRACK) => acc
  | (RPAREN | RBRACE | RBRACK | EOF, _) =>
    raise([@implicit_arity] Lex_balanced_failed(acc, None))
  | (
      LBRACK 
      // | LBRACKLESS | LBRACKGREATER | LBRACKAT | LBRACKPERCENT |
      // LBRACKPERCENTPERCENT
      ,_,
    ) =>
    lex_balanced(state, closing, lex_balanced(state, RBRACK, acc))
  | (LPAREN | LBRACE, _) =>
    let rparen =
      try (lex_balanced(state, closing_of(tok), [])) {
      | [@implicit_arity] Lex_balanced_failed(rparen, None) =>
        raise([@implicit_arity] Lex_balanced_failed(rparen @ acc, None))
      };

    switch (token(state)) {
    | exception exn =>
      raise([@implicit_arity] Lex_balanced_failed(rparen @ acc, Some(exn)))
    | tok' =>
      let acc =
        if (is_triggering_token(tok')) {
          inject_es6_fun(acc);
        } else {
          acc;
        };
      lex_balanced_step(state, closing, rparen @ acc, tok');
    };
  | (VAR(_) | UNDERSCORE, _) =>
    switch (token(state)) {
    | exception exn =>
      raise([@implicit_arity] Lex_balanced_failed(acc, Some(exn)))
    | tok' =>
      let acc =
        if (is_triggering_token(tok')) {
          inject_es6_fun(acc);
        } else {
          acc;
        };
      lex_balanced_step(state, closing, acc, tok');
    }
  /* `...` with a closing `}` indicates that we're definitely not in an es6_fun
   * Image the following:
   *    true ? (Update({...a, b: 1}), None) : x;
   *    true ? ({...a, b: 1}) : a;
   *    true ? (a, {...a, b: 1}) : a;
   * The lookahead_esfun is triggered initiating the lex_balanced procedure.
   * Since we now "over"-parse spread operators in pattern position (for
   * better errors), the ... pattern in ({...a, b: 1}) is now a valid path.
   * This means that the above expression `({...a, b: 1}) :` is seen as a pattern.
   * I.e. the arguments of an es6 function: (pattern) :type => expr
   * We exit here, to indicate that an expression needs to be parsed instead
   * of a pattern.
   */
  | (DOTDOTDOT, RBRACE) => acc
  | _ => lex_balanced(state, closing, acc)
  };
}
and lex_balanced = (state, closing, acc) =>
  switch (token(state)) {
  | exception exn =>
    raise([@implicit_arity] Lex_balanced_failed(acc, Some(exn)))
  | tok => lex_balanced_step(state, closing, acc, tok)
  };

let lookahead_esfun = (state, (tok, _, _) as lparen) =>
  switch (lex_balanced(state, closing_of(tok), [])) {
  | exception ([@implicit_arity] Lex_balanced_failed(tokens, exn)) =>
    state.queued_tokens = List.rev(tokens);
    state.queued_exn = exn;
    lparen;
  | tokens =>
    switch (token(state)) {
    | exception exn =>
      state.queued_tokens = List.rev(tokens);
      state.queued_exn = Some(exn);
      lparen;
    | token =>
      let tokens = [save_triple(state.lexbuf, token), ...tokens];
      if (is_triggering_token(token)) {
        state.queued_tokens = [lparen, ...List.rev(tokens)];
        fake_triple(ES6_FUN, lparen);
      } else {
        state.queued_tokens = List.rev(tokens);
        lparen;
      };
    }
  };

let token = state => {
  let lexbuf = state.lexbuf;
  switch (state.queued_tokens, state.queued_exn) {
  | ([], Some(exn)) =>
    state.queued_exn = None;
    raise(exn);
  | ([(LPAREN, _, _) as lparen], None) => lookahead_esfun(state, lparen)
  | ([(LBRACE, _, _) as lparen], None) => lookahead_esfun(state, lparen)
  | ([], None) =>
    switch (token(state)) {
    | (LPAREN | LBRACE) as tok =>
      lookahead_esfun(state, save_triple(state.lexbuf, tok))
    | (VAR(_) | UNDERSCORE) as tok =>
      let tok = save_triple(lexbuf, tok);
      switch (token(state)) {
      | exception exn =>
        state.queued_exn = Some(exn);
        tok;
      | tok' =>
        if (is_triggering_token(tok')) {
          state.queued_tokens = [tok, save_triple(lexbuf, tok')];
          fake_triple(ES6_FUN, tok);
        } else {
          state.queued_tokens = [save_triple(lexbuf, tok')];
          tok;
        }
      };
    | token => save_triple(lexbuf, token)
    }
  | ([x, ...xs], _) =>
    state.queued_tokens = xs;
    x;
  };
};

let token = state => {
  let space_start = state.last_cnum;
  let (token', start_p, curr_p) as token = token(state);
  let token_start = start_p.Lexing.pos_cnum;
  let token_stop = curr_p.Lexing.pos_cnum;
  state.last_cnum = token_stop;
  if (state.completion_ident_offset > min_int
      && space_start <= state.completion_ident_offset
      && token_stop >= state.completion_ident_offset) {
    switch (token') {
    // | UIDENT(_)
    | VAR(_) when token_start <= state.completion_ident_offset =>
      state.completion_ident_offset = min_int;
      token;
    | _ =>
      state.queued_tokens = [token, ...state.queued_tokens];
      state.completion_ident_offset = min_int;
      (VAR("_"), state.completion_ident_pos, state.completion_ident_pos);
    };
  } else {
    token;
  };
};

let string_of_token = fun
  | UNIT => "UNIT"
  | BOOL(_) => [%string "BOOL"]
  | INT(_) => [%string "INT"]
  | FLOAT(_) => [%string "FLOAT"]
  | STRING(s) => [%string "STRING %{s}"]
  | VAR(s) => [%string "VAR %{s}"]
  | MOD(s) => [%string "MOD %{s}"]
  | OPERATOR(s) => [%string "OPERATOR %{s}"]
  | LET => "LET"
  | MODULE => "MODULE"
  | ES6_FUN => "ES6_FUN"
  | EXTERNAL => "EXTERNAL"
  | OPEN => "OPEN"
  | SEMICOLON => "SEMICOLON"
  | COLON => "COLON"
  | LBRACK => "LBRACK" 
  | RBRACK => "RBRACK"
  | LPAREN => "LPAREN" 
  | RPAREN => "RPAREN"
  | LBRACE => "LBRACE" 
  | RBRACE => "RBRACE"
  | COMMA => "COMMA"
  | DOT => "DOT"
  | UNDERSCORE => "UNDERSCORE"
  | EQUALS => "EQUALS"
  | ARROW => "ARROW"
  | DOTDOTDOT => "DOTDOTDOT"
  | EOF => "EOF"
;
