{
  open Parser

  exception Syntax_error of string

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c
}

(* Helpers *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int     = '-'? digit+
let float   = '-'? digit+ '.' digit*
let string  = (alpha|digit|'_')*
let lident     = ['a'-'z'] string 
let uident     = ['A'-'Z'] string

let operator = ('+' | '-' | '*' | '/' | '=' | '!' | '?' | '<' | '>' | '|' | '&' | ':' | '@' | '~')+

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule token = parse
(* White space *)
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }

| '_'               { UNDERSCORE }
| '='               { EQUALS }
| "=>"              { ARROW }
| "..."             { DOTDOTDOT }

| ';'               { SEMICOLON }
| ':'               { COLON }
| '['               { LBRACK }
| ']'               { RBRACK }
| '('               { LPAREN }
| ')'               { RPAREN }
| '{'               { LBRACE }
| '}'               { RBRACE }
| ','               { COMMA }
| '.'               { DOT }

| "let"             { LET }
| "module"          { MODULE }
| "esfun"           { ES6_FUN }
| "external"        { EXTERNAL }
| "open"            { OPEN }

| operator as op    { OPERATOR (op) }

(* Literals *)
| "()"              { UNIT }
| "{}"              { EMPTY }
| "true"            { BOOL (true) }
| "false"           { BOOL (false) }
| int as lit        { INT (int_of_string lit) }
| float as lit      { FLOAT (float_of_string lit) }
| '"'               { read_string (Buffer.create 16) lexbuf }
| "```" lident as extname
  { read_extension (String.sub extname 3 (String.length extname - 3)) (Buffer.create 16) lexbuf }
| lident as lident  { LIDENT (lident) }
| uident as uident  { UIDENT (uident) }
| eof               { EOF }
| _                 { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| '"'               { STRING (Buffer.contents buf) }
| '\\' (backslash_escapes as c) 
  { Buffer.add_char buf (char_for_backslash c);
    read_string buf lexbuf }
| [^ '"' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
| eof               { raise (Syntax_error ("String is not terminated")) }
| _                 { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_extension name buf = parse
| "```"             { EXTENSION (name, Buffer.contents buf) }
| [^ '`' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_extension name buf lexbuf}
| eof               { raise (Syntax_error ("Extension block is not terminated")) }
| _                 { raise (Syntax_error ("Illegal extension block character: " ^ Lexing.lexeme lexbuf)) }
