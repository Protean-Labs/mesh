{
  open Parser

  exception SyntaxError of string
}

(* Helpers *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int     = '-'? digit+
let float   = '-'? digit+ '.' digit*
let string  = (alpha|digit|'_')*
let lident     = ['a'-'z'] string 
let uident     = ['A'-'Z'] string

let operator_symbol = ('+' | '-' | '*' | '/' | '=' | '!' | '?' | '<' | '>' | '|' | '&' | ':' | '@' | '~')
let operator = operator_symbol (operator_symbol | '.')*

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
(* White space *)
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }
| "//"              { read_single_line_comment lexbuf }

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
| lident as lident  { LIDENT (lident) }
| uident as uident  { UIDENT (uident) }
| eof               { EOF }
| _                 { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| '"'               { STRING (Buffer.contents buf) }
| [^ '"' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
| eof               { raise (SyntaxError ("String is not terminated")) }
| _                 { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
| newline           { Lexing.new_line lexbuf; token lexbuf }
| eof               { EOF }
| _                 { read_single_line_comment lexbuf }