{
  open Parser

  exception SyntaxError of string
}

(* Helpers *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int     = '-'? digit+
let float   = '-'? digit+ '.' digit+
let string  = (alpha|digit|'_')*
let var     = ['a'-'z'] string 
let mod     = ['A'-'Z'] string

let operator = ('+' | '-' | '*' | '/' | '=' | '!' | '?' | '<' | '>' | '|' | '&' | ':' | '@')+

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

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

| operator as op    { OPERATOR (op) }

(* Literals *)
| "()"              { UNIT }
| "true"            { BOOL (true) }
| "false"           { BOOL (false) }
| int as lit        { INT (int_of_string lit) }
| float as lit      { FLOAT (float_of_string lit) }
| '"'               { read_string (Buffer.create 16) lexbuf }
| var as varname    { VAR (varname) }
| mod as modname    { MOD (modname) }
| eof               { EOF }
| _                 { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| '"'               { STRING (Buffer.contents buf) }
| [^ '"' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
| eof               { raise (SyntaxError ("String is not terminated")) }
| _                 { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }