{
  open Parser

  exception ParsingError of string
}

rule token = parse
(* End *)
| "placeholder"     { PLACEHOLDER }
| eof               { EOF }
| _                 { raise (ParsingError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
