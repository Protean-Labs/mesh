exception Parsetree_error(string);

let parse_extension_header = (header) =>
  switch (String.split_on_char('(', header)) {
  | [extname, extarg] => (
    String.sub(extname, 3, (String.length(extname) - 3)),
    String.sub(extarg, 0, (String.length(extarg) - 1))
  )
  | _ => raise(Parsetree_error([%string "extension header: %{header}"]))
  };