// ================================================================
// Misc
// ================================================================
let indent: int => string;

// ================================================================
// Literals helpers
// ================================================================
let mk_lit_int: int => Parsetree.literal;
let mk_lit_float: float => Parsetree.literal;
let mk_lit_string: string => Parsetree.literal;
let mk_lit_bool: bool => Parsetree.literal;
let mk_lit_unit: unit => Parsetree.literal;

let string_of_literal: Parsetree.literal => string;

// ================================================================
// Expression helpers
// ================================================================
let mk_expr: (~loc:Location.t=?) => Parsetree.expr_desc => Parsetree.expr;

let mk_elit: (~loc:Location.t=?) => Parsetree.literal => Parsetree.expr;
let mk_evar: (~loc:Location.t=?) => (~path:list(Parsetree.name)=?) => Parsetree.name => Parsetree.expr;

let mk_elit_int: (~loc:Location.t=?) => int => Parsetree.expr;
let mk_elit_float: (~loc:Location.t=?) => float => Parsetree.expr;
let mk_elit_string: (~loc:Location.t=?) => string => Parsetree.expr;
let mk_elit_bool: (~loc:Location.t=?) => bool => Parsetree.expr;
let mk_elit_unit: (~loc:Location.t=?) => unit => Parsetree.expr;

let string_of_expr: (~level:int=?) => (~print_loc:bool=?) => Parsetree.expr => string;

// ================================================================
// Pattern helpers
// ================================================================
let mk_pat: (~loc:Location.t=?) => Parsetree.pattern_desc => Parsetree.pattern;

let mk_pany: (~loc:Location.t=?) => unit => Parsetree.pattern;
let mk_pvar: (~loc:Location.t=?) => Parsetree.name => Parsetree.pattern;
let mk_plit: (~loc:Location.t=?) => Parsetree.literal => Parsetree.pattern;
let mk_ptuple: (~loc:Location.t=?) => list(Parsetree.pattern) => Parsetree.pattern;

let string_of_pattern: Parsetree.pattern => string;