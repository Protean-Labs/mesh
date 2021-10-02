// ================================================================
// Literals helpers
// ================================================================
let mk_lit_int: int => Syntax.literal;
let mk_lit_float: float => Syntax.literal;
let mk_lit_string: string => Syntax.literal;
let mk_lit_bool: bool => Syntax.literal;
let mk_lit_unit: unit => Syntax.literal;

let string_of_literal: Syntax.literal => string;

// ================================================================
// Expression helpers
// ================================================================
let mk_expr: (~loc:Location.t=?) => Syntax.expr_desc => Syntax.expr;

let mk_elit: (~loc:Location.t=?) => Syntax.literal => Syntax.expr;
let mk_evar: (~loc:Location.t=?) => (~path:list(Syntax.name)=?) => Syntax.name => Syntax.expr;

let mk_elit_int: (~loc:Location.t=?) => int => Syntax.expr;
let mk_elit_float: (~loc:Location.t=?) => float => Syntax.expr;
let mk_elit_string: (~loc:Location.t=?) => string => Syntax.expr;
let mk_elit_bool: (~loc:Location.t=?) => bool => Syntax.expr;
let mk_elit_unit: (~loc:Location.t=?) => unit => Syntax.expr;

let string_of_expr: (~level:int=?) => (~print_loc:bool=?) => Syntax.expr => string;

// ================================================================
// Pattern helpers
// ================================================================
let mk_pat: (~loc:Location.t=?) => Syntax.pattern_desc => Syntax.pattern;

let mk_pany: (~loc:Location.t=?) => unit => Syntax.pattern;
let mk_pvar: (~loc:Location.t=?) => Syntax.name => Syntax.pattern;
let mk_plit: (~loc:Location.t=?) => Syntax.literal => Syntax.pattern;
let mk_ptuple: (~loc:Location.t=?) => list(Syntax.pattern) => Syntax.pattern;

let string_of_pattern: Syntax.pattern => string;