open Syntax

exception InvalidPattern(string);

/** [pattern_of_expr(e)] returns a pattern [p] that matches the expression. 
    Used to convert "tuples" to function argument patterns. Raises 
    [InvalidPattern] if the epxression does not have a pattern equivalent. */
let rec pattern_of_expr = fun
  | ELit(lit)      => PLit(lit)
  | EVar(name)     => PVar(name)
  | ETuple(exprs)  => PTuple(List.map(pattern_of_expr, exprs))
  | e              => raise(InvalidPattern(string_of_expr(0, e)))
;

let fmt_fun_pattern = fun
  | ETuple(l)   => List.map(pattern_of_expr, l)
  | _           => raise(InvalidPattern("function argument pattern is not a tuple"))
;

/** [fold_fun(body, args)] returns a new expression [e = EFun(...)] containing
    nested functions (one for each argument). [body] is a value of type {expr}
    containing the body of the function and [args] is a list of values of type 
    {pattern} representing the arguments of the function. 
    
    E.g.: [fold_fun(e, [PVar("a"), PVar("b")])] would return the following 
    AST subtree: 
    
    (EFun PVar a =>
      (EFun PVar b =>
        e)) */
let fold_fun = (body, args) => 
  List.fold_right((arg, acc) => EFun(arg, acc), args, body);

/** [fold_app(e, args)] returns a new expression [e' = EApp(...)] containing
    nested function applications [EApp] (one for each argument). [e] is a 
    value of type {expr} representing the function that should be applied 
    to the arguments and [args] is a list of values of type {expr} containing 
    the arguments of the function.

    E.g.: [fold_app(EVar("f"), [EVar("a"), EVar("b")])] would return the following 
    AST subtree: 
    
    (EApp 
      (EApp (EVar f) (EVar a))
    (EVar b)) */
let fold_app = (e, args) =>
  List.fold_left((acc, arg) => EApp(acc, arg), e, args);

/** [fold_cons(ele, e)] 

  EApp(EVar("cons"), )*/
let fold_cons = (ele, e) =>
  List.fold_right((ele, acc) => EApp(EApp(EVar("cons"), ele), acc), ele, e);