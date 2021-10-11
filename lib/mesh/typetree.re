type id = int;
type level = int;
type name = string;

type typ =
  | TConst(name)
  | TFun(typ, typ)
  | TApp(typ, typ)
  | TTuple(list(typ))
  | TList(typ)
  | TVar(ref(tvar))
  | TRec(typ)
  | TRowEmpty
  | TRowExtend(name, typ, typ)
  | TOpt(typ)
  | TTag(typ)
  | TMod(tenv)
and tvar = 
  | Free(id, level)
  | Constrained(typ)
  | Quantified(id)
and tenv = list((name, typ));