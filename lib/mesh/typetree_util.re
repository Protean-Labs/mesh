open Typetree;
open Parsetree_util;

let string_of_typ = (typ) => {
  let id_name_map = Hashtbl.create(10);
  let count = ref(0);

  let next_name = () => {
    let i = count^;
    incr(count);
    let tvar_char = String.make(1, (Char.chr(97 + i mod 26)));
    let tvar_index = i > 26 ? string_of_int(1 / 26) : ""; 
    [%string "%{tvar_char}%{tvar_index}"];
  }

  // let concat_typ_strings = (f, typ_list) => List.map(f(false), typ_list) |>  String.concat(", ");

  // let rec_to_string = (f,row:tenv) => List.map(((name, typ)) => [%string "%{name}: %{f false typ}"], row) |>  String.concat(", ");

  let rec f = (~level=0, ~is_simple=false, typ) => {
    let indent = indent(level);
    switch(typ) {
    | TConst(name)          => [%string "%{indent}(TConst %{name})"]
    | TApp(ftyp, param_typ) => [%string "%{indent}(TApp %{f ~is_simple:true ftyp}[%{f param_typ}])"]
    | TFun(param_typ, rtyp) => 
      let arrow_typ_string = [%string "%{indent}(TFun\n%{f ~level:(level + 1) ~is_simple:true param_typ}\n%{f rtyp})"]
      is_simple ? [%string "%{indent}(TFun %{arrow_typ_string})"] : arrow_typ_string;
    | TTuple(typs)          => 
      let elements = List.map((ele) => f(~level=level + 1, ele), typs) |> String.concat("\n");
      [%string "%{indent}(TTuple \n%{elements})"]
    | TList(typ)            => [%string "(TList %{f typ})"]
    | TMod(tenv)               => 
      let typ_strings = List.map(((name, typ)) => [%string "%{name}: %{f ~level:(level + 1) ~is_simple:true typ})"], tenv) |> String.concat("\n");
      [%string "Module \n%{typ_strings}}"];
    | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_name_map, id)) {
        | Not_found => {
          let name = next_name();
          Hashtbl.add(id_name_map, id, name);
          [%string "(TVar Quantified %{name} %{string_of_int id})"]; 
        }
      }
    }
    | TVar({contents: Free(id, _)}) => [%string "(TVar Free %{string_of_int id})"]
    | TVar({contents: Constrained(typ)}) => [%string "(TVar Constrained %{f ~is_simple typ})"]
    | TRec(row) => [%string "(TRec\n%{f ~level:(level+1) row})"]
    | TRowEmpty => "TRowEmpty"
    | TRowExtend(name, field_typ, rest) => [%string "(TRowExt %{name}\n%{f ~level:(level+1) ~is_simple field_typ}\n%{f ~level:(level+1) ~is_simple rest})"]
    | TOpt(typ) => [%string "(TOpt %{f typ})"]
    | TTag(typ) => [%string "(TTag %{f typ})"]
    };
  };
  
  f(typ);
};

let sig_of_typ = (typ) => {
  let id_name_map = Hashtbl.create(10);
  let count = ref(0);

  let next_name = () => {
    let i = count^;
    incr(count);
    let tvar_char = String.make(1, (Char.chr(97 + i mod 26)));
    let tvar_index = i > 26 ? string_of_int(1 / 26) : ""; 
    [%string "%{tvar_char}%{tvar_index}"];
  }

  let concat_typ_strings = (f, typ_list) => List.map(f(false), typ_list) |>  String.concat(", ");

  // let rec_to_string = (f,row:tenv) => List.map(((name, typ)) => [%string "%{name}: %{f false typ}"], row) |>  String.concat(", ");

  let rec f = (is_simple, typ) => 
    switch(typ) {
    | TConst(name) => name
    | TApp(ftyp, param_typ) => [%string "%{f true ftyp}[%{f false param_typ}]"]
    | TFun(param_typ, rtyp) => {
      let arrow_typ_string = [%string "%{f true param_typ} => %{f false rtyp}"]
      is_simple ? [%string "(%{arrow_typ_string})"] : arrow_typ_string;
    }
    | TTuple(l) => [%string "(%{concat_typ_strings f l})"]
    | TList(typ) => [%string "list(%{f false typ})"]
    | TMod(_) => [%string "module"]
    | TVar({contents: Quantified(id)}) => {
        try (Hashtbl.find(id_name_map, id)) {
        | Not_found => {
          let name = next_name();
          Hashtbl.add(id_name_map, id, name);
          [%string "%{name}%{string_of_int(id)}"]; 
        }
      }
    }
    | TVar({contents: Free(id, _)}) => [%string "_%{string_of_int(id)}"]
    | TVar({contents: Constrained(typ)}) => f(is_simple, typ)
    | TRec(row) => [%string "{%{f false row}}"]
    | TRowEmpty => "TRowEmpty"
    | TRowExtend(_) => "TRow extend"
    | TOpt(_) => "?"
    | TTag(_) => "tag"
    };
  
  f(false, typ);
};