open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast_convenience

open Printf

let error loc msg =
  raise @@ Location.Error (Location.error ~loc msg)

type definition =
  { ocaml_name : string
  ; sql_name   : string
  }

let sql_defs : (string, string * (string, string * (string, string) Hashtbl.t) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 4

let check_redefinition name things thing =
  let name = String.capitalize name in
  if Hashtbl.mem things thing then
    error !default_loc (sprintf "%s `%s` has already been defined" name thing)

let hashtbl_of_list l =
  let h = Hashtbl.create 4 in
  List.iter (fun (k, v) -> Hashtbl.replace h k v) l;
  h

let define_table database table fields =
  try
    let tables = snd (Hashtbl.find sql_defs database.ocaml_name) in
    check_redefinition "table" tables table.ocaml_name;
    let fields = hashtbl_of_list fields in
    Hashtbl.replace tables table.ocaml_name (table.sql_name, fields);
  with Not_found ->
    let fields = hashtbl_of_list fields in
    let tables = Hashtbl.create 4 in
    Hashtbl.replace tables table.ocaml_name (table.sql_name, fields);
    Hashtbl.replace sql_defs database.ocaml_name (database.sql_name, tables)

let database_sql_name database =
  fst (Hashtbl.find sql_defs database)

let table_sql_name database table =
  let database, tables = Hashtbl.find sql_defs database in
  database, fst (Hashtbl.find tables table)

let column_sql_name database table column =
  let database, tables = Hashtbl.find sql_defs database in
  let table, fields = Hashtbl.find tables table in
  database, table, Hashtbl.find fields column

let split_db_table s =
  try
    let i = String.index s '.' in
    let len = String.length s in
    (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))
  with Not_found ->
    error !default_loc "blah"


type select =
  { expr : Parsetree.expression
  ; from : Parsetree.expression
  ; joins : Parsetree.expression
  ; where : Parsetree.expression
  ; group_by : Parsetree.expression
  ; order_by : Parsetree.expression
  ; limit : Parsetree.expression
  }

let some x = constr "Some" [x]
let none = constr "None" []
let true_ = constr "true" []
let false_ = constr "false" []
let bool b = if b then true_ else false_

type select_validations =
  { columns : (string, (string, (string, bool) Hashtbl.t) Hashtbl.t) Hashtbl.t
  ; tables : (string, (string, bool) Hashtbl.t) Hashtbl.t
  ; defined_aliases : (string, string * string) Hashtbl.t
  ; used_aliases : (string, bool) Hashtbl.t
  }

let table_exists database table =
  try
    Hashtbl.mem (snd (Hashtbl.find sql_defs database)) table
  with Not_found ->
    false

let column_exists database table column =
  try
    let tables = snd (Hashtbl.find sql_defs database) in
    let fields = snd (Hashtbl.find tables table) in
    Hashtbl.mem fields column
  with Not_found ->
    false

let selected_table valid database table =
  try
    let tables = Hashtbl.find valid.tables database in
    Hashtbl.replace tables table true
  with Not_found ->
    let tables = Hashtbl.create 4 in
    Hashtbl.replace tables table true;
    Hashtbl.replace valid.tables database tables

let selected_column valid database table column =
  try
    let tables = Hashtbl.find valid.columns database in
    begin try
      let columns = Hashtbl.find tables table in
      Hashtbl.replace columns column true
    with Not_found ->
      let columns = Hashtbl.create 4 in
      Hashtbl.replace columns column true;
      Hashtbl.replace tables table columns
    end
  with Not_found ->
    let tables = Hashtbl.create 4 in
    let columns = Hashtbl.create 4 in
    Hashtbl.replace columns column true;
    Hashtbl.replace tables table columns;
    Hashtbl.replace valid.columns database tables

let clone x =
  Marshal.from_string (Marshal.to_string x []) 0

let define_alias validations alias database table =
  if Hashtbl.mem validations.defined_aliases alias then
    error !default_loc (sprintf "alias `%s` already defined" alias)
  else
    Hashtbl.replace validations.defined_aliases alias (database, table)

let resolve_alias defined_aliases alias =
  try
    Hashtbl.find defined_aliases alias
  with Not_found ->
    error !default_loc (sprintf "undefined alias `%s`" alias)

let used_alias validations alias =
  Hashtbl.replace validations.used_aliases alias true

let validate_columns validations =
  Hashtbl.iter
    (fun database tables ->
      Hashtbl.iter
        (fun table cols ->
          if table_exists database table then
            Hashtbl.iter
              (fun col _ ->
                if not (column_exists database table col) then
                  let err =
                    sprintf "Column `%s.%s` does not exist" table col in
                  error !default_loc err)
              cols
          else
            let err = sprintf "Table `%s` not selected in query" table in
            error !default_loc err)
        tables)
    validations.columns

let validate_tables validations =
  Hashtbl.iter
    (fun database tables ->
      Hashtbl.iter
        (fun table _ ->
          if not (table_exists database table) then
            let err = sprintf "Cannot select from unknown table `%s`" table in
            error !default_loc err)
        tables)
    validations.tables

let validate_aliases validations =
  Hashtbl.iter
    (fun alias _ ->
      if not (Hashtbl.mem validations.defined_aliases alias) then
        error !default_loc (sprintf "undefined alias `%s`" alias))
    validations.used_aliases

let validate_select validations =
  validate_aliases validations; (* Must come first for alias translation *)
  validate_tables validations;
  validate_columns validations

let operator_name = function
  | "="  -> "Eq"
  | "=="  -> "Eq"
  | "<=>" -> "Nulleq"
  | "!=" -> "Ne"
  | "<>" -> "Ne"
  | ">"  -> "Gt"
  | "<"  -> "Lt"
  | ">=" -> "Ge"
  | "<=" -> "Le"
  | "+"  -> "Add"
  | "-"  -> "Sub"
  | "*"  -> "Mul"
  | "/"  -> "Div"
  | "&&" -> "And"
  | "||" -> "Or"
  | "&." -> "Bit_and"
  | "|." -> "Bit_or"
  | "<<" -> "Shift_left"
  | ">>" -> "Shift_right"
  | "%" -> "Mod"
  | s -> String.capitalize s

let resolve_aliases defined_aliases expr =
  let rec resolve = function
    | { pexp_desc = Pexp_record (fields, optexpr) } as e ->
        let fields = List.map (fun (k, e) -> (k, resolve e)) fields in
        let optexpr =
          match optexpr with
          | Some expr -> Some (resolve expr)
          | None -> None in
        { e with pexp_desc = Pexp_record (fields, optexpr) }
    | { pexp_desc = Pexp_tuple elems } as e ->
        let elems = List.map resolve elems in
        { e with pexp_desc = Pexp_tuple elems }
    | { pexp_desc =
        Pexp_construct ({ txt = Lident "Some"; loc }, Some expr) } as e ->
        let expr = resolve expr in
        { e with
          pexp_desc =
          Pexp_construct ({ txt = Lident "Some"; loc }, Some expr) }
    | { pexp_desc =
        Pexp_construct ({ txt = Lident "::"; loc }, Some expr) } as e ->
        let expr = resolve expr in
        { e with
          pexp_desc = Pexp_construct ({ txt = Lident "::"; loc }, Some expr) }
    | { pexp_desc =
        Pexp_variant ("Aliased",
        Some { pexp_desc = Pexp_tuple [
          { pexp_desc = Pexp_constant (Const_string (alias, None)) };
          { pexp_desc = Pexp_constant (Const_string (column, None)) }
        ] }) } as e ->
        let database, table = resolve_alias defined_aliases alias in
        let (_, _, column) = column_sql_name database table column in
        let expr = list [str alias; str column] in
        { e with pexp_desc = Pexp_variant ("Column", Some expr) }
    | { pexp_desc = Pexp_variant (name, Some expr) } as e ->
        let expr = resolve expr in
        { e with pexp_desc = Pexp_variant (name, Some expr) }
    | e ->
        e in
  resolve expr

let rec map_select validations loc = function
  | [] ->
      error loc "empty select body"
  | (_, expr) :: exprs ->
      let rec map select = function
        | [] ->
            select

        (* from ... as ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "from" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "as_" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident alias } }) ::
          rest ->
            selected_table validations database table;
            define_alias validations alias database table;
            let database, table = table_sql_name database table in
            let from = tuple [str database; str table; some @@ str alias] in
            map { select with from = some from } rest

        (* from *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "from" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          rest ->
            selected_table validations database table;
            let database, table = table_sql_name database table in
            let from = tuple [str database; str table; none] in
            map { select with from = some from } rest

        (* left|right join ... as ... on ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "as_" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident alias } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "on" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            define_alias validations alias database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~alias
                ~kind
                ~outer:false
                ~cond:(`On expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right join ... on ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "on" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~kind
                ~outer:false
                ~cond:(`On expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right outer join ... as ... on ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "outer" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "as_" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident alias } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "on" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            define_alias validations alias database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~alias
                ~kind
                ~outer:true
                ~cond:(`On expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right outer join ... on ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "outer" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "on" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~kind
                ~outer:true
                ~cond:(`On expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right join ... as ... using ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "as_" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident alias } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "using" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            define_alias validations alias database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~alias
                ~kind
                ~outer:false
                ~cond:(`Using expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right join ... using ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "using" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~kind
                ~outer:false
                ~cond:(`Using expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right outer join ... as ... using ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "outer" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "as_" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident alias } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "using" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            define_alias validations alias database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~alias
                ~kind
                ~outer:true
                ~cond:(`Using expr) () in
            map { select with joins = cons join select.joins } rest

        (* left|right outer join ... using ... *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident kind } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "outer" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "join" } }) ::
          (_, { pexp_desc =
                Pexp_ident { txt = Ldot (Lident database, table) } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "using" } }) ::
          (_, expr) :: rest when kind = "left" || kind = "right" ->
            selected_table validations database table;
            let join =
              map_join
                ~validations
                ~database
                ~table
                ~kind
                ~outer:true
                ~cond:(`Using expr) () in
            map { select with joins = cons join select.joins } rest

        (* where *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "where" } }) ::
          (_, expr) :: rest ->
            map { select with where = some @@ map_expr validations expr } rest

        (* group by ... having *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "group" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "by" } }) ::
          (_, group_by_expr) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "having" } }) ::
          (_, having_expr) :: rest ->
            let group_by =
              let having = map_expr validations having_expr in
              tuple [map_expr validations group_by_expr; some having] in
            map { select with group_by = some group_by } rest

        (* group by *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "group" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "by" } }) ::
          (_, expr) :: rest ->
            let group_by = tuple [map_expr validations expr; none] in
            map { select with group_by = some group_by } rest

        (* order by *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "order" } }) ::
          (_, { pexp_desc = Pexp_ident { txt = Lident "by" } }) ::
          (_, expr) :: rest ->
            let order_by =  map_expr validations expr in
            map { select with order_by = some order_by } rest

        (* limit n *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "limit" } }) ::
          (_, ({ pexp_desc = Pexp_constant (Const_int _) } as expr)) ::
          rest ->
            map { select with limit = some @@ tuple [none; expr] } rest

        (* limit n, m *)
        | (_, { pexp_desc = Pexp_ident { txt = Lident "limit" } }) ::
          (_, ({ pexp_desc =
            Pexp_tuple
              [({ pexp_desc = Pexp_constant (Const_int _) } as off);
               ({ pexp_desc = Pexp_constant (Const_int _) } as lim)] })) ::
          rest ->
            map { select with limit = some @@ tuple [some off; lim] } rest

        | _ ->
            error loc "invalid select" in

      let init =
        { expr = map_expr validations expr
        ; from = none
        ; joins = nil ()
        ; where = none
        ; group_by = none
        ; order_by = none
        ; limit = none
        } in
      let select = map init exprs in
      resolve_aliases validations.defined_aliases @@ record
        [ "Sequoia.Select.expr", select.expr
        ; "Sequoia.Select.from", select.from
        ; "Sequoia.Select.joins", select.joins
        ; "Sequoia.Select.where", select.where
        ; "Sequoia.Select.group_by", select.group_by
        ; "Sequoia.Select.order_by", select.order_by
        ; "Sequoia.Select.limit", select.limit
        ]

and map_expr validations = function
  (* Aliased expression, ie. expr as alias *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "as_" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident alias } })]) } ->
      Exp.variant "Alias" (Some (tuple [map_expr validations expr; str alias]))

  (* Integer *)
  | { pexp_desc = Pexp_constant (Const_int _) } as e ->
      Exp.variant "Int" (Some e)

  (* Character *)
  | { pexp_desc = Pexp_constant (Const_char c) } ->
      let expr = str (String.make 1 c) in
      Exp.variant "String" (Some expr)

  (* String *)
  | { pexp_desc = Pexp_constant (Const_string _) } as e ->
      Exp.variant "String" (Some e)

  (* Float *)
  | { pexp_desc = Pexp_constant (Const_float s) } as e ->
      Exp.variant "Float" (Some e)

  (* null *)
  | ({ pexp_desc = Pexp_ident { txt = Lident "null" } }) ->
      Exp.variant "Null" None

  (* Qualified identifier, ie. database.table.column *)
  | { pexp_desc =
      Pexp_field
        ({ pexp_desc =
           Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident database } },
                       { txt = Lident table })
         ; pexp_loc = loc }, { txt = Lident column }) } ->
      selected_column validations database table column;
      let database, table, column = column_sql_name database table column in
      Exp.variant "Column" (Some (list [str database; str table; str column]))

  (* Qualified identifier, ie. Database.table.column *)
  | { pexp_desc =
      Pexp_field
        ({ pexp_desc = Pexp_ident { txt = Ldot (Lident database, table) } },
         { txt = Lident column }) } ->
      selected_column validations database table column;
      let database, table, column = column_sql_name database table column in
      Exp.variant "Column" (Some (list [str database; str table; str column]))

  (* Aliased identifier, ie. table_alias.column *)
  | { pexp_desc =
      Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident alias } },
                  { txt = Lident column }) } ->
      used_alias validations alias;
      Exp.variant "Aliased" (Some (tuple [str alias; str column]))

  (* expr LIKE "pattern" *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "like" } });
                (_, { pexp_desc = Pexp_constant (Const_string (p, _)) })]) } ->
      let tup = tuple [map_expr validations expr; str p] in
      Exp.variant "Like" (Some tup)

  (* expr LIKE expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "like" } });
                (_, pat)]) } ->
      let tup = tuple [map_expr validations expr; map_expr validations pat] in
      Exp.variant "Like" (Some tup)

  (* expr NOT LIKE "pattern" *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "like" } });
                (_, { pexp_desc = Pexp_constant (Const_string (p, _)) })]) } ->
      let tup = tuple [map_expr validations expr; str p] in
      Exp.variant "Not" (Some (Exp.variant "Like" (Some tup)))

  (* expr NOT LIKE expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "like" } });
                (_, pat)]) } ->
      let tup = tuple [map_expr validations expr; map_expr validations pat] in
      Exp.variant "Not" (Some (Exp.variant "Like" (Some tup)))

  (* expr REGEXP "pattern" *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "regexp" } });
                (_, { pexp_desc = Pexp_constant (Const_string (r, _)) })]) } ->
      let tup = tuple [map_expr validations expr; str r] in
      Exp.variant "Regexp" (Some tup)

  (* expr REGEXP expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "regexp" } });
                (_, re)]) } ->
      let tup = tuple [map_expr validations expr; map_expr validations re] in
      Exp.variant "Regexp" (Some tup)

  (* expr NOT LIKE "pattern" *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "regexp" } });
                (_, { pexp_desc = Pexp_constant (Const_string (r, _)) })]) } ->
      let tup = tuple [map_expr validations expr; str r] in
      Exp.variant "Not" (Some (Exp.variant "Regexp" (Some tup)))

  (* expr NOT LIKE expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "regexp" } });
                (_, re)]) } ->
      let tup = tuple [map_expr validations expr; map_expr validations re] in
      Exp.variant "Not" (Some (Exp.variant "Regexp" (Some tup)))

  (* expr IN (...) *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "in_" } });
                (_, { pexp_desc = Pexp_tuple exprs })]) } ->
      let expr = map_expr validations expr in
      let exprs = list (List.map (map_expr validations) exprs) in
      Exp.variant "In" (Some (tuple [expr; exprs]))

  (* expr IN expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "in_" } });
                (_, set)]) } ->
      let expr = map_expr validations expr in
      let set = list [map_expr validations set] in
      Exp.variant "In" (Some (tuple [expr; set]))

  (* expr NOT IN (...) *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "in_" } });
                (_, { pexp_desc = Pexp_tuple exprs })]) } ->
      let expr = map_expr validations expr in
      let set = list (List.map (map_expr validations) exprs) in
      Exp.variant "Not" (Some (Exp.variant "In" (Some (tuple [expr; set]))))

  (* expr NOT IN expr *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "in_" } });
                (_, set)]) } ->
      let expr = map_expr validations expr in
      let set = map_expr validations set in
      Exp.variant "Not" (Some (Exp.variant "In" (Some (tuple [expr; set]))))

  (* expr IS NULL *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "is" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "null" } })]) } ->
      Exp.variant "Null" (Some (map_expr validations expr))

  (* expr IS NOT NULL *)
  | { pexp_desc =
      Pexp_apply
        (expr, [(_, { pexp_desc = Pexp_ident { txt = Lident "is" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "not" } });
                (_, { pexp_desc = Pexp_ident { txt = Lident "null" } })]) } ->
      let is_null = Exp.variant "Null" (Some (map_expr validations expr)) in
      Exp.variant "Not" (Some is_null)

  (* EXISTS *)
  | { pexp_desc =
    Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "exists" }
                ; pexp_loc = loc }, [sub]) } ->
      let validations' = clone validations in
      let expr = map_select validations' loc [sub] in
      validate_select validations';
      Exp.variant "Exists" (Some expr)

  (* NOT EXISTS *)
  | { pexp_desc =
    Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "not" }
                ; pexp_loc = loc },
                [(_, { pexp_desc = Pexp_ident { txt = Lident "exists" } });
                 sub]) } ->
      let validations' = clone validations in
      let expr = map_select validations' loc [sub] in
      validate_select validations';
      Exp.variant "Not" (Some (Exp.variant "Exists" (Some expr)))

  (* Reference to OCaml variable *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "!" } },
                  [_, ({ pexp_desc = Pexp_ident _ } as var)]) } ->
      var

  (* Sub-select *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc =
                    Pexp_ident { txt = Lident "select"; loc } }, l) } ->
      (* Clone validations to avoid adding tables/columns to the outer scope *)
      let validations' = clone validations in
      let expr = map_select validations' loc l in
      validate_select validations';
      Exp.variant "Select" (Some expr)

  (* SQL function call - no arguments *)
  | { pexp_desc =
      Pexp_apply
        ({ pexp_desc = Pexp_ident { txt = Lident name } },
         [_, { pexp_desc = Pexp_construct ({ txt = Lident "()" }, None) }]) } ->
      let func = Exp.variant (String.capitalize name) None in
      Exp.variant "Fun" (Some func)

  (* SQL function call - multiple arguments *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name } },
                  [_, { pexp_desc = Pexp_tuple args }]) } ->
      let args = tuple @@ List.map (map_expr validations) args in
      let func = Exp.variant (String.capitalize name) (Some args) in
      Exp.variant "Fun" (Some func)

  (* SQL function call - one argument *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name } },
                  [_, expr]) } ->
      let args = tuple [map_expr validations expr] in
      let func = Exp.variant (String.capitalize name) (Some args) in
      Exp.variant "Fun" (Some func)

  (* SQL function call - operator *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name } },
                  [_, arg1; _, arg2]) } ->
      let args = tuple [map_expr validations arg1; map_expr validations arg2] in
      let func = Exp.variant (operator_name name) (Some args) in
      Exp.variant "Fun" (Some func)

  (* Multiple select expressions *)
  | { pexp_desc = Pexp_tuple tup } ->
      let exprs = list @@ List.map (map_expr validations) tup in
      Exp.variant "List" (Some exprs)

  | _ ->
      error !default_loc "parse error in SQL expression"

and map_ident_list validations expr =
  let rec map_list mapped = function
    | [] ->
        Some mapped
    | ({ pexp_desc = Pexp_ident { txt = Lident _ } } as expr):: rest ->
        map_list (cons (map_expr validations expr) mapped) rest
    | _ ->
        None in
  match expr with
  | { pexp_desc = Pexp_tuple tuple } ->
      map_list (nil ()) tuple
  | { pexp_desc = Pexp_ident { txt = Lident _ } } as expr ->
      Some (list [map_expr validations expr])
  | _ ->
      None

and map_join ~validations ~database ~table ?alias ~kind ~outer ~cond () =
  let alias =
    match alias with
    | Some a -> some (str a)
    | None -> none in
  let map_kind = function
    | ("left" | "right" as k) ->
        Exp.variant (String.capitalize k) (Some (bool outer))
    | k ->
        error !default_loc (sprintf "unsupported join kind '%s'" k) in
  let kind = map_kind kind in
  match cond with
  | `On expr ->
      let database, table = table_sql_name database table in
      let on = Exp.variant "On" (Some (map_expr validations expr)) in
      tuple [kind; str database; str table; alias; on]
  | `Using expr ->
      begin match map_ident_list validations expr with
      | Some expr ->
          let database, table = table_sql_name database table in
          let using = Exp.variant "Using" (Some expr) in
          tuple [kind; str database; str table; alias; using]
      | None ->
          let err = "join ... using ...: invalid table list" in
          error !default_loc err
      end

let rec map_query loc = function
  | Pexp_let (rec_flag, [{ pvb_expr = { pexp_desc = desc } } as b], expr) ->
      let query_expr = map_query loc desc in
      let binding = { b with pvb_expr = query_expr } in
      Exp.let_ rec_flag [binding] expr
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "select"; loc } }, l) ->
      let validations =
        { columns = Hashtbl.create 4
        ; tables = Hashtbl.create 4
        ; defined_aliases = Hashtbl.create 4
        ; used_aliases = Hashtbl.create 4
        } in
      let expr = map_select validations loc l in
      validate_select validations;
      expr
  | _ -> assert false

let match_sql_name ocaml_name = function
  | ({ txt = "sql" },
     PStr
      [{ pstr_desc =
         Pstr_eval
           ({ pexp_desc =
              Pexp_constant (Const_string (sql_name, _)) }, _) }]) ->
      Some sql_name
  | ({ txt = "sql" }, PStr []) ->
      Some ocaml_name
  | _ ->
      None

let rec find_sql_name ocaml_name = function
  | [] -> None
  | attr :: attrs ->
      begin match match_sql_name ocaml_name attr with
      | Some sql_name -> Some sql_name
      | None -> find_sql_name ocaml_name attrs
      end

let find_sql_table_records strs =
  let rec records acc = function
  | { pstr_desc = Pstr_type decls } ->
      begin match decls with
      | [{ ptype_name = { txt = ocaml_name }
         ; ptype_kind = Ptype_record r
         ; ptype_attributes = attrs }] ->
          begin match find_sql_name ocaml_name attrs with
          | Some sql_name -> ((ocaml_name, sql_name), r)::acc
          | None -> acc
          end
      | _ ->
          acc
      end
  | _ ->
      acc in
  List.fold_left records [] strs

let map_table strs =
  let field_names { pld_name = { txt = ocaml_name }
                  ; pld_type = { ptyp_attributes = attrs } } =
    match find_sql_name ocaml_name attrs with
    | Some sql_name -> Some (ocaml_name, sql_name)
    | None -> None
    in
  let sql_field_names fields =
    List.fold_left
      (fun acc field ->
        match field_names field with
        | Some names -> names::acc
        | None -> acc)
      [] fields in
  List.map
    (fun (table_names, fields) ->
      (table_names, sql_field_names fields))
    (find_sql_table_records strs)

let sql_mapper argv =
  { default_mapper with
    expr = (fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "sql"; loc }, pstr) } ->
          begin match pstr with
          | PStr [{ pstr_desc =
                    Pstr_eval ({ pexp_loc = loc; pexp_desc = desc }, _) }] ->
              let select = map_query loc desc in
              Ast_helper.with_default_loc loc (fun () -> select)
          | _ ->
              error loc "invalid sql"
          end
      | x -> default_mapper.expr mapper x)

  ; structure_item = (fun mapper str ->
      match str with
      (* [@sql], [@@sql] *)
      | { pstr_desc =
          Pstr_module
            { pmb_name = { txt = ocaml_database_name }
            ; pmb_expr = { pmod_desc = Pmod_structure strs }
            ; pmb_attributes = attrs } } as x ->
          begin match find_sql_name ocaml_database_name attrs with
          | None -> default_mapper.structure_item mapper x
          | Some sql_database_name ->
              let database_def =
                { ocaml_name = ocaml_database_name
                ; sql_name = sql_database_name
                } in
              List.iter
                (fun ((ocaml_table_name, sql_table_name), fields) ->
                  let table_def =
                    { ocaml_name = ocaml_table_name
                    ; sql_name = sql_table_name
                    } in
                  define_table database_def table_def fields)
                (map_table strs);
              default_mapper.structure_item mapper x
          end

      (* let%sql = ... *)
      | { pstr_desc = Pstr_extension (({ txt = "sql"; loc }, pstr), _) } ->
          begin match pstr with
          | PStr
              [{ pstr_desc =
                 Pstr_value (rec_flag,
                             [{ pvb_expr = { pexp_desc = desc } } as b]) }] ->
              let query = map_query loc desc in
              let binding = { b with pvb_expr = query } in
              let str = Str.value rec_flag [binding] in
              Ast_helper.with_default_loc loc (fun () -> str)
          | _ ->
              error loc "invalid sql"
          end

      | x -> default_mapper.structure_item mapper x)
  }

let () =
  run_main sql_mapper
