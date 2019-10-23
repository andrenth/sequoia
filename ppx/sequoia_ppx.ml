open Migrate_parsetree
open Ast_404

open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree
open Ast_convenience_404

open Printf

type state =
  { tables : string list ref
  ; references : (string * string, string * string) Hashtbl.t
  }

let global_references = ref None

let load_state () =
  match !global_references with
  | Some references -> Some { tables = ref []; references }
  | None -> None

let error loc msg =
  raise @@ Location.Error (Location.error ~loc msg)

let construct s = Exp.construct (lid s)

let there = construct "There" None
let skip x = construct "Skip" (Some x)

let rec snoc y = function
  | [] -> [y]
  | x::xs -> x::(snoc y xs)

let rec build_steps = function
  | 0 -> there
  | n -> skip (build_steps (n-1))

let index loc v l =
  let rec idx acc = function
    | [] -> error loc (sprintf "index for %s not found" v)
    | x::xs -> if x = v then acc else idx (acc + 1) xs in
  idx 0 l

let find_reference loc refs ref =
  try
    Hashtbl.find refs ref
  with Not_found ->
    error loc (sprintf "reference for %s.%s not found" (fst ref) (snd ref))

module StringSet = Set.Make(struct
  type t = string
  let compare = compare
end)

let rec map_expr_list loc f = function
  | { pexp_desc =
      Pexp_construct ({ txt = Lident "::" } as cons,
                      Some ({ pexp_desc = Pexp_tuple [expr; rest]; pexp_loc } as args)) } as e ->
    let l = [f pexp_loc expr; map_expr_list pexp_loc f rest] in
    { e with pexp_desc = Pexp_construct (cons,
                                         Some { args with pexp_desc = Pexp_tuple l }) }
  | { pexp_desc =
      Pexp_construct ({ txt = Lident "[]" }, None) } as e ->
    e

  | { pexp_desc = Pexp_open (ovr, loc, ({ pexp_loc } as e)) } as expr ->
      let e = map_expr_list pexp_loc f e in
      { expr with pexp_desc = Pexp_open (ovr, loc, e) }

  | _e ->
      error loc "unexpected element in select expression"

let add_table t u ts =
  let rec add = function
    | [] -> t::!ts
    | x::xs -> if x = u then t::x::xs else x :: add xs in
  ts := add !ts

let rec map_query st loc = function
  | { pexp_desc =
      Pexp_let (rec_flag, [{ pvb_expr } as b], ({ pexp_loc } as expr)) } ->

      let e = map_query st loc pvb_expr in
      let binding = { b with pvb_expr = e } in
      let expr = map_query st pexp_loc expr in
      Exp.let_ rec_flag [binding] expr

  (* nested let%sql = ... *)
  | { pexp_desc = Pexp_extension ({ txt = "sql"; loc }, pstr) } ->
      begin match pstr with
      | PStr [{ pstr_desc = Pstr_eval (expr, _) }] ->
          let expr =
            match load_state () with
            | Some st -> map_query st loc expr
            | None -> expr in
          Ast_helper.with_default_loc loc (fun () -> expr)
      | _ ->
          error loc "invalid sql"
      end

  (* from MyTable.table *)
  | { pexp_desc =
      Pexp_apply ({ pexp_desc =
                    Pexp_ident { txt = Lident "from" } },
                  [(_, { pexp_desc =
                         Pexp_ident { txt = Ldot (Lident t, _) } })]) } as e ->
      st.tables := [t];
      e

  (* {left,right,inner}_join (that Table.fk) *)
  | { pexp_desc =
      Pexp_apply
        (({ pexp_desc = Pexp_ident { txt = Lident join_fun } } as join),
         [(Nolabel,
           ({ pexp_desc =
             Pexp_apply
               (({ pexp_desc =
                   Pexp_ident { txt = Lident "that" } } as rel),
                (([(Nolabel,
                    { pexp_desc =
                      Pexp_ident
                        { txt = Ldot (Lident tbl, fld)
                        ; loc } })]) as args)) } as join_args))]) } as e
        when join_fun = "left_join"
          || join_fun = "right_join"
          || join_fun = "inner_join" ->

      let ref_tbl, _ = find_reference loc st.references (tbl, fld) in
      add_table ref_tbl tbl st.tables;
      let steps = build_steps (index loc ref_tbl !(st.tables)) in
      { e with
        pexp_desc =
        Pexp_apply
          (join,
           [(Nolabel,
             { join_args with
               pexp_desc =
               Pexp_apply
                 (rel, snoc (Nolabel, steps) args) })]) }

  (* {left,right,inner}_join (this Table.fk) *)
  | { pexp_desc =
      Pexp_apply
        (({ pexp_desc = Pexp_ident { txt = Lident join_fun } } as join),
         [(Nolabel,
           ({ pexp_desc =
             Pexp_apply
               (({ pexp_desc =
                   Pexp_ident { txt = Lident "this" } } as rel),
                (([(Nolabel,
                    { pexp_desc =
                      Pexp_ident
                        { txt = Ldot (Lident tbl, fld)
                        ; loc } })]) as args)) } as join_args))]) } as e
        when join_fun = "left_join"
          || join_fun = "right_join"
          || join_fun = "inner_join" ->

      let ref_tbl, _ = find_reference loc st.references (tbl, fld) in
      add_table tbl ref_tbl st.tables;
      let steps = build_steps (index loc tbl !(st.tables)) in
      { e with
        pexp_desc =
        Pexp_apply
          (join,
           [(Nolabel,
             { join_args with
               pexp_desc =
               Pexp_apply
                 (rel, snoc (Nolabel, steps) args) })]) }

  (* {left,right,inner}_join (self Table.k) *)
  | { pexp_desc =
      Pexp_apply
        (({ pexp_desc = Pexp_ident { txt = Lident join_fun } } as join),
         [(Nolabel,
           ({ pexp_desc =
             Pexp_apply
               (({ pexp_desc = Pexp_ident { txt = Lident "self" } } as rel),
                (([ (Nolabel,
                     { pexp_desc =
                       Pexp_ident
                         { txt = Ldot (Lident tbl, _fld) ; loc } })
                  ; _
                  ]) as args)) } as join_args))]) } as e
        when join_fun = "left_join"
          || join_fun = "right_join"
          || join_fun = "inner_join" ->

      add_table tbl tbl st.tables;
      let steps = build_steps (index loc tbl !(st.tables)) in
      { e with
        pexp_desc =
        Pexp_apply
          (join,
           [(Nolabel,
             { join_args with
               pexp_desc =
               Pexp_apply
                 (rel, snoc (Nolabel, steps) args) })]) }

  (* select *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "select"; loc } } as select), [lbl, args]) } as e ->
      { e with pexp_desc =
               Pexp_apply (select, [lbl, map_expr_list loc (map_select st) args]) }

  (* select ~distinct:... *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "select"; loc } } as select),
                     [(Labelled "distinct", _) as dist; lbl, args]) } as e ->
      { e with pexp_desc =
               Pexp_apply (select, [dist; lbl, map_expr_list loc (map_select st) args]) }

  (* group_by Expr.[...] *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "group_by"; loc } } as gb), [lbl, args]) } as e ->
      { e with pexp_desc =
               Pexp_apply (gb, [lbl, map_expr_list loc (map_select st) args]) }

  (* group_by Expr.[...] ~having:... *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "group_by"; loc } } as gb),
                  [(lbl, args); (Labelled "having", hav)]) } as e ->
      { e with pexp_desc =
               Pexp_apply
                 (gb,
                  [ lbl, map_expr_list loc (map_select st) args
                  ; Labelled "having", map_select st loc hav
                  ]) }

  (* group_by ~having:... Expr.[...] *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "group_by"; loc } } as gb),
                  [(Labelled "having", hav); (lbl, args)]) } as e ->
      { e with pexp_desc =
               Pexp_apply
                 (gb,
                  [ Labelled "having", map_select st loc hav
                  ; lbl, map_expr_list loc (map_select st) args
                  ]) }

  (* order_by Expr.[...] *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "order_by"; loc } } as ob), [lbl, args]) } as e ->
      { e with pexp_desc =
               Pexp_apply (ob, [lbl, map_expr_list loc (map_select st) args]) }

  (* field Table.field *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident fn; loc } } as fld),
                  ([ (Nolabel,
                      { pexp_desc =
                        Pexp_ident { txt = Ldot (Lident table, _field) } })
                  ] as args)) } as e
        when fn = "field"|| fn = "foreign_key" ->
      let steps = build_steps (index loc table !(st.tables)) in
      { e with pexp_desc = Pexp_apply (fld, snoc (Nolabel, steps) args) }

  | { pexp_desc = Pexp_apply (({ pexp_loc } as e), args) } as expr ->
      let args =
        List.map
          (fun (lbl, ({ pexp_loc } as e)) ->
            (lbl, map_query st pexp_loc e))
          args in
      let e = map_query st pexp_loc e in
      { expr with pexp_desc = Pexp_apply (e, args) }

  | { pexp_desc = Pexp_open (ovr, loc, ({ pexp_loc } as e)) } as expr ->
      let e = map_query st pexp_loc e in
      { expr with pexp_desc = Pexp_open (ovr, loc, e) }

  | e ->
      e

and map_select st _loc = function
  (* select [...; field|foreign_key|unwrap Table.field; ...] *)
  | { pexp_desc =
      Pexp_apply
        (({ pexp_desc =
            Pexp_ident { txt = Lident fn; loc } } as fld),
         ([(_, { pexp_desc =
                 Pexp_ident { txt = Ldot (Lident t, _) } })] as args)) } as e
        when fn = "field" || fn = "foreign_key" || fn = "unwrap" ->
      let steps = build_steps (index loc t !(st.tables)) in
      { e with pexp_desc = Pexp_apply (fld, snoc (Nolabel, steps) args) }

  (* select [...; subquery (from ...); ...] *)
  | { pexp_desc =
      Pexp_apply (({ pexp_desc =
                     Pexp_ident { txt = Lident "subquery" } } as sub),
                  [(lbl, expr)]) } as e ->

      let st' = { st with tables = ref [] } in
      let expr = map_query st' expr.pexp_loc expr in
      { e with pexp_desc = Pexp_apply (sub, [(lbl, expr)]) }

  | { pexp_desc = Pexp_apply (fn, args) } as e ->
      let args =
        List.map
          (fun (lbl, arg_expr) ->
            match arg_expr with
            (* List argument: map each expression *)
            | { pexp_desc = Pexp_construct ({ txt = Lident "::" }, _) } ->
                (lbl, map_expr_list arg_expr.pexp_loc (map_select st) arg_expr)
            | _ ->
                (lbl, map_select st arg_expr.pexp_loc arg_expr))
          args in
      { e with pexp_desc = Pexp_apply (fn, args) }

  | e ->
      e

let rec map_module table references _loc = function
  | [] -> ()

  (* Field.foreign_key name ~references:Some.field *)
  | { pstr_desc =
      Pstr_value (_,
        [{ pvb_pat  = { ppat_desc = Ppat_var { txt = field } }
         ; pvb_expr =
           { pexp_desc =
             Pexp_apply ({
               pexp_desc =
               Pexp_ident { txt = Ldot (Lident "Field", "foreign_key") } },
                 [ (Nolabel,
                     { pexp_desc = Pexp_constant (Pconst_string (_, None)) })
                 ; (Labelled "references",
                     { pexp_desc =
                       Pexp_ident
                        { txt = Ldot (Lident ref_table, ref_field) } })
                 ]) }
             }])
    ; pstr_loc } :: rest ->
      Hashtbl.add references (table, field) (ref_table, ref_field);
      map_module table references pstr_loc rest

  (* Field.foreign_key ~references:Some.field name *)
  | { pstr_desc =
      Pstr_value (_,
        [{ pvb_pat  = { ppat_desc = Ppat_var { txt = field } }
         ; pvb_expr =
           { pexp_desc =
             Pexp_apply ({
               pexp_desc =
               Pexp_ident { txt = Ldot (Lident "Field", "foreign_key") } },
                 [ (Labelled "references",
                     { pexp_desc =
                       Pexp_ident
                        { txt = Ldot (Lident ref_table, ref_field) } })
                 ; (Nolabel,
                     { pexp_desc = Pexp_constant (Pconst_string (_, None)) })
                 ]) }
             }])
    ; pstr_loc } :: rest ->
      Hashtbl.add references (table, field) (ref_table, ref_field);
      map_module table references pstr_loc rest

  | { pstr_loc } :: rest ->
      map_module table references pstr_loc rest

let sql_mapper references _config _cookies =
  { default_mapper with
    expr =
      (fun mapper expr ->
        match expr with
        (* let%sql = ... *)
        | { pexp_desc = Pexp_extension ({ txt = "sql"; loc }, pstr) } ->
            begin match pstr with
            | PStr [{ pstr_desc = Pstr_eval (expr, _) }] ->
                let expr =
                  match load_state () with
                  | Some st -> map_query st loc expr
                  | None -> expr in
                Ast_helper.with_default_loc loc (fun () -> expr)
            | _ ->
                error loc "invalid sql"
            end
        | x -> default_mapper.expr mapper x)

  ; structure_item =
      (fun mapper str ->
        match str with
        (* module%sql ... = struct ... end *)
        | { pstr_desc = Pstr_extension (({ txt = "sql"; loc }, pstr), _) } ->
            begin match pstr with
            | PStr
                [{ pstr_desc =
                   Pstr_module
                    ({ pmb_name = { txt = name }
                     ; pmb_expr = { pmod_desc = Pmod_structure strs }
                     ; pmb_loc } as m) }] ->
                map_module name references pmb_loc strs;
                global_references := Some references;
                let str = Str.module_ m in
                Ast_helper.with_default_loc loc (fun () -> str)
            | PStr
                [{ pstr_desc =
                  Pstr_value (rec_flag, [{ pvb_expr } as b]) }] ->
                let str =
                  match load_state () with
                  | Some st ->
                      let expr = map_query st loc pvb_expr in
                      let binding = { b with pvb_expr = expr } in
                      Str.value rec_flag [binding]
                  | None ->
                      Str.value rec_flag [b] in
                Ast_helper.with_default_loc loc (fun () -> str)
            | _ ->
                error loc "invalid sql"
            end
        | x -> default_mapper.structure_item mapper x)
  }

let migration =
  Versions.migrate Versions.ocaml_404 Versions.ocaml_current

let () =
  let references = Hashtbl.create 16 in
  Driver.register ~name:"ppx_sequoia"
    Versions.ocaml_404 (sql_mapper references)
