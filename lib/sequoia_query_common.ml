open Printf
open Sequoia_common

module Expr   = Sequoia_expr
module Field  = Sequoia_field
module Lit    = Sequoia_lit
module Table  = Sequoia_table
module Vector = Sequoia_vector
module Nat    = Vector.Nat

let build_where
  : type a. placeholder:(int -> string)
         -> handover:Expr.handover
         -> build_step
         -> a Expr.t option
         -> build_step =
  fun ~placeholder ~handover st -> function
    | Some expr ->
        let st = Expr.build ~placeholder ~handover st expr in
        { st with repr = sprintf "WHERE (%s)" st.repr }
    | None ->
        { blank_step with pos = st.pos }

let build_limit placeholder st = function
  | Some (0, lim) ->
      { repr = sprintf "LIMIT %s" (placeholder st.pos)
      ; params = [Param.Int lim]
      ; pos = st.pos + 1
      }
  | Some (off, lim) ->
      { repr = sprintf "LIMIT %s, %s" (placeholder st.pos) (placeholder (st.pos + 1))
      ; params = [Param.Int off; Param.Int lim]
      ; pos = st.pos + 2
      }
  | None ->
      { blank_step with pos = st.pos }

module UpdateDeleteExpr = struct
  type _ Expr.t +=
    | Field : ('t, 'a) Field.t * 't Table.t -> 'a Expr.t
    | Foreign : ('t, 'u) Field.foreign_key * 't Table.t -> 'a Expr.t

  let field fld = fun table -> Field (fld, table)
  let foreign_key fk = fun table -> Foreign (fk, table)

  let rec build
    : type a. placeholder:(int -> string)
           -> handover:Expr.handover
           -> build_step
           -> a Expr.t
           -> build_step =
    fun ~placeholder ~handover st e ->
      match e with
      | Field (Field.Bool _ as fld, _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | Field (Field.Int _ as fld, _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | Field (Field.Float _ as fld, _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | Field (Field.String _ as fld, _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | Field (Field.Blob _ as fld, _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | Foreign ((fld, _), _) ->
          { repr = Field.to_string fld; params = []; pos = st.pos }
      | e ->
          Expr.build ~placeholder ~handover st e
end

module InsertReplace = struct
  module Vector = Vector.Make(struct
    type ('t, 'a) elem = ('t, 'a) Field.t
  end)

  type _ t =
    | I :
        { table  : 't Table.t
        ; fields : ('t, 'a, 'n Nat.s) Vector.t
        ; values : ('u, 'a, 'm Nat.s, 'n Nat.s) Lit.Vector.matrix
        } -> 't t

  let create =
    fun ~into ~fields ~values ->
      I { table = into; fields; values }

  let rec join_fields
    : type t a n. (t, a, n) Vector.t -> string =
    fun flds ->
      let open Vector in
      match flds with
      | [] -> assert false
      | [f] -> Field.name f
      | f::fs -> Field.name f ^ ", " ^ join_fields fs

  let expr_placeholders ~placeholder i vs =
    let open Lit in
    let rec eps
      : type t a n. int -> (t, a, n) Lit.Vector.t -> string list =
      fun i exprs ->
        let open Lit.Vector in
        match exprs with
        | [] -> []
        | _::es -> placeholder i :: eps (i+1) es in
    eps i vs

  let placeholders
    : type a m. placeholder:(int -> string)
             -> ('v, a, m, 'n) Lit.Vector.matrix
             -> string =
    fun ~placeholder values ->
      let rec pss
        : type a m. int -> ('v, a, m, 'n) Lit.Vector.matrix -> string =
        fun i vals ->
          let open Lit.Vector in
          match vals with
          | [] -> ""
          | [v] ->
              let ps = expr_placeholders ~placeholder i v in
              sprintf "(%s)" (String.concat ", " ps)
          | v::vs ->
              let ps = expr_placeholders ~placeholder i v in
              let n = i + List.length ps in
              sprintf "(%s)\n%s" (String.concat ", " ps) (pss n vs) in
      pss 1 values

  let params_of_values
    : type a. (a, 'b, 'm, 'n) Lit.Vector.matrix -> Param.t list =
    fun values ->
      List.rev @@
        Lit.Vector.matrix_fold_left
          { Lit.Vector.f = fun acc e -> Lit.to_param e :: acc }
          []
          values

  let seal ~placeholder ~query (I { table; fields; values }) =
    let s =
      sprintf "%s INTO %s (%s) VALUES\n%s"
        query
        (Table.to_string table)
        (join_fields fields)
        (placeholders ~placeholder values) in
    s, params_of_values values
end
