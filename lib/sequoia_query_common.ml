open Printf
open Sequoia_common

module Expr  = Sequoia_expr
module Field = Sequoia_field
module Table = Sequoia_table

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
      | Field (fld, _) ->
          { repr = Field.to_string fld
          ; params = []
          ; pos = st.pos
          }
      | Foreign ((fld, _), _) ->
          { repr = Field.to_string fld
          ; params = []
          ; pos = st.pos
          }
      | e ->
          Expr.build ~placeholder ~handover st e
end
