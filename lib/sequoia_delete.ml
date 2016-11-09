open Printf
open Sequoia_common

module Driver = Sequoia_driver
module Expr   = Sequoia_expr
module Param  = Sequoia_param
module Table  = Sequoia_table

module type S = sig
  type _ t

  val delete : ?where:('t Table.t -> bool Expr.t) -> from:'t Table.t -> 't t
  val seal : handover:Expr.handover -> 't t -> string * Param.t list

  module Expr : module type of Sequoia_query_common.UpdateDeleteExpr
end

module Make (D : Driver.S) : S = struct
  open Sequoia_query_common

  type 't t =
    | D : { table : 't Table.t; where : 'a Expr.t option } -> 't t

  let delete ?where ~from =
    let where =
      match where with
      | Some expr -> Some (expr from)
      | None -> None in
    D { table = from; where = where }

  let seal ~handover (D { table; where }) =
    let st = { blank_step with pos = 1 } in
    let st = build_where ~placeholder:D.placeholder ~handover st where in
    let s = sprintf "DELETE FROM %s" (Table.to_string table) in
    let repr = join_lines [ s; st.repr ] in
    repr, st.params

  module Expr = UpdateDeleteExpr
end
