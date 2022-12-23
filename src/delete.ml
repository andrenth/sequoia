open Printf
open Common

module type S = sig
  type _ t

  val delete : ?where:('t Table.t -> bool Expr.t) -> from:'t Table.t -> unit -> 't t
  val seal : handover:Expr.handover -> 't t -> string * Param.t list

  module Expr : module type of Query_common.UpdateDeleteExpr
end

module Make (D : Driver.S) : S = struct
  open Query_common

  type 't t =
    | D : { table : 't Table.t; where : 'a Expr.t option } -> 't t

  let delete ?where ~from () =
    let where =
      match where with
      | Some expr -> Some (expr from)
      | None -> None in
    D { table = from; where = where }

  let seal ~handover (D { table; where }) =
    let st = { blank_step with pos = 1 } in
    let st = build_where ~placeholder:D.placeholder ~handover st where in
    let s = sprintf "DELETE FROM %s" (Table.name table) in
    let repr = join_lines [ s; st.repr ] in
    repr, st.params

  module Expr = UpdateDeleteExpr
end
