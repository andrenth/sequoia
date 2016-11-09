module type S = sig
  type _ t

  val delete : ?where:('t Sequoia_table.t -> bool Sequoia_expr.t)
            -> from:'t Sequoia_table.t
            -> 't t

  val seal : handover:Sequoia_expr.handover
          -> 't t
          -> string * Sequoia_param.t list

  module Expr : module type of Sequoia_query_common.UpdateDeleteExpr
end

module Make (D : Sequoia_driver.S) : S
