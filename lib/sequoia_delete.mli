(** [DELETE] query builder. *)
module type S = sig
  type _ t
		(** The type of [DELETE] queries. *)

  val delete : ?where:('t Sequoia_table.t -> bool Sequoia_expr.t)
            -> from:'t Sequoia_table.t
            -> 't t
		(** Define a [DELETE] query on the table given by [from] and conditions
        specified by [where]. *)

  val seal : handover:Sequoia_expr.handover
          -> 't t
          -> string * Sequoia_param.t list
		(** Mark the end of a [DELETE] query. *)

  module Expr : module type of Sequoia_query_common.UpdateDeleteExpr
		(** Valid expressions for use in [DELETE] query. *)
end

module Make (D : Sequoia_driver.S) : S
	(** Functor that generates a [DELETE] query builder for the given driver. *)
