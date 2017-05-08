(** [DELETE] query builder. *)
module type S = sig
  type _ t
		(** The type of [DELETE] queries. *)

  val delete : ?where:('t Table.t -> bool Expr.t)
            -> from:'t Table.t
            -> 't t
		(** Define a [DELETE] query on the table given by [from] and conditions
        specified by [where]. *)

  val seal : handover:Expr.handover
          -> 't t
          -> string * Param.t list
		(** Mark the end of a [DELETE] query. *)

  module Expr : module type of Query_common.UpdateDeleteExpr
		(** Valid expressions for use in [DELETE] query. *)
end

module Make (D : Driver.S) : S
	(** Functor that generates a [DELETE] query builder for the given driver. *)
