(** [UPDATE] query builder. *)

module type S = sig
  type _ t
    (** The type of [UPDATE] queries. *)

  val seal : handover:Sequoia_expr.handover -> 't t
          -> string * Sequoia_param.t list
		(** Mark the end of a [UPDATE] query. *)

  (** Definitions for [ORDER BY] clauses. *)
  module OrderBy : sig
    type order
      (** The order type, i.e. ascending or descending. *)

    type ('t, 'a) expr = 'a Sequoia_expr.t * order
      (** Expressions in [ORDER BY] clauses must specify an order. *)

    module Expr : sig
      type ('t, 'a) mk = 't Sequoia_table.t -> 'a Sequoia_expr.t * order
        (** Type of [ORDER BY] expressions builders. *)

      module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk
        (** Vector of [ORDER BY] expressions builders. *)

      val asc : ('t Sequoia_table.t -> 'a Sequoia_expr.t)
             -> 't Sequoia_table.t
             -> ('t, 'a) expr
        (** Order by the given expression ascendingly. *)

      val desc : ('t Sequoia_table.t -> 'a Sequoia_expr.t)
              -> 't Sequoia_table.t
              -> ('t, 'a) expr
        (** Order by the given expression descendingly. *)
    end

    module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) expr
      (** Vector of [ORDER BY] expressions. *)

    (** {3 Functions useful for driver writers} *)

    val vectormk_to_vector : 't Sequoia_table.t
                          -> ('t, 'a, 'n) Expr.Vector.t
                          -> ('t, 'a, 'n) Vector.t
      (** Convert a vector of expression builders into a vector of expressions
          when given a [source]. *)
  end

  (** [UPDATE] query expressions. *)
  module Expr : sig
    include module type of Sequoia_query_common.UpdateDeleteExpr

    type ('t, 'a) mk = 't Sequoia_table.t -> ('t, 'a) Sequoia_expr.expr
      (** The type of [UPDATE] expression builders. *)

    module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk
      (** Vector of [UPDATE] expression builders. *)
  end

  type ('t, 'a) mk = ('t, 'a) Sequoia_field.t
                   * ('t Sequoia_table.t -> ('t, 'a) Sequoia_expr.expr)
    (** The type of [UPDATE] builders. *)

  module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk
    (** Vector of [UPDATE] builders. *)

  val update : 't Sequoia_table.t
            -> set:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
            -> 't t
    (** Defines an [UPDATE] query for the given field/expression pairs. *)

  val where : ('t Sequoia_table.t -> 'a Sequoia_expr.t) -> 't t -> 't t
    (** Defines a [WHERE] clause for an [UPDATE] query. *)

  val order_by : ('t, 'a, 'n Sequoia_vector.Nat.s) OrderBy.Expr.Vector.t
              -> 't t
              -> 't t
    (** Defines an [ORDER BY] clause for an [UPDATE] query. *)

  val limit : ?offset:int -> int -> 't t -> 't t
    (** Defines a [LIMIT] clause for an [UPDATE] query. *)
end

module Make (D : Sequoia_driver.S) : S
	(** Functor that generates an [UPDATE] query builder for the given driver. *)
